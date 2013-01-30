{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

-- | Check paste sites (as defined in PasteWatch.Sites) for content matching
-- given strings
module PasteWatch.Core
    (
        pastewatch
    ) where

import           Prelude hiding (catch)

import           Control.Concurrent         (forkIO, threadDelay, ThreadId)
import           Control.Concurrent.STM
import           Control.DeepSeq            ( ($!!) )
import           Control.Error
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.HashMap.Strict as Map
import           Data.List                  (unfoldr)
import           Data.Maybe                 (fromJust)
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import           Data.Time.Format           (formatTime)
import           Data.Time.LocalTime        (getZonedTime)
import qualified Database.MongoDB as DB
import           Database.MongoDB           ( (=:) )
import           GHC.Conc                   (numCapabilities)
import           System.Exit                (exitWith, ExitCode(..))
import           System.IO                  (stderr)
import           System.Locale              (defaultTimeLocale)
import           System.Log.Logger
import           System.Log.Handler.Simple
import           System.Log.Handler         (setFormatter)
import           System.Log.Formatter
import           System.Random
import qualified System.Remote.Counter as SRC
import qualified System.Remote.Gauge as SRG
import qualified System.Remote.Label as SRL
import           System.Remote.Monitoring   (forkServer, getLabel, Server)

import PasteWatch.Alert  (checkContent)
import PasteWatch.Config (parseArgs, parseConfig)
import PasteWatch.Sites  (createCounters, createGauges, doCheck, getNewPastes, siteConfigs)
import PasteWatch.Types
import PasteWatch.Utils  (sendEmail)

---------------------------------------------------
-- Low level functions
---------------------------------------------------

-- | email the admins
-- If we get an error, just throw away this message, report the error and keep going
emailFile::Bool
         -> ( ResultCode->IO () )
         -> URL
         -> MatchText
         -> PasteContents
         -> Worker ()
emailFile False _ _ _ _ = return ()
emailFile True sendResult url match content = do
    UserConfig{..} <- ask
    liftIO $ do
        infoM "pastewatch.emailFile" $ show url ++ " matches " ++ show match
        res <- runEitherT $ tryIO $ sendEmail sender recipients domain smtpServer
                                       ("Pastebin alert. Match on " ++ show match)
                                       (show url ++ "\n\n" ++ show content)
        case res of
            Left  e -> do
                        errorM "pastewatch.emailFile" $ "Error sending email " ++ show e
                        sendResult SMTP_ERR
            Right _ -> return ()

-- | Store matching paste in DB
-- If we get a DB error, kill the program
storeInDB::Maybe DB.Pipe
         -> Site
         -> URL
         -> Maybe MatchText
         -> PasteContents
         -> Worker ()
storeInDB Nothing _ _ _ _ = return ()
storeInDB (Just pipe) site url (Just match) content =
    storeInDB' pipe site url match content

storeInDB (Just pipe) site url Nothing content =
    storeInDB' pipe site url (""::T.Text) content

storeInDB' pipe site url match content =
    do
        WorkerState{..} <- get
        ts   <- liftIO Time.getCurrentTime
        let run = DB.access pipe DB.master db
        let paste = ["schemaVer" =: (1::Int),
                     "ts"        =: ts,
                     "url"       =: url,
                     "content"   =: content,
                     "tags"      =: [match],
                     "alertedOn" =: match,
                     "site"      =: site
                    ]
        liftIO $ do
            if match == ""
                then debugM "pastewatch.storeInDB" $ show url
                else infoM  "pastewatch.storeInDB" $ show url ++ " matches " ++ show match
            res <- run $ DB.insert "pastes" paste
            case res of
                Left  e -> do
                            errorM "pastewatch.storeInDB" $ "Error storing in DB " ++ show e
                            atomically $ putTMVar criticalError True
                Right _ -> return ()

---------------------------------------------------
-- Functions to maintain our map of urls and time
-- seen
---------------------------------------------------

-- | Add a link to the map of links we have seen
-- Key is the url, value is timestamp
insertURL :: URL -> Control ()
insertURL l = do
    gauge <- gets seenHashLen
    liftIO $ SRG.inc gauge
    time <- liftIO Time.getCurrentTime
    modify $ \s ->
      s { linksSeen = Map.insert l time (linksSeen s), seenHashLen = gauge }

-- prune all URLs first added more than pruneTime ago
pruneURLs::Control ()
pruneURLs = do
    now <- liftIO Time.getCurrentTime
    SiteConfig{..}   <- ask
    ControlState{..} <- get
    let filterf x = Time.diffUTCTime now x < pruneTime
      in
        modify $ \s -> s { linksSeen =
            Map.filter filterf linksSeen }
    liftIO $ SRG.set seenHashLen (Map.size linksSeen)
    modify $ \s -> s { seenHashLen = seenHashLen }

-- | If we have not seen a link before, return True and record that we have
-- now seen it.  Otherwise, return False.
notSeenURL::URL -> Control Bool
notSeenURL url =  do
    seen <- Map.member url `liftM` gets linksSeen
    if seen
        then return False
        else insertURL url >> return True

---------------------------------------------------
-- Worker thread code
-- (deals with checking individual pastes)
---------------------------------------------------

-- | Pause for a random period between 10,000 microseconds and pauseMax seconds
pause::Worker()
pause = do
    UserConfig{..} <- ask
    st   <- get
    let (delayt, gen') = randomR (10000, pauseMax * 1000000) (randGen st)
    put st { randGen  = gen' }
    liftIO $ threadDelay delayt
    return ()

-- | Check one url, resheduling on failure if required
-- To avoid the thundering herd problem, we spin for a
-- random time before starting
checkone::Worker ()
checkone = forever $ do
    WorkerState{..} <- get
    UserConfig{..}  <- ask
    job@Task{..} <- liftIO $ atomically $ readTChan jobsQueue
    pause
    let sendResult mess = liftIO . atomically $ writeTChan rStatus mess
    result <- liftIO $ do
        infoM "pastewatch.checkone" $ show paste
        doCheck site paste checkFunction
    case result of
        (RETRY, _, _) -> reschedule job
        (TESTED, _, Just content) -> do
            sendResult TESTED
            when logAllToDB $
                storeInDB dbPipe site paste Nothing content
        (SUCCESS, Just match, Just content) -> do
            sendResult SUCCESS
            emailFile alertToEmail sendResult paste match content
            storeInDB dbPipe site paste (Just match) content
        (r, _, _) -> sendResult r


-- | Put task back on a queue for later
-- unless we've already seen it 5 times
reschedule::Task -> Worker ()
reschedule job = do
    liftIO $ infoM "pastewatch.reschedule" $ show (paste job)
    chan  <- gets jobsQueue
    liftIO . atomically $ writeTChan (rStatus job) RETRY
    liftIO $ unless (ntimes' > 5) $ do
        threadDelay $ 627000000 * ntimes'   -- 5 mins + a bit
        atomically $ writeTChan chan Task {
                        site = site job,
                        ntimes = ntimes',
                        paste = paste job,
                        rStatus = rStatus job
                      }
  where
    ntimes' = ntimes job + 1

---------------------------------------------------
-- Master thread code
-- (one thread per site)
---------------------------------------------------

-- | Put urls into the shared queue
sendJobs::Site -> [URL] -> Control ()
sendJobs sitet links = do
    ControlState{..} <- get
    liftIO . atomically $ mapM_ (writeTChan linkQueue . Task sitet 0 resultQueue) links

-- | Loop forever, pulling the new pastes for a give site and
-- putting into the queue for other threads to pick up
-- Some sites firewall us after too many requests, so if we get an error,
-- wait then try again
controlThread::Site -> Control ()
controlThread sitet = forever $ do
    dtime <- asks delayTime
    getURLs
    liftIO $ threadDelay (dtime * 1000000)
    pruneURLs
  where
    getURLs = do
        etime <- asks errorTime
        urls  <- runEitherT $ tryIO $ getNewPastes sitet
        case urls of
            Left  e -> liftIO $ do
                                errorM "pastewatch.controlThread" $
                                       "Error retrieving list of new pastes " ++ show e
                                threadDelay (etime * 1000000)
            Right u -> filterM notSeenURL u >>= sendJobs sitet

-- | Update the counters for this site
counterThread::TChan ResultCode -> Counters -> IO ()
counterThread chan ctrs =
    forever $ do
        result <- atomically $ readTChan chan
        case result of
            SUCCESS  -> do
                incCtr SUCCESS
                incCtr TESTED
            x -> incCtr x
  where
    incCtr resultCode = SRC.inc (getCtr resultCode ctrs)

-- | spawn the control thread for a given site
spawnControlThread::Server -> TChan Task -> Site -> IO ThreadId
spawnControlThread ekg jobs sitet = do
    ctrs   <- createCounters ekg sitet
    gauge  <- createGauges ekg sitet
    rQueue <- newTChanIO
    _ <- forkIO $ counterThread rQueue ctrs
    forkIO
        (void $ execControl (controlThread sitet) (ControlState jobs Map.empty gauge rQueue)
            (fromJust $!! Map.lookup sitet siteConfigs))

-- | Spawn set of worker threads
spawnWorkerThread::TMVar Bool
                -> TChan Task
                -> UserConfig
                -> Maybe DB.Pipe
                -> Int
                -> IO ThreadId
spawnWorkerThread errorv jobs conf dbPipe seed =
    forkIO
        (void $ execWorker checkone (WorkerState errorv jobs checkf (mkStdGen seed) dbPipe dbName') conf)
  where
    dbName' = dbName conf
    checkf = checkContent (alertStrings conf) (alertStringsCI conf)

---------------------------------------------------
-- | main
--
-- After starting all sub-threads, this thread just
-- watches the errorv TMVar. If any thread writes
-- to this, shutdown
---------------------------------------------------
pastewatch :: IO ()
pastewatch = do
    errorv <- newEmptyTMVarIO
    config <- parseArgs >>= parseConfig
    setUpLogging (logLevel config)
    jobs   <- newTChanIO
    seed   <- newStdGen
    ekg    <- forkServer "localhost" 8000
    setLabels ekg
    dbPipe <- genDbPipe (logAllToDB config || alertToDB config) (dbHost config)
    let seeds = randomlist (nthreads config) seed
    infoM "pastewatch.main" "Starting"
    mapM_ (spawnWorkerThread errorv jobs config dbPipe) seeds
    mapM_ (spawnControlThread ekg jobs) [minBound .. maxBound]
    _ <- atomically $ takeTMVar errorv
    exitWith (ExitFailure 1)
  where

    dbConnect host = do
        r <- runEitherT $ scriptIO $ DB.runIOE $ DB.connect host
        case r of
            Left  e -> do
                errorM "pastewatch.pastewatch" $ "Database connection error: " ++ show e
                exitWith (ExitFailure 1)
            Right c -> return $ Just c

    genDbPipe True host = dbConnect host
    genDbPipe False _   = return Nothing

    randomlist n = take n . unfoldr (Just . random)

    setLabels ekg = do
        stLabel    <- getLabel "Start time" ekg
        paramLabel <- getLabel "# cores" ekg
        startTime  <- getZonedTime
        SRL.set stLabel $ T.pack $ formatTime defaultTimeLocale "%c" startTime
        SRL.set paramLabel $ T.pack $ show numCapabilities

    setUpLogging level = do
        ha <- streamHandler stderr DEBUG >>= \h -> return $
                setFormatter h (simpleLogFormatter "[$time : $loggername] $msg")
        getRootLogger >>= \r -> saveGlobalLogger $ setLevel level . setHandlers [ha] $ r
