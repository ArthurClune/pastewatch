{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

-- | Check paste sites (as defined in PasteWatch.Sites) for content matching
-- given strings
module PasteWatch.Core
    (
        pastewatch
    ) where

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
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime        (getZonedTime)
import qualified Database.MongoDB as DB
import           Database.MongoDB           ( (=:) )
import           GHC.Conc                   (numCapabilities)
import           System.Exit                (exitWith, ExitCode(..))
import           System.IO                  (stderr)
import           System.Log.Logger
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog  (openlog, Facility(..), Option(..))
import           System.Log.Handler         (setFormatter)
import           System.Log.Formatter
import           System.Random
import qualified System.Remote.Counter as SRC
import qualified System.Remote.Gauge as SRG
import qualified System.Remote.Label as SRL
import           System.Remote.Monitoring   (forkServer, getLabel, Server)

import PasteWatch.Config (parseArgs, parseConfig)
import PasteWatch.Sites  (checkContent, createCounters, createGauges, doCheck,
                          getNewPastes, siteConfigs)
import PasteWatch.Types
import PasteWatch.Utils  (sendEmail)

---------------------------------------------------
-- Low level functions
---------------------------------------------------

-- | email the admins
-- If we get an error, just throw away this message, report the error and keep going
-- The smtp standard requies '\r\n' for EOL not '\n' so we fix that up here as well
emailFile::Bool
         -> ( ResultCode->IO () )
         -> URL
         -> MatchText
         -> PasteContents
         -> Worker ()
emailFile False _ _ _ _ = return ()
emailFile True sendResult url match (PasteContents content) = do
    UserConfig{..} <- ask
    liftIO $ do
        debugM "pastewatch.emailFile" $ "Sending email for " ++ show url
        res <- runEitherT $ tryIO $ sendEmail sender recipients smtpServer match (show url ++ "\r\n\r\n" ++ mailbody)
        case res of
            Left  e -> do
                        errorM "pastewatch.emailFile" $ "Error sending email " ++ show e
                        sendResult SMTP_ERR
            Right _ -> return ()
  where
    mailbody = T.unpack $ T.unlines $ map (`T.snoc` '\r') (T.lines content)

-- | Store matching paste in DB
-- If we get a DB error, kill the program
storeInDB::Maybe DB.Pipe
         -> Site
         -> URL
         -> Maybe MatchText
         -> PasteContents
         -> Worker ()
storeInDB Nothing _ _ _ _ = return ()

storeInDB (Just pipe) site url match content =
    case match of
        Nothing -> storeInDB' (""::T.Text)
        Just m  -> storeInDB' m
  where
    storeInDB' m =
        do
            WorkerState{..} <- get
            ts   <- liftIO Time.getCurrentTime
            let paste = ["schemaVer" =: (1::Int),
                         "ts"        =: ts,
                         "url"       =: url,
                         "content"   =: content,
                         "tags"      =: [m],
                         "alertedOn" =: m,
                         "site"      =: site
                        ]
            liftIO $ do
                debugM "pastewatch.storeInDB" $ show url
                res <- runEitherT $ DB.access pipe DB.master db (DB.insert_ "pastes" paste)
                case res of
                    Left  _ -> do
                                errorM "pastewatch.storeInDB" $ "Error storing in DB "
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
    liftIO $ SRG.set seenHashLen (fromIntegral $ Map.size linksSeen)
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
        (RETRY, _, _) -> reschedule job -- reschedule handles logging
        (TESTED, _, Just content) -> do
            sendResult TESTED
            when logAllToDB $
                storeInDB dbPipe site paste Nothing content
        (STACK_OVERFLOW, _, _) -> liftIO $ do
            errorM "pastewatch.checkone" $ "Stackover flow for " ++ show paste
            sendResult STACK_OVERFLOW
        (SUCCESS, Just match, Just content) -> do
            liftIO $ infoM "pastewatch.checkone" $ show paste ++ " matches " ++ show match
            sendResult SUCCESS
            emailFile alertToEmail sendResult paste match content
            storeInDB dbPipe site paste (Just match) content
        (r, _, _) -> sendResult r


-- | Put task back on a queue for later
-- unless we've already seen it 5 times
reschedule::Task -> Worker ()
reschedule Task{..} = do
    liftIO $ infoM "pastewatch.reschedule" $ show paste
    chan  <- gets jobsQueue
    liftIO . atomically $ writeTChan rStatus RETRY
    liftIO $ unless (ntimes' > 5) $ do
        threadDelay $ 627000000 * ntimes'   -- 5 mins + a bit
        atomically $ writeTChan chan Task{ ntimes = ntimes', .. }
  where
    ntimes' = ntimes + 1

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
    checkf = checkContent (alertRe conf)

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
    setUpLogging (logTo config) (logLevel config)
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
        r <- runEitherT $ scriptIO $ DB.connect host
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

    -- log to stdout and/or syslog
    setUpLogging logdest level =
        case logdest of
            LOGBOTH   -> do
                             setupStderr
                             setupSyslog addHandler
            LOGSYSLOG -> setupSyslog (\s -> setHandlers [s])
            LOGSTDERR -> setupStderr
      where
        setupStderr = do
            ha <- streamHandler stderr DEBUG >>= \h -> return $
                    setFormatter h (simpleLogFormatter "[$time : $loggername] $msg")
            getRootLogger >>= \r -> saveGlobalLogger $ setLevel level . setHandlers [ha] $ r
        setupSyslog handlerf = do
            s <- openlog "pastewatch" [PID] DAEMON DEBUG
            updateGlobalLogger rootLoggerName (setLevel level . handlerf s)


