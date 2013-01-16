{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

-- | Check paste sites (as defined in PasteWatch.Sites) for content matching
-- given strings

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
import           System.Locale              (defaultTimeLocale)
import           System.Random
import qualified System.Remote.Counter as SRC
import qualified System.Remote.Gauge as SRG
import qualified System.Remote.Label as SRL
import           System.Remote.Monitoring   (forkServer, getLabel, Server)

import PasteWatch.Alert  (checkContent)
import PasteWatch.Config (parseArgs, parseConfig)
import PasteWatch.Sites  (createCounters, createGauges, doCheck, getNewPastes, siteConfigs)
import PasteWatch.Types

---------------------------------------------------
-- Low level functions
---------------------------------------------------

storeInDB::URL -> MatchText -> PasteContents -> Worker ()
storeInDB (URL url) (MatchText match) (PasteContents content) = do
    conf <- get
    ts   <- liftIO Time.getCurrentTime
    let run = DB.access (dbPipe conf) DB.master (db conf)
    let paste = ["schemaVer" =: (1::Int),
                 "ts"        =: ts,
                 "url"       =: url,
                 "content"   =: content,
                 "tags"      =: [match],
                 "alertedOn" =: match
                ]
    liftIO $ do
        putStrLn $ "Writing to DB: URL " ++ show url ++ " matches " ++ show match
        _ <- run $ DB.insert "pastes" paste
        return ()

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
    sc  <- ask
    let filterf x = Time.diffUTCTime now x < pTime
        pTime = pruneTime sc
      in
        modify $ \s -> s { linksSeen =
            Map.filter filterf (linksSeen s) }
    gauge <- gets seenHashLen
    hash  <- gets linksSeen
    liftIO $ SRG.set gauge (Map.size hash)
    modify $ \s -> s { seenHashLen = gauge }

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
    conf <- ask
    st   <- get
    let (delayt, gen') = randomR (10000, pauseMax conf * 1000000) (randGen st)
    put st { randGen  = gen' }
    liftIO $ threadDelay delayt
    return ()

-- | Check one url, resheduling on failure if required
-- To avoid the thundering herd problem, we spin for a
-- random time before starting
checkone::Worker ()
checkone = forever $ do
    st  <- get
    job <- liftIO $ atomically $ readTChan (jobsQueue st)
    let rq = rStatus job
    let url = paste job
    pause
    liftIO $ putStrLn $ "Checking " ++ show url
    result <- runEitherT $ tryIO $ doCheck (site job) url (checkFunction st)
    case result of
        Left _  -> writeResult rq FAILED
        Right r -> case r of
            Left e -> case e of
                RETRY    -> reschedule job
                FAILED   -> writeResult rq FAILED
                NO_MATCH -> writeResult rq NO_MATCH
                SUCCESS  -> error "Bad error result code in checkone"
            Right (match, content) -> do
                liftIO . atomically $ writeTChan rq SUCCESS
                storeInDB url match content
  where
    writeResult rq code = liftIO . atomically $ writeTChan rq code

-- | Put task back on a queue for later
-- unless we've already seen it 5 times
reschedule::Task -> Worker ()
reschedule job = do
    liftIO $ putStrLn $ "Rescheduling " ++ show (paste job)
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
    chan  <- gets linkQueue
    rchan <- gets resultQueue
    liftIO . atomically $ mapM_ (writeTChan chan . Task sitet 0 rchan) links

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
        urls <- runEitherT $ tryIO $ getNewPastes sitet
        case urls of
            Left _  -> liftIO $ threadDelay (etime * 1000000)
            Right u -> filterM notSeenURL u >>= sendJobs sitet

-- | Update the counters for this site
counterThread::TChan ResultCode -> Counters -> IO ()
counterThread chan (Counters {tested, matched, retries, failed}) =
    forever $ do
        result <- atomically $ readTChan chan
        case result of
            SUCCESS  -> do
                SRC.inc tested
                SRC.inc matched
            RETRY    -> SRC.inc retries
            NO_MATCH -> SRC.inc tested
            FAILED   -> SRC.inc failed

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
spawnWorkerThread::TChan Task
                -> UserConfig
                -> DB.Pipe
                -> Int
                -> IO ThreadId
spawnWorkerThread jobs conf dbPipe seed =
    forkIO
        (void $ execWorker checkone (WorkerState jobs checkf (mkStdGen seed) dbPipe dbName') conf)
  where
    dbName' = dbName conf
    checkf = checkContent (alertStrings conf) (alertStringsCI conf)

---------------------------------------------------
-- | main
--
-- After starting all sub-threads, this thread just spins
---------------------------------------------------
main :: IO ()
main = do
    file   <- parseArgs
    config <- parseConfig file
    jobs   <- newTChanIO
    seed   <- newStdGen
    ekg    <- forkServer "localhost" 8000
    setLabels ekg
    dbPipe <- DB.runIOE $ DB.connect $ dbHost config
    let seeds = randomlist (nthreads config) seed
    mapM_ (spawnWorkerThread jobs config dbPipe) seeds
    mapM_ (spawnControlThread ekg jobs) [minBound .. maxBound]
    forever $ threadDelay (360000 * 1000000)
  where
    randomlist n = take n . unfoldr (Just . random)
    setLabels ekg = do
        stLabel <- getLabel "Start time" ekg
        paramLabel <- getLabel "# cores" ekg
        startTime  <- getZonedTime
        SRL.set stLabel $ T.pack $ formatTime defaultTimeLocale "%c" startTime
        SRL.set paramLabel $ T.pack $ show numCapabilities
