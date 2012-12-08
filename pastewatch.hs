{-# LANGUAGE OverloadedStrings #-}

-- | Check paste sites (as defined in PasteWatch.Sites) for content matching
-- tests from PasteWatch.Alerts
--
-- See Real World Haskell, Chap 28 for the STM code
--

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as S
import qualified Data.HashMap.Strict as Map
import GHC.Conc.Sync (ThreadId)

import PasteWatch.Alert  (checkContent)
import PasteWatch.Config (config, siteConfigs)
import PasteWatch.Sites  (doCheck, getNewPastes)
import PasteWatch.Types
import PasteWatch.Utils

---------------------------------------------------
-- Low level functions
---------------------------------------------------

-- | email the admins
emailFile::URL -> String -> Worker ()
emailFile url content = do
    conf <- ask
    liftIO $ do
        print $ "Alerting on URL " ++ url
        sendEmail (sender conf)
               (recipients conf)
               (domain conf)
               (smtpServer conf)
               "Pastebin alert"
               (url ++ "\n\n" ++ content)

---------------------------------------------------
-- Worker thread code
-- (deals with checking individual pastes)
---------------------------------------------------

-- | Check one url, resheduling on failure if required
checkone::Worker ()
checkone = forever $ do
    chan   <- gets jobsQueue
    checkf <- gets checkFunction
    job    <- liftIO $ atomically $ readTChan chan
    let url = paste job
    liftIO $ print $ "Checking " ++ url
    result <- runEitherT $ tryIO $ doCheck (site job) url checkf
    case result of
        Left _  -> return ()
        Right r -> case r of
            Left e -> case e of
                RETRY    -> reschedule job
                FAILED   -> return ()
                NO_MATCH -> return ()
            Right content -> emailFile url content

-- | Put task back on a queue for later
-- unless we've already seen it 5 times
reschedule::Task -> Worker ()
reschedule job = do
    liftIO $ print $ "Rescheduling " ++ show job
    chan <- gets jobsQueue
    liftIO $ unless (ntimes' > 5) $ do
        threadDelay $ 627000000 * ntimes'   -- 5 mins + a bit
        atomically $ writeTChan chan Task {
                        site = site job,
                        ntimes = ntimes',
                        paste = paste job
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
    chan <- gets linkQueue
    liftIO . atomically $ mapM_ (writeTChan chan . Task sitet 0) links

-- | Loop forever, pulling the new pastes for a give site and
-- putting into the queue for other threads to pick up
controlThread::Control ()
controlThread = forever $ do
    sc <- ask
    let sitet = siteType sc
    let dtime = delayTime sc * 1000000
    getURLs sitet
    liftIO $ threadDelay dtime
    pruneURLs
  where
    getURLs sitet = do
        urls <- runEitherT $ tryIO $ getNewPastes sitet
        case urls of
            Left _  -> return ()
            Right u -> filterM notSeenURL u >>= sendJobs sitet

-- | Spawn a control thread
spawnControlThread::MonadIO m => TChan Task -> SiteConfig -> m ThreadId
spawnControlThread jobs sc =
    liftIO $ forkIO
        (void $ execControl controlThread (JobState jobs Map.empty) sc)

-- | Spawn a new worker thead
spawnWorkerThread::MonadIO m => TChan Task
                             -> Config
                             -> (S.ByteString -> Bool)
                             -> m ThreadId
spawnWorkerThread jobs conf checkf =
    liftIO $ forkIO
        (void $ execWorker checkone (WorkerState jobs checkf) conf)

---------------------------------------------------
-- | main
--
-- After starting all sub-threads, this thread just spins
---------------------------------------------------

main :: IO ()
main = do
    jobs <- newTChanIO
    replicateM_ (nthreads config)
        (spawnWorkerThread jobs config
            (checkContent (alertStrings config) (alertStringsCI config)))
    mapM_ (spawnControlThread jobs) siteConfigs
    forever $ threadDelay (360000 * 1000000)
