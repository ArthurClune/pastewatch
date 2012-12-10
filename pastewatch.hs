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
import qualified Data.HashMap.Strict as Map
import Data.List (unfoldr)
import System.Random

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


pause::Worker()
pause = do
    st <- get
    conf <- ask
    let (delayt, gen') = randomR (10000, pauseMax conf * 1000000) (randGen st)
    liftIO $ threadDelay $ delayt
    put st { randGen  = gen' }
    return ()

-- | Check one url, resheduling on failure if required
-- To avoid the thundering herd problem, we spin for a
-- random time before starting
checkone::Worker ()
checkone = forever $ do
    st <- get
    job  <- liftIO $ atomically $ readTChan (jobsQueue st)
    let url = paste job
    pause
    liftIO $ print $ "Checking " ++ url
    result <- runEitherT $ tryIO $ doCheck (site job) url (checkFunction st)
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

---------------------------------------------------
-- | main
--
-- After starting all sub-threads, this thread just spins
---------------------------------------------------

main :: IO ()
main = do
    jobs <- newTChanIO
    seed <- newStdGen
    let seeds = randomlist (nthreads config) seed
    mapM_ (spawnWorkerThread jobs config checkf) seeds
    mapM_ (spawnControlThread jobs) siteConfigs
    forever $ threadDelay (360000 * 1000000)
  where
    spawnWorkerThread jobs conf f seed = forkIO
            (void $ execWorker checkone (WorkerState jobs f (mkStdGen seed)) conf)
    spawnControlThread jobs sc = forkIO
            (void $ execControl controlThread (JobState jobs Map.empty) sc)
    checkf = checkContent (alertStrings config) (alertStringsCI config)
    randomlist n = take n . unfoldr (Just . random)
