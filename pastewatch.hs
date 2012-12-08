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

-- | email the admins
emailFile::URL -> String -> IO ()
emailFile url content = do
    print $ "Alerting on URL " ++ url
    sendEmail (sender config)
               (recipients config)
               (domain config)
               (smtpServer config)
               "Pastebin alert"
               (url ++ "\n\n" ++ content)

---------------------------------------------------
-- Worker thread code
-- (deals with checking individual pastes)
---------------------------------------------------

-- | Check one url, resheduling on failure if required
checkone::TChan Task -> (S.ByteString->Bool) -> IO ()
checkone jobs checkf = do
    job <- atomically $ readTChan jobs
    let url = paste job
    print $ "Checking " ++ url
    result <- runEitherT $ tryIO $ doCheck (site job) url checkf
    case result of
        Left _  -> return ()
        Right r -> case r of
            Left e -> case e of
                RETRY    -> reschedule jobs job
                FAILED   -> return ()
                NO_MATCH -> return ()
            Right content -> emailFile url content

-- | Put task back on a queue for later
-- unless we've already seen it 5 times
reschedule::TChan Task -> Task -> IO ()
reschedule jobs job = do
    print $ "Rescheduling " ++ show job
    unless (ntimes' > 5) $ do
        threadDelay $ 627000000 * ntimes'   -- 5 mins + a bit
        atomically $ writeTChan jobs Task {
                        site = site job,
                        ntimes = ntimes',
                        paste = paste job
                      }
  where
    ntimes' = ntimes job + 1

-- | Grab a url from the job queue, download and check
checkPaste::TChan Task -> IO ()
checkPaste jobs = forever $ checkone jobs checkContent

---------------------------------------------------
-- Master thread code
-- (one thread per site)
---------------------------------------------------

-- | Put urls into the shared queue
sendJobs::Site -> [URL] -> Job ()
sendJobs sitet links = do
    chan <- gets linkQueue
    liftIO . atomically $ mapM_ (writeTChan chan . Task sitet 0) links

-- | Loop forever, pulling the new pastes for a give site and
-- putting into the queue for other threads to pick up
runControl::Job ()
runControl = do
    sc <- ask
    let sitet = siteType sc
    let dtime = delayTime sc * 1000000
    getURLs sitet
    liftIO $ threadDelay dtime
    pruneURLs
    runControl
  where
    getURLs sitet = do
        urls <- runEitherT $ tryIO $ getNewPastes sitet
        case urls of
            Left _  -> return ()
            Right u -> filterM notSeenURL u >>= sendJobs sitet

-- | Spawn a control thread
spawnControlThreads::MonadIO m => TChan Task -> SiteConfig -> m ThreadId
spawnControlThreads jobs sc =
    liftIO $ forkIO
        (void $ execJob runControl (JobState jobs Map.empty) sc)

---------------------------------------------------
-- | main
--
-- After starting all sub-threads, this thread just spins
---------------------------------------------------

main :: IO ()
main = do
    jobs <- newTChanIO
    forkN (nthreads config) (checkPaste jobs)
    mapM_ (spawnControlThreads jobs) siteConfigs
    forever $ threadDelay (360000 * 1000000)
