{-# LANGUAGE OverloadedStrings #-}

-- | Check paste sites (as defined in PasteWatch.Sites) for content matching 
-- tests from PasteWatch.Alerts
-- 
-- See Real World Haskell, Chap 28 for the STM code
-- 

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (onException)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as S
import qualified Data.Map as Map
import GHC.Conc.Sync (ThreadId)
import System.Exit (exitWith, ExitCode(..))

import PasteWatch.Alert  (checkContent)
import PasteWatch.Config (config, siteConfigs)
import PasteWatch.Sites  (doCheck, getNewPastes)
import PasteWatch.Types 
import PasteWatch.Utils

-- email file to the admins
emailFile::URL -> String -> IO ()
emailFile url content = do
    print ("Alerting on URL " ++ url)
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

-- grab the given URL and perform our check
runCheck::Task -> (S.ByteString->Bool) -> IO ()
runCheck (Check site url) checkf = do
    print $ "Checking " ++ url
    result <- doCheck site url checkf
    case result of
        Just content -> emailFile url content
        Nothing      -> return ()

-- wrapper to grab a url, ignoring any exceptions raised
checkone::TChan Task -> (S.ByteString->Bool) -> IO ()
checkone jobs checkf = do
    job <- atomically $ readTChan jobs
    onException (runCheck job checkf) (exitWith (ExitFailure 1))

-- grab a url from the job queue, download and check
checkPaste::TChan Task -> IO ()
checkPaste jobs = forever $ checkone jobs checkContent

---------------------------------------------------
-- Master thread code
-- (one thread per site)
---------------------------------------------------

-- put urls into the shared queue
sendJobs::Site -> [URL] -> Job ()
sendJobs site links = do
    chan <- gets linkQueue
    liftIO . atomically $ mapM_ (writeTChan chan . Check site) links

-- Loop forever, pulling the new pastes for a give site and 
-- putting into the queue for other threads to pick up
runControl::Job ()
runControl = do
    sc <- ask
    let site = siteType sc
    let dtime = delayTime sc
    getURLs site
    liftIO $ threadDelay dtime
    pruneURLs
    runControl
  where
    getURLs site = do
        urls <- liftIO $ getNewPastes site
        filterM notSeenURL urls >>= sendJobs site

-- Spawn a control thread
spawnControlThreads::MonadIO m => TChan Task -> SiteConfig -> m ThreadId
spawnControlThreads jobs sc = 
    liftIO $ forkIO
        (void $ execJob runControl (JobState jobs Map.empty) sc)

---------------------------------------------------
-- main
---------------------------------------------------

main :: IO ()
main = do
    jobs <- newTChanIO
    forkN (nthreads config) (checkPaste jobs)
    mapM_ (spawnControlThreads jobs) siteConfigs
    forever $ threadDelay (360000 * 1000000)
