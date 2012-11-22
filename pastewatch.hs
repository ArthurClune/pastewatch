{-# LANGUAGE OverloadedStrings #-}

-- | Check pastebin new pastes for content matching checkContent
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
import qualified Data.Time.Clock as Time
import GHC.Conc.Sync (ThreadId)
import System.Exit (exitWith, ExitCode(..))

import PasteWatch.Alert (checkContent)
import PasteWatch.Config (config, siteConfigs)
import PasteWatch.Sites
import PasteWatch.Types 
import PasteWatch.Utils (sendEmail)

-- forkN k actions
forkN::Int -> IO () -> IO ()
forkN k action = 
    replicateM_ k . forkIO $ action 

-- email file to the admins
emailFile::URL -> String -> IO()
emailFile url content = do
    print ("Alerting on URL " ++ url)
    sendEmail (sender config) 
               (recipients config)
               (domain config) 
               (smtpServer config)
               "Pastebin alert" 
               (url ++ "\n\n" ++ content)

-- grab the given URL and perform our check
runCheck::Site -> URL -> (S.ByteString->Bool) -> IO ()
runCheck site url checkf = do
    print $ "Checking " ++ url
    result <- doCheck site url checkf
    case result of
        Just content -> emailFile url content
        Nothing      -> return ()

-- wrapper to grab a url, ignoring any exceptions raised
checkone::TChan Task -> (S.ByteString->Bool) -> IO ()
checkone jobs checkf = do
    job <- atomically $ readTChan jobs
    case job of
        Check site url -> onException (runCheck site url checkf) (exitWith (ExitFailure 1))

-- grab a url from the job queue, download and check
checkPaste::TChan Task -> IO ()
checkPaste jobs = forever $ checkone jobs checkContent

-- | Add a link to the map of links we have seen
-- Key is the url, value is timestamp
insertURL :: URL -> Job ()
insertURL l = do
    time <- liftIO Time.getCurrentTime
    modify $ \s ->
      s { linksSeen = Map.insert l time (linksSeen s) }

-- | If we have not seen a link before, return True and record that we have
-- now seen it.  Otherwise, return False.
notSeenURL::URL -> Job Bool
notSeenURL url =  do
    seen <- Map.member url `liftM` gets linksSeen
    if seen 
        then return False
        else insertURL url >> return True

-- put urls into the shared queue
sendJobs::Site -> [URL] -> Job ()
sendJobs site links = do
    chan <- gets linkQueue
    liftIO . atomically $ mapM_ (writeTChan chan . Check site) links

-- prune all URLs first added more than pruneTime ago
pruneURLs::Job ()
pruneURLs = do
    now <- liftIO Time.getCurrentTime
    sc <- ask
    let pTime = pruneTime sc
    let filterf x = Time.diffUTCTime now x < pTime in
        modify $ \s -> s { linksSeen = 
            Map.filter filterf (linksSeen s) }

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
        urls <- getNewPastes site
        filterM notSeenURL urls >>= sendJobs site

-- Spawn one control thread per site
spawnControlThreads::MonadIO m => TChan Task -> SiteConfig -> m ThreadId
spawnControlThreads jobs sc = 
    liftIO $ forkIO
        (void $ execJob runControl (JobState jobs Map.empty) sc)

-- main()
main :: IO ()
main = do
    jobs <- newTChanIO
    forkN (nthreads config) (checkPaste jobs)
    mapM_ (spawnControlThreads jobs) siteConfigs
    threadDelay (360000 * 1000000)
    return ()
