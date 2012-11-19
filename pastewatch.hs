{-# LANGUAGE OverloadedStrings #-}

-- | Check pastebin new pastes for content matching checkContent
-- 
-- See Real World Haskell, Chap 28 for the STM code
-- 

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (onException)
import Control.Monad.State
import Data.ByteString as S hiding (map)
import Data.Map as Map
import Data.Time.Clock as Time
import System.Exit (exitWith, ExitCode(..))

import PasteWatch.Alert (checkContent)
import PasteWatch.Config (config)
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
    --print ("Alerting on URL " ++ url)
    sendEmail (sender config) 
               (recipients config)
               (domain config) 
               (smtpServer config)
               "Pastebin alert" 
               (url ++ "\n\n" ++ content)

-- grab the given URL and perform our check
runCheck::Site -> URL -> (S.ByteString->Bool) -> IO ()
runCheck site url checkf = do
    --print $ "Checking " ++ url
    result <- doCheck site url checkf
    case result of
        Just content -> emailFile url content
        Nothing      -> return ()

-- wrapper to grab a url, ignoring any exceptions raised
checkone::TChan Task -> (S.ByteString->Bool) -> IO ()
checkone jobs checkf = do
    job <- atomically $ readTChan jobs
    case job of
        Done           -> return ()
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

-- | If we have seen a link, return False.  Otherwise, record that we
-- have seen it, and return True.
seenURL::URL -> Job Bool
seenURL url =  do
    seen <- Map.notMember url `liftM` gets linksSeen
    insertURL url
    return seen

-- put urls into the shared queue
sendJobs::Site -> [URL] -> Job ()
sendJobs site links = do
    chan <- gets linkQueue
    liftIO . atomically $ mapM_ (writeTChan chan . Check site) links

-- prune all URLs first added more than ten minutes (600s) ago
-- XXX fix me magic number here
pruneURLs::Job ()
pruneURLs = do
    now <- liftIO Time.getCurrentTime
    modify $ \s -> s { linksSeen = 
        Map.filter (\x -> diffUTCTime now x < 600) (linksSeen s) }

-- our main thread. Loop forever, pulling the new pastes and 
-- putting into the queue for other threads to pick up
runMain::Job ()
runMain = do
        mapM_ getURLs sites
        liftIO $ threadDelay (delayTime config)
        pruneURLs
        runMain
  where
    getURLs site = do
        urls <- getNewPastes site
        filterM (seenURL) urls >>= sendJobs site

-- main()
main :: IO ()
main = do
    jobs <- newTChanIO
    forkN (nthreads config) (checkPaste jobs)
    _  <- (execStateT . runJob) runMain (JobState jobs Map.empty)
    return ()
