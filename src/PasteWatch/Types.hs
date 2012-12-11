{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Core types
module PasteWatch.Types
    (
        Control,
        Domain,
        Email,
        ErrorCode(..),
        Host,
        JobState(..),
        Task(..) ,
        Site(..),
        SiteConfig(..),
        URL,
        UserConfig(..),
        Worker,
        WorkerState(..),
        execControl,
        runControl,
        execWorker,
        runWorker
    ) where

import Control.Concurrent.STM (TChan)
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Char8 as S
import Data.HashMap.Strict as Map
import Data.Time.Clock as Time
import System.Random

--------------------------------------------------------------
-- simple types
--------------------------------------------------------------

-- | A domain domain (e.g. "example.com")
type Domain = String

-- | Email address ("Real Name", "email address")
type Email = String

-- | Custom errors when getting paste
data ErrorCode = NO_MATCH | FAILED | RETRY deriving (Eq, Show)

-- | A hostname (e.g. smtp.example.com)
type Host = String

-- | Simple type to store URLs
type URL = String

--------------------------------------------------------------
-- Config data structures
--------------------------------------------------------------

-- | The different sites
--
-- doCheck and getnewPastes in PasteWatch.Sites must
-- be implemented for every new site
data Site = Pastebin | Pastie | SkidPaste | Slexy deriving (Show, Eq)

-- | Per-site config
data SiteConfig = SiteConfig {
    -- Type of site
    siteType  :: !Site,
    -- | Time to pause main thread each loop (seconds)
    -- Adjust depending on the volume of pastes on the target site
    delayTime :: !Int,
    -- time (in seconds) after which we remove URLs
    -- from the seen list for this site
    pruneTime :: !Time.NominalDiffTime
} deriving (Show, Eq)


-- | Type to hold user config
data UserConfig = UserConfig {
   -- | Strings to alert on (case sensitive) if seen in a paste
  alertStrings   :: ![S.ByteString],
  -- | Strings to alert on (case insensitive) if seen in a paste
  alertStringsCI :: ![S.ByteString],
   -- | domain that email comes from
  domain         :: !Domain,
  -- | Number of Haskell (lightweight) threads to use
  -- for downloading. Total number of threads used
  -- equals nthreads + number of sites + 1
  -- N of these are mapped onto M OS threads where M is set
  -- by the +RTS -N option (see README.md)
  nthreads       :: !Int,
  -- pauseMax
  -- we wait for a random number between 0 and pauseMax seconds before
  -- downloading the URL
  -- Use this to stop sites blocking downloads due to too many requests in too short
  -- a time period
  pauseMax      :: !Int,
  -- | Send alert emails to?
  recipients     :: ![Email],
  -- | Send alert emails as?
  sender         :: !Email,
  -- | SMTP server to use to send email via
  smtpServer     :: !Host
}

--------------------------------------------------------------
-- Monad transformer stacks
--------------------------------------------------------------

-- | Monad stack wrapping State and Reader
-- This the monad that the control threads run in
newtype Control a  = Control {
      runControl :: StateT JobState (ReaderT SiteConfig IO) a
    } deriving (Monad, MonadReader SiteConfig, MonadState JobState, MonadIO)

execControl::Control a -> JobState -> SiteConfig -> IO JobState
execControl s = runReaderT . (execStateT . runControl) s

-- | Monad stack wrapping State and Reader
-- This the monad that the worker threads run in
newtype Worker a = Worker {
    runWorker :: StateT WorkerState (ReaderT UserConfig IO) a
  } deriving (Monad, MonadReader UserConfig, MonadState WorkerState, MonadIO)

execWorker::Worker a -> WorkerState -> UserConfig -> IO WorkerState
execWorker s = runReaderT . (execStateT . runWorker) s

--------------------------------------------------------------
-- Data to pass between worker and control threads
--------------------------------------------------------------

-- | State for our channel: list of done URLs and
-- config for the site we are doing
data JobState = JobState {
    -- STM queue for passing URLs around
    linkQueue :: TChan Task,
    -- per thread map of URLs seen for this site
    linksSeen :: Map.HashMap URL Time.UTCTime
}

data WorkerState = WorkerState {
  -- our shared queue
  jobsQueue     :: TChan Task,
  -- the check function
  -- this is static for now, but in time we want this to change, so put it in
  -- State not Reader
  checkFunction :: S.ByteString -> Bool,
  -- random number generator for each thread
  randGen       :: StdGen
}

-- | A Task is a URL to check
data Task = Task {
  -- Type of site
  site   :: !Site,
  -- How many time have we checked this URL already?
  ntimes :: !Int,
  -- URL of the paste to check
  paste  :: !URL
} deriving (Eq, Show)
