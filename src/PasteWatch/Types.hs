{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
-- | Core types
module PasteWatch.Types
    (
        Config(..),
        Domain,
        Email,
        ErrorCode(..),
        Host,
        Job,
        JobState(..),
        Task(..) ,
        Site(..),
        SiteConfig(..),
        URL,
        execJob,
        runJob
    ) where

import Control.Concurrent.STM (TChan)
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Char8 as S
import Data.HashMap.Strict as Map
import Data.Time.Clock as Time

-- | Type to hold user config
data Config = Config {
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
  -- | Send alert emails as?
  sender         :: !Email,
  -- | Send alert emails to?
  recipients     :: ![Email],
  -- | SMTP server to use to send email via
  smtpServer     :: !Host
}

-- | A domain domain (e.g. "example.com")
type Domain = String

-- | Email address ("Real Name", "email address")
type Email = (String, String)

-- | Custom errors when getting paste
-- NO_MATCH Doesn't match our check
-- FAILED Failed permentently e.g. 404
-- RETRY Temporary failure.
data ErrorCode = NO_MATCH | FAILED | RETRY deriving (Eq, Show)

-- | A hostname (e.g. smtp.example.com)
type Host = String

-- | Monad stack wrapping State
newtype Job a  = Job {
      runJob:: StateT JobState (ReaderT SiteConfig IO) a
    } deriving (Monad, MonadReader SiteConfig, MonadState JobState, MonadIO)

execJob::Job a -> JobState -> SiteConfig -> IO JobState
execJob s = runReaderT . (execStateT . runJob) s

-- | State for our channel: list of done URLs and
-- config for the site we are doing
data JobState = JobState {
    -- STM queue for passing URLs around
    linkQueue :: TChan Task,
    -- per thread map of URLs seen for this site
    linksSeen :: Map.HashMap URL Time.UTCTime
}

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
    pruneTime :: !Time.NominalDiffTime,
    -- pauseMax
    -- we wait for a random number between 0 and pauseMax seconds before
    -- downloading the URL
    -- Use this to stop sites blocking downloads due to too many requests in too short
    -- a time period
    pauseMax  :: !Int
} deriving (Show, Eq)

-- | A Task is a URL to check
data Task = Task {
  -- Type of site
  site   :: !Site,
  -- How many time have we checked this URL already?
  ntimes :: !Int,
  -- URL of the paste to check
  paste  :: !URL
} deriving (Eq, Show)

-- | Simple type to store URLs
type URL = String
