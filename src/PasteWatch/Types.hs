{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Core types
module PasteWatch.Types
    (
        Config(..),
        Domain,
        Email,
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
import Data.Map as Map
import Data.Time.Clock as Time

-- | Type to hold user config
data Config = Config {
   -- | Strings to alert on (case sensitive) if seen in a paste
  alertStrings   :: [S.ByteString],
  -- | Strings to alert on (case insensitive) if seen in a paste
  alertStringsCI :: [S.ByteString],
   -- | domain that email comes from
  domain         :: Domain,
  -- | Number of Haskell (lightweight) threads to use
  -- N of these are mapped onto M OS threads where M is set
  -- by the +RTS -N option (see README.md)
  nthreads       :: Int,
  -- | Send alert emails as?
  sender         :: Email,
  -- | Send alert emails to?
  recipients     :: [Email],
  -- | SMTP server to use to send email via
  smtpServer     :: Host
}

-- | A domain domain (e.g. "example.com")
type Domain = String

-- | Email address ("Real Name", "email address")
type Email = (String, String)

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
    linkQueue::TChan Task,
    -- per thread map of URLs seen for this site
    linksSeen::Map.Map URL Time.UTCTime
} 
 
-- | The different sites
-- 
-- doCheck and getnewPastes in PasteWatch.Sites must
-- be implemented for every new site
data Site = Pastebin | Pastie | SkidPaste | Slexy deriving (Show, Eq)

-- | Per-site config
data SiteConfig = SiteConfig {
    -- Type of site
    siteType  :: Site,
    -- | Time to pause main thread each loop (seconds)
    -- Adjust depending on the volume of pastes on the target site
    delayTime :: Int,
    -- time (in seconds) after which we remove URLs 
    -- from the seen list for this site
    pruneTime::Time.NominalDiffTime
} deriving (Show, Eq)

-- | A Task is a URL to check
data Task = Check Site URL

-- | Simple type to store URLs
type URL = String        


