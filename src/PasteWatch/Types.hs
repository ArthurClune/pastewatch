{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Core types
module PasteWatch.Types
    (
        Control,
        ControlState(..),
        Counters(..),
        Domain(..),
        Email(..),
        Host(..),
        MatchLine(..),
        PasteContents(..),
        ResultCode(..),
        Site(..),
        SiteConfig(..),
        SiteConfigs,
        Task(..),
        URL(..),
        UserConfig(..),
        Worker,
        WorkerState(..),
        execControl,
        runControl,
        execWorker,
        runWorker,
    ) where

import           Control.Concurrent.STM         (TChan)
import           Control.DeepSeq
import           Control.DeepSeq.Generics       (genericRnf)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Configurator.Types as DCT
import           Data.Hashable
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import           GHC.Exts( IsString(..) )
import           GHC.Generics
import           System.Remote.Counter          (Counter)
import           System.Remote.Gauge            (Gauge)
import           System.Random

--------------------------------------------------------------
-- simple types
--------------------------------------------------------------

-- | A domain (e.g. "example.com")
newtype Domain = Domain T.Text deriving (Eq, Generic, IsString, Show)

instance DCT.Configured Domain where
    convert (DCT.String v) = Just $ Domain v
    convert _              = Nothing

instance NFData Domain where rnf = genericRnf

-- | Email address ("Real Name", "email address")
newtype Email = Email T.Text deriving (Eq, Generic, IsString, Show)

instance DCT.Configured Email where
    convert (DCT.String v) = Just $ Email v
    convert _              = Nothing

instance NFData Email where rnf = genericRnf

-- | A hostname (e.g. smtp.example.com)
newtype Host = Host T.Text deriving (Eq, Generic, IsString, Show)

instance DCT.Configured Host where
    convert (DCT.String v) = Just $ Host v
    convert _              = Nothing

instance NFData Host where rnf = genericRnf

-- | A line in a paste that we have alerted on
newtype MatchLine = MatchLine T.Text deriving (Eq, Generic, IsString, Show)

instance NFData MatchLine where rnf = genericRnf

-- | Plain text contents of a paste
newtype PasteContents = PasteContents T.Text deriving (Eq, Generic, IsString, Show)

instance NFData PasteContents where rnf = genericRnf

-- | Custom results when getting paste
data ResultCode = SUCCESS | NO_MATCH | FAILED | RETRY deriving (Enum, Eq, Generic, Show)

instance NFData ResultCode where rnf = genericRnf

-- | Simple type to store URLs
newtype URL = URL T.Text deriving (Eq, Generic, Hashable, IsString, Show)

instance DCT.Configured URL where
    convert (DCT.String v) = Just $ URL v
    convert _              = Nothing

instance NFData URL where rnf = genericRnf

--------------------------------------------------------------
-- Config data structures
--------------------------------------------------------------

-- | The different sites
--
-- doCheck and getnewPastes in PasteWatch.Sites must
-- be implemented for every new site
data Site = Pastebin | Pastie | SkidPaste | Slexy | Snipt
                deriving (Bounded, Enum, Eq, Generic, Show)

instance Hashable Site where
    hash = fromEnum

instance NFData Site where rnf = genericRnf

-- | Per-site config
data SiteConfig = SiteConfig {
    -- | Time to pause main thread each loop (seconds)
    -- Adjust depending on the volume of pastes on the target site
    delayTime :: !Int,
    -- | Time (in seconds) to wait after a http error (e.g. network timeout)
    errorTime :: !Int,
    -- | Time (in seconds) after which we remove URLs
    -- from the seen list for this site
    pruneTime :: !Time.NominalDiffTime
} deriving (Eq, Generic, Show)

instance NFData SiteConfig where rnf = genericRnf

-- | HashMap to store site specific config
type SiteConfigs = Map.HashMap Site SiteConfig

-- | Type to hold user config
data UserConfig = UserConfig {
     -- | Strings to alert on (case sensitive) if seen in a paste
    alertStrings   :: ![T.Text],
    -- | Strings to alert on (case insensitive) if seen in a paste
    alertStringsCI :: ![T.Text],
     -- | Domain that email comes from
    domain         :: !Domain,
    -- | Number of Haskell (lightweight) threads to use
    -- for downloading. Total number of threads used
    -- equals nthreads + number of sites + 1
    -- N of these are mapped onto M OS threads where M is set
    -- by the +RTS -N option (see README.md)
    nthreads       :: !Int,
    -- | We wait for a random number between 0 and pauseMax seconds before
    -- downloading the URL
    -- Use this to stop sites blocking downloads due to too many requests in too short
    -- a time period
    pauseMax      :: !Int,
    -- | Send alert emails to?
    recipients    :: ![Email],
    -- | Send alert emails as?
    sender        :: !Email,
    -- | SMTP server to use to send email via
    smtpServer    :: !Host
}

--------------------------------------------------------------
-- Monad transformer stacks
--------------------------------------------------------------

-- | Monad stack wrapping State and Reader
-- This the monad that the control threads run in
newtype Control a  = Control {
      runControl :: StateT ControlState (ReaderT SiteConfig IO) a
    } deriving (Monad, MonadReader SiteConfig, MonadState ControlState, MonadIO)

execControl::Control a -> ControlState -> SiteConfig -> IO ControlState
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

-- | State for each site master thread
data ControlState = ControlState {
    -- | STM queue for passing URLs around
    linkQueue   :: TChan Task,
    -- | P1er thread map of URLs seen for this site
    linksSeen   :: Map.HashMap URL Time.UTCTime,
    -- | Number of urls we're tested in total
    seenHashLen :: Gauge,
    -- | Channel for results from the workers
    resultQueue :: TChan ResultCode
}

-- | State for the worker threads
data WorkerState = WorkerState {
  -- | Shared queue
  jobsQueue     :: TChan Task,
  -- | The check function
  -- | This is static for now, but in time we want this to change, so put it in
  -- | State not Reader
  checkFunction :: PasteContents -> Maybe MatchLine,
  -- | Random number generator for each thread
  randGen       :: StdGen
}

-- | All the per-site EKG Counters
data Counters = Counters {
    -- | Total number of URLs successfully tested
    tested  :: !Counter,
    -- | Total number of URLs that have matched an alert
    matched :: !Counter,
    -- | Total number of retries. Multiple retries of the same
    -- url count on each retry
    retries :: !Counter,
    -- | Total number of failed URLs (404 errors etc)
    failed  :: !Counter
}

-- | A Task is a URL to check
data Task = Task {
  -- | Type of site
  site    :: !Site,
  -- | How many time have we checked this URL already?
  ntimes  :: !Int,
  -- | Queue to put result status in
  rStatus :: TChan ResultCode,
  -- | URL of the paste to check
  paste   :: !URL
}
