{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Core types
module PasteWatch.Types
    (
        Control,
        ControlState(..),
        Counters,
        Domain(..),
        Email(..),
        Host(..),
        LogDestination(..),
        MatchText(..),
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
        getCtr,
        runControl,
        execWorker,
        runWorker,
    ) where

import           Control.Concurrent.STM         (TChan)
import           Control.Concurrent.STM.TMVar   (TMVar)
import           Control.DeepSeq
import           Control.DeepSeq.Generics       (genericRnf)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Configurator.Types as DCT
import           Data.Hashable
import           Data.Hashable.Generic          (gHashWithSalt)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import           Data.Typeable
import qualified Database.MongoDB as DB
import           GHC.Exts                       ( IsString(..) )
import           GHC.Generics
import           System.Log as Log
import           System.Remote.Counter          (Counter)
import           System.Remote.Gauge            (Gauge)
import           System.Random

--------------------------------------------------------------
-- simple types
--------------------------------------------------------------

-- | A domain (e.g. "example.com")
newtype Domain = Domain String deriving (Eq, Generic, IsString, Show)

instance DCT.Configured Domain where
    convert (DCT.String v) = Just $ Domain (T.unpack v)
    convert _              = Nothing

instance NFData Domain where rnf = genericRnf

-- | Email address (e.g. "fred@example.com")
newtype Email = Email String deriving (Eq, Generic, IsString, Show)

instance DCT.Configured Email where
    convert (DCT.String v) = Just $ Email (T.unpack v)
    convert _              = Nothing

instance NFData Email where rnf = genericRnf

-- | A hostname (e.g. smtp.example.com)
newtype Host = Host String deriving (Eq, Generic, IsString, Show)

instance DCT.Configured Host where
    convert (DCT.String v) = Just $ Host (T.unpack v)
    convert _              = Nothing

instance NFData Host where rnf = genericRnf

-- | Type to define where we log to
data LogDestination = LOGSTDERR | LOGSYSLOG | LOGBOTH deriving (Bounded, Enum, Eq)

instance DCT.Configured LogDestination where
    convert (DCT.String "both")   = Just LOGBOTH
    convert (DCT.String "stderr") = Just LOGSTDERR
    convert (DCT.String "syslog") = Just LOGSYSLOG
    convert _                     = Nothing

-- | A line in a paste that we have alerted on
newtype MatchText = MatchText T.Text
  deriving (Eq, Generic, IsString, Show, Typeable, DB.Val)

instance NFData MatchText where rnf = genericRnf

-- | Plain text contents of a paste
newtype PasteContents = PasteContents T.Text
  deriving (Eq, Generic, IsString, Show, Typeable, DB.Val)

instance NFData PasteContents where rnf = genericRnf

-- | Custom results when getting paste
data ResultCode = DB_ERR | FAILED | RETRY | SMTP_ERR | STACK_OVERFLOW
                  | SUCCESS | TESTED deriving (Bounded, Enum, Eq, Generic, Show)

instance NFData ResultCode where rnf = genericRnf

-- | All the per-site EKG Counters
type Counters = [Counter]

getCtr::ResultCode -> Counters -> Counter
getCtr rc ctrs = ctrs !! fromEnum rc

-- | Simple type to store URLs
newtype URL = URL String deriving (Eq, Generic, Hashable, IsString, Show, Typeable, DB.Val)

instance DCT.Configured URL where
    convert (DCT.String v) = Just $ URL (T.unpack v)
    convert _              = Nothing

instance NFData URL where rnf = genericRnf

-- | Config instance for MongoDB.Host
instance DCT.Configured DB.Host where
    convert (DCT.String h) = Just $ DB.host (T.unpack h)
    convert _              = Nothing

instance DCT.Configured Log.Priority where
    convert (DCT.String "debug") = Just Log.DEBUG
    convert (DCT.String "info")  = Just Log.INFO
    convert (DCT.String "error") = Just Log.ERROR
    convert _                    = Nothing

instance DCT.Configured [Email] where
    convert (DCT.List xs) = mapM DCT.convert xs
    convert _             = Nothing

--------------------------------------------------------------
-- Config data structures
--------------------------------------------------------------

-- | The different sites
--
-- doCheck and getnewPastes in PasteWatch.Sites must
-- be implemented for every new site
data Site = Pastebin | Pastie | SkidPaste | Slexy | Snipt
                deriving (Bounded, Enum, Eq, Generic, Show, Typeable)

instance DB.Val Site where
    val a = DB.String (T.pack $ show a)
    cast' (DB.String a) = case a of
      "Pastebin"  -> Just Pastebin
      "Pastie"    -> Just Pastie
      "SkidPaste" -> Just SkidPaste
      "Slexy"     -> Just Slexy
      "Snipt"     -> Just Snipt
      _           -> Nothing
    cast' _ = Nothing

-- No ETA reduce on this instance. See
-- http://hackage.haskell.org/packages/archive/hashable-generics/1.1.8/doc/html/Data-Hashable-Generic.html
instance Hashable Site where
    hashWithSalt s x  = gHashWithSalt s x
    {-# INLINEABLE hashWithSalt #-}

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
     -- | Regular expression to alert on if seen in a paste
    alertRe        :: !T.Text,
    -- | Should we send alerts to the DB?
    alertToDB      :: !Bool,
    -- | Should we send email alerts?
    alertToEmail   :: !Bool,
    -- | Hostname of mongoDB server
    dbHost         :: !DB.Host,
    -- | dbName
    dbName         :: !DB.Database,
    -- | Domain that email comes from
    domain         :: !Domain,
    -- | Log all pastes in DB?
    logAllToDB     :: !Bool,
    -- | Log level for hslogger
    logLevel       :: !Log.Priority,
    -- | Where should we log to?
    logTo          :: !LogDestination,
    -- | Number of Haskell (lightweight) threads to use
    -- for downloading. Total number of threads used
    -- equals nthreads + (2 * number of sites) + 1
    nthreads       :: !Int,
    -- | We wait for a random number between 0 and pauseMax seconds before
    -- downloading the URL
    -- Use this to stop sites blocking downloads due to too many requests in too short
    -- a time period
    pauseMax       :: !Int,
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
  -- | TMVar for critical errors. Writing True to this shuts down all threads
  criticalError :: TMVar Bool,
  -- | Shared queue
  jobsQueue     :: TChan Task,
  -- | The check function
  -- | This is static for now, but in time we want this to change, so put it in
  -- | State not Reader
  checkFunction :: PasteContents -> Maybe MatchText,
  -- | Random number generator for each thread
  randGen       :: StdGen,
  -- | DB connection
  dbPipe        :: Maybe DB.Pipe,
  db            :: DB.Database

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
