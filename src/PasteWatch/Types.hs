{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Core types
module PasteWatch.Types
    (
        Config(..),
        Domain,
        Email,
        Host,
        Job(..),
        JobState(..),
        Task(..),
        Site(..),
        sites,
        URL,
    ) where

import Control.Concurrent.STM
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
  smtpServer     :: Host,
  -- | Time to pause main thread each loop (microseconds)
  delayTime      :: Int
}

-- | A domain domain (e.g. "example.com")
type Domain = String

-- | Email address ("Real Name", "email address")
type Email = (String, String)

-- | A hostname (e.g. smtp.example.com)
type Host = String

-- | Monad stack wrapping State
newtype Job a = Job { runJob::StateT JobState IO a }
    deriving (Monad, MonadState JobState, MonadIO)

-- | State monad for our channel and list of done URLs
data JobState = JobState { linkQueue::TChan Task
                           ,linksSeen::Map.Map URL Time.UTCTime
                         }

-- | The different sites
--
--   An instance of PasteSite is required for every site
data Site = Pastebin | Pastie | SkidPaste | Slexy

-- | List of the different site types
sites::[Site]
sites = [Pastebin, Pastie, SkidPaste, Slexy]

-- | A Task is a URL to check
data Task = Check Site URL | Done

-- | Simple type to store URLs
type URL = String        


