{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PasteWatch.Types
    (
        execJob,
        Email,
        Job(..),
        JobState(..),
        Task(..),
        Site(..),
        URL,
    ) where

import Control.Concurrent.STM
import Control.Monad.State
import Data.Map as Map
import Data.Time.Clock as Time


execJob::Job a -> JobState -> IO JobState
execJob = execStateT . runJob

type Email = (String, String)

-- | Monad stack wrapping State
newtype Job a = Job { runJob::StateT JobState IO a }
    deriving (Monad, MonadState JobState, MonadIO)

-- | State monad for our channel and list of done URLs
data JobState = JobState { linkQueue::TChan Task
                           ,linksSeen::Map.Map URL Time.UTCTime
                         }

-- | The different sites
--   An instance of PasteSite is required for every site
--   Site Pastebin | Pastie or .....
data Site = Pastebin | Pastie | SkidPaste | Slexy

-- | A Task is either a URL to check, or Done
--   The code doesn't use Done yet
data Task = Check Site URL | Done

-- | Simple type to store URLs
type URL = String        


