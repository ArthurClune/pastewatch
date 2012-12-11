{-# LANGUAGE OverlappingInstances, OverloadedStrings #-}
-- | All user config goes here
module PasteWatch.Config
    (
      parseArgs,
      parseConfig
    ) where

import Control.Error
import Control.Monad (ap, liftM)
import Data.Configurator
import Data.Configurator.Types
import Safe (abort)
import System.Environment (getArgs)
import System.Exit

import PasteWatch.Types

instance Configured a => Configured [a] where
    convert (List xs) = mapM convert xs
    convert _         = Nothing

-- quick and dirty command line args handling
parseArgs::IO FilePath
parseArgs = do
    args <- getArgs
    if length args == 1
        then return (head args)
        else abort "Usage: urllogs.hs <config file>"

parseConfig::FilePath -> IO UserConfig
parseConfig file = do
  c <- runEitherT $ tryIO $ load [Required file]
  case c of
    Left _ -> do
        putStrLn "Error loading config file"
        exitFailure
    Right c' -> do
        UserConfig `liftM` (require c' "alertStrings")
                      `ap` (require c' "alertStringsCI")
                      `ap` (require c' "domain")
                      `ap` (require c' "nthreads")
                      `ap` (require c' "pauseMax")
                      `ap` (require c' "recipients")
                      `ap` (require c' "sender")
                      `ap` (require c' "smtpServer")

