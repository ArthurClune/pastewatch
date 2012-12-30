{-# LANGUAGE OverlappingInstances, OverloadedStrings #-}

module PasteWatch.Config
    (
      parseArgs,
      parseConfig
    ) where

import Control.Applicative      ( (<$>), (<*>) )
import Control.Error
import Data.Configurator
import Data.Configurator.Types
import Safe                     (abort)
import System.Environment       (getArgs)
import System.Exit

import PasteWatch.Types

instance Configured a => Configured [a] where
    convert (List xs) = mapM convert xs
    convert _             = Nothing

-- | Quick and dirty command line args handling
parseArgs::IO FilePath
parseArgs = do
    args <- getArgs
    if length args == 1
        then return (head args)
        else abort "Usage: urllogs.hs <config file>"

-- | Parse config file
parseConfig::FilePath -> IO UserConfig
parseConfig file = do
  c <- runEitherT $ tryIO $ load [Required file]
  case c of
    Left _ -> do
        putStrLn "Error loading config file"
        exitFailure
    Right c' ->
        UserConfig <$> require c' "alertStrings"
                     <*> require c' "alertStringsCI"
                     <*> require c' "domain"
                     <*> require c' "nthreads"
                     <*> require c' "pauseMax"
                     <*> require c' "recipients"
                     <*> require c' "sender"
                     <*> require c' "smtpServer"

