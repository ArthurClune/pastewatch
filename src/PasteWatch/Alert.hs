{-# LANGUAGE OverloadedStrings #-}
-- | Code to generate our parser based on the given config
module PasteWatch.Alert
   (
     checkContent
   ) where

import           Data.Maybe()
import qualified Text.Regex.PCRE as RE
--import           Text.Regex.PCRE.String ( (.=~) )

import PasteWatch.Types

-- | If the given Paste includes our pattern, return the one that matched
checkContent::String -> PasteContents -> Maybe MatchText
checkContent r (PasteContents s) = do
  case (s RE.=~ r :: String) of
    ""    -> Nothing
    match -> Just $ MatchText match
