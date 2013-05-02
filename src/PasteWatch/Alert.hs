{-# LANGUAGE OverloadedStrings #-}
-- | Code to generate our parser based on the given config
module PasteWatch.Alert
   (
     checkContent
   ) where

import           Data.Maybe()
import           Text.RegexPR

import PasteWatch.Types

-- | If the given Paste includes our pattern, return the one that matched
checkContent::String -> PasteContents -> Maybe MatchText
checkContent regex (PasteContents s) =
  case matchRegexPR regex s of
    Just ((matchString,(_)),_) -> Just $ MatchText matchString
    Nothing                     -> Nothing
