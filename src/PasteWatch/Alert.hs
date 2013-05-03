{-# LANGUAGE OverloadedStrings #-}
-- | Code to generate our parser based on the given config
module PasteWatch.Alert
   (
     checkContent
   ) where

import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.ICU

import PasteWatch.Types

-- | If the given Paste includes our pattern, return the match
checkContent::T.Text -> PasteContents -> Maybe MatchText
checkContent r (PasteContents s) =
    case find (regex [] r) s of
      Just m' -> Just $ MatchText (fromJust $ group 0 m')
      Nothing -> Nothing
