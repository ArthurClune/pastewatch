{-# LANGUAGE OverloadedStrings #-}
-- | Code to generate our parser based on the given config
module PasteWatch.Alert
   (
     checkContent
   ) where

import           Control.Applicative
import qualified Data.Attoparsec.Text as A
import           Data.Maybe()
import qualified Data.Text as T

import PasteWatch.Types

-- | If the given Paste includes our pattern, return the one that matched
checkContent::[T.Text] -> [T.Text] -> PasteContents -> Maybe MatchText
checkContent alerts alertsci (PasteContents s)
  | logall    = Just (MatchText "ANY")
  | otherwise =
      case A.maybeResult $ A.feed (A.parse alertp s) T.empty of
                Nothing -> Nothing
                Just v  -> Just $ MatchText v
  where
    alertp = manyTill (A.choice matchlist) <* A.takeText
    matchlist = map A.string alerts ++ map A.asciiCI alertsci
    logall = (alerts == ["ALL"]) && (alertsci == ["ALL"])

-- | parse zero or more instances of end, skipping over one character on failure
-- and returning the matched text if one succeeds
manyTill::A.Parser T.Text-> A.Parser T.Text
manyTill end = scan
    where scan = end <|> A.take 1 *> scan