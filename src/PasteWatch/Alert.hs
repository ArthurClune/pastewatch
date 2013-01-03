{-# LANGUAGE OverloadedStrings #-}
-- | Code to generate our parser based on the given config
module PasteWatch.Alert
   (
     checkContent
   ) where

import           Control.Applicative
import           Control.Monad              (join)
import qualified Data.Attoparsec.Text as A
import           Data.List                  (find)
import           Data.Maybe
import qualified Data.Text as T

import PasteWatch.Types

-- | If the given string includes our patterns, return the first matching line
checkContent::[T.Text] -> [T.Text] -> PasteContents -> Maybe MatchLine
checkContent alerts alertsci (PasteContents s) =
    join $ find isJust $ map parseLine (T.lines s)
  where
    parseLine l = case A.maybeResult $ A.feed (A.parse alertp l) T.empty of
                Nothing -> Nothing
                Just _  -> Just $ MatchLine l
    alertp = A.manyTill A.anyChar (A.choice matchlist) <* A.takeText
    matchlist = map A.string alerts ++ map A.asciiCI alertsci
