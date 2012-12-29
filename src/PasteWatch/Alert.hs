{-# LANGUAGE OverloadedStrings #-}
-- | Code to generate our parser based on the given config
module PasteWatch.Alert
   (
     checkContent
   ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as S
import           Data.Maybe()
import qualified Data.Text as T

import PasteWatch.Types

-- | Return True iff the given string includes our patterns
checkContent::[S.ByteString] -> [S.ByteString] -> S.ByteString -> Maybe MatchText
checkContent alerts alertsci s = case r of
                Left  _ -> Nothing
                Right v -> Just (MatchText $ T.pack v)
              where
                r = eitherResult $ feed (parse alertp s) S.empty
                alertp = manyTill anyChar matchlist <* many1 anyChar
                matchlist = foldl1 (<|>) $ map string alerts ++ map stringCI alertsci
