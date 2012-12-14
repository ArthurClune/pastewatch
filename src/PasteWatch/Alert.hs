{-# LANGUAGE OverloadedStrings #-}
-- | Code to generate our parser based on the given config
module PasteWatch.Alert
   (
     checkContent
   ) where

import           Control.Applicative
import           Data.Monoid                        (mconcat)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as S

-- | Return True iff the given string includes our patterns
checkContent::[S.ByteString] -> [S.ByteString] -> S.ByteString -> Bool
checkContent alerts alertsci s = case r of
                Left  _ -> False
                Right _ -> True
              where
                r = eitherResult $ feed (parse alertp s) S.empty
                alertp = manyTill anyChar (mconcat matchlist) <* many1 anyChar
                matchlist = map string alerts ++ map stringCI alertsci
