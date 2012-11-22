{-# LANGUAGE OverloadedStrings #-}
-- | Code to generate our parser based on the given config
module PasteWatch.Alert
   (
     checkContent
   ) where

import Control.Applicative
import Data.Monoid (mconcat)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as S

import PasteWatch.Config (config)
import PasteWatch.Types (Config(..))

-- | Parser for a set of strings to match on,
-- generated from the Config.hs
strings::Parser S.ByteString
strings = mconcat matchlist
  where
    matchlist = map string (alertStrings config) ++
                map stringCI (alertStringsCI config)

alerts::Parser String
alerts = manyTill anyChar (try strings) <* many anyChar

-- | Return True iff the given string includes our patterns
checkContent::S.ByteString -> Bool
checkContent s = case r of
                Left  _ -> False
                Right _ -> True
              where
                r = eitherResult $ feed (parse alerts s) S.empty 
