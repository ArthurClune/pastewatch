{-# LANGUAGE OverloadedStrings #-}

module PasteWatch.Alert
   (
     checkContent
   ) where

import Control.Applicative
import Data.Monoid (mconcat)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as S hiding (map)

import PasteWatch.Config (config)
import PasteWatch.Types (Config(..))

-- set of strings to match on
strings::Parser S.ByteString
strings = mconcat matchlist
  where
    matchlist = (map string $ alertStrings config) ++
                (map stringCI $ alertStringsCI config)

alerts::Parser String
alerts = manyTill anyChar (try strings) <* many anyChar

-- return True if the string includes out patterns
checkContent::S.ByteString -> Bool
checkContent s = case r of
                Left  _ -> False
                Right _ -> True
              where
                r = eitherResult $ feed (parse alerts s) S.empty 
