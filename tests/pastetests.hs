{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Monoid      (mconcat)
import Data.Maybe
import PasteWatch.Alert (checkContent)
import Test.HUnit
import System.Exit      (exitFailure, exitSuccess)
import System.IO.Unsafe (unsafePerformIO)

import PasteWatch.Config
import PasteWatch.Sites
import PasteWatch.Types

-- skid paste output includes a "Parsed in 0.000 seconds" type output
-- so just do a simple match
skidpasteMatch c = case c of
    Left  _ -> False
    Right s -> "@example.com" `elem` mconcat (map words $ lines s)

checkContent' = checkContent ["@example.com", "@sub.example.com"] ["my company inc"]


-- using unsafePerformIO here means that the
-- tests will fail if no internet connectivity is available
unsafeDoCheck site url = unsafePerformIO $ doCheck site url checkContent'

testList = [TestCase $ assertBool "Test single line T1"
                        (checkContent' "stuff in a@example.com dsfd"),
            TestCase $ assertBool "Test single line F1"
                        (not $ checkContent' "some content in here"),
            TestCase $ assertBool "Test single line T2"
                        (checkContent' "yeah root@example.com/password stuff"),
            TestCase $ assertBool "Test multi line T1"
                        (checkContent' "one line\ntwo line\ntree @sub.example.com line"),
            TestCase $ assertBool "Test multi line F1"
                        (not $ checkContent' "one line  \n two line"),
            TestCase $ assertBool "Test singe line F2"
                        (not $ checkContent' "some text about a company doing stuff"),
            TestCase $ assertBool "Test single line T3"
                        (checkContent' "stuff about My Company Inc being hacked"),
            TestCase $ assertEqual "get pastebin"
                        (Right "testing @example.com testing")
                        (unsafeDoCheck Pastebin $ URL "http://pastebin.com/bLFduQqs"),
            TestCase $ assertEqual "get pastie"
                        (Right "testing @example.com testing\n")
                        (unsafeDoCheck Pastie $ URL "http://pastie.org/5406980"),
            TestCase $ assertBool "get skidpaste"
                        (skidpasteMatch $ unsafeDoCheck SkidPaste $
                            URL "http://skidpaste.org/3cOMCRpA"),
            TestCase $ assertEqual "get slexy"
                        (Right "testing @example.com testing\n")
                        (unsafeDoCheck Slexy $ URL "http://slexy.org/view/s2Fv9q8J2H"),
            TestCase $ assertEqual "get snipt"
                        (Right "testing @example.com testing")
                        (unsafeDoCheck Snipt $ URL "http://snipt.org/zkfe8/plaintext")
           ]

-- test the new paste functions don't return an empty list
testNewPastes =   map
                    (\s -> TestCase $ assertBool ""
                                (null $ unsafePerformIO $ getNewPastes s))
                    [minBound..maxBound]

main::IO ()
main =
    do c1 <- runTestTT $ TestList testList
       c2 <- runTestTT $ TestList testNewPastes
       if any (\x -> errors x /= 0 && failures x /= 0) [c1, c2]
            then exitFailure
            else exitSuccess
