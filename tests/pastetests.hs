{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import PasteWatch.Alert (checkContent)
import Test.HUnit
import System.Exit
import System.IO.Unsafe (unsafePerformIO)

import PasteWatch.Sites
import PasteWatch.Types

import Debug.Trace (traceShow)

-- skid paste output includes a "Parsed in 0.000 seconds" type output
-- so just do a simple match
doesSkidpasteMatch c = case c of
    Just s -> if "@example.com" `elem` (words $ lines s !! 2)
                then traceShow (lines s) True 
                else traceShow (lines s) False
    Nothing -> False

-- tests will fail if no internet connectivity is available
urlGet site url = do
    unsafePerformIO $ doCheck site url checkContent

testList = [TestCase $ assertEqual "Test single line T1" True  (checkContent "stuff in a@example.com dsfd"),
            TestCase $ assertEqual "Test single line F1" False (checkContent "some content in here"),
            TestCase $ assertEqual "Test single line T2" True  (checkContent "yeah root@example.com/password stuff"),
            TestCase $ assertEqual "Test multi line T1"  True  (checkContent "one line\ntwo line\ntree @sub.example.com line"),
            TestCase $ assertEqual "Test multi line F1"  False (checkContent "one line  \n two line"),
            TestCase $ assertEqual "get pastebin" (Just "testing @example.com testing")
                        (urlGet Pastebin "http://pastebin.com/bLFduQqs"),
            TestCase $ assertEqual "get pastie" (Just "testing @example.com testing\n")
                        (urlGet Pastie "http://pastie.org/5406980"),
            TestCase $ assertEqual "get skidpaste" True
                        (doesSkidpasteMatch $ urlGet SkidPaste "http://skidpaste.org/3cOMCRpA"),
            TestCase $ assertEqual "get slexy" (Just "testing @example.com testing\n")
                        (urlGet Slexy "http://slexy.org/view/s2Fv9q8J2H")
           ]


main::IO ()
main = 
    do c <- runTestTT $ TestList testList
       if errors c /= 0 || failures c /= 0
            then exitFailure
            else exitSuccess