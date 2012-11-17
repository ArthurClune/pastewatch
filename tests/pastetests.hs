{-# LANGUAGE OverloadedStrings #-}

import PasteWatch.Alert (checkContent)
import Test.HUnit
import System.Exit

-- These samples assume that the strings "@mydomain.com" is in alertStrings 
-- and "my company inc" is in alertStringsCI in Config.hs
-- edit suitably

testList = [TestCase $ assertEqual "Test single line T1" True  (checkContent "user me@example.com has password dsfd"),
            TestCase $ assertEqual "Test single line F1" False (checkContent "some content in here"),
            TestCase $ assertEqual "Test single line T2" True  (checkContent "yeah root@example.com/password stuff"),
            TestCase $ assertEqual "Test multi line T1"  True  (checkContent "one line\ntwo line\ntree My Company Inc line"),
            TestCase $ assertEqual "Test multi line F1"  False (checkContent "one line  \n two line")
           ]


main::IO ()
main = 
    do c <- runTestTT $ TestList testList
       if errors c /= 0 || failures c /= 0
            then exitFailure
            else exitSuccess