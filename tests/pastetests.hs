{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception (AsyncException(StackOverflow), handle)
import           Control.Monad
import           Data.Monoid       (mconcat)
import           Data.Maybe
import qualified Data.Text as T
import           Test.HUnit
import           System.Exit      (exitFailure, exitSuccess)
import           System.IO.Unsafe  (unsafePerformIO)

import           PasteWatch.Sites
import           PasteWatch.Types  hiding (UserConfig(..))

alertRe   = "@example.com|@sub.example.com|(?i)(?<!New )University(?-i)"

checkContentF s = case checkContent alertRe s of
    Nothing  -> True
    Just  _  -> False

checkContentT s r = case checkContent alertRe s of
    Nothing             -> False
    Just (MatchText s') -> s' == r

-- | run a check that gets a given URL (paste) from a site
-- using unsafePerformIO here means that the
-- tests will fail if no internet connectivity is available
unsafeDoCheck site url = unsafePerformIO $
                            doCheck site url (checkContent alertRe)

matchTests = [("Test single line T1", checkContentT "stuff in a@example.com dsfd" "@example.com"),
              ("Test single line F1", checkContentF "some content in here"),
              ("Test single line T2", checkContentT "yeah root@example.com/password stuff" "@example.com"),
              ("Test multi line T1",  checkContentT "one line\ntwo line\ntree @sub.example.com line" "@sub.example.com"),
              ("Test multi line F1",  checkContentF "one line  \n two line"),
              ("Test singe line F2",  checkContentF "some text about a company doing stuff"),
              ("Test look behind T1", checkContentT "the university of dork" "university"),
              ("Test look behind F1", checkContentF "the New University of stuff")
           ]

getPasteTests = [("get pastebin", "@example.com", "testing @example.com testing",
                   Pastebin, URL "http://pastebin.com/raw.php?i=bLFduQqs"),
                ("get pastie", "@example.com", "\ntesting @example.com testing\n",
                   Pastie, URL "http://pastie.org/pastes/5406980/text"),
                ("get slexy", "@example.com", "testing @example.com testing\n",
                   Slexy, URL "http://slexy.org/view/s2Fv9q8J2H"),
                ("get snipt", "@example.com", "testing @example.com testing\n",
                   Snipt, URL "http://snipt.org/zkfe8/plaintext"),
                ("get skidpaste", "@example.com",
                   "testing @example.com testing",
                   SkidPaste, URL "http://skidpaste.org/3cOMCRpA.txt")
               ]

stackOverflowTests = [ (Pastebin, URL "http://pastebin.com/9JyuyRGB") ]

-- test the new paste functions don't return an empty list
newPasteTests = map (\s -> TestCase $ assertBool "Checking for non-empty getNewPastes"
                    (not $ null $ unsafePerformIO $ getNewPastes s))
                    [minBound..maxBound]

runGetPasteTest (text, matcht, contents, site, url) =
    TestCase $ assertEqual text (SUCCESS, Just (MatchText matcht), Just (PasteContents contents))
                                (unsafeDoCheck site url)

runMatchTest (text, test) = TestCase $ assertBool text test

runStackOverflowTest (site, url) = TestCase $ assertEqual "Testing stackoverflow" TESTED runCheck
  where
    runCheck = fst3 $ unsafePerformIO $
      handle (\StackOverflow -> return (STACK_OVERFLOW, Nothing, Nothing))
             $ doCheck site url (checkContent alertRe)
    fst3 (a, _, _) = a


main::IO ()
main =
    do c1 <- runTestTT $ TestList $ map runMatchTest matchTests
       --c2 <- runTestTT $ TestList $ map runGetPasteTest getPasteTests
       --c3 <- runTestTT $ TestList newPasteTests
       --c4 <- runTestTT $ TestList $ map runStackOverflowTest stackOverflowTests
       --if any (\x -> errors x /= 0 || failures x /= 0) [c1, c2, c3, c4]
       if any (\x -> errors x /= 0 || failures x /= 0) [c1]
            then exitFailure
            else exitSuccess
