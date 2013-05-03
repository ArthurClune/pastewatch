{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Code to deal with each specific type of site
--
-- Contains all the special case code that differs per site
module PasteWatch.Sites
    (
        checkContent,
        createCounters,
        createGauges,
        doCheck,
        getNewPastes,
        siteConfigs
    ) where

import          Prelude hiding (catch)

import           Control.DeepSeq            ( ($!!) )
import           Control.Error
import           Control.Exception          (AsyncException(StackOverflow), handle)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe                 (fromJust)
import qualified Data.Text as T
import qualified Data.Text.ICU as ICU
import           Data.Tree.NTree.TypeDefs
import           Network.HTTP
import           System.Log.Logger
import           System.Remote.Gauge        (Gauge)
import           System.Remote.Monitoring   (getCounter, getGauge, Server)
import           Text.HandsomeSoup          ((!), css)
import           Text.XML.HXT.Core hiding   (trace)
import           Text.XML.HXT.TagSoup

import PasteWatch.Types
import PasteWatch.Utils  (stripQuotes)

-- | Config for our sites
-- delayTime is lower for busier sites
-- errorTime is higher for pastebin as they block clients for a period after
-- too many requests
-- pruneTime is lower for busier sites
siteConfigs::SiteConfigs
siteConfigs = Map.fromList [
  -- (Site, SiteConfig delayTime errorTime pruneTime)
  (Pastebin,  SiteConfig  19 3603  600),
  (Pastie,    SiteConfig  41  203 1200),
  (SkidPaste, SiteConfig 247  203 7200),
  (Slexy,     SiteConfig 251  203 7200),
  (Snipt,     SiteConfig 254  203 7200)
  ]

-- | Check contents of a URL against given check function
-- Must be implemented for every new site
-- Argument to doCheck' is a css selector to match the paste contents
doCheck::Site
       -> URL
       -> (PasteContents->Maybe MatchText)
       -> IO (ResultCode, Maybe MatchText, Maybe PasteContents)
doCheck Pastebin  = doCheck' Nothing
doCheck Pastie    = doCheck' (Just (css "pre"))
doCheck SkidPaste = doCheck' Nothing
doCheck Slexy     = doCheck' (Just (css "div[class=text]"))
doCheck Snipt     = doCheck' (Just (css "textarea"))

-- | Get all the new pastes from a given site
-- Must be implemented for every new site
-- args to be supplied are
-- 1) URL for a page containing new/recent paste list
-- 2) css expression to match that list
-- 3) fn to make that list canonical and absolute URLs
getNewPastes::Site -> IO [URL]

getNewPastes Pastebin =
    getNewPastes' "http://pastebin.com/trends"
                  (css "ul[class=right_menu] a" ! "href")
                  (\u -> "http://pastebin.com/raw.php?i=" ++ filter (/='/') u)

getNewPastes Pastie =
    getNewPastes' "http://www.pastie.org/pastes"
                  (css "div[class=pastePreview] a" ! "href")
                  (++ "/text")

getNewPastes SkidPaste =
    getNewPastes' "http://skidpaste.org"
                  (css "ul[class=ipsList_withminiphoto] a" ! "href")
                  (\u -> "http://skidpaste.org/" ++ u ++ ".txt")

getNewPastes Slexy =
    getNewPastes' "http://slexy.org/recent"
                  (css "div[class=main] tr a" ! "href")
                  ("http://slexy.org" ++)

getNewPastes Snipt =
    getNewPastes' "http://snipt.org/code/recent"
                  (css "div[class=grid-block-container] a" ! "href")
                  (++ "/plaintext")

-----------------------------------------------------------
-- Nothing below here needs changing when adding a new site
-----------------------------------------------------------
getNewPastes'::URL -> IOSLA (XIOState ()) XmlTree String -> (String -> String) -> IO [URL]
getNewPastes' url cssf makeCanonical = do
  page <- fetchURL url
  case page of
      Left _ -> return []
      Right doc -> do
        links <- runX $ readString [ withTagSoup,
                            withValidate no,
                            withWarnings no]
                            doc >>> cssf
        return $!! map (URL . makeCanonical) links


-- run check for matching text against the given url, using the cssfunc
-- to filter the input if necessary first
doCheck'::Maybe (IOSLA (XIOState ()) (NTree XNode) (NTree XNode))
        -> URL
        -> (PasteContents->Maybe MatchText)
        -> IO (ResultCode, Maybe MatchText, Maybe PasteContents)
doCheck' cssfunc url contentMatch  = do
    !res <- fetchURL url
    case res of
        Left  e   -> return (e, Nothing, Nothing)
        Right page -> handle (\StackOverflow -> return (STACK_OVERFLOW, Nothing, Nothing))
                           $ do
                               pastetxt <- extractContent page cssfunc
                               case contentMatch pastetxt of
                                  Just x  -> return $!! (SUCCESS, Just x, Just pastetxt)
                                  Nothing -> return $!! (TESTED, Nothing, Just pastetxt)

-- parse a page to tags, then apply filter function if given
-- before converting to a PasteContents
extractContent :: String
               -> Maybe (IOSLA (XIOState ()) XmlTree (NTree XNode))
               -> IO PasteContents

extractContent page (Just cssfunc) = do
    !content <- runX . xshow $ doc >>> cssfunc >>> deep isText
    return $ PasteContents $ fixLineEndings $ T.pack $ head content
  where
    doc = readString [ withTagSoup, withValidate no, withWarnings no] page
    fixLineEndings = T.unlines . map (T.dropWhileEnd (== '\r')) . T.lines

extractContent page Nothing = return $ PasteContents (T.pack page)

-- fetch a URL, trapping errors
fetchURL :: URL -> IO (Either ResultCode String)
fetchURL (URL url) = do
    !resp <- runEitherT $ tryIO $ simpleHTTP $ getRequest url
    case resp of
        Left  e -> do
                    errorM "pastewatch.Sites.fetchURL" $ "Error retrieving paste " ++
                            show url ++ " " ++ show e
                    return $ Left FAILED
        Right r -> case r of
          Left e -> do
                      errorM "pastewatch.Sites.fetchURL" $ "Error retrieving paste " ++
                              show url ++ " " ++ show e
                      return $ Left FAILED
          Right r' -> case rspCode r' of
            (2, 0, 0) -> return $ Right $ rspBody r'
            (4, 0, 8) -> return $ Left RETRY
            (5, _, _) -> return $ Left RETRY
            _         -> return $ Left FAILED


-- | Generate the Counters for a given site
createCounters::Server -> Site -> IO Counters
createCounters srv sitet =
    mapM (counterf . show) ([minBound .. maxBound] :: [ResultCode])
  where
    counterf = (`getCounter` srv) . T.pack . (prefix ++)
    prefix = (stripQuotes . show $ sitet) ++ " "

-- | Generate the Gauges for a given site
createGauges::Server -> Site -> IO Gauge
createGauges srv sitet = getGauge label srv
  where
      label = T.pack $ (stripQuotes . show $ sitet) ++ "Hash Length"


-- | If the given Paste includes our pattern, return the match
checkContent::T.Text -> PasteContents -> Maybe MatchText
checkContent r (PasteContents s) =
    case ICU.find (ICU.regex [] r) s of
      Just m' -> Just $ MatchText (fromJust $ ICU.group 0 m')
      Nothing -> Nothing
