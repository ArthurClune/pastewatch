{-# LANGUAGE OverloadedStrings #-}
-- | Code to deal with each specific type of site
--
-- Contains all the special case code that differs per site
module PasteWatch.Sites
    (
        createCounters,
        createGauges,
        doCheck,
        getNewPastes,
        siteConfigs
    ) where

import          Prelude hiding (catch)

import           Control.DeepSeq            ( ($!!) )
import           Control.Error
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import           Data.Tree.NTree.TypeDefs
import           Network.HTTP
import           System.Log.Logger
import           System.Remote.Gauge        (Gauge)
import           System.Remote.Monitoring   (getCounter, getGauge, Server)
import           Text.HandsomeSoup          ((!), css, parseHtml, fromUrl)
import           Text.XML.HXT.Core hiding   (trace)

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
doCheck::Site
       -> URL
       -> (PasteContents->Maybe MatchText)
       -> IO (ResultCode, Maybe MatchText, Maybe PasteContents)
doCheck Pastebin  = doCheck' (css "textarea")
doCheck Pastie    = doCheck' (css "pre[class=textmate-source]")
doCheck SkidPaste = doCheck' (css "div[class=content]")
doCheck Slexy     = doCheck' (css "div[class=text]")
doCheck Snipt     = doCheck' (css "textarea")

-- | Get all the new pastes from a given site
-- Must be implemented for every new site
getNewPastes::Site -> IO [URL]

getNewPastes Pastebin = do
    doc   <- fromUrl "http://www.pastebin.com/trends"
    links <- runX $ doc >>> css "ul[class=right_menu] a" ! "href"
    return $!! map (URL . T.pack . ("http://pastebin.com" ++ )) links

getNewPastes Pastie = do
    doc   <- fromUrl "http://www.pastie.org/pastes"
    links <- runX $ doc >>> css "div[class=pastePreview] a" ! "href"
    return $!! map (URL . T.pack) links

getNewPastes SkidPaste = do
    doc   <- fromUrl "http://skidpaste.org/index.html"
    links <- runX $ doc >>> css "div[id=sidemenu] ul[class=submenu] a" ! "href"
    return $!! map (URL . T.pack) $ filter (/= "") links

getNewPastes Slexy = do
    doc   <- fromUrl "http://slexy.org/recent"
    links <- runX $ doc >>> css "td a" ! "href"
    return $!! map (URL . T.pack . ("http://slexy.org" ++)) links

getNewPastes Snipt = do
    doc   <- fromUrl "http://snipt.org/code/recent"
    links <- runX $ doc >>> css "div[class=grid-block-container] a" ! "href"
    return $!! map (URL. T.pack . (++ "/plaintext")) links

-----------------------------------------------------------
-- Nothing below here needs changing when adding a new site
-----------------------------------------------------------

-- internal helper function
doCheck'::IOSLA (XIOState ()) (NTree XNode) (NTree XNode)
        -> URL
        -> (PasteContents->Maybe MatchText)
        -> IO (ResultCode, Maybe MatchText, Maybe PasteContents)
doCheck' cssfunc url contentMatch  = do
    res <- fetchURL url
    case res of
        Left  e   -> return (e, Nothing, Nothing)
        Right doc -> extractContent doc
  where
    extractContent doc = do
        content <- runX . xshow $ doc >>> cssfunc >>> deep isText
        let pastetxt = PasteContents $ fixLineEndings $ T.pack $ head content in
          case contentMatch pastetxt of
              Just x  -> return $!! (SUCCESS, Just x, Just pastetxt)
              Nothing -> return $!! (TESTED, Nothing, Just pastetxt)
    fixLineEndings t = T.unlines $ map (T.dropWhileEnd (== '\r')) $ T.lines t

fetchURL::URL -> IO (Either ResultCode (IOSArrow XmlTree (NTree XNode)))
fetchURL (URL url) = do
    resp <- runEitherT $ tryIO $ simpleHTTP $ getRequest $ T.unpack url
    case resp of
        Left  e -> do
                    errorM "pastewatch.fetchURL" $ "Error retrieving paste " ++ show e
                    return $ Left FAILED
        Right r -> case r of
          Left e -> do
                      errorM "pastewatch.fetchURL" $ "Error retrieving paste " ++ show e
                      return $ Left FAILED
          Right r' -> case rspCode r' of
            (2, 0, 0) -> return $ Right $ parseHtml (rspBody r')
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
