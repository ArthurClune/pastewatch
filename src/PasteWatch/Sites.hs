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

import           Control.DeepSeq            ( ($!!) )
import           Control.Exception          (onException)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import           Data.Tree.NTree.TypeDefs
import           Network.HTTP
import           System.Remote.Gauge        (Gauge)
import           System.Remote.Monitoring   (getCounter, getGauge, Server)
import           Text.HandsomeSoup          ((!), css, parseHtml, fromUrl)
import           Text.XML.HXT.Core hiding   (trace)

import PasteWatch.Types
import PasteWatch.Utils  (sq)

-- | Config for our sites
siteConfigs::SiteConfigs
siteConfigs = Map.fromList [
  -- (Site, SiteConfig siteType delayTime errorTime pruneTime)
  (Pastebin,  SiteConfig  16 1803  600),
  (Pastie,    SiteConfig  41 1803 1200),
  (SkidPaste, SiteConfig 247 1803 7200),
  (Slexy,     SiteConfig 251 1803 7200),
  (Snipt,     SiteConfig 254 1803 7200)
  ]

-- | Check contents of a URL against given check function
-- Must be implemented for every new site
doCheck::Site -> URL -> (S.ByteString->Maybe T.Text) -> IO (Either ResultCode (T.Text, String))
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
        -> (S.ByteString->Maybe T.Text)
        -> IO (Either ResultCode (T.Text, String))
doCheck' cssfunc url contentMatch  = do
    resp <- onException (fetchURL url) (return FAILED)
    case resp of
        Left a -> return $!! Left a
        Right doc -> extractContent doc
  where
    extractContent doc = do
        content <- runX . xshow $ doc >>> cssfunc >>> deep isText
        case contentMatch (B.pack $ head content) of
            Just x  -> return $!! Right (x, head content)
            Nothing -> return $! Left NO_MATCH

fetchURL::URL -> IO (Either ResultCode (IOSArrow XmlTree (NTree XNode)))
fetchURL url = do
    resp <- simpleHTTP $ getRequest (toString url)
    case resp of
        Left _  -> return $ Left FAILED
        Right r -> case rspCode r of
            (2, 0, 0) -> return $ Right $ parseHtml (rspBody r)
            (4, 0, 8) -> return $ Left RETRY
            (5, _, _) -> return $ Left RETRY
            _         -> return $ Left FAILED


-- | Generate the Counters for a given site
createCounters::Server -> Site -> IO Counters
createCounters srv sitet = do
     [c1, c2, c3, c4] <- mapM counterf counterLabels
     return $ Counters c1 c2 c3 c4
  where
    counterLabels = ["Tested", "Matched", "Retries", "Failed"]
    counterf = (`getCounter` srv) . T.pack . (prefix ++)
    prefix = (sq . show) sitet ++ " "

-- | Generate the Gauges for a given site
createGauges::Server -> Site -> IO Gauge
createGauges srv sitet = getGauge label srv
  where
      label = T.pack $ (sq . show) sitet ++ "Hash Length"
