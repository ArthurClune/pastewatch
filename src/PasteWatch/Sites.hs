{-# LANGUAGE OverloadedStrings #-}
-- | Code to deal with each specific type of site
--
-- Contains all the special case code that differs per site
module PasteWatch.Sites
    (
        doCheck,
        getNewPastes,
        siteConfigs
    ) where

import           Control.Exception          (onException)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.Text as T
import           Data.Tree.NTree.TypeDefs
import           Network.HTTP
import           Text.HandsomeSoup          ((!), css, parseHtml, fromUrl)
import           Text.XML.HXT.Core hiding   (trace)

import PasteWatch.Types

-- | Config for our sites
siteConfigs::[SiteConfig]
siteConfigs = [
  SiteConfig {
    siteType  = Pastebin,
    delayTime = 10,
    pruneTime = 600
  },
  SiteConfig {
    siteType  = Pastie,
    delayTime = 33,      -- 30 sec + skew
    pruneTime = 1200
  },
  SiteConfig {
    siteType  = SkidPaste,
    delayTime = 247,     -- 4 mins + skew
    pruneTime = 7200
  },
  SiteConfig {
    siteType  = Slexy,
    delayTime = 251,     -- 4 mins + skew
    pruneTime = 7200
  }
 ]

-- | Check contents of a URL against given check function
doCheck::Site -> URL -> (S.ByteString->Bool) -> IO (Either ErrorCode String)
doCheck sitet url contentMatch =
    case sitet of
        Pastebin  -> doCheck' url contentMatch (css "textarea")
        Pastie    -> doCheck' url contentMatch (css "pre[class=textmate-source]")
        SkidPaste -> doCheck' url contentMatch (css "div[class=content]")
        Slexy     -> doCheck' url contentMatch (css "div[class=text]")

-- | Get all the new pastes from a given site
getNewPastes::Site -> IO [URL]

getNewPastes Pastebin = do
    doc   <- fromUrl "http://www.pastebin.com/trends"
    links <- runX $ doc >>> css "ul[class=right_menu] a" ! "href"
    return $ map (URL . T.pack . ("http://pastebin.com" ++ )) links

getNewPastes Pastie = do
    doc   <- fromUrl "http://www.pastie.org/pastes"
    links <- runX $ doc >>> css "div[class=pastePreview] a" ! "href"
    return $ map (URL . T.pack) links

getNewPastes SkidPaste = do
    doc   <- fromUrl "http://skidpaste.org/index.html"
    links <- runX $ doc >>> css "div[id=sidemenu] ul[class=submenu] a" ! "href"
    return $ map (URL . T.pack) $ filter (/= "") links

getNewPastes Slexy = do
    doc   <- fromUrl "http://slexy.org/recent"
    links <- runX $ doc >>> css "td a" ! "href"
    return $ map (URL . T.pack . ("http://slexy.org" ++)) links

-- internal helper function
doCheck'::URL
        -> (S.ByteString->Bool)
        -> IOSLA (XIOState ()) (NTree XNode) (NTree XNode)
        -> IO (Either ErrorCode String)
doCheck' url contentMatch cssfunc = do
    resp <- onException (fetchURL url) (return FAILED)
    case resp of
        Left a -> return $ Left a
        Right doc -> extractContent doc
  where
    extractContent doc = do
        content <- runX . xshow $ doc >>> cssfunc >>> deep isText
        if contentMatch (B.pack $ head content)
            then return $ Right (head content)
            else return $ Left NO_MATCH

fetchURL::URL -> IO (Either ErrorCode (IOSArrow XmlTree (NTree XNode)))
fetchURL url = do
    let req = getRequest (toString url)
    resp <- simpleHTTP req
    case resp of
        Left _  -> return $ Left FAILED
        Right r -> case rspCode r of
            (2, 0, 0) -> return $ Right $ parseHtml (rspBody r)
            (4, 0, 8) -> return $ Left RETRY
            (5, _, _) -> return $ Left RETRY
            _         -> return $ Left FAILED