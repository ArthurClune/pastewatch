-- | Code to deal with each specific type of site
--
-- Contains all the special case code that differs per site
module PasteWatch.Sites
    (
        doCheck,
        getNewPastes,
    ) where

import Control.Exception (onException)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B (pack)   
import Data.Tree.NTree.TypeDefs  
import Network.HTTP
import Text.HandsomeSoup ((!), css, parseHtml, fromUrl)
import Text.XML.HXT.Core hiding (trace)

import PasteWatch.Types

-- | Check contents of a URL against given check function
doCheck::Site -> URL -> (S.ByteString->Bool) -> IO (Either ErrorCode String)

doCheck Pastebin url contentMatch =
    doCheck' url contentMatch (css "textarea")

doCheck Pastie url contentMatch = 
    doCheck' url contentMatch (css "pre[class=textmate-source]")

doCheck SkidPaste url contentMatch = 
    doCheck' url contentMatch (css "div[class=content]")

doCheck Slexy url contentMatch =
    doCheck' url contentMatch (css "div[class=text]")    

-- | Get all the new pastes from a given site
getNewPastes::Site -> IO [URL]

getNewPastes Pastebin = do
    doc   <- fromUrl "http://www.pastebin.com/trends"
    links <- runX $ doc >>> css "ul[class=right_menu] a" ! "href"
    return $ map ("http://pastebin.com" ++ ) links

getNewPastes Pastie = do
    doc   <- fromUrl "http://www.pastie.org/pastes"
    runX $ doc >>> css "div[class=pastePreview] a" ! "href"

getNewPastes SkidPaste = do
    doc   <- fromUrl "http://skidpaste.org/index.html"
    links <- runX $ doc >>> css "div[id=sidemenu] ul[class=submenu] a" ! "href"
    return $ filter (/= "") links 

getNewPastes Slexy = do
    doc   <- fromUrl "http://slexy.org/recent"
    links <- runX $ doc >>> css "td a" ! "href"
    return $ map ("http://slexy.org" ++) links

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
    let req = getRequest url
    resp <- simpleHTTP req
    case resp of
        Left _  -> return $ Left FAILED
        Right r -> case rspCode r of            
            (2, 0, 0) -> return $ Right $ parseHtml (rspBody r)
            (4, 0, 8) -> return $ Left RETRY
            (5, _, _) -> return $ Left RETRY
            _         -> return $ Left FAILED