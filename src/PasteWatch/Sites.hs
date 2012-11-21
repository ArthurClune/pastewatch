-- | Code to deal with each specific type of site
--
-- Contains all the special case code that differs per site
module PasteWatch.Sites
    (
        doCheck,
        getNewPastes,
    ) where

import Control.Monad.State (liftIO)
import Data.ByteString as S hiding (head, filter, map)   
import Data.ByteString.Char8 as B (pack)   
import Data.Tree.NTree.TypeDefs  
import Text.HandsomeSoup ((!), css, fromUrl)
import Text.XML.HXT.Core

import PasteWatch.Types

-- | Check contents of a URL against given check function
doCheck::Site -> URL -> (S.ByteString->Bool) -> IO (Maybe String)

doCheck Pastebin url contentMatch =
    doCheck' url contentMatch (css "textarea")

doCheck Pastie url contentMatch = 
    doCheck' url contentMatch (css "pre[class=textmate-source]")

doCheck SkidPaste url contentMatch = 
    doCheck' url contentMatch (css "div[class=content]")

doCheck Slexy url contentMatch =
    doCheck' url contentMatch (css "div[class=text]")    

-- | Get all the new pastes from a given site
getNewPastes::Site -> Job [URL]

getNewPastes Pastebin = do
    doc   <- liftIO $ fromUrl "http://www.pastebin.com/trends"
    links <- liftIO $ runX $ doc >>> css "ul[class=right_menu] a" ! "href"
    return $ map ("http://pastebin.com" ++ ) links

getNewPastes Pastie = do
    doc   <- liftIO $ fromUrl "http://www.pastie.org/pastes"
    liftIO $ runX $ doc >>> css "div[class=pastePreview] a" ! "href"

getNewPastes SkidPaste = do
    doc   <- liftIO $ fromUrl "http://skidpaste.org/index.html"
    links <- liftIO $ runX $ doc >>> css "div[id=sidemenu] ul[class=submenu] a" ! "href"
    return $ filter (/= "") links 

getNewPastes Slexy = do
    doc   <- liftIO $ fromUrl "http://slexy.org/recent"
    links <-liftIO $ runX $ doc >>> css "td a" ! "href"
    return $ map ("http://slexy.org" ++) links

-- internal helper function
doCheck'::String
        -> (S.ByteString->Bool)
        -> IOSLA (XIOState ()) (NTree XNode) (NTree XNode)
        -> IO (Maybe String)
doCheck' url contentMatch cssfunc = do
    doc     <- fromUrl url
    content <- runX . xshow $ doc >>> cssfunc >>> deep isText
    if contentMatch (B.pack $ head content)
        then return $ Just (head content)
        else return Nothing
