module PasteWatch.Sites
    (
        doCheck,
        getNewPastes,
    ) where

import Control.Monad.State (liftIO)
import Data.ByteString as S hiding (head, map)   
import Data.ByteString.Char8 as B (pack)     
import Text.HandsomeSoup ((!), css, fromUrl)
import Text.XML.HXT.Core ((>>>), deep, runX, isText, xshow)

import PasteWatch.Types

-- Check contents of a URL against given check function
doCheck::Site -> URL -> (S.ByteString->Bool) -> IO (Maybe String)

doCheck Pastebin url contentMatch =
    doCheck' url contentMatch (css "textarea" >>> deep isText)

doCheck Pastie url contentMatch = 
    doCheck' url contentMatch (css "pre[class=textmate-source]" 
                               >>> deep isText)

-- Get all the new pastes from a given site
getNewPastes::Site -> Job [URL]

getNewPastes Pastebin = do
    doc   <- liftIO $ fromUrl "http://www.pastebin.com/trends"
    links <- liftIO $ runX $ doc >>> css "ul[class=right_menu] a" ! "href"
    return $ map ("http://pastebin.com" ++ ) links

getNewPastes Pastie = do
    doc   <- liftIO $ fromUrl "http://www.pastie.org/pastes"
    liftIO $ runX $ doc >>> css "div[class=pastePreview] a" ! "href"

-- internal helper function
doCheck' url contentMatch cssfunc = do
    doc     <- fromUrl url
    content <- runX . xshow $ doc >>> cssfunc
    if contentMatch (B.pack $ head content)
        then return $ Just (head content)
        else return Nothing
