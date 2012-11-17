module PasteWatch.Utils
    (
      explode,
      sendEmail
    ) where


import Data.IORef
import Network.SMTP.Client
import Network.Socket
import System.Time (getClockTime, toCalendarTime)

import PasteWatch.Types (Email)

-- send an email with contents content
-- 
-- E.g.
-- 
-- sendEmail ("Arthur Clune","me@me.com")
--           [("Fred Blogs", "fred@exmaple.com")]
--           "hi there" This is the body of the email"
-- 
sendEmail::Email
           -> [Email]
           -> String 
           -> String 
           -> IO()
sendEmail sender
          recipients
          subject 
          content = do
    now <- getClockTime
    nowCT <- toCalendarTime now
    let message = Message [
                From sender',
                To   recipients',
                Subject subject,
                Date nowCT
            ]
            content
    addrs <- getAddrInfo Nothing (Just smtpHost) Nothing
    let SockAddrInet _ hostAddr = addrAddress (head addrs)
        sockAddr = SockAddrInet 25 hostAddr
    sentRef <- newIORef []
    sendSMTP (Just sentRef) myDomain sockAddr [message]
    return ()
  where
    smtpHost     = "smtp.york.ac.uk"
    myDomain     = "york.ac.uk"
    recipients'  = map toName recipients
    sender'      = [toName sender]
    toName (n,e) = NameAddr (Just n) e

-- | break a list apart on seperator sep
explode::Eq a => a -> [a] -> [[a]]
explode _ [] = []
explode sep (x':xs) | sep == x' = explode sep xs
explode sep xs = takeWhile (/=sep) xs : explode sep (dropWhile (/=sep) xs)


