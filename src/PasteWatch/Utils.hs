-- | Some misc utility functions
module PasteWatch.Utils
    (
      explode,
      sendEmail
    ) where

import Data.IORef
import Network.SMTP.Client
import Network.Socket
import System.Time (getClockTime, toCalendarTime)

import PasteWatch.Types (Domain, Email, Host)

-- | break a list apart on seperator sep
explode::Eq a => a -> [a] -> [[a]]
explode _ [] = []
explode sep (x':xs) | sep == x' = explode sep xs
explode sep xs = takeWhile (/=sep) xs : explode sep (dropWhile (/=sep) xs)

-- | Send an email with given subject and contents
-- using the given (unauthenicated) smtp server
-- myDomain is the domain of the sender
sendEmail::Email
           -> [Email]
           -> Domain 
           -> Host
           -> String
           -> String 
           -> IO()
sendEmail sender
          recipients
          myDomain
          smtpServer
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
    addrs <- getAddrInfo Nothing (Just smtpServer) Nothing
    let SockAddrInet _ hostAddr = addrAddress (head addrs)
        sockAddr = SockAddrInet 25 hostAddr
    sentRef <- newIORef []
    sendSMTP (Just sentRef) myDomain sockAddr [message]
    return ()
  where
    recipients'  = map toName recipients
    sender'      = [toName sender]
    toName (n,e) = NameAddr (Just n) e
