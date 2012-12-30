-- | Some misc utility functions
module PasteWatch.Utils
    (
      sendEmail,
      sq
    ) where

import Data.IORef
import Network.SMTP.Client
import Network.Socket
import System.Time         (getClockTime, toCalendarTime)

import PasteWatch.Types hiding (recipients, sender, smtpServer)

-- | Send an email with given subject and contents
-- using the given (unauthenicated) smtp server
-- myDomain is the domain of the sender
-- This function taken from the example code in Network.SMTP.Client
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
    addrs <- getAddrInfo Nothing (Just $ show smtpServer) Nothing
    let SockAddrInet _ hostAddr = addrAddress (head addrs)
        sockAddr = SockAddrInet 25 hostAddr
    sentRef <- newIORef []
    sendSMTP (Just sentRef) (show myDomain) sockAddr [message]
    return ()
  where
    recipients'  = map ((toName . (\x -> ("", x))) . show) recipients
    sender'      = [toName ("", show sender)]
    toName (n,e) = NameAddr (Just n) e

-- | Remote outmost double quotes from a string
sq::String -> String
sq ('"':s)  | last s == '"'  = init s
            | otherwise      = s
sq ('\'':s) | last s == '\'' = init s
            | otherwise      = s
sq s                         = s
