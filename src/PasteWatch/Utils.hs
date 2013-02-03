-- | Some misc utility functions
module PasteWatch.Utils
    (
      sendEmail,
      stripQuotes
    ) where

import           Data.IORef
import qualified Data.Text as T
import           Network.SMTP.Client
import           Network.Socket
import           System.Time         (getClockTime, toCalendarTime)

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
sendEmail (Email sender)
          recipients
          (Domain myDomain)
          (Host smtpServer)
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
    addrs <- getAddrInfo Nothing (Just smtpServer') Nothing
    let SockAddrInet _ hostAddr = addrAddress (head addrs)
        sockAddr = SockAddrInet 25 hostAddr
    sentRef <- newIORef []
    sendSMTP (Just sentRef) myDomain' sockAddr [message]
    return ()
  where
    myDomain'    = stripQuotesT myDomain
    recipients'  = map (toName . ( \(Email x) -> ("", stripQuotesT x))) recipients
    sender'      = [toName ("", stripQuotesT sender)]
    smtpServer'  = stripQuotesT smtpServer
    toName (n,e) = NameAddr (Just n) e

-- | Remove outmost double quotes from a string
stripQuotesT::T.Text -> String
stripQuotesT = stripQuotes . show

stripQuotes::String -> String
stripQuotes ('"':s)  | last s == '"'  = init s
                     | otherwise      = s
stripQuotes ('\'':s) | last s == '\'' = init s
                     | otherwise      = s
stripQuotes s                         = s
