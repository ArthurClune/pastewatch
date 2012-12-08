-- | Some misc utility functions
module PasteWatch.Utils
    (
      insertURL,
      notSeenURL,
      pruneURLs,
      sendEmail
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import qualified Data.HashMap.Strict as Map
import Network.SMTP.Client
import Network.Socket
import System.Time (getClockTime, toCalendarTime)
import qualified Data.Time.Clock as Time

--import PasteWatch.Types (Domain, Email, Host, Control, SiteConfig, URL)
import PasteWatch.Types hiding (recipients, sender, smtpServer)

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

---------------------------------------------------
-- Functions to maintain our map of urls and time
-- seen
---------------------------------------------------

-- | Add a link to the map of links we have seen
-- Key is the url, value is timestamp
insertURL :: URL -> Control ()
insertURL l = do
    time <- liftIO Time.getCurrentTime
    modify $ \s ->
      s { linksSeen = Map.insert l time (linksSeen s) }

-- prune all URLs first added more than pruneTime ago
pruneURLs::Control ()
pruneURLs = do
    now <- liftIO Time.getCurrentTime
    sc <- ask
    let filterf x = Time.diffUTCTime now x < pTime
        pTime = pruneTime sc
      in
        modify $ \s -> s { linksSeen =
            Map.filter filterf (linksSeen s) }

-- | If we have not seen a link before, return True and record that we have
-- now seen it.  Otherwise, return False.
notSeenURL::URL -> Control Bool
notSeenURL url =  do
    seen <- Map.member url `liftM` gets linksSeen
    if seen
        then return False
        else insertURL url >> return True