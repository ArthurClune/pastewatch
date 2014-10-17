{-# LANGUAGE OverloadedStrings #-}

-- | Some misc utility functions
module PasteWatch.Utils
    (
      sendEmail,
      stripQuotes
    ) where

import qualified Data.Text.Lazy as T
import           Network.Socket
import           Network.Mail.SMTP

import Data.Monoid ( (<>) )

import PasteWatch.Types (MatchText (..) )

-- | Send an email with given subject and contents
-- using the given (unauthenicated) smtp server
-- myDomain is the domain of the sender
-- This function taken from the example code in Network.SMTP.Client
sendEmail::Address -> [Address] -> HostName -> MatchText -> String -> IO()
sendEmail sender recipients smtpServer (MatchText match) content =  sendMail smtpServer mail
  where
    to         = recipients
    cc         = []
    bcc        = []
    body       = plainTextPart $ T.pack content
    subject   = "Pastebin alert. Match on " <> match
    mail       = simpleMail sender to cc bcc subject [body]


stripQuotes::String -> String
stripQuotes ('"':s)  | last s == '"'  = init s
                     | otherwise      = s
stripQuotes ('\'':s) | last s == '\'' = init s
                     | otherwise      = s
stripQuotes x = x



