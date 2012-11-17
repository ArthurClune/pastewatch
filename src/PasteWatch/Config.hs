{-# LANGUAGE OverloadedStrings #-}

module PasteWatch.Config
    ( 
      alertStrings,
      alertStringsCI, 
      recipients,
      sender
    ) where

import Data.ByteString.Char8 as S
import PasteWatch.Types (Email)


-- Strings to alert on (case sensitive) if seen in a paste
alertStrings::[S.ByteString]    
alertStrings  = ["@example.com", 
                 "@sub.example.com"
                ]

-- Strings to alert on (case insensitive) if seen in a paste
alertStringsCI::[S.ByteString]    
alertStringsCI = ["my company inc"]

-- Send alert emails as?
-- Format: ("Real Name", "email")
sender::(Email)
sender        = ("Me", "do-not-reply@example.com")

-- Send alert emails to?
-- List of recipients
recipients::[Email]
recipients    = [("Mr. Me", "me@example.com")]
