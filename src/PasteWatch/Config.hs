{-# LANGUAGE OverloadedStrings #-}

module PasteWatch.Config
    ( 
      config
    ) where

import PasteWatch.Types

config::Config
config = Config {
    -- Strings to alert on (case sensitive) if seen in a paste
    alertStrings   = ["@example.com",
                      "@sub.example.com"
                     ],
    -- Strings to alert on (case insensitive) if seen in a paste
    alertStringsCI = ["my company inc"],
    -- domain 
    domain         = "example.com",
    -- Number of Haskell (lightweight) threads to use
    -- N of these are mapped onto M OS threads where M is set
    -- by the +RTS -N option (see README.md)
    nthreads       = 32,
    -- Send alert emails as?
    -- Format: ("Real Name", "email")
    sender         = ("Me", "do-not-reply@example.com"),
    -- Send alert emails to?
    -- List of recipients
    smtpServer     = "smtp.example.com",
    recipients     = [("Mr. Me", "me@example.com")],
    -- Time to pause on main thread each loop (microseconds)
    delayTime      = 10*1000000
}

