{-# LANGUAGE OverloadedStrings #-}
-- | All user config goes here
module PasteWatch.Config
    ( 
      config
    ) where

import PasteWatch.Types

-- | Data structure for the config
--
-- See the haddock docs for Types.hs for the details
config::Config
config = Config {
    alertStrings   = ["@example.com",
                      "@sub.example.com"
                     ],
    alertStringsCI = ["my company inc"],
    domain         = "example.com",
    nthreads       = 32,
    sender         = ("Me", "do-not-reply@example.com"),
    smtpServer     = "smtp.example.com",
    recipients     = [("Mr. Me", "me@example.com")],
    delayTime      = 10*1000000
}

