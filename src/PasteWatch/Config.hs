{-# LANGUAGE OverloadedStrings #-}
-- | All user config goes here
module PasteWatch.Config
    (
      config,
      siteConfigs
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
    pauseMax       = 5,
    sender         = ("Me", "do-not-reply@example.com"),
    smtpServer     = "smtp.example.com",
    recipients     = [("Mr. Me", "me@example.com")]
}

siteConfigs::[SiteConfig]
siteConfigs = [
  SiteConfig {
    siteType  = Pastebin,
    delayTime = 10,
    pruneTime = 600
  },
  SiteConfig {
    siteType  = Pastie,
    delayTime = 33,      -- 30 sec + skew
    pruneTime = 1200
  },
  SiteConfig {
    siteType  = SkidPaste,
    delayTime = 247,     -- 4 mins + skew
    pruneTime = 7200
  },
  SiteConfig {
    siteType  = Slexy,
    delayTime = 251,     -- 4 mins + skew
    pruneTime = 7200
  }
 ]

