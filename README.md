PasteWatch
==========


Code to watch pastesites and alert on specific strings

To use install the Haskell Platform (http://www.haskell.org/platform/, version 2012.2.0.0 or later)

Edit src/PasteWatch/Config.hs to taste.
```
$ cabal configure --enable-tests
$ cabal install
```
You'll get a binary pastewatch in ~/.cabal/bin (Linux) or ~/Library/Haskell/bin (OS X)

Copy sample.config to my.config (or whatever) and edit the values in there suitably.

Run with
```
$ pastewatch +RTS -N4 -T -RTS my.config
```
where the number -N4 = number of cores you want the program to use.

Monitoring is available on http://localhost:8000

This is currently not authenticated - set firewall rules suitably!


Compilation on OS X
===================

You'll need to install the ICU libraries. The easiest way is using HomeBrew:
```
brew install icu4c
cabal-dev install text-icu --extra-include-dirs=/usr/local/opt/icu4c/include --extra-lib-dirs=/usr/local/opt/icu4c/lib
```

then proceed as above.

Database Support
----------------

Experimental DB support for MongoDB.

Run MongoDB on the same host as this script with no auth to play.

This is not suitable for real use yet. The schema will change, there are no indexes etc.

Current schema:
```
schemaVer =: Int,
ts        =: Int,
url       =: String,    # URL for this paste
content   =: String,    # Content of the paste
tags      =: [String],  # List of tags
alertedOn =: String     # String that triggered the alert
site      =: String     # "PASTEBIN",  "PASTIE" etc
```


Extras
------

A Debian/Ubuntu init script and a script to make a Debian/Ubuntu package are included in extras/

A .deb for Ubuntu 12.04 is available from http://files.clune.org/pastewatch.deb (SHA256 a7a591ba2bc46ef6e87360710697c8eeb88f2f28a0a47db6301ac5200ce54eca)

To use:
```
# dpkg -i pastewatch.deb
```
Edit /etc/pastewatch.conf to taste and then run
```
/etc/init.d/pastewatch start
```

Tests
-----

A simple test runner is included (tests/pastetests.hs)
```
$ cabal test
```
to run.

Sites Covered
-------------

Currently pastebin.com, pastie.org, skidpaste.org, slexy.org and snipt.org

Adding a new site
-----------------

Add the new site to "Sites" type in Types.hs

In Sites.hs, you must

* Implement two functions for each new site: one that gets the list of new/recent sites (getNewPastes) and one that give a URL for a paste runs the check function (doCheck)
* Add some values for the various timeouts in siteConfigs for your new site.



