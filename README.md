PasteWatch
==========


Code to watch pastesites and alert on specific strings

To use install the Haskell Platform (http://www.haskell.org/platform/, version 2012.2.0.0 or later)

Edit src/PasteWatch/Config.hs to taste.

$ cabal configure --enable-tests
$ cabal install

You'll get a binary pastewatch in ~/.cabal/bin (Linux) or ~/Library/Haskell/bin (OS X)

Copy sample.config to my.config (or whatever) and edit the values in there suitably.

Run with
```
$ pastewatch +RTS -N4 -k48m -RTS my.config
```
where the number -N4 = number of cores you want the program to use.

Tests
-----

A simple test runner is included (tests/pastetests.hs)
```
$ cabal test
```
to run.

Bugs
----

1) It currently uses Attoparsec.ByteString. It should use Attoparsec.Text instead for unicode goodness


