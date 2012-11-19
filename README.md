PasteWatch
==========


Code to watch pastesites and alert on specific strings

To use install the Haskell Platform (http://www.haskell.org/platform/, version 2012.2.0.0 or later)

Edit src/PasteWatch/Config.hs to taste.

$ cabal configure --enable-tests
$ cabal install

You'll get a binary pastewatch in ~/.cabal/bin (Linux) or ~/Library/Haskell/bin (OS X)

Run with
```
$ pastewatch +RTS -N4 -RTS 
```
where the number -N4 = number of cores you want the program to use.

Tests
-----

A simple test runner is included (tests/pastetests.hs) and can be used to check that your patterns are matching as expected. 
Edit pastetests.hs to match your Config.hs file, then run
```
$ cabal test
```
to run.

Bugs
----

1) It currently uses Attoparsec.ByteString. It should use Attoparsec.Text instead for unicode goodness

2) The URL fetch code needs to implement backoff and retry on failure

3) The delay for timing out URLs shoud be per-site 


