lvars
=====

[![Build Status](https://travis-ci.org/iu-parfunc/lvars.svg?branch=master)](https://travis-ci.org/iu-parfunc/lvars)

#### A collection of prototype LVar implementations, tools, and mechanized semantics.

This is an umbrella repository for work related to LVars.
Subdirectories include:

  * [race-detector-interps]: an implementation of lambdaLVar extended
     with a destructive `consume` operation and a data-race detector
     for lambdaLVar + `consume`.

  * [haskell]: LVar libraries for Haskell, based on the
    [monad-par](http://hackage.haskell.org/package/monad-par) library.

[race-detector-interps]: https://github.com/iu-parfunc/lvars/tree/master/race-detector-interps
[haskell]: https://github.com/iu-parfunc/lvars/tree/master/haskell
