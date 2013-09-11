lvars
=====

#### A collection of prototype LVar implementations, tools, and mechanized semantics.

This is an umbrella repository for work related to LVars.
Subdirectories include:

  * [redex]: PLT Redex models of LVar calculi ([see
    README](https://github.com/iu-parfunc/lvars/tree/master/redex#readme)).

  * [race-detector-interps]: an implementation of lambdaLVar extended
     with a destructive `consume` operation and a data-race detector
     for lambdaLVar + `consume`.

  * [haskell]: LVar libraries for Haskell, based on the
    [monad-par](http://hackage.haskell.org/package/monad-par) library.

[redex]: https://github.com/iu-parfunc/lvars/tree/master/redex
[race-detector-interps]: https://github.com/iu-parfunc/lvars/tree/master/race-detector-interps
[haskell]: https://github.com/iu-parfunc/lvars/tree/master/haskell
