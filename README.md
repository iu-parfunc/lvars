lvars
=====

#### A collection of prototype LVar implementations, tools, and mechanized semantics.

This is an umbrella repository for work related to LVars.
Subdirectories include:

  * [redex]: a PLT Redex model of the lambdaLVar calculus ([see
    README](https://github.com/iu-parfunc/lvars/tree/master/redex#readme));

  * [race_detector_interps]: an implementation of lambdaLVar extended
     with a destructive `consume` operation and a data-race detector
     for lambdaLVar + `consume`.

  * [haskell-prototype]: a prototype implementation of LVars based on
    the [monad-par](http://hackage.haskell.org/package/monad-par)
    Haskell library.


[redex]: https://github.com/iu-parfunc/lvars/tree/master/redex
[race_detector_interps]: https://github.com/iu-parfunc/lvars/tree/master/race_detector_interps
[haskell-prototype]: https://github.com/iu-parfunc/lvars/tree/master/haskell-prototype
