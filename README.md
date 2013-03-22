lvars
=====

#### A collection of prototype LVar implementations, tools, and mechanized semantics.

This is an umbrella repository for work related to LVars, including:

  * [lambdaLVar-redex], a PLT Redex model of the lambdaLVar calculus
    ([see
    README](https://github.com/iu-parfunc/lvars/tree/master/redex#readme));

  * [race_detector_interps], an implementation of the lambdaLVar
    calculus extended with destructive observations (`consume`), and a
    data race detector for lambdaLVar + `consume` ([see
    README](https://github.com/iu-parfunc/lvars/tree/master/race_detector_interps#readme));

  * [haskell-prototype], a practical prototype implementation of LVars
    in Haskell based on the monad-par library ([see
    README](https://github.com/iu-parfunc/lvars/tree/master/haskell-prototype#readme)).


[lambdaLVar-redex]: https://github.com/iu-parfunc/lvars/tree/master/redex
[race_detector_interps]: https://github.com/iu-parfunc/lvars/tree/master/race_detector_interps
[haskell-prototype]: https://github.com/iu-parfunc/lvars/tree/master/haskell-prototype
