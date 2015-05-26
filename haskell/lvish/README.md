# A Haskell implementation of the LVish parallel programming model, based on LVars

_LVish_ is a deterministic or [quasi-deterministic] parallel programming model that
extends [LVars] to incorporate _freezing_ and _event handlers_.  This
directory contains an implementation of LVish as a monadic Haskell
library based on [monad-par].

[quasi-deterministic]: http://www.cs.indiana.edu/~lkuper/papers/2013-lvish-draft.pdf
[LVars]: https://www.cs.indiana.edu/~lkuper/papers/lvars-fhpc13.pdf
[monad-par]: http://hackage.haskell.org/package/monad-par

## Applications for LVish

### Existing

  * [Graph algorithms from PBBS](https://github.com/iu-parfunc/lvars/tree/master/pbbs-haskell/benchmarks/graphs)

  * [Parallel _k_-CFA](https://github.com/iu-parfunc/lvars/blob/master/apps/cfa) using LVars for sharing.
  
### In progress

  * [PhyBin](https://github.com/rrnewton/PhyBin): A tool for
    classifying phylogenetic trees by their topology.  Uses LVars for
    a particular algorithm that computes the all-to-all tree edit
    distance among a set of phylogenetic trees.  (The code that uses
    LVars is on a branch, waiting for improvements to LVish.)  Mailing
    list post about it by Ryan:
    https://groups.google.com/forum/#!topic/lattice-variables/HD7e6NImCnA.

    * Lindsey wonders what the relationship is to general topological
      sorting and whether this algorithm could be generalized.
	  
  * Parallel logic programming: Will Byrd has
    [a rough outline of a version of miniKanren](https://github.com/webyrd/latticeKanren)
    that could use an LVar-like mechanism for communication.
	
### Not yet started

  * Parallel alpha-beta search
  
  * More graph algorithms


