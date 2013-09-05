# A prototype Haskell implementation of LVish

_LVish_ is a quasi-deterministic [0] parallel programming model that
extends LVars [1] to incorporate _freezing_ and _event handlers_.
This directory contains an implementation of LVish as a monadic
Haskell library based on the Par monad [2] [3].

[0]: http://www.cs.indiana.edu/~lkuper/papers/2013-lvish-draft.pdf
[1]: https://www.cs.indiana.edu/~lkuper/papers/lvars-fhpc13.pdf
[2]: http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/monad-par.pdf
[3]: http://hackage.haskell.org/package/monad-par

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


