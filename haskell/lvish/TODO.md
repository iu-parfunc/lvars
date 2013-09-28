# LVish implementation TODOs

  * Do Hackage release as soon as possible

  * Make sure there are SHOW instances.  
  * XX (Ryan) Factor out Data.LVar.Generic.
  * XX (Ryan) Finish providing instances/methods for LVarData1.
  * (Ryan) Use modern (released) HSBencher.

  * ~~(Lindsey) Finish factoring out the old implementation into its own cabal pkg.~~ [LK: done 9/16/2013]
  * (Lindsey) Turn on Safe Haskell for real. [LK: I need help figuring out how to do this]
  * ~~(Lindsey) Finish porting LVars (those not done) to the shiny new
    DeepFrz method (use IVar as an example):
      * Set 
      * Map
      * SLSet      
      * SLMap 
      * IStructure ...~~ [LK: done 9/20/2013]
  
  * New monad-par release with necessary tweaks

## Scheduler and data-structure layer concerns
 
  * Leveraging idempotence -- use idemp-WS deque?

  * Turning OFF idempotence: What about BUMP / atomic counters!?!?!?!?
	  - cheap to prevent dups of: blocked-getters
	  - expensive to prevent dups of: launched-handlers
	  - (CRDT people struggled with atomic counter thing.)
	  - PhyBin is an app that absolutely DEPENDS on bump.  
		It falls back to IO right now...

  * Recover non-idempotent operations with a hidden IORef...
	Idea is that thread dup will NOT happen between parallel API ops.
	Where do we manufacture such IORefs for use before the counter?
	Where the counter is made? 
	  - OR why not have ANOTHER MONAD... (kind of a sub-monad)
		   In the submonad it does dedup on every parallel op.
	  - OR link against multiple schedulers... 

  * (Ryan and Aaron) Microbenchmark and optimize SLMap/SLSet

  * (Aaron Todd) Concurrent Bag?
  
  * Other Map implementations (non-skiplist)

  * Make freezing for SLSet O(1)

  * Continue expanding benchmark suite in general
	  - PBBS outstanding TODOs: variants of IStructure/BitVector
		Ran into big problems re: scalability of blocked-lists.  
		  - (Boost C++ `intrusive_ptr` vs. `shared_ptr` analogy)
	  - PBBS good performance would require better support 
		for DETERMINISTIC RESERVATIONS!  (*Bulk* handling of blocked 
		iterations of a parallel for-loop.)

  * __Outstanding bugs?????__  SNZI?
	-- Testing framework is WEIRD... sometimes things fail in combination
	   that do not fail individually.  Strange observations that we don't 
	   understand.

## Old TODOs (circa July)

  * Adapt tests to LVarIdempotent.

  * Examples/benchmarks:
	 - graph traversal
	 - CFA
	 - other PBBS graph algorithms

  * Scalable data structures:
	 - Counter (use SNZI, http://dl.acm.org/citation.cfm?id=1281106)
	 - Bags
	 - Hashtables (=> sets)

	 - Work-stealing deque (could use bag)

  * Fill out data structure interfaces:

	 * in addition to "newEmpty*" we should have a few ways to provide
	   elements at the outset, before it is exposed for concurrent
	   modification.

  * Fixes:

	 * modify and insert operations on the same IMap probably should be
	   disqualified at the type level...

  * Optimizations:

	 * Expose a version of addHandler that does NOT support quiesce.
