Adapt tests to LVarIdempotent.

Examples/benchmarks:
  - graph traversal
  - CFA
  - other PBBS graph algorithms

Scalable data structures:
  - Counter (use SNZI, http://dl.acm.org/citation.cfm?id=1281106)
  - Bags
  - Hashtables (=> sets)
  
  - Work-stealing deque (could use bag)


Fill out data structure interfaces:

  * in addition to "newEmpty*" we should have a few ways to provide
    elements at the outset, before it is exposed for concurrent
    modification.


Fixes:

  * modify and insert operations on the same IMap probably should be
    disqualified at the type level...

Optimizations:

  * Expose a version of addHandler that does NOT support quiesce.

