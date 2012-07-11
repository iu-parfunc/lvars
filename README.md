lambdapar-redex
===============

A PLT Redex model of a variant of the lambdapar language.

### lambdapar in a nutshell

lambdapar is a deterministic parallel calculus with shared state,
based on the untyped lambda calculus, and extended with `put` and
`get` operations that write to and read from shared variables in the
store.  In this setting of shared mutable state, the trick that
lambdapar employs to maintain determinism is that writes must be
_monotonically increasing_ according to the partial order on a
user-specified partially ordered set, and reads must make only limited
observations of the states of variables -- for instance, in a
lambdapar program it might be possible to observe that a store
location contained "at least 4", but not possible to observe the
precise value.  The code in this repository is a PLT Redex model of a
variant of the lambdapar language where the reduction relation has
been tweaked from its "canonical" on-paper version and the partially
ordered set is the set of natural numbers ordered by `<=`.

### Why we don't use evaluation contexts

With Redex, one typically defines a Felleisen-and-Hieb-style [1]
reduction semantics based on _evaluation contexts_. An
evaluation-context-based semantics is what Redex is best at
expressing, and such a semantics would eliminate the need to
explicitly specify "structural" rules like E-Put-1 and E-Put-2 in our
semantics.  Instead, we could simply specify a set of evaluation
contexts:

```
(E hole (put E e) (put e E) ...)
```

Unfortunately, such _single-hole_ evaluation contexts force evaluation
to happen sequentially, and we want to model the explicit simultaneous
evaluation steps of the E-ParApp rule of our semantics.  To be sure, a
semantics specified with single-hole evaluation contexts can express
_arbitrary_ evaluation order and therefore remain open to the
possibility of parallel _implementation_. Still, since parallelism is
lambdapar's _raison d'etre_, we want to bake parallelism into the
model.

Since Redex does not (at the time of this writing) have support for
multiple-hole evaluation contexts [2], we opted instead for an
inference-rule-based semantics implemented using Redex's
`define-judgment-form` feature [3].  Unfortunately, in so doing, we
miss out on some of Redex's most useful features.  As a tiny example,
`define-judgment-form` offers no way to name individual reduction
rules, so although using Redex's `traces` feature [4] with our
semantics will show us a beautiful reduction graph of a configuration,
it won't label the edges in the graphs with the names of the reduction
rules as it would normally, because Redex has no way of knowing their
names.

### Dealing with reflexive relations

One of the advantages of using Redex for semantics engineering is its
built-in support for unit testing and randomized
testing. Unfortunately, the reflexive reduction rules E-Refl and
E-ReflErr of our paper semantics pose a dilemma for the Redex testing
infrastructure. Redex's built-in `test-->>` mechanism for testing a
reduction relation finds all irreducible terms reachable from a given
term, but with E-Refl and E-ReflErr present in the reduction relation,
no lambdapar terms would be irreducible under it, so it wouldn't be
possible to write tests with `test-->>`.  An alternative testing
mechanism, `test-->>E`, checks if there _exists_ a reduction path from
one given term to another, but since the property we are most
interested in testing for is determinism, that mechanism is also
unsatisfactory since we wish to know not only that one term reduces to
another, but that _all_ possible reductions take us from the
first to the second.

Fortunately, there is a simple workaround: we drop the E-Refl and
 E-ReflErr rules from our semantics and instead add two new rules,
 E-App-1 and E-App-2, by which parallel application expressions may
 take a step even if only one of their subexpressions can take a step.
 The result is a semantics that is feasible to test with Redex.
This reduction relation is called `small-step-base-rr`.

### Speed tweaks

Under the semantics just described, if both subexpressions in an
application can step, then any of three rules can apply next --
E-App-1, E-App-2, and E-ParApp -- leading to an exponential increase
in the number of evaluation paths that an configuration might take. It
is easy to construct lambdapar programs that are very slow to test in
Redex under this semantics, because the system must take all
evaluation paths in order to confirm that a term is reducing
deterministically.  To ameliorate the slowness, we can define more
restricted versions of E-App-1 and E-App-2, in which the subexpression
that is not taking a step must be a _value_.  Finally, we add an
E-GetValBlock rule, which allows a _blocked_ `get` expression to step
to itself. This is necessary because a blocked `get` is not a value.
We call the resulting reduction relation `small-step-fast-rr`.

Under the `small-step-fast-rr` rules, an application expression in
which one subexpression is a blocked `get` will always be able to take
a step under one of the three application rules, but not all thread
interleavings will be explored.  The speed boost we get from that
comes at the price of modeling only a less realistic class of
implementations in which parallel evaluation is ``lockstep''.

### Building and running

Running `make all` from the lambdapar-redex directory will build and
all the tests will run, using both reduction relations.  There's one
particular test that is so slow under `small-step-base-rr` that we put
it in a "slow test suite" by itself.  It's the one with the "warning"
comment in lambdapar-test.rkt.

Here are what the performance results of from a recent run of `make
all` look like:

```
Running metafunction tests...All 51 tests passed.
cpu time: 22 real time: 22 gc time: 0
Running test suite with small-step-fast-rr...All 17 tests passed.
cpu time: 307 real time: 309 gc time: 0
Running test suite with small-step-base-rr...All 17 tests passed.
cpu time: 1004 real time: 1012 gc time: 66
Running slow test suite with small-step-fast-rr...One test passed.
cpu time: 320 real time: 324 gc time: 15
Running slow test suite with small-step-base-rr...One test passed.
cpu time: 1010991 real time: 1022071 gc time: 13267
```

Here, the "slow test suite" -- which, again, is just one test -- takes
_three orders of magnitude_ longer when run with `small-step-base-rr`
than with `small-step-fast-rr` does.  (Those numbers are in
milliseconds -- so the slow test is taking about 17 minutes!)
Stepping through the test manually using `traces` finds 64 terms
for the slow version, and 15 for the fast version.

### ...but Redex is still awesome

Lest this README sound like a litany of complaints about Redex, we
feel it's important to point out that even despite its ill-suitedness
for the setting of truly simultaneous reduction, our Redex model
helped reveal bugs in our semantics -- and then test that proposed
fixes would work -- much faster than we would have been able to do
otherwise.  We also don't believe that we have yet learned everything
that Redex has to teach us about lambdapar.  Having said that, an
interesting exercise would be to try modeling Redex in a framework
that has better support for simultaneous reductions -- the K system
[5] comes to mind -- and see what else we can learn.

### References

[1] http://www.ccs.neu.edu/racket/pubs/tcs92-fh.pdf
[2] http://lists.racket-lang.org/users/archive/2012-July/053000.html
[3] http://docs.racket-lang.org/redex/Other_Relations.html#%28form._%28%28lib._redex/reduction-semantics..rkt%29._define-judgment-form%29%29
[4] http://docs.racket-lang.org/redex/GUI.html?q=traces#%28def._%28%28lib._redex/gui..rkt%29._traces%29%29
[5] http://k-framework.org

