# Quasi-deterministic parallel programming with LVars and callbacks

`freeze ... after ...` is a language construct that takes two arguments, an LVar and a callback function, and does two things with them:

   * Attaches the callback to the LVar so that it runs for every `put` to the LVar.  The callback is a function that takes a lattice element as its argument.  Its return value is ignored, so it runs solely for effect.  For instance, a callback might itself do a `put` to the LVar to which it is attached, triggering yet more callbacks.

   * As soon as all callbacks are done running, `freeze ... after ...` _freezes_ the LVar that was passed to it and returns its _exact_ state (rather than an underapproximation of the state, as with _get_). 

Attaching a callback to an LVar with `freeze ... after ...` is _not_ the same thing as typical event handler registration!  Typically, if one registers an event handler to, say, respond to mouse clicks, then any clicks that happened _before_ the handler was registered will just go unhandled; it is as though those clicks never happened.  LVar callbacks are different: if we write to an LVar several times and then attach a callback, then the callback will run for each write that already occurred.

In fact, if A and B are possible states of an LVar, where A <= B, and a write has occurred that leaves the LVar in state B, then the callback will be triggered _twice_, once for A and once for B.  In general, the callback will run for every LVar state _at or below_ the current state at the time the callback is attached.

Moreover, if the LVar's state increases further _while_ those callbacks are running, then the callback will run for the newly reached state and all the intermediate states below it.

## How do we know when to freeze?

`freeze ... after ...` keeps track of those LVar states that have been handled by the callback so far.  It can also determine the set of all states that are at or below the current state of the LVar (which will always be a finite set, even if the LVar has infinite states).  When we reach a point of _quiescence_ -- that is, a point such that the set of handled states includes all states that are at or below the current state of the LVar -- the `freeze ... after ...` computation ends, returning the LVar's current state.

Of course, whether or not a computation is quiescent is a non-monotonic property: we can move in and out of quiescence as more `put`s to an LVar occur, and even if all states at or below the current state have been handled, there is no way to know that more `put`s will not arrive to increase the state and trigger more callbacks.  This is why we _freeze_ the state of the LVar before returning it.  A frozen LVar's state can no longer change, and any attempt to `put` a value to a frozen LVar that would increase its state will result in an error.

Therefore, freezing is a way of "betting" that all callbacks have completed.  For a given run of a program, either all `put`s to an LVar arrive before it has been frozen, in which case the value returned by `freeze ... after ...` is the least upper bound of those values, or some `put` arrives after the LVar has been frozen, in which case the progrm will fault.  We call this property _quasi-determinism_: a program will always either evaluate to the same answer or they will fault.  Moreover, if we write our programs in such a way that all writes to an LVar are done _by the callback function_ rather than by some other `put` that is racing to finish before `freeze ... after ...` does, then we are guaranteed determinism and not just quasi-determinism.

## Semantics of `freeze ... after ...`

The expression `freeze lv after (lambda (x) e)` has the following semantics.  First, we look up the state of the LVar `lv`.  Then, for each state in the lattice that is at or below the state of `lv`, we call `(lambda (x) e)` with that state as its argument.  Even if `lv` has never been written to, in which case its state is `Bot`, the least element of the lattice, we still run `(lambda (x) e)` once, with `Bot` as its argument.

Whenever we invoke the callback function on a state, we add that state to the "handled" set.  When we reach a point where the handled set contains all states at or below the current state of the LVar, we're done, and we "freeze" the LVar and return its current state.

## Callbacks that write back to the LVar

A callbacks attached to an LVar is invoked every time the LVar is written to.  Such a callback can itself write to the LVar, triggering yet more callbacks.  This allows for some interesting programming patterns.

For instance, imagine using an LVar `lv` to accumulate a set of nodes in a graph.  Suppose we want to find the connected component of the graph containing a particular node `n0`.  We define the following callback function (here, suppose that `neighbors` is a function that takes a node and returns the set of its neighbors):

```
let cb =
  lambda (state):
    foreach node in state:
      foreach neighbor_node in (neighbors node):
        put lv { neighbor_node }
```

Then, suppose we run `put lv { n0 }`, followed by `freeze lv after cb`.

Since `n0` is already in the set, the callback runs with `n0` as its argument, and `n0` is added to the handled set.  If `n0` has neighbors `n1` and `n2`, then `put lv { n1 }` and `put lv { n2 }` both run, themselves each triggering another set of callbacks and bumping the state of `lv` up to `{ n0, n1, n2 }`.  (In this example, the callback will be invoked multiple times for each state.  This is no problem, since LVar writes are idempotent.  This process continues until the handled set contains all states that are at or below the state of `lv`, at which point the LVar is considered to have quiesced, and we freeze it and return its state.

What if another `put` to `lv` is racing with this whole process?  If it completes first, then the LVar will not quiesce; it will keep running until `lv`'s new state and the states below it have all been handled.  If the `freeze ... after ...` completes first, then the `put` will fault.  These are the two possibilities that quasi-determinism permits.
