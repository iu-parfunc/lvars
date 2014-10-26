{-# LANGUAGE BangPatterns #-}

-- LK: N.B. Once Data.LVar.Counter is done, we should just be able to
-- glue two of those together for this, just as AddRemoveSet does with
-- sets.

{-|

This module provides a /PN-Counter/, a counter that allows both
increment and decrement operations.  This is possible because, under
the hood, it's represented with two monotonically growing counters,
one for increments and one for decrements.  The name "PN-Counter"
comes from the literature on /conflict-free replicated data types/.

 -}
module Data.LVar.PNCounter
       (
         PNCounter,
         newCounter, newCounterWithValue,
         increment, waitForIncrements,
         decrement, waitForDecrements,

         freezeCounter
         
       ) where
import           Control.LVish
import           Control.LVish.Internal
import qualified Data.Atomics.Counter.Reference as AC
-- LK: FIXME: it can't be okay to use SchedIdempotent if we're using bump, can it?!
-- import           Control.LVish.SchedIdempotent (newLV)
import           Data.IORef


-- | The counter datatype.

-- LK: LVar around the outside, or PureLVar?  What's the difference?
data PNCounter s = LVar s (AC.AtomicCounter, AC.AtomicCounter)
  
-- | Create a new `PNCounter` set to zero.
newCounter :: Par d s (PNCounter s)
newCounter = newCounterWithValue 0

-- | Create a new `PNCounter` with the specified initial value.
newCounterWithValue :: Int -> Par d s (PNCounter s)
-- LK: hm, how do I create IORefs and then return a Par?  I think what
-- I'm supposed to be doing here is wrapping an unsafe internal Par
-- computation (that's allowed to do IO) in a safe one that I return.
newCounterWithValue n = undefined
-- FIXME...
  --                       do
  -- incs <- newIORef (Just n)
  -- decs <- newIORef Nothing

-- | Increment the `PNCounter`.
increment :: PNCounter s -> Par d s ()
increment = undefined

-- | Wait for the number of increments to reach a given number.
waitForIncrements :: Int -> PNCounter s -> Par d s ()
waitForIncrements = undefined

-- | Decrement the `PNCounter`.
decrement :: PNCounter s -> Par d s ()
decrement = undefined

-- | Wait for the number of decrements to reach a given number.
waitForDecrements :: Int -> PNCounter s -> Par d s ()
waitForDecrements = undefined

-- | Get the exact contents of the counter.  As with any
-- quasi-deterministic operation, using `freezeCounter` may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
freezeCounter :: PNCounter s -> QPar s Int
-- Freezing takes the difference of increments and decrements.
freezeCounter = undefined
