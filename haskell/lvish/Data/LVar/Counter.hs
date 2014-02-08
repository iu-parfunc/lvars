{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | An integer LVar that exposes a non-idempotent `increment`
-- operation.  It differs from `Data.LVar.MaxPosInt` in that it allows
-- non-idempotent updates and does _not_ allow least-upper-bound
-- updates.

module Data.LVar.Counter
       ( Counter(..),
         newCounter, increment, waitThresh, freezeCounter, fromCounter
       ) where

import Control.LVish hiding (freeze, put)
import Control.LVish.Internal (Par(WrapPar), LVar(WrapLVar), state)
import Control.LVish.DeepFrz.Internal
-- LK: Isn't it wrong to use SchedIdempotent here, because increment
-- isn't idempotent?
import qualified Control.LVish.SchedIdempotent as LI 
import qualified Data.Atomics.Counter.Reference as AC
import           System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

-- | A @Counter@ is a wrapper around the `AtomicCounter` exposed by
-- `Data.Atomics.Counter`.

-- LK: I'm using Int for the delta because that makes sense as far as
-- I can tell, but I might not really understand what delta is.

-- LK: does it matter if this is newtype, data, or type?
type Counter s = LVar s AC.AtomicCounter Int

-- | Create a new `Counter` with the given initial value.
newCounter :: Int -> Par e s (Counter s)
-- LK: I don't understand why this doesn't work! :(
-- newCounter n = WrapPar $ fmap (Counter . WrapLVar) $
--                LI.newLV $ newCounter n
newCounter = undefined

-- | Increment the counter by a given amount.
increment :: HasBump e => Counter s -> Int -> Par e s ()
-- LK: Uhhh, this is the only way I can think of to increment the
-- counter while in Par.
increment = undefined

-- increment (LVar s ctr delt) n = do
--   return $ unsafePerformIO $ AC.incrCounter_ n ctr

-- | Wait until the maximum observed value reaches some threshold, then return.
waitThresh :: HasGet e => Counter s -> Int -> Par e s ()
waitThresh = undefined

-- | Observe what the final value of the `Counter` was.
freezeCounter :: HasFreeze e => Counter s -> Par e s Int
freezeCounter = undefined

-- | Once frozen, for example by `runParThenFreeze`, a `Counter` can be converted
-- directly into an `Int`.
fromCounter :: Counter Frzn -> Int
fromCounter = undefined

-- LK: Don't understand what I'm supposed to do here, if anything
-- instance DeepFrz (Counter s) where
--    type FrzType (Counter s) = Counter s
