{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | An unsigned integer (aka `Word`) LVar that exposes a
-- non-idempotent `increment` operation.  It differs from
-- `Data.LVar.MaxPosInt` in that it allows non-idempotent updates and
-- does _not_ allow least-upper-bound updates.

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
import           Data.Word
import           System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

-- | A @Counter@ is a wrapper around the `AtomicCounter` exposed by
-- `Data.Atomics.Counter`.

-- LK: I'm using Word for the delta because that makes sense as far as
-- I can tell, but I might not really understand what delta is.

newtype Counter s = Counter (LVar s AC.AtomicCounter Word)

-- | Create a new `Counter` with the given initial value.
<<<<<<< HEAD
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
=======
newCounter :: Word -> Par d s (Counter s)
newCounter n = WrapPar $ fmap (Counter . WrapLVar) $
               LI.newLV $ AC.newCounter (fromIntegral n)

-- | Increment the counter by a given amount.
increment :: Counter s -> Word -> Par d s ()
increment (Counter (WrapLVar lv)) n =
  WrapPar $ LI.putLV lv putter where
    putter :: AC.AtomicCounter -> IO (Maybe Word)
    putter ctr = do
      n' <- AC.incrCounter (fromIntegral n) ctr
      return $ Just (fromIntegral n')

-- | Wait until the maximum observed value reaches some threshold, then return.
waitThresh :: Counter s -> Word -> Par d s ()
waitThresh = undefined

-- | Observe what the final value of the `Counter` was.
freezeCounter :: Counter s -> Par QuasiDet s Word
>>>>>>> 5818e50... Some progress on Data.LVar.Counter.
freezeCounter = undefined

-- | Once frozen, for example by `runParThenFreeze`, a `Counter` can be converted
-- directly into a `Word`.
fromCounter :: Counter Frzn -> Word
fromCounter = undefined

-- LK: Don't understand what I'm supposed to do here, if anything
-- instance DeepFrz (Counter s) where
--    type FrzType (Counter s) = Counter s
