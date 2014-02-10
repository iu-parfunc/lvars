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
import qualified Control.LVish.SchedIdempotent as LI 
import qualified Data.Atomics.Counter.Reference as AC
import           Data.Word
import           System.IO.Unsafe (unsafeDupablePerformIO)

--------------------------------------------------------------------------------

-- | A @Counter@ is a wrapper around the `AtomicCounter` exposed by
-- `Data.Atomics.Counter`.
newtype Counter s = Counter (LVar s AC.AtomicCounter Word)

-- | Create a new `Counter` with the given initial value.
newCounter :: Word -> Par e s (Counter s)
newCounter n = WrapPar $ fmap (Counter . WrapLVar) $
               LI.newLV $ AC.newCounter (fromIntegral n)

-- | Increment the counter by a given amount.
increment :: HasBump e => Counter s -> Word -> Par e s ()
increment (Counter (WrapLVar lv)) n =
  WrapPar $ LI.putLV lv putter where
    putter :: AC.AtomicCounter -> IO (Maybe Word)
    putter ctr = do
      n' <- AC.incrCounter (fromIntegral n) ctr
      return $ Just (fromIntegral n')

-- | Wait until the maximum observed value reaches some threshold, then return.
waitThresh :: HasGet e => Counter s -> Word -> Par e s ()
waitThresh (Counter (WrapLVar lv)) thrsh = 
  WrapPar $ LI.getLV lv globalThresh deltaThresh
  where globalThresh ctr _ = do
          x <- AC.readCounter ctr
          deltaThresh $ fromIntegral x
        deltaThresh x | thrsh <= x = do return $ Just ()
                      | otherwise    = do return Nothing 

-- | Observe what the final value of the `Counter` was.
freezeCounter :: HasFreeze e => Counter s -> Par e s Word
freezeCounter (Counter (WrapLVar lv)) =
  WrapPar $ do
    LI.freezeLV lv
    n <- LI.getLV lv globalThresh deltaThresh
    return $ fromIntegral n
  where
    globalThresh ctr True = fmap Just $ AC.readCounter ctr
    globalThresh _  False = return Nothing
    deltaThresh  _        = return Nothing

-- | Once frozen, for example by `runParThenFreeze`, a `Counter` can be converted
-- directly into a `Word`.
fromCounter :: Counter Frzn -> Word
fromCounter (Counter (WrapLVar lv)) =
  undefined
  --unsafeDupablePerformIO $ AC.readCounter (state lv)
