{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE DataKinds, BangPatterns, MagicHash #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

-- | A counter that contains the maximum value of all puts.

-- TODO: Add 'Min', 'Or', 'And' and other idempotent ops...

module Data.LVar.MaxCounter
       ( MaxCounter,
         newMaxCounter, put, waitThresh, freezeMaxCounter
       ) where

import Control.LVish hiding (freeze)
import Control.LVish.Internal (state)
import Control.LVish.DeepFrz.Internal
import Data.IORef
import Data.LVar.Generic
import Data.LVar.Internal.Pure as P
import Algebra.Lattice
import           System.IO.Unsafe  (unsafeDupablePerformIO)
import           GHC.Prim (unsafeCoerce#)

--------------------------------------------------------------------------------

-- | A @MaxCounter@ is really a constant-space ongoing @fold max@ operation.
-- 
-- A @MaxCounter@ is an example of a `PureLVar`.  It is implemented simply as a
-- pure value in a mutable box.
type MaxCounter s = PureLVar s MC

newtype MC = MC Int
  deriving (Eq, Show, Ord, Read)

instance JoinSemiLattice MC where 
  join (MC !a) (MC !b) = MC (a `max` b)

instance BoundedJoinSemiLattice MC where
  bottom = MC minBound

-- | Create a new counter with the given initial value.
newMaxCounter :: Int -> Par d s (MaxCounter s)
newMaxCounter n = newPureLVar (MC n)

-- | Incorporate a new value in the max-fold.  If the previous maximum is less than
-- the new value, increase it.
put :: MaxCounter s -> Int -> Par d s ()
put lv n = putPureLVar lv (MC n)

-- | Wait until the maximum observed value reaches some threshold, then return.
waitThresh :: MaxCounter s -> Int -> Par d s ()
waitThresh lv n = waitPureLVar lv (MC n)

-- | Observe what the final value of the counter was.
freezeMaxCounter :: MaxCounter s -> Par QuasiDet s Int
freezeMaxCounter lv = do
  MC n <- freezePureLVar lv
  return n

-- | Once frozen, for example by `runParThenFreeze`, a MaxCounter can be converted
-- directly into an Int.
fromMaxCounter :: MaxCounter Frzn -> Int
fromMaxCounter (PureLVar lv) =
  case unsafeDupablePerformIO (readIORef (state lv)) of
    MC n -> n

instance DeepFrz MC where
   type FrzType MC = MC

-- Don't need this because there is an instance for `PureLVar`:
{-
-- | @MaxCounter@ values can be returned in the results of a
--   `runParThenFreeze`.  Hence they need a `DeepFrz` instance.
--   @DeepFrz@ is just a type-coercion.  No bits flipped at runtime.
instance DeepFrz (MaxCounter s) where
   type FrzType (MaxCounter s) = (MaxCounter Frzn)
   frz = unsafeCoerce#
-}
