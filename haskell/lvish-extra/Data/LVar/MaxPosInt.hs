{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE DataKinds, BangPatterns, MagicHash #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-} 
-- | A positive integer LVar that contains the maximum value of all `put`s.

-- TODO: Add 'Min', 'Or', 'And' and other idempotent ops...

module Data.LVar.MaxPosInt
       ( MaxPosInt,
         newMaxPosInt, put, waitThresh, freezeMaxPosInt, fromMaxPosInt
       ) where

import Control.LVish hiding (freeze, put)
import Control.LVish.Internal (state)
import Control.LVish.DeepFrz.Internal
import Data.IORef
import Data.LVar.Generic
import Data.LVar.Internal.Pure as P
import Algebra.Lattice
import           System.IO.Unsafe  (unsafeDupablePerformIO)
import           GHC.Prim (unsafeCoerce#)

--------------------------------------------------------------------------------

-- | A @MaxPosInt@ is really a constant-space ongoing @fold max@ operation.
-- 
-- A @MaxPosInt@ is an example of a `PureLVar`.  It is implemented simply as a
-- pure value in a mutable box.
type MaxPosInt s = PureLVar s MC

newtype MC = MC Int
  deriving (Eq, Show, Ord, Read)

instance JoinSemiLattice MC where 
  join (MC !a) (MC !b) = MC (a `max` b)

instance BoundedJoinSemiLattice MC where
  bottom = MC minBound

-- | Create a new `MaxPosInt` with the given initial value.
newMaxPosInt :: Int -> Par e s (MaxPosInt s)
newMaxPosInt n = newPureLVar (MC n)

-- | Incorporate a new value in the max-fold.  If the previous maximum is less than
-- the new value, increase it.
put :: HasPut e => MaxPosInt s -> Int -> Par e s ()
put lv n = putPureLVar lv (MC n)

-- | Wait until the maximum observed value reaches some threshold, then return.
waitThresh :: HasGet e => MaxPosInt s -> Int -> Par e s ()
waitThresh lv n = waitPureLVar lv (MC n)

-- | Observe what the final value of the `MaxPosInt` was.
freezeMaxPosInt :: HasFreeze e => MaxPosInt s -> Par e s Int
freezeMaxPosInt lv = do
  MC n <- freezePureLVar lv
  return n

-- | Once frozen, for example by `runParThenFreeze`, a `MaxPosInt` can be converted
-- directly into an `Int`.
fromMaxPosInt :: MaxPosInt Frzn -> Int
fromMaxPosInt (PureLVar lv) =
  case unsafeDupablePerformIO (readIORef (state lv)) of
    MC n -> n

instance DeepFrz MC where
   type FrzType MC = MC
