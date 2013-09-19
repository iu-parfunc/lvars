{-# LANGUAGE DataKinds, BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

-- | A counter that contains the maximum value of all puts.

-- TODO: Add 'Min', 'Or', 'And' and other idempotent ops...

module Data.LVar.MaxCounter
       ( MaxCounter,
         newMaxCounter, put, waitThresh, freezeMaxCounter
       ) where

import Control.LVish hiding (freeze)
import Control.LVish.DeepFreeze
import Data.LVar.Generic
import Data.LVar.Internal.Pure as P
import Algebra.Lattice

-- newtype MaxCounter = MaxCounter (PureLVar Int)
-- instance JoinSemiLattice MaxCounter where 
--   join (MaxCounter a) (MaxCounter b) = a `max` b 

type MaxCounter s = PureLVar s MC
newtype MC = MC Int
  deriving (Eq, Show, Ord, Read)

instance JoinSemiLattice MC where 
  join (MC a) (MC b) = MC (a `max` b)

instance BoundedJoinSemiLattice MC where
  bottom = MC minBound

newMaxCounter :: Int -> Par d s (MaxCounter s)
newMaxCounter n = newPureLVar (MC n)

put :: MaxCounter s -> Int -> Par d s ()
put lv n = putPureLVar lv (MC n)

waitThresh :: MaxCounter s -> Int -> Par d s ()
waitThresh lv n = waitPureLVar lv (MC n)

freezeMaxCounter :: MaxCounter s -> Par QuasiDet s Int
freezeMaxCounter lv = do
  MC n <- freezePureLVar lv
  return n
  
instance DeepFreeze (MaxCounter s) Int where
  type Session (MaxCounter s) = s
  deepFreeze = freezeMaxCounter
