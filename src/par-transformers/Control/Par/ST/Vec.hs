{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             Rank2Types, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances
             #-}

-- | A convenience interface -- simply a restriction of `ParST` to the case
--   of a single, boxed vector as the mutable state.
--
--   This library exposes simple versions of common operations from
--   "Data.Vector.Mutable", which operate directly on the implicit vector state
--   threaded through the monad.

module Control.Par.ST.Vec
       ( -- * A type alias for parallel computations with @Vector@ state
         ParVecT,
         runParVecT, --runParVec',

         -- * Reexported from the generic interface
         forkSTSplit, liftPar, liftST,

         -- * Retrieving an explict pointer to the Vector
         reify,

         -- * Common vector operations
         write, read, length, swap,
         drop, take, grow, set
       )
       where

import Control.Par.ST hiding (reify)

import qualified Control.Monad.Reader as R
import qualified Data.Vector.Mutable as MV

--import GHC.Conc (getNumProcessors)
--import System.IO.Unsafe (unsafeDupablePerformIO)

import Prelude hiding (drop, length, read, take)

--import qualified Control.Par.Class as PC
import Control.Par.Class.Unsafe (ParMonad, ParThreadSafe)
import Control.Par.EffectSigs

--------------------------------------------------------------------------------
-- Convenience interface ONE VECTOR state:
--------------------------------------------------------------------------------

-- | A type alias for parallel computations with an `MVector` state.
type ParVecT s1 va p (e :: EffectSig) s a = ParST (MVectorFlp va s1) p e s a


-- | Restricted version of `runParST` which initialized the state with a single,
-- boxed vector of a given size.  All elements start uninitialized.
runParVecT :: forall p e s a va .
              (ParThreadSafe p, ParMonad p)
               -- S.MonadState (MVectorFlp va s2) (ParST (MVectorFlp va s2) p e s))
             => Int
             -> (forall s1 . ParVecT s1 va p e s a)
             -> p e s a
runParVecT size comp =
   -- We start with a dummy state because we haven't yet done "new"
   unsafeRunParST
     (error "runParVecT: this initial value should be unused.") comp'
   -- runParST (VFlp MV.null) comp'
  where
    comp' :: ParST (MVectorFlp va s1) p e s a
    comp' = do
      vec <- liftST $ MV.new size
      unsafeInstall $ VFlp vec
      comp

-- | Extract a pointer to the whole Vector in its normal, usable @STVector@ form.
--   Use the `liftST` operator to act on it.
reify :: (ParThreadSafe p) => ParVecT s1 va p e s (MV.STVector s1 va)
reify = do
  VFlp vec <- R.ask
  return vec

--------------------------------------------------------------------------------

-- | Do an in-place parallel map on the vector state.
--
--   This function reserves the right to sequentialize some iterations.
{-
parMapM :: forall s1 va p e s .
           (HasPut e, HasGet e, PC.ParIVar p, ParThreadSafe p) =>
           (va -> p e s va) -> ParVecT s1 va p e s ()
parMapM = mkParMapM read write length id

overPartition :: Int
overPartition = 8

numProcs :: Int
numProcs = unsafeDupablePerformIO getNumProcessors
-}
--------------------------------------------------------------------------------

-- | Write to the (implicit) vector state.
write :: ParThreadSafe p => Int -> va -> ParVecT s1 va p e s ()
write ind val = do
  VFlp vec <- R.ask
  liftST $ MV.write vec ind val

-- | Read the (implicit) vector state.
read :: ParThreadSafe p => Int -> ParVecT s1 va p e s va
read ind = do
  VFlp vec <- R.ask
  liftST $ MV.read vec ind

-- | Return the length of the (implicit) vector state.
length :: ParThreadSafe p => ParVecT s1 va p e s Int
length = R.ask >>= (return . MV.length . unFlp)


-- | Update the vector state by swapping two elements.
swap :: ParThreadSafe p => Int -> Int -> ParVecT s1 va p e s ()
swap x y = do
  VFlp vec <- R.ask
  liftST $ MV.swap vec x y

-- | Update the vector state by dropping the first @n@ elements.
drop :: ParThreadSafe p => Int -> ParVecT s1 va p e s ()
drop n = do
  VFlp vec <- R.ask
  unsafeInstall (VFlp (MV.drop n vec))

-- | Update the vector state by taking the first @n@ elements, discarding the rest.
take :: ParThreadSafe p => Int -> ParVecT s1 va p e s ()
take n = do
  VFlp vec <- R.ask
  unsafeInstall (VFlp (MV.take n vec))

-- | Destructively replace the vector with a bigger vector, adding the given number
-- of elements.  The new elements are uninitialized and will result in errors if
-- read.
grow :: ParThreadSafe p => Int -> ParVecT s1 va p e s ()
grow n = do
  VFlp vec <- R.ask
  vec' <- liftST$ MV.grow vec n
  unsafeInstall (VFlp vec')

-- | Mutate all the elements of the vector, setting them to the given value.
set :: ParThreadSafe p => va -> ParVecT s1 va p e s ()
set val = do
  VFlp vec <- R.ask
  liftST $ MV.set vec val

{-
-- Ops not exposed:
----------------------------------------
-- MV.tail MV.null MV.clear MV.init MV.slice

-- Here are ops that don't directly make sense with a single, global vector.
----------------------------------------
--    MV.copy MV.clone MV.splitAt MV.move  MV.unsafeInit  MV.overlaps

-- Vector creation:
--    MV.replicate    MV.new MV.replicateM

-- And here are the unsafe ops:
----------------------------------------
-- MV.unsafeDrop MV.unsafeRead MV.unsafeWrite
-- MV.unsafeGrow MV.unsafeSlice MV.unsafeSwap
-- MV.unsafeMove MV.unsafeTail
-- MV.unsafeCopy MV.unsafeNew MV.unsafeTake
-}
