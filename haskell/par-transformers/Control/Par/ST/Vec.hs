{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-} 
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE GADTs #-}

-- | A convenience interface -- simply a restriction of `ParST` to the case
--   of a single, boxed vector as the mutable state.
--
--   This library exposes simple versions of common operations from
--   "Data.Vector.Mutable", which operate directly on the implicit vector state
--   threaded through the monad.

module Control.Par.ST.Vec
       ( -- * A type alias for parallel computations with @Vector@ state
         ParVec, 
         runParVec, --runParVec',

         -- * Reexported from the generic interface
         forkSTSplit, liftPar, liftST, 
         
         -- * Retrieving an explict pointer to the Vector
         reify, 
                
         -- * Common vector operations
         write, read, length, swap,
         drop, take, grow, set
       )
       where

import Control.Par.ST

import qualified Control.Monad.State.Strict as S
import qualified Data.Vector.Mutable as MV

import GHC.Conc (getNumProcessors)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Prelude hiding (read, length, drop, take)

import Control.Par.Class.Unsafe (ParThreadSafe(unsafeParIO))

--------------------------------------------------------------------------------
-- Convenience interface ONE VECTOR state:
--------------------------------------------------------------------------------

-- | A type alias for parallel computations with an `MVector` state.
--type ParVec s1 elt det s2 ans = ParST (MVectorFlp elt s1) det s2 ans
type ParVec s1 elt parM ans = ParST (MVectorFlp elt s1) parM ans

-- | Restricted version of `runParST` which initialized the state with a single,
-- boxed vector of a given size.  All elements start uninitialized.
runParVec :: forall elt parM ans . (ParThreadSafe parM) =>
             Int
             -> (forall s1 . ParVec s1 elt parM ans)
             -> parM ans
runParVec size comp = 
  runParST (error "runParVec -- this initial value should be unused.") $ do 
    vec <- liftST $ MV.new size
    S.put (VFlp vec)
    comp

-- | Extract a pointer to the whole Vector in its normal, usable @STVector@ form.
--   Use the `liftST` operator to act on it.
reify :: (ParThreadSafe parM) => ParVec s1 elt parM (MV.STVector s1 elt)
reify = do
  VFlp vec <- S.get
  return vec

--------------------------------------------------------------------------------

-- TODO: Redo this with the generic mkParMapM:
{-
-- | Do an in-place parallel map on the vector state.
--
--   This function reserves the right to sequentialize some iterations.
parMapM :: forall elt parM s2 . (ParThreadSafe parM) =>
           (elt -> Par parM elt) -> ParVec s1 elt parM ()
-- parMapM :: forall elt det s1 s2 .
--            (forall s0 . elt -> ParVec s0 elt det s2 elt) -> ParVec s1 elt det s2 ()
parMapM fn = do
  VFlp vec <- S.get
  -- vecParMap_ fn vec
  -- return ()  
  len <- length
  let share = max 1 (len `quot` (numProcs * overPartition))
      loop :: Int -> (forall s3 . ParVec s3 elt det s2 ())
      loop iters
        | iters <= share = 
          -- Bottom out to a sequential loop:
          for_ (0,iters) $ \ ind -> do  
            x <- read ind
            y <- liftPar $ fn x
            write ind y
            return ()
          
        | otherwise = do
            let (iters2,extra) = iters `quotRem` 2
                iters1 = iters2+extra
            forkSTSplit iters1
              (loop iters1)
              (loop iters2)
            return ()
  return ()
  -}

overPartition :: Int
overPartition = 8

numProcs :: Int
numProcs = unsafeDupablePerformIO getNumProcessors

--------------------------------------------------------------------------------

-- | Write to the (implicit) vector state.
write :: ParThreadSafe parM => Int -> elt -> ParVec s1 elt parM ()
write ind val = do
  VFlp vec <- S.get
  liftST$ MV.write vec ind val

-- | Read the (implicit) vector state.
read :: ParThreadSafe parM => Int -> ParVec s1 elt parM elt
read ind = do
  VFlp vec <- S.get
  liftST$ MV.read vec ind 

-- | Return the length of the (implicit) vector state.
length :: ParThreadSafe parM => ParVec s1 elt parM Int
length = S.get >>= (return . MV.length . unFlp)

-- | Update the vector state by swapping two elements.
swap :: ParThreadSafe parM => Int -> Int -> ParVec s1 elt parM ()
-- swap x y = S.get >>= ((\ v -> MV.swap v x y) . unFlp)
swap x y = do
  VFlp vec <- S.get
  liftST$ MV.swap vec x y

-- | Update the vector state by dropping the first @n@ elements.
drop :: ParThreadSafe parM => Int -> ParVec s1 elt parM ()
drop n = do 
  VFlp vec <- S.get
  S.put (VFlp (MV.drop n vec))

-- | Update the vector state by taking the first @n@ elements, discarding the rest.
take :: ParThreadSafe parM => Int -> ParVec s1 elt parM ()
take n = do 
  VFlp vec <- S.get
  S.put (VFlp (MV.take n vec))


-- | Destructively replace the vector with a bigger vector, adding the given number
-- of elements.  The new elements are uninitialized and will result in errors if
-- read.
grow :: ParThreadSafe parM => Int -> ParVec s1 elt parM ()
grow n = do
  VFlp vec <- S.get
  vec' <- liftST$ MV.grow vec n
  S.put (VFlp vec')

-- | Mutate all the elements of the vector, setting them to the given value.
set :: ParThreadSafe parM => elt -> ParVec s1 elt parM ()
set val = do 
  VFlp vec <- S.get
  liftST $ MV.set vec val

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
unFlp :: MVectorFlp t t1 -> MV.MVector t1 t
unFlp (VFlp v) = v


