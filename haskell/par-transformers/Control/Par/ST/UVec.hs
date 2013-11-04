{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

-- | A convenience interface -- simply a restriction of `ParST` to the case
--   of a single, boxed vector as the mutable state.
--
--   This library exposes simple versions of common operations from
--   "Data.Vector.Mutable", which operate directly on the implicit vector state
--   threaded through the monad.

module Control.Par.ST.UVec
       ( -- * A type alias for parallel computations with @Vector@ state
         ParVecT, 
         runParVecT,

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
import qualified Data.Vector.Unboxed.Mutable as MU
import Prelude hiding (read, length, drop, take)
import Control.Par.Class.Unsafe (ParThreadSafe(unsafeParIO))

--------------------------------------------------------------------------------
-- Convenience interface ONE VECTOR state:
--------------------------------------------------------------------------------

-- | A type alias for parallel computations with an unboxed `MVector` state.
type ParVecT s1 elt parM ans = ParST (UVectorFlp elt s1) parM ans

-- | Restricted version of `runParST` which initialized the state with a single,
-- boxed vector of a given size.  All elements start uninitialized.
runParVecT :: forall elt parM ans . (MU.Unbox elt, ParThreadSafe parM) =>
             Int
             -> (forall s1 . ParVecT s1 elt parM ans)
             -> parM ans
runParVecT size comp = 
  runParST (error "runParVec -- this initial value should be unused.") $ do 
    vec <- liftST $ MU.new size
    S.put (UFlp vec)
    comp

-- | Extract a pointer to the whole Vector in its normal, usable @STVector@ form.
--   Use the `liftST` operator to act on it.
reify :: ParThreadSafe parM => ParVecT s1 elt parM (MU.STVector s1 elt)
reify = do
  UFlp vec <- S.get
  return vec

--------------------------------------------------------------------------------

-- | Write to the (implicit) vector state.
write :: ParThreadSafe parM => Int -> elt -> ParVecT s1 elt parM ()
write ind val = do
  UFlp vec <- S.get
  liftST$ MU.write vec ind val

-- | Read the (implicit) vector state.
read :: ParThreadSafe parM => Int -> ParVecT s1 elt parM elt
read ind = do
  UFlp vec <- S.get
  liftST$ MU.read vec ind 

-- | Return the length of the (implicit) vector state.
length :: (ParThreadSafe parM, MU.Unbox elt) => ParVecT s1 elt parM Int
length = S.get >>= (return . MU.length . unFlp)

-- | Update the vector state by swapping two elements.
swap :: ParThreadSafe parM => Int -> Int -> ParVecT s1 elt parM ()
swap x y = do
  UFlp vec <- S.get
  liftST$ MU.swap vec x y

-- | Update the vector state by dropping the first @n@ elements.
drop :: ParThreadSafe parM => Int -> ParVecT s1 elt parM ()
drop n = do 
  UFlp vec <- S.get
  S.put (UFlp (MU.drop n vec))

-- | Update the vector state by taking the first @n@ elements, discarding the rest.
take :: ParThreadSafe parM => Int -> ParVecT s1 elt parM ()
take n = do 
  UFlp vec <- S.get
  S.put (UFlp (MU.take n vec))

-- | Destructively replace the vector with a bigger vector, adding the given number
-- of elements.  The new elements are uninitialized and will result in errors if
-- read.
grow :: ParThreadSafe parM => Int -> ParVecT s1 elt parM ()
grow n = do
  UFlp vec <- S.get
  vec' <- liftST$ MU.grow vec n
  S.put (UFlp vec')

-- | Mutate all the elements of the vector, setting them to the given value.
set :: ParThreadSafe parM => elt -> ParVecT s1 elt parM ()
set val = do 
  UFlp vec <- S.get
  liftST $ MU.set vec val

unFlp :: UVectorFlp t t1 -> MU.MVector t1 t
unFlp (UFlp v) = v
