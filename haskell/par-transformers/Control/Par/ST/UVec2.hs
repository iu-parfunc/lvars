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

module Control.Par.ST.UVec2
       ( -- * A type alias for parallel computations with @Vector@ state
         ParVec2T, 
         runParVec2T,

         -- * Reexported from the generic interface
         forkSTSplit, liftPar, liftST, 
       
         -- * Retrieving an explict pointer to the Vector
         reify, 
         
         -- * Useful vector helpers
         writeL, writeR, readL, readR, lengthL, lengthR,
         swapL, swapR, dropL, dropR, takeL, takeR,
         growL, growR, setL, setR, swapState
       )
       where

import Control.Par.ST
import Control.Par.Class.Unsafe (ParThreadSafe(unsafeParIO))
import qualified Control.Monad.State.Strict as S
import qualified Data.Vector.Unboxed.Mutable as MU
import Prelude hiding (read, length, drop, take)

--------------------------------------------------------------------------------

type ParVec2T s1 elt1 elt2 parM ans =
     ParST (STTup2 (UVectorFlp elt1) (UVectorFlp elt2) s1) parM ans

-- | Restricted version of `runParST` which initialized the state with a single,
-- boxed vector of a given size.  All elements start uninitialized.
runParVec2T :: forall elt1 elt2 parM ans . (MU.Unbox elt1, MU.Unbox elt2, ParThreadSafe parM) =>
             (Int,Int)
             -> (forall s1 . ParVec2T s1 elt1 elt2 parM ans)
             -> parM ans
runParVec2T (size1,size2) comp = 
  runParST (error "runParVec -- this initial value should be unused.") $ do 
    vec1 <- liftST $ MU.new size1
    vec2 <- liftST $ MU.new size2
    S.put (STTup2 (UFlp vec1) (UFlp vec2))
    comp

-- | Extract a pointer to the whole Vector in its normal, usable @STVector@ form.
--   Use the `liftST` operator to act on it.
reify :: ParThreadSafe parM => ParVec2T s1 elt1 elt2 parM (MU.MVector s1 elt1, MU.MVector s1 elt2)
reify = do
  (STTup2 (UFlp vec1) (UFlp vec2)) <- S.get
  return (vec1,vec2)

--------------------------------------------------------------------------------

-- | Swap the two state vectors.
swapState :: ParThreadSafe parM => ParVec2T s elt elt parM ()
swapState = do
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  S.put$ STTup2 (UFlp vecR) (UFlp vecL)

-- | Write to the (implicit) left vector state.
writeL :: ParThreadSafe parM => Int -> eltL -> ParVec2T s eltL eltR parM ()
writeL ind val = do
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  liftST$ MU.write vecL ind val

-- | Read the (implicit) left vector state.
readL :: ParThreadSafe parM => Int -> ParVec2T s eltL eltR parM eltL
readL ind = do
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  liftST$ MU.read vecL ind 

-- | Return the length of the (implicit) left vector state.
lengthL :: ParThreadSafe parM => ParVec2T s1 eltL eltR parM Int
lengthL = do
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  return $ MU.length vecL

-- | Update the left vector state by swapping two elements.
swapL :: ParThreadSafe parM => Int -> Int -> ParVec2T s1 eltL eltR parM ()
swapL x y = do
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get  
  liftST$ MU.swap vecL x y

-- | Update the left vector state by dropping the first @n@ elements.
dropL :: ParThreadSafe parM => Int -> ParVec2T s1 eltL eltR parM ()
dropL n = do 
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  S.put$ STTup2 (UFlp (MU.drop n vecL)) (UFlp vecR)

-- | Update the left vector state by taking the first @n@ elements, discarding the rest.
takeL :: ParThreadSafe parM => Int -> ParVec2T s1 eltL eltR parM ()
takeL n = do 
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  S.put$ STTup2 (UFlp (MU.take n vecL)) (UFlp vecR)


-- | Destructively replace the left vector with a bigger vector,
-- adding the given number of elements.  The new elements are
-- uninitialized and will result in errors if read.
growL :: ParThreadSafe parM => Int -> ParVec2T s1 eltL eltR parM ()
growL n = do
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  vecL' <- liftST$ MU.grow vecL n
  S.put$ STTup2 (UFlp vecL') (UFlp vecR)

-- | Mutate all the elements of the left vector, setting them to the
-- given value.
setL :: ParThreadSafe parM => eltL -> ParVec2T s1 eltL eltR parM ()
setL val = do 
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  liftST $ MU.set vecL val

-- Helpers for the other vector in the state.

-- | Write to the (implicit) right vector state.
writeR :: ParThreadSafe parM => Int -> eltR -> ParVec2T s eltL eltR parM ()
writeR ind val = do
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  liftST$ MU.write vecR ind val

-- | Read the (implicit) right vector state.
readR :: ParThreadSafe parM => Int -> ParVec2T s eltL eltR parM eltR
readR ind = do
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  liftST$ MU.read vecR ind 

-- | Return the length of the (implicit) right vector state.
lengthR :: ParThreadSafe parM => ParVec2T s1 eltL eltR parM Int
lengthR = do
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  return $ MU.length vecR

-- | Update the vector state by swapping two elements.
swapR :: ParThreadSafe parM => Int -> Int -> ParVec2T s1 eltL eltR parM ()
swapR x y = do
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get  
  liftST$ MU.swap vecR x y

-- | Update the right vector state by dropping the first @n@ elements.
dropR :: ParThreadSafe parM => Int -> ParVec2T s1 eltL eltR parM ()
dropR n = do 
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  S.put$ STTup2 (UFlp vecL) (UFlp (MU.drop n vecR))

-- | Update the right vector state by taking the first @n@ elements,
-- discarding the rest.
takeR :: ParThreadSafe parM => Int -> ParVec2T s1 eltL eltR parM ()
takeR n = do 
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  S.put$ STTup2 (UFlp vecL) (UFlp (MU.take n vecR))


-- | Destructively replace the right vector with a bigger vector,
-- adding the given number of elements.  The new elements are
-- uninitialized and will result in errors if read.
growR :: ParThreadSafe parM => Int -> ParVec2T s1 eltL eltR parM ()
growR n = do
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  vecR' <- liftST$ MU.grow vecR n
  S.put$ STTup2 (UFlp vecL) (UFlp vecR')

-- | Mutate all the elements of the right vector, setting them to the
-- given value.
setR :: ParThreadSafe parM => eltR -> ParVec2T s1 eltL eltR parM ()
setR val = do 
  STTup2 (UFlp vecL) (UFlp vecR) <- S.get
  liftST $ MU.set vecR val

