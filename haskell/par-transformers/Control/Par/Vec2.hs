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

module Control.Par.Vec2
       ( -- * A type alias for parallel computations with @Vector@ state
         ParVec2, 
         runParVec2, --runParVec2',

         -- * Reexported from the generic interface
         forkSTSplit, liftPar, 
         
         -- * Retrieving an explict pointer to the Vector
         reify, liftST,
       )
       where

--import Control.LVish (Par, Determinism(Det), runPar)
--import Control.LVish.ST

import Control.Par.ST
import Control.Par.Class.Unsafe (ParThreadSafe(unsafeParIO))

import qualified Control.Monad.State.Strict as S
import qualified Data.Vector.Mutable as MV
import Prelude hiding (read, length, drop, take)

--------------------------------------------------------------------------------

type ParVec2 s1 eltL eltR parM ans =
     ParST (STTup2 (MVectorFlp eltL) (MVectorFlp eltR) s1) parM ans

-- | Restricted version of `runParST` which initialized the state with a single,
-- boxed vector of a given size.  All elements start uninitialized.
runParVec2 :: forall eltL eltR parM ans . (ParThreadSafe parM) => 
             (Int,Int)
             -> (forall s1 . ParVec2 s1 eltL eltR parM ans)
             -> parM ans
runParVec2 (size1,size2) comp = 
  runParST (error "runParVec -- this initial value should be unused.") $ do 
    vecL <- liftST $ MV.new size1
    vecR <- liftST $ MV.new size2
    S.put (STTup2 (VFlp vecL) (VFlp vecR))
    comp

-- | Extract a pointer to the whole Vector in its normal, usable @STVector@ form.
--   Use the `liftST` operator to act on it.
reify :: (ParThreadSafe parM) => ParVec2 s1 eltL eltR parM (MV.STVector s1 eltL, MV.STVector s1 eltR)
reify = do
  (STTup2 (VFlp vecL) (VFlp vecR)) <- S.get
  return (vecL,vecR)

--------------------------------------------------------------------------------

-- | Write to the (implicit) vector state.
writeL :: ParThreadSafe parM => Int -> eltL -> ParVec2 s eltL eltR parM ()
writeL ind val = do
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  liftST$ MV.write vecL ind val

-- | Read the (implicit) vector state.
readL :: ParThreadSafe parM => Int -> ParVec2 s eltL eltR parM eltL
readL ind = do
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  liftST$ MV.read vecL ind 

-- | Return the length of the (implicit) vector state.
lengthL :: ParThreadSafe parM => ParVec2 s1 eltL eltR parM Int
lengthL = do
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  return $ MV.length vecL

-- | Update the vector state by swapping two elements.
swapL :: ParThreadSafe parM => Int -> Int -> ParVec2 s1 eltL eltR parM ()
swapL x y = do
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get  
  liftST$ MV.swap vecL x y

-- | Update the vector state by dropping the first @n@ elements.
dropL :: ParThreadSafe parM => Int -> ParVec2 s1 eltL eltR parM ()
dropL n = do 
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  S.put$ STTup2 (VFlp (MV.drop n vecL)) (VFlp vecR)

-- | Update the vector state by taking the first @n@ elements, discarding the rest.
takeL :: ParThreadSafe parM => Int -> ParVec2 s1 eltL eltR parM ()
takeL n = do 
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  S.put$ STTup2 (VFlp (MV.take n vecL)) (VFlp vecR)


-- | Destructively replace the vector with a bigger vector, adding the given number
-- of elements.  The new elements are uninitialized and will result in errors if
-- read.
growL :: ParThreadSafe parM => Int -> ParVec2 s1 eltL eltR parM ()
growL n = do
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  vecL' <- liftST$ MV.grow vecL n
  S.put$ STTup2 (VFlp vecL') (VFlp vecR)

-- | Mutate all the elements of the vector, setting them to the given value.
setL :: ParThreadSafe parM => eltL -> ParVec2 s1 eltL eltR parM ()
setL val = do 
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  liftST $ MV.set vecL val

--unFlp :: ST t t1 -> MV.MVector t1 t
--unFlp (VFlp v) = v

-- | Write to the (implicit) vector state.
writeR :: ParThreadSafe parM => Int -> eltR -> ParVec2 s eltL eltR parM ()
writeR ind val = do
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  liftST$ MV.write vecR ind val

-- | Read the (implicit) vector state.
readR :: ParThreadSafe parM => Int -> ParVec2 s eltL eltR parM eltR
readR ind = do
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  liftST$ MV.read vecR ind 

-- | Return the length of the (implicit) vector state.
lengthR :: ParThreadSafe parM => ParVec2 s1 eltL eltR parM Int
lengthR = do
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  return $ MV.length vecR

-- | Update the vector state by swapping two elements.
swapR :: ParThreadSafe parM => Int -> Int -> ParVec2 s1 eltL eltR parM ()
swapR x y = do
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get  
  liftST$ MV.swap vecR x y

-- | Update the vector state by dropping the first @n@ elements.
dropR :: ParThreadSafe parM => Int -> ParVec2 s1 eltL eltR parM ()
dropR n = do 
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  S.put$ STTup2 (VFlp vecL) (VFlp (MV.drop n vecR))

-- | Update the vector state by taking the first @n@ elements, discarding the rest.
takeR :: ParThreadSafe parM => Int -> ParVec2 s1 eltL eltR parM ()
takeR n = do 
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  S.put$ STTup2 (VFlp vecL) (VFlp (MV.take n vecR))


-- | Destructively replace the vector with a bigger vector, adding the given number
-- of elements.  The new elements are uninitialized and will result in errors if
-- read.
growR :: ParThreadSafe parM => Int -> ParVec2 s1 eltL eltR parM ()
growR n = do
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  vecR' <- liftST$ MV.grow vecR n
  S.put$ STTup2 (VFlp vecL) (VFlp vecR')

-- | Mutate all the elements of the vector, setting them to the given value.
setR :: ParThreadSafe parM => eltR -> ParVec2 s1 eltL eltR parM ()
setR val = do 
  STTup2 (VFlp vecL) (VFlp vecR) <- S.get
  liftST $ MV.set vecR val
