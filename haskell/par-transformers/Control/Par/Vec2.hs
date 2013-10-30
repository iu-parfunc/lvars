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

type ParVec2 s1 elt1 elt2 parM ans =
     ParST (STTup2 (MVectorFlp elt1) (MVectorFlp elt2) s1) parM ans

-- | Restricted version of `runParST` which initialized the state with a single,
-- boxed vector of a given size.  All elements start uninitialized.
runParVec2 :: forall elt1 elt2 parM ans . (ParThreadSafe parM) => 
             (Int,Int)
             -> (forall s1 . ParVec2 s1 elt1 elt2 parM ans)
             -> parM ans
runParVec2 (size1,size2) comp = 
  runParST (error "runParVec -- this initial value should be unused.") $ do 
    vec1 <- liftST $ MV.new size1
    vec2 <- liftST $ MV.new size2
    S.put (STTup2 (VFlp vec1) (VFlp vec2))
    comp

{-
-- | A shortcut for the common case of discharging both the vector computation and
-- the underlying `Par` monad at the same time.
runParVec2' :: forall elt1 elt2 ans .
              (Int,Int)
              -> (forall s1 s2 . ParVec2 s1 elt1 elt2 'Det s2 ans)
              -> ans
runParVec2' pr m = runPar (runParVec2 pr m)
-}

-- | Extract a pointer to the whole Vector in its normal, usable @STVector@ form.
--   Use the `liftST` operator to act on it.
reify :: (ParThreadSafe parM) => ParVec2 s1 elt1 elt2 parM (MV.STVector s1 elt1, MV.STVector s1 elt2)
reify = do
  (STTup2 (VFlp vec1) (VFlp vec2)) <- S.get
  return (vec1,vec2)

--------------------------------------------------------------------------------

-- | Write to the (implicit) vector state.
writeL :: ParThreadSafe parM => Int -> elt1 -> ParVec2 s1 elt1 elt2 parM ()
writeL ind val = do
  STTup2 (VFlp vecl) (VFlp vecr) <- S.get
  liftST$ MV.write vecl ind val

{-
-- | Read the (implicit) vector state.
readl :: ParThreadSafe parM => Int -> ParVec2 s1 elt1 elt2 parM elt
readl ind = do
  VFlp vec <- fst S.get
  liftST$ MV.read vec ind 

-- | Return the length of the (implicit) vector state.
lengthl :: ParThreadSafe parM => ParVec2 s1 elt1 elt2 parM Int
--lengthl = (fst S.get) >>= (return . MV.length . unFlp)
lengthl = do
  VFlp vec <- fst S.get
  return $ MV.length vec

-- | Update the vector state by swapping two elements.
swapl :: ParThreadSafe parM => Int -> Int -> ParVec2 s1 elt1 elt2 parM ()
-- swap x y = S.get >>= ((\ v -> MV.swap v x y) . unFlp)
swapl x y = do
  VFlp vec <- fst S.get
  liftST$ MV.swap vec x y

-- | Update the vector state by dropping the first @n@ elements.
dropl :: ParThreadSafe parM => Int -> ParVec2 s1 elt1 elt2 parM ()
dropl n = do 
  VFlp vec <- fst S.get
  S.put (VFlp (MV.drop n vec))

-- | Update the vector state by taking the first @n@ elements, discarding the rest.
takel :: ParThreadSafe parM => Int -> ParVec2 s1 elt1 elt2 parM ()
takel n = do 
  VFlp vec <- fst S.get
  S.put (VFlp (MV.take n vec))


-- | Destructively replace the vector with a bigger vector, adding the given number
-- of elements.  The new elements are uninitialized and will result in errors if
-- read.
growl :: ParThreadSafe parM => Int -> ParVec2 s1 elt1 elt2 parM ()
growl n = do
  VFlp vec <- fst S.get
  vec' <- liftST$ MV.grow vec n
  S.put (VFlp vec')

-- | Mutate all the elements of the vector, setting them to the given value.
setl :: ParThreadSafe parM => elt -> ParVec2 s1 elt1 elt2 parM ()
setl val = do 
  VFlp vec <- fst S.get
  liftST $ MV.set vec val
-}
--unFlp :: ST t t1 -> MV.MVector t1 t
--unFlp (VFlp v) = v
