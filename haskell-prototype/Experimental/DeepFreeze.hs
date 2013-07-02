{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- | The idea here is to be able to freeze nested structures in one step.  This would
-- be useful for allowing a "runParThenFreeze" that can atomically do a nested freeze
-- as the implicit last action (after the global qiesce).

module Experimental.DeepFreeze where

import           Control.LVish
import           Data.Traversable (traverse)
import           Data.LVar.IVar
import           Data.LVar.Set
import           Data.Traversable

--------------------------------------------------------------------------------

test0 = do
  iv1 <- newBottom :: Par (IVar (IVar String))
  iv2 <- newBottom
  put_ iv1 iv2
  put_ iv2 "hello"
  IVarSnap m <- freeze iv1
  return m


-- | Should return (Just (Just "hello"))
test1 :: IO(Maybe (Maybe String))
test1 = runParIO $ do
  iv1 <- newBottom :: Par (IVar (IVar String))
  iv2 <- newBottom
  put_ iv1 iv2
  put_ iv2 "hello"
--  m <- freeze iv1  
--  return m
  deepFreeze iv1

-- | This uses the more generic lifting... but it's more annoying to unpack:
test2 :: IO (Snapshot IVar (Snapshot IVar String))
test2 = runParIO $ do
  iv1 <- newBottom :: Par (IVar (IVar String))
  iv2 <- newBottom
  put_ iv1 iv2
  put_ iv2 "hello"
--  m <- freeze iv1  
--  return m
  deepFreeze iv1


test3 :: IO (Snapshot IVar (Snapshot ISet String))
test3 = runParIO $ do
  iv1 <- newBottom :: Par (IVar (ISet String))
  iv2 <- newBottom :: Par (ISet String)
  put_ iv1 iv2
  putInSet "hello" iv2 
  deepFreeze iv1


--------------------------
-- EXPERIMENTING  

-- | This establishes an unrestricted *relation* between input and output types.  Thus
-- it is powerful, but can be painful to use.  The input and output types of
-- deepFreeze must be fully constrained at every call site.  This allows the user to
-- potentially freeze a nested structure in various ways of their choosing.
class DeepFreeze (from :: *) (to :: *) where
  deepFreeze :: from -> Par to 

instance DeepFreeze (IVar a) (Maybe a) where
  deepFreeze iv = do IVarSnap m <- freeze iv
                     return m

#if 0
-- This much works:
instance (Traversable f) =>                     
         DeepFreeze (f (IVar a)) (f (Maybe a)) where
  deepFreeze = traverse freezeIVar
#elif 0
-- Or this, but not both of course:
instance (LVarData1 f) =>
         DeepFreeze (f (IVar a)) (Snapshot f (Maybe a)) where
  deepFreeze lvd = do
    -- let fn = (fmap (\ (IVarSnap m) -> m)) . freezeIVar
    x <- freeze lvd                :: Par (Snapshot f (IVar a))
    y <- traverseSnap freezeIVar x :: Par (Snapshot f (Maybe a))
    return y
#else
instance (LVarData1 f, LVarData1 g) =>
         DeepFreeze (f (g a)) (Snapshot f (Snapshot g a)) where
  deepFreeze lvd = do
    -- let fn = (fmap (\ (IVarSnap m) -> m)) . freezeIVar
    x <- freeze lvd            :: Par (Snapshot f (g a))
    y <- traverseSnap freeze x :: Par (Snapshot f (Snapshot g a))
    return y    
#endif


#if 1
instance DeepFreeze (IVar a) b =>
         DeepFreeze (IVar (IVar a)) (Maybe b)
  where
    deepFreeze (from :: (IVar (IVar a))) = do
      x <- freezeIVar from       :: Par (Maybe (IVar a))
      y <- traverse deepFreeze x :: Par (Maybe b)
      return y
#else
-- But how do we generalize to arbitrary combinations of DIFFERENT LVar types?

instance (LVarData1 f, DeepFreeze (f a) (g a)) =>
         DeepFreeze (f (IVar a)) (g (Maybe a))
  where
    deepFreeze (from :: (f (IVar a))) = do
      x <- freeze from               :: Par (Snapshot f (IVar a))
--      y <- traverseSnap freezeIVar x :: Par (Snapshot f (Maybe a))
--      x <- deepFreeze from       :: Par (g (IVar a))
--      y <- traverse deepFreeze x :: Par (Maybe b)
--      return y
      undefined
#endif

--------------------------------------------------------------------------------

-- class DeepFreeze a b =>
--       DeepFreeze1 (t1 :: * -> *) (t2 :: * -> *) where
--   deepFreeze1 :: t1 a -> Par (t2 b)

-- class DeepFreeze a b =>
--       DeepFreeze1 (t1 a) (t2 b) where
--   deepFreeze1 :: t1 a -> Par (t2 b)

-- class (LVarData1 t1, DeepFreeze (t1 a) (t2 b)) =>
--       DeepFreeze1 (t1 :: * -> *) (t2 :: * -> *) where

-- instance (LVarData1 t1, DeepFreeze (t1 a) (t2 b)) =>
--          DeepFreeze t1 t2 where




