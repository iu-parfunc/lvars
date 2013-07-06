{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- | The idea here is to be able to freeze nested structures in one step.  This would
-- be useful for allowing a "runParThenFreeze" that can atomically do a nested freeze
-- as the implicit last action (after the global qiesce).

module Control.LVish.DeepFreeze where

import           Control.LVish
import qualified Control.LVish.SchedIdempotent as L
import           Data.Traversable (traverse)
import           Data.LVar.IVar

import           Data.LVar.Set
import           Data.Traversable

--------------------------

-- | This establishes an unrestricted *relation* between input and output types.  Thus
-- it is powerful, but can be painful to use.  The input and output types of
-- deepFreeze must be fully constrained at every call site.  This allows the user to
-- potentially freeze a nested structure in various ways of their choosing.
class DeepFreeze (from :: *) (to :: *) where
  deepFreeze :: from -> Par to 

instance (LVarData1 f, LVarData1 g) =>
         DeepFreeze (f (g a)) (Snapshot f (Snapshot g a)) where
  deepFreeze lvd = do
    -- let fn = (fmap (\ (IVarSnap m) -> m)) . freezeIVar
    x <- L.unsafeUnQPar$ freeze lvd               :: Par (Snapshot f (g a))
    y <- traverseSnap (L.unsafeUnQPar . freeze) x :: Par (Snapshot f (Snapshot g a))
    return y

-- Inherit everything that regular freeze can do:
instance LVarData1 f => DeepFreeze (f a) (Snapshot f a) where
  deepFreeze = L.unsafeUnQPar . freeze

--------------------------------------------------------------------------------
-- IVar specific instances:
--------------------------------------------------------------------------------

-- Teach it how to freeze WITHOUT the annoying snapshot constructor:
instance DeepFreeze (IVar a) (Maybe a) where
  deepFreeze iv = do IVarSnap m <- L.unsafeUnQPar$ freeze iv
                     return m

instance DeepFreeze (IVar a) b =>
         DeepFreeze (IVar (IVar a)) (Maybe b)
  where
    deepFreeze (from :: (IVar (IVar a))) = do
      x <- L.unsafeUnQPar$ freezeIVar from       :: Par (Maybe (IVar a))
      y <- traverse deepFreeze x :: Par (Maybe b)
      return y

--------------------------------------------------------------------------------
-- Unit Tests
--------------------------------------------------------------------------------

test0 = do
  iv1 <- newBottom :: Par (IVar (IVar String))
  iv2 <- newBottom
  put_ iv1 iv2
  put_ iv2 "hello"
  IVarSnap m <- L.unsafeUnQPar $ freeze iv1
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
  deepFreeze iv1


test3 :: IO (Snapshot IVar (Snapshot ISet String))
-- test3 :: IO (Maybe (Snapshot ISet String)) -- Won't work atm.
test3 = runParIO $ do
  iv1 <- newBottom 
  iv2 <- newBottom 
  put_ iv1 iv2
  putInSet "hello" iv2 
  deepFreeze iv1

test4 :: DeepFreeze (IVar Int) b => IO b
test4 = runParIO $ do
  iv1 <- newBottom 
  put_ iv1 (3::Int)
  deepFreeze iv1

-- More flexible than regular freeze, can pick either type:
test4a :: IO (Snapshot IVar Int)
test4a = test4

-- uh... how is this one actually working?
test4b :: IO (Maybe Int)
test4b = test4

--------------------------------------------------------------------------------
