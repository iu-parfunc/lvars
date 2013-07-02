{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.LVar.Set
       (
         ISet, newEmptySet, putInSet, withCallbacksThenFreeze,
         waitElem, waitSize, freezeSet
       ) where

import           Data.IORef
import qualified Data.Set as S
import qualified Data.LVar.IVar as IV
import qualified Data.Foldable as F

import           Control.LVish

------------------------------------------------------------------------------
-- ISets and setmap implemented on top of LVars:
------------------------------------------------------------------------------

-- | We only have one mutable location here, so this is not a scalable implementation.
-- newtype ISet a = ISet (LVar (IORef (S.Set a))) a
newtype ISet a = ISet (LVar (IORef (S.Set a)) a)

-- | Physical identity, just as with IORefs.
instance Eq (ISet v) where
  ISet lv1 == ISet lv2 = state lv1 == state lv2 

instance LVarData1 ISet where
  type Snapshot ISet a = S.Set a
  freeze    = freezeSet
  newBottom = newEmptySet

newEmptySet :: Par (ISet a)
newEmptySet = fmap ISet $ newLV$ newIORef S.empty

-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- valueof the Set variable.
withCallbacksThenFreeze :: Eq b => ISet a -> (a -> Par ()) -> Par b -> Par b
withCallbacksThenFreeze (ISet lv) callback action =
    do
       res <- IV.new -- TODO, specialize to skip this when the init action returns ()
       freezeLVAfter lv (initCB res) (\x -> return$ Just$ callback x)
       -- freezeSet lv -- This does nothing, but it gives us the value.
       IV.get res
  where 
    initCB resIV ref = do
      -- The implementation guarantees that all elements will be caught either here,
      -- or by the delta-callback:
      set <- readIORef ref -- Snapshot
      return $ Just $ do
        F.foldlM (\() v -> fork$ callback v) () set -- Non-allocating traversal.
        res <- action -- Any additional puts here trigger the callback.
        IV.put_ resIV res

-- | Put a single element in the set.  (WHNF) Strict in the element being put in the
-- set.     
putInSet :: Ord a => a -> ISet a -> Par () 
putInSet !elm (ISet lv) = putLV lv putter
  where putter ref  = atomicModifyIORef ref update
        update set =
          let set' = S.insert elm set in
          -- Here we do a constant time check to see if we actually changed anything:
          -- For idempotency it is important that we return Nothing if not.
          if S.size set' > S.size set
          then (set',Just elm)
          else (set,Nothing)


-- | Wait for the set to contain a specified element.
waitElem :: Ord a => a -> ISet a -> Par ()
waitElem !elm (ISet lv) = getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      set <- readIORef ref
      case S.member elm set of
        True  -> return (Just ())
        False -> return (Nothing)
    deltaThresh e2 | e2 == elm = return $ Just ()
                   | otherwise  = return Nothing 


-- | Wait on the SIZE of the set, not its contents.
waitSize :: Int -> ISet a -> Par ()
waitSize !sz (ISet lv) = getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      set <- readIORef ref
      case S.size set >= sz of
        True  -> return (Just ())
        False -> return (Nothing)
    -- Here's an example of a situation where we CANNOT TELL if a delta puts it over
    -- the threshold.a
    deltaThresh _ = globalThresh (state lv) False

-- | Get the exact contents of the set.  Using this may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
freezeSet :: ISet a -> Par (S.Set a)
freezeSet (ISet lv) =
   do freezeLV lv
      getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing
