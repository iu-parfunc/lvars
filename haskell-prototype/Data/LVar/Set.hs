{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.LVar.Set
       (
         ISet, newEmptySet, putInSet, freezeAfterCallbacks,
         waitElem, waitSize, freezeSet
       ) where

import           Data.IORef
import qualified Data.Set as S
import qualified Data.LVar.IVar as IV

import           Control.LVish

------------------------------------------------------------------------------
-- ISets and setmap implemented on top of LVars:
------------------------------------------------------------------------------

-- | We only have one mutable location here, so this is not a scalable implementation.
-- newtype ISet a = ISet (LVar (IORef (S.Set a))) a
type ISet a = LVar (IORef (S.Set a)) a

newEmptySet :: Par (ISet a)
newEmptySet = newLV$ newIORef S.empty

-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- valueof the Set variable.
freezeAfterCallbacks :: ISet a -> (a -> Par ()) -> Par b -> Par b
freezeAfterCallbacks lv callback action =
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
        mapM_ (fork . callback) (S.toList set) -- Alas, no non-allocating iterator over Data.Set
        res <- action -- Any additional puts here trigger the callback.
        IV.put_ resIV res

-- | Put a single element in the set.  (WHNF) Strict in the element being put in the
-- set.     
putInSet :: Ord a => a -> ISet a -> Par () 
putInSet !elm lv = putLV lv putter
  where putter ref  = atomicModifyIORef ref update
        update set =
          let set' = S.insert elm set in
          if S.size set' > S.size set
          then (set',Just elm)
          else (set,Nothing)


-- | Wait for the set to contain a specified element.
waitElem :: Ord a => a -> ISet a -> Par ()
waitElem !elm lv = getLV lv globalThresh deltaThresh
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
waitSize !sz lv = getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      set <- readIORef ref
      case S.size set > sz of
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
freezeSet lv = do freezeLV lv
                  getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing
