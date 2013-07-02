{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.LVar.Map
       (
--         IMap, newEmptySet, putInSet, withCallbacksThenFreeze,
--         waitElem, waitSize, freezeSet
       ) where

import           Data.IORef
import qualified Data.Map as M
import qualified Data.LVar.IVar as IV

import           Control.LVish
import           Data.LVar.Classes

------------------------------------------------------------------------------
-- IMaps and setmap implemented on top of LVars:
------------------------------------------------------------------------------

-- | We only have one mutable location here, so this is not a scalable implementation.
newtype IMap k v = IMap (LVar (IORef (M.Map k v)) (k,v))

newEmptyMap :: Par (IMap k v)
newEmptyMap = fmap IMap $ newLV$ newIORef M.empty

{-
-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- valueof the Set variable.
withCallbacksThenFreeze :: Eq b => IMap a -> (a -> Par ()) -> Par b -> Par b
withCallbacksThenFreeze (IMap lv) callback action =
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
        mapM_ (fork . callback) (M.toList set) -- Alas, no non-allocating iterator over Data.Set
        res <- action -- Any additional puts here trigger the callback.
        IV.put_ resIV res

-}

-- | Put a single element in the set.  (WHNF) Strict in the element being put in the
-- set.     
insert :: Ord k => k -> v -> IMap k v -> Par () 
insert !key !elm (IMap lv) = putLV lv putter
  where putter ref  = atomicModifyIORef ref update
        update set =
          let set' = M.insert key elm set in
          -- Here we do a constant time check to see if we actually changed anything:
          -- For idempotency it is important that we return Nothing if not.
          if M.size set' > M.size set
          then (set',Just (key,elm))
          else (set, Nothing)

-- | IMap's containing other LVars have some additional capabilities compared to
-- those containing regular Haskell data.  In particular, it is possible to modify
-- existing entries (monotonically).  Further, the "modify" function implicitly
-- inserts a "bottom" element if there is no existing entry for the key.
modify :: LVarData1 f => key -> IMap key (f a) -> (f a -> Par b) -> Par b
modify = error "finishme"

{-

-- | Wait for the set to contain a specified element.
waitElem :: Ord a => a -> IMap a -> Par ()
waitElem !elm (IMap lv) = getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      set <- readIORef ref
      case M.member elm set of
        True  -> return (Just ())
        False -> return (Nothing)
    deltaThresh e2 | e2 == elm = return $ Just ()
                   | otherwise  = return Nothing 


-- | Wait on the SIZE of the set, not its contents.
waitSize :: Int -> IMap a -> Par ()
waitSize !sz (IMap lv) = getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      set <- readIORef ref
      case M.size set >= sz of
        True  -> return (Just ())
        False -> return (Nothing)
    -- Here's an example of a situation where we CANNOT TELL if a delta puts it over
    -- the threshold.a
    deltaThresh _ = globalThresh (state lv) False

-- | Get the exact contents of the set.  Using this may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
freezeSet :: IMap a -> Par (M.Set a)
freezeSet (IMap lv) =
   do freezeLV lv
      getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing

-}

