{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

{-|

  This module provides finite maps that only grow.  It is based on the popular "Data.Map"
  balanced-tree representation of maps.  Thus scalability is /not/ good for this
  implementation.  However, there are some interoperability benefits.  For example,
  after running a parallel computation with a map result, this module can produce a
  `Map` in /O(1)/ without copying, which may be useful downstream.

 -}

module Data.LVar.PureMap
       (
         -- * Basic operations
         IMap(..), 
         newEmptyMap, newMap, newFromList,
         insert, 
         getKey, waitValue, waitSize, modify,

         -- * Generic routines and convenient aliases
         gmodify, getOrInit,
         
         -- * Iteration and callbacks
         forEach, forEachHP,
         withCallbacksThenFreeze,

         -- * Quasi-deterministic operations
         freezeMap, fromIMap,
         traverseFrzn_,

         -- * Higher-level derived operations
         copy, traverseMap, traverseMap_,  union,
         
         -- * Alternate versions of derived ops that expose @HandlerPool@s they create
         traverseMapHP, traverseMapHP_, unionHP                                        
       ) where

import           Control.LVish.DeepFrz.Internal
import           Control.LVish
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, putLV_, getLV, freezeLV, freezeLVAfter)
import qualified Control.LVish.SchedIdempotent as L
import qualified Data.LVar.IVar as IV
import           Data.LVar.Generic as G
import           Data.LVar.PureMap.Unsafe
import           Data.UtilInternal (traverseWithKey_)

import           Control.Exception (throw)
import           Data.IORef
import qualified Data.Map.Strict as M
import           System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import           System.Mem.StableName (makeStableName, hashStableName)

--------------------------------------------------------------------------------

-- | Create a fresh map with nothing in it.
newEmptyMap :: Par d s (IMap k s v)
newEmptyMap = WrapPar$ fmap (IMap . WrapLVar) $ newLV$ newIORef M.empty

-- | Create a new map populated with initial elements.
newMap :: M.Map k v -> Par d s (IMap k s v)
newMap m = WrapPar$ fmap (IMap . WrapLVar) $ newLV$ newIORef m

-- | A convenience function that is equivalent to calling `Data.Map.fromList`
-- followed by `newMap`.
newFromList :: (Ord k, Eq v) =>
               [(k,v)] -> Par d s (IMap k s v)
newFromList = newMap . M.fromList

-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- value of the provided action.
withCallbacksThenFreeze :: forall k v b s . Eq b =>
                           IMap k s v -> (k -> v -> QPar s ()) -> QPar s b -> QPar s b
withCallbacksThenFreeze (IMap (WrapLVar lv)) callback action =
    do hp  <- newPool 
       res <- IV.new 
       WrapPar$ freezeLVAfter lv (initCB hp res) deltaCB
       -- We additionally have to quiesce here because we fork the inital set of
       -- callbacks on their own threads:
       quiesce hp
       IV.get res
  where
    deltaCB (k,v) = return$ Just$ unWrapPar $ callback k v
    initCB :: HandlerPool -> IV.IVar s b -> (IORef (M.Map k v)) -> IO (Maybe (L.Par ()))
    initCB hp resIV ref = do
      -- The implementation guarantees that all elements will be caught either here,
      -- or by the delta-callback:
      mp <- readIORef ref -- Snapshot
      return $ Just $ unWrapPar $ do 
        traverseWithKey_ (\ k v -> forkHP (Just hp)$ callback k v) mp
        res <- action -- Any additional puts here trigger the callback.
        IV.put_ resIV res

        
-- | Add an (asynchronous) callback that listens for all new new key/value pairs added to
-- the map.
forEach :: IMap k s v -> (k -> v -> Par d s ()) -> Par d s ()
forEach = forEachHP Nothing 

-- | Put a single entry into the map.  Strict (WHNF) in the key and value.
-- 
--   As with other container LVars, if a key is inserted multiple times, the values had
--   better be equal @(==)@, or a multiple-put error is raised.
insert :: (Ord k, Eq v) =>
          k -> v -> IMap k s v -> Par d s () 
insert !key !elm (IMap (WrapLVar lv)) = WrapPar$ putLV lv putter
  where putter ref  = atomicModifyIORef' ref update
        update mp =
          let mp' = M.insertWith fn key elm mp
              fn v1 v2 | v1 == v2  = v1
                       | otherwise = throw$ ConflictingPutExn$ "Multiple puts to one entry in an IMap!"
          in
          -- Here we do a constant time check to see if we actually changed anything:
          -- For idempotency it is important that we return Nothing if not.
          if M.size mp' > M.size mp
          then (mp',Just (key,elm))
          else (mp, Nothing)

-- | `IMap`s containing other LVars have some additional capabilities compared to
-- those containing regular Haskell data.  In particular, it is possible to modify
-- existing entries (monotonically).  Further, this `modify` function implicitly
-- inserts a \"bottom\" element if there is no existing entry for the key.
-- 
-- Unfortunately, that means that this takes another computation for creating new
-- \"bottom\" elements for the nested LVars stored inside the `IMap`.
modify :: forall f a b d s key . (Ord key, LVarData1 f, Show key, Ord a) =>
          IMap key s (f s a)
          -> key                  -- ^ The key to lookup.
          -> (Par d s (f s a))    -- ^ Create a new \"bottom\" element whenever an entry is not present.
          -> (f s a -> Par d s b) -- ^ The computation to apply on the right-hand side of the keyed entry.
          -> Par d s b
modify (IMap lv) key newBottom fn = WrapPar $ do 
  let ref = state lv      
  mp  <- L.liftIO$ readIORef ref
  case M.lookup key mp of
    Just lv2 -> do L.logStrLn$ " [Map.modify] key already present: "++show key++
                               " adding to inner "++show(unsafeName lv2)
                   unWrapPar$ fn lv2
    Nothing -> do 
      bot <- unWrapPar newBottom :: L.Par (f s a)
      L.logStrLn$ " [Map.modify] allocated new inner "++show(unsafeName bot)
      let putter _ = L.liftIO$ atomicModifyIORef' ref $ \ mp2 ->
            case M.lookup key mp2 of
              Just lv2 -> (mp2, (Nothing, unWrapPar$ fn lv2))
              Nothing  -> (M.insert key bot mp2,
                           (Just (key, bot), 
                            do L.logStrLn$ " [Map.modify] key absent, adding the new one."
                               unWrapPar$ fn bot))
      act <- putLV_ (unWrapLVar lv) putter
      act

{-# INLINE gmodify #-}
-- | A generic version of `modify` that does not require a `newBottom` argument,
-- rather, it uses the generic version of that function.
gmodify :: forall f a b d s key . (Ord key, LVarWBottom f, LVContents f a, Show key, Ord a) =>
          IMap key s (f s a)
          -> key                  -- ^ The key to lookup.
          -> (f s a -> Par d s b) -- ^ The computation to apply on the right-hand side of the keyed entry.
          -> Par d s b
gmodify map key fn = modify map key G.newBottom fn

{-# INLINE getOrInit #-}
-- | Return the preexisting value for a key if it exists, and otherwise return
-- 
--   This is a convenience routine that can easily be defined in terms of `gmodify`
getOrInit :: forall f a b d s key . (Ord key, LVarWBottom f, LVContents f a, Show key, Ord a) =>
          key -> IMap key s (f s a) -> Par d s (f s a)
getOrInit key mp = gmodify mp key return

-- | Wait for the map to contain a specified key, and return the associated value.
getKey :: Ord k => k -> IMap k s v -> Par d s v
getKey !key (IMap (WrapLVar lv)) = WrapPar$ getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      mp <- readIORef ref
      return (M.lookup key mp)
    deltaThresh (k,v) | k == key  = return$ Just v
                      | otherwise = return Nothing 

-- | Wait until the map contains a certain value (on any key).
waitValue :: (Ord k, Eq v) => v -> IMap k s v -> Par d s ()
waitValue !val (IMap (WrapLVar lv)) = WrapPar$ getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      mp <- readIORef ref
      -- This is very inefficient:
      let fn Nothing v | v == val  = Just ()
                       | otherwise = Nothing
          fn just _  = just
      -- FIXME: no short-circuit for this fold:
      return $! M.foldl fn Nothing mp
    deltaThresh (_,v) | v == val  = return$ Just ()
                      | otherwise = return Nothing 


-- | Wait on the /size/ of the map, not its contents.
waitSize :: Int -> IMap k s v -> Par d s ()
waitSize !sz (IMap (WrapLVar lv)) = WrapPar $
    getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      mp <- readIORef ref
      case M.size mp >= sz of
        True  -> return (Just ())
        False -> return (Nothing)
    -- Here's an example of a situation where we CANNOT TELL if a delta puts it over
    -- the threshold.a
    deltaThresh _ = globalThresh (L.state lv) False

-- | Get the exact contents of the map.  As with any
-- quasi-deterministic operation, using `freezeMap` may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
--
-- This "Data.Map"-based implementation has the special property that
-- you can retrieve the full map without any `IO`, and without
-- nondeterminism leaking.  (This is because the internal order is
-- fixed for the tree-based representation of maps that "Data.Map"
-- uses.)
freezeMap :: IMap k s v -> QPar s (M.Map k v)
freezeMap (IMap (WrapLVar lv)) = WrapPar $
   do freezeLV lv
      getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing

-- | /O(1)/: Convert from an `IMap` to a plain `Data.Map`.
--   This is only permitted when the `IMap` has already been frozen.
--   This is useful for processing the result of `Control.LVish.DeepFrz.runParThenFreeze`.    
fromIMap :: IMap k Frzn a -> M.Map k a 
fromIMap (IMap lv) = unsafeDupablePerformIO (readIORef (state lv))

-- | Traverse a frozen map for side effect.  This is useful (in comparison with more
-- generic operations) because the function passed in may see the key as well as the
-- value.
traverseFrzn_ :: (Ord k) =>
                 (k -> a -> Par d s ()) -> IMap k Frzn a -> Par d s ()
traverseFrzn_ fn mp =
 traverseWithKey_ fn (fromIMap mp)

--------------------------------------------------------------------------------
-- Higher level routines that could (mostly) be defined using the above interface.
--------------------------------------------------------------------------------

-- | Establish a monotonic map between the input and output sets.
-- Produce a new result based on each element, while leaving the keys
-- the same.
traverseMap :: (Ord k, Eq b) =>
               (k -> a -> Par d s b) -> IMap k s a -> Par d s (IMap k s b)
traverseMap f s = traverseMapHP Nothing f s

-- | An imperative-style, in-place version of 'traverseMap' that takes the output set
-- as an argument.
traverseMap_ :: (Ord k, Eq b) =>
                (k -> a -> Par d s b) -> IMap k s a -> IMap k s b -> Par d s ()
traverseMap_ f s o = traverseMapHP_ Nothing f s o

-- | Return a new map which will (ultimately) contain everything in either input
-- map.  Conflicting entries will result in a multiple put exception.
union :: (Ord k, Eq a) => IMap k s a -> IMap k s a -> Par d s (IMap k s a)
union = unionHP Nothing

-- TODO: Intersection

--------------------------------------------------------------------------------
-- Alternate versions of functions that EXPOSE the HandlerPools
--------------------------------------------------------------------------------

-- | Return a fresh map which will contain strictly more elements than the input.
-- That is, things put in the former go in the latter, but not vice versa.
copy :: (Ord k, Eq v) => IMap k s v -> Par d s (IMap k s v)
copy = traverseMap (\ _ x -> return x)

-- | A variant of `traverseMap` that optionally ties the handlers to a pool.
traverseMapHP :: (Ord k, Eq b) =>
                 Maybe HandlerPool -> (k -> a -> Par d s b) -> IMap k s a ->
                 Par d s (IMap k s b)
traverseMapHP mh fn set = do
  os <- newEmptyMap
  traverseMapHP_ mh fn set os  
  return os

-- | A variant of `traverseMap_` that optionally ties the handlers to a pool.
traverseMapHP_ :: (Ord k, Eq b) =>
                  Maybe HandlerPool -> (k -> a -> Par d s b) -> IMap k s a -> IMap k s b ->
                  Par d s ()
traverseMapHP_ mh fn set os = do
  forEachHP mh set $ \ k x -> do 
    x' <- fn k x
    insert k x' os

-- | A variant of `union` that optionally ties the handlers in the
-- resulting set to the same handler pool as those in the two input
-- sets.
unionHP :: (Ord k, Eq a) => Maybe HandlerPool ->
           IMap k s a -> IMap k s a -> Par d s (IMap k s a)
unionHP mh m1 m2 = do
  os <- newEmptyMap
  forEachHP mh m1 (\ k v -> insert k v os)
  forEachHP mh m2 (\ k v -> insert k v os)
  return os

{-# NOINLINE unsafeName #-}
unsafeName :: a -> Int
unsafeName x = unsafePerformIO $ do 
   sn <- makeStableName x
   return (hashStableName sn)
