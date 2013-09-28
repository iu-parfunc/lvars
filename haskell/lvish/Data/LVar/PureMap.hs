{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-|

  This module provides finite maps that only grow.  It is based on the popular `Data.Map`
  balanced-tree representation of maps.  Thus scalability is /not/ good for this
  implementation.  However, there are some interoperability benefits.  For example,
  after running a parallel computation with a map result, this module can produce a
  `Data.Map` in /O(1)/ without copying, which may be useful downstream.

 -}

module Data.LVar.PureMap
       (
         -- * Basic operations
         IMap, 
         newEmptyMap, newMap, newFromList,
         insert, 
         getKey, waitValue, waitSize, modify, 

         -- * Freezing results (Quasi-determinism) 
         freezeMap, fromIMap,
         
         -- * Iteration and callbacks
         forEach, forEachHP,
         withCallbacksThenFreeze,

         -- * Higher-level derived operations
         copy, traverseMap, traverseMap_,  union,
         
         -- * Alternate versions of derived ops that expose HandlerPools they create.
         traverseMapHP, traverseMapHP_, unionHP
       ) where

import           Control.Monad (void)
import           Control.Applicative (Applicative, (<$>),(*>), pure, getConst, Const(Const))
import           Data.Monoid (Monoid(..))
import           Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.LVar.IVar as IV
import qualified Data.Foldable as F
import           Data.LVar.Generic
import           Data.LVar.Generic.Internal (unsafeCoerceLVar)
import           Data.UtilInternal (traverseWithKey_)
import           Control.LVish.DeepFrz.Internal
import           Control.LVish
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, putLV_, getLV, freezeLV, freezeLVAfter)
import qualified Control.LVish.SchedIdempotent as L
import           System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import           System.Mem.StableName (makeStableName, hashStableName)

type QPar = Par QuasiDet  -- Shorthand.

------------------------------------------------------------------------------
-- IMaps implemented on top of LVars:
------------------------------------------------------------------------------

-- | The map datatype itself.  Like all other LVars, it has an @s@ parameter (think
--  `STRef`) in addition to the @a@ parameter that describes the type of elements
-- in the set.
-- 
-- Performance note: There is only ONE mutable location in this implementation.  Thus
-- it is not a scalable implementation.
newtype IMap k s v = IMap (LVar s (IORef (M.Map k v)) (k,v))

-- | Equality is physical equality, as with @IORef@s.
instance Eq (IMap k s v) where
  IMap lv1 == IMap lv2 = state lv1 == state lv2 

-- | An `IMap` can be treated as a generic container LVar.  However, the polymorphic
-- operations are less useful than the monomorphic ones exposed by this module.
instance LVarData1 (IMap k) where
  freeze orig@(IMap (WrapLVar lv)) = WrapPar$ do freezeLV lv; return (unsafeCoerceLVar orig)  
  sortFreeze is = AFoldable <$> freezeMap is
  -- Unlike the Map-specific forEach variants, this takes only values, not keys.
  addHandler mh mp fn = forEachHP mh mp (\ _k v -> fn v)

-- | The `IMap`s in this module also have the special property that they support an
-- `O(1)` freeze operation which immediately yields a `Foldable` container
-- (`snapFreeze`).
instance OrderedLVarData1 (IMap k) where
  snapFreeze is = unsafeCoerceLVar <$> freeze is

-- | As with all LVars, after freezing, map elements can be consumed. In the case of
-- this `IMap` implementation, it need only be `Frzn`, not `Trvrsbl`.
instance F.Foldable (IMap k Frzn) where
  foldr fn zer (IMap lv) =
    let set = unsafeDupablePerformIO (readIORef (state lv)) in
    F.foldr fn zer set 

-- | Of course, the stronger `Trvrsbl` state is still fine for folding.
instance F.Foldable (IMap k Trvrsbl) where
  foldr fn zer mp = F.foldr fn zer (castFrzn mp)

-- | `IMap` values can be returned as the result of a `runParThenFreeze`.
--   Hence they need a `DeepFrz` instace.
--   @DeepFrz@ is just a type-coercion.  No bits flipped at runtime.
instance DeepFrz a => DeepFrz (IMap k s a) where
  type FrzType (IMap k s a) = IMap k Frzn a 
  frz = unsafeCoerceLVar

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
-- valueof the Map variable.
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

-- | Add an (asynchronous) callback that listens for all new key/value pairs added to
-- the map, optionally enrolled in a handler pool
forEachHP :: Maybe HandlerPool           -- ^ optional pool to enroll in 
          -> IMap k s v                  -- ^ Map to listen to
          -> (k -> v -> Par d s ())      -- ^ callback
          -> Par d s ()
forEachHP mh (IMap (WrapLVar lv)) callb = WrapPar $ do
    L.addHandler mh lv globalCB deltaCB
    return ()
  where
    deltaCB (k,v) = return$ Just$ unWrapPar $ callb k v
    globalCB ref = do
      mp <- readIORef ref -- Snapshot
      return $ Just $ unWrapPar $ 
        traverseWithKey_ (\ k v -> forkHP mh$ callb k v) mp
        
-- | Add an (asynchronous) callback that listens for all new new key/value pairs added to
-- the map
forEach :: IMap k s v -> (k -> v -> Par d s ()) -> Par d s ()
forEach = forEachHP Nothing 

-- | Put a single entry into the map.  Strict (WHNF) in the key and value.
insert :: (Ord k, Eq v) =>
          k -> v -> IMap k s v -> Par d s () 
insert !key !elm (IMap (WrapLVar lv)) = WrapPar$ putLV lv putter
  where putter ref  = atomicModifyIORef' ref update
        update mp =
          let mp' = M.insertWith fn key elm mp
              fn v1 v2 | v1 == v2  = v1
                       | otherwise = error "Multiple puts to one entry in an IMap!"
          in
          -- Here we do a constant time check to see if we actually changed anything:
          -- For idempotency it is important that we return Nothing if not.
          if M.size mp' > M.size mp
          then (mp',Just (key,elm))
          else (mp, Nothing)

-- | IMap's containing other LVars have some additional capabilities compared to
-- those containing regular Haskell data.  In particular, it is possible to modify
-- existing entries (monotonically).  Further, this `modify` function implicitly
-- inserts a "bottom" element if there is no existing entry for the key.
--
-- Unfortunately, that means that this takes another computation for creating new
-- "bottom" elements for the nested LVars stored inside the Map.
modify :: forall f a b d s key . (Ord key, LVarData1 f, Show key, Ord a) =>
          IMap key s (f s a)
          -> key                  -- ^ The key to lookup.
          -> (Par d s (f s a))    -- ^ Create a new "bottom" element whenever an entry is not present.
          -> (f s a -> Par d s b) -- ^ The computation to apply on the right-hand-side of the keyed entry.
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


-- | Wait on the SIZE of the map, not its contents.
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

-- | Get the exact contents of the map  Using this may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
--
-- This Data.Map based LVar has the property that you can
-- retrieve the full set without any IO, and without nondeterminism
-- leaking.  (This is because the internal order is fixed for the
-- tree-based Data.Set.)    
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

--------------------------------------------------------------------------------
-- Higher level routines that could (mostly) be defined using the above interface.
--------------------------------------------------------------------------------

-- | Establish monotonic map between the input and output sets.  Produce a new result
-- based on each element, while leaving the keys the same.
traverseMap :: (Ord k, Eq b) =>
               (k -> a -> Par d s b) -> IMap k s a -> Par d s (IMap k s b)
traverseMap f s = traverseMapHP Nothing f s

-- | An imperative-style, inplace version of 'traverseMap' that takes the output set
-- as an argument.
traverseMap_ :: (Ord k, Eq b) =>
                (k -> a -> Par d s b) -> IMap k s a -> IMap k s b -> Par d s ()
traverseMap_ f s o = traverseMapHP_ Nothing f s o

-- | Return a new map which will (ultimately) contain everything in either input
-- map.  Conflicting entries will result in a multiple put exception.
-- Optionally ties the handlers to a pool.
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

-- | Variant that optionally ties the handlers to a pool.
traverseMapHP :: (Ord k, Eq b) =>
                 Maybe HandlerPool -> (k -> a -> Par d s b) -> IMap k s a ->
                 Par d s (IMap k s b)
traverseMapHP mh fn set = do
  os <- newEmptyMap
  traverseMapHP_ mh fn set os  
  return os

-- | Variant that optionally ties the handlers to a pool.
traverseMapHP_ :: (Ord k, Eq b) =>
                  Maybe HandlerPool -> (k -> a -> Par d s b) -> IMap k s a -> IMap k s b ->
                  Par d s ()
traverseMapHP_ mh fn set os = do
  forEachHP mh set $ \ k x -> do 
    x' <- fn k x
    insert k x' os

-- | Variant that optionally ties the handlers in the resulting set to the same
-- handler pool as those in the two input sets.
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

