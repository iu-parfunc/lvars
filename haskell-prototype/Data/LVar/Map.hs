{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.LVar.Map
       (
         IMap, Snapshot(IMapSnap),
         newEmptyMap, newMap, newFromList,
         insert, 
         getKey, waitValue, waitSize, modify, freezeMap,

         -- * Iteration and callbacks
         forEach, addHandler, 
         withCallbacksThenFreeze,

         -- * Higher-level derived operations
         copy, traverseMap, traverseMap_, 
         
         -- * Alternate versions of derived ops that expose HandlerPools they create.
         forEachHP, traverseMapHP, traverseMapHP_, 
         unionHP
       ) where

import           Data.IORef
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.LVar.IVar as IV
import qualified Data.Traversable as T

import           Control.Monad (void)
import           Control.LVish hiding (addHandler)
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV,
                                                freezeLVAfter, liftIO)
import qualified Control.LVish.SchedIdempotent as L

type QPar = Par QuasiDet 

------------------------------------------------------------------------------
-- IMaps implemented on top of LVars:
------------------------------------------------------------------------------

-- | We only have one mutable location here, so this is not a scalable implementation.
newtype IMap k s v = IMap (LVar s (IORef (M.Map k v)) (k,v))

instance Eq (IMap k s v) where
  IMap lv1 == IMap lv2 = state lv1 == state lv2 

-- Need LVarData2:

instance LVarData1 (IMap k) where
  newtype Snapshot (IMap k) a = IMapSnap (M.Map k a)
      deriving (Show,Ord,Read,Eq)
  freeze    = fmap IMapSnap . freezeMap
  newBottom = newEmptyMap

--------------------------------------------------------------------------------

-- | Create a fresh map with nothing in it.
newEmptyMap :: Par d s (IMap k s v)
newEmptyMap = WrapPar$ fmap (IMap . WrapLVar) $ newLV$ newIORef M.empty

-- | Create a new map populated with initial elements.
newMap :: M.Map k v -> Par d s (IMap k s v)
newMap m = WrapPar$ fmap (IMap . WrapLVar) $ newLV$ newIORef m

-- | Create a new 'IMap' drawing initial elements from an existing list.
newFromList :: (Ord k, Eq v) =>
               [(k,v)] -> Par d s (IMap k s v)
newFromList ls = newMap (M.fromList ls)


-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- valueof the Map variable.
withCallbacksThenFreeze :: forall k v b s . Eq b =>
                           IMap k s v -> (k -> v -> QPar s ()) -> QPar s b -> QPar s b
withCallbacksThenFreeze (IMap (WrapLVar lv)) callback action =
    do
       res <- IV.new -- TODO, specialize to skip this when the init action returns ()
       WrapPar$ freezeLVAfter lv (initCB res) deltaCB
       -- freezeSet lv -- This does nothing, but it gives us the value.
       IV.get res
  where
    deltaCB (k,v) = return$ Just$ unWrapPar $ callback k v
    initCB :: IV.IVar s b -> (IORef (M.Map k v)) -> IO (Maybe (L.Par ()))
    initCB resIV ref = do
      -- The implementation guarantees that all elements will be caught either here,
      -- or by the delta-callback:
      mp <- readIORef ref -- Snapshot
      return $ Just $ unWrapPar $ do 
        -- Data.Foldable should give us a non-copying way to iterate:
        -- But it's actually insufficient because it only exposes the values:
        -- F.foldlM (\() v -> fork$ callback undefined v) () mp
        mapM_ (\(k,v) -> fork$ callback k v) (M.toList mp)
-- FIXME: forkInPool
        
        res <- action -- Any additional puts here trigger the callback.
        IV.put_ resIV res


addHandler :: HandlerPool                 -- ^ pool to enroll in 
           -> IMap k s v                    -- ^ Map to listen to
           -> (k -> v -> Par d s ())          -- ^ callback
           -> Par d s ()
addHandler hp (IMap (WrapLVar lv)) callb = WrapPar $ do
    L.addHandler hp lv globalCB deltaCB
    return ()
  where
    deltaCB (k,v) = return$ Just$ unWrapPar $ callb k v
    globalCB ref = do
      mp <- readIORef ref -- Snapshot
      return $ Just $ unWrapPar $ 
        -- FIXME: need traverseWithKey_ to be added to 'containers':
        mapM_ (\(k,v) -> fork$ callb k v) (M.toList mp)


-- | Put a single entry into the map.  (WHNF) Strict in the key and value.
insert :: (Ord k, Eq v) =>
          k -> v -> IMap k s v -> Par d s () 
insert !key !elm (IMap (WrapLVar lv)) = WrapPar$ putLV lv putter
  where putter ref  = atomicModifyIORef ref update
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
modify :: forall f a b d s key . (Ord key, LVarData1 f) =>
          IMap key s (f s a) -> key -> (f s a -> Par d s b) -> Par d s b
modify (IMap lv) key fn = WrapPar $ do 
  let ref = state lv      
  mp  <- L.liftIO$ readIORef ref
  case M.lookup key mp of
    Just lv2 -> unWrapPar$ fn lv2
    Nothing -> do 
      bot <- unWrapPar newBottom :: L.Par (f s a)
      act <- L.liftIO$ atomicModifyIORef ref $ \ mp2 ->
               case M.lookup key mp2 of
                 Just lv2 -> (mp2, unWrapPar$ fn lv2)
                 Nothing  -> (M.insert key bot mp2,
                              unWrapPar$ fn bot)
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
freezeMap :: IMap k s v -> QPar s (M.Map k v)
freezeMap (IMap (WrapLVar lv)) = WrapPar $
   do freezeLV lv
      getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing

--------------------------------------------------------------------------------
-- Higher level routines that could (mostly) be defined using the above interface.
--------------------------------------------------------------------------------

-- | Shorthandfor creating a new handler pool and adding a single handler to it.
forEach :: IMap k s v -> (k -> v -> Par d s ()) -> Par d s HandlerPool
forEach = forEachHP Nothing

-- | Establish monotonic map between the input and output sets.  Produce a new result
-- based on each element, while leaving the keys the same.
traverseMap :: (Ord k, Eq b) =>
               (k -> a -> Par d s b) -> IMap k s a -> Par d s (IMap k s b)
traverseMap f s = fmap snd $ traverseMapHP Nothing f s

-- | An imperative-style, inplace version of 'traverseMap' that takes the output set
-- as an argument.
traverseMap_ :: (Ord k, Eq b) =>
                (k -> a -> Par d s b) -> IMap k s a -> IMap k s b -> Par d s ()
traverseMap_ f s o = void $ traverseMapHP_ Nothing f s o

--------------------------------------------------------------------------------
-- Alternate versions of functions that EXPOSE the HandlerPools
--------------------------------------------------------------------------------

-- | Variant of 'forEach' that optionally uses an existing handler pool.
forEachHP :: Maybe HandlerPool -> IMap k s v -> (k -> v -> Par d s ()) -> Par d s HandlerPool
forEachHP mh is cb = do 
   hp <- fromMaybe newPool (fmap return mh)
   addHandler hp is cb
   return hp

-- | Return a fresh map which will contain strictly more elements than the input.
-- That is, things put in the former go in the latter, but not vice versa.
copy :: (Ord k, Eq v) => IMap k s v -> Par d s (IMap k s v)
copy = traverseMap (\ _ x -> return x)

-- | Variant that optionally uses an existing handler pool.
traverseMapHP :: (Ord k, Eq b) =>
                 Maybe HandlerPool -> (k -> a -> Par d s b) -> IMap k s a ->
                 Par d s (HandlerPool, IMap k s b)
traverseMapHP mh fn set = do
  os <- newEmptyMap
  hp <- traverseMapHP_ mh fn set os  
  return (hp,os)

-- | Variant that optionally uses an existing handler pool.
traverseMapHP_ :: (Ord k, Eq b) =>
                  Maybe HandlerPool -> (k -> a -> Par d s b) -> IMap k s a -> IMap k s b ->
                  Par d s HandlerPool
traverseMapHP_ mh fn set os = do
  forEachHP mh set $ \ k x -> do 
    x' <- fn k x
    insert k x' os

-- | Return a new map which will (ultimately) contain everything in either input map.
--   Conflicting entries will result in a multiple put exception.
unionHP :: (Ord k, Eq a) => Maybe HandlerPool ->
           IMap k s a -> IMap k s a -> Par d s (HandlerPool, IMap k s a)
unionHP mh m1 m2 = do
  hp <- fromMaybe newPool (fmap return mh)  
  os <- newEmptyMap
  addHandler hp m1 (\ k v -> insert k v os)
  addHandler hp m2 (\ k v -> insert k v os)
  return (hp, os)

--------------------------------------------------------------------------------
-- Map specific DeepFreeze instances:
--------------------------------------------------------------------------------

-- Teach it how to freeze WITHOUT the annoying snapshot constructor:
instance DeepFreeze (IMap k s a) (M.Map k a) where
  type Session (IMap k s a) = s
  deepFreeze iv = do IMapSnap m <- freeze iv
                     return m

instance (LVarData1 f, DeepFreeze (f s0 a) b, Ord b, Ord key) =>
         DeepFreeze (IMap key s0 (f s0 a))
                    (M.Map key b)  where
    type Session (IMap key s0 (f s0 a)) = s0
    deepFreeze from = do
      x <- freezeMap from
      let fn :: key -> f s0 a -> M.Map key b -> QPar s0 (M.Map key b)
          fn k elm acc = do elm' <- deepFreeze elm
                            return (M.insert k elm' acc)
      T.traverse deepFreeze x
