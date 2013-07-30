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
         forEach, forEachHP,
         withCallbacksThenFreeze,

         -- * Higher-level derived operations
         copy, traverseMap, traverseMap_, 
         
         -- * Alternate versions of derived ops that expose HandlerPools they create.
         traverseMapHP, traverseMapHP_, unionHP
       ) where

import           Control.Monad (void)
import           Control.Applicative (Applicative, (*>), pure, getConst, Const(Const))
import           Data.Monoid (Monoid(..))
import           Data.IORef
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.LVar.IVar as IV
import qualified Data.Traversable as T

import           Control.LVish 
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, putLV_, getLV, freezeLV,
                                                freezeLVAfter, liftIO, addHandler)
import qualified Control.LVish.SchedIdempotent as L
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.StableName (makeStableName, hashStableName)

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

  traverseSnap fn (IMapSnap mp) = 
    fmap IMapSnap $ T.traverse fn mp 

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
    addHandler mh lv globalCB deltaCB
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

-- | Put a single entry into the map.  (WHNF) Strict in the key and value.
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
modify :: forall f a b d s key . (Ord key, LVarData1 f, Show key, Ord a) =>
          IMap key s (f s a) -> key -> (f s a -> Par d s b) -> Par d s b
modify (IMap lv) key fn = WrapPar $ do 
  let ref = state lv      
  mp  <- L.liftIO$ readIORef ref
  case M.lookup key mp of
    Just lv2 -> do L.logStrLn$ " [Map.modify] key already present: "++show key++" adding to inner "++show(unsafeName lv2)
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

-- | Return a new map which will (ultimately) contain everything in either input
-- map.  Conflicting entries will result in a multiple put exception.
-- Optionally ties the handlers to a pool.
unionHP :: (Ord k, Eq a) => Maybe HandlerPool ->
           IMap k s a -> IMap k s a -> Par d s (IMap k s a)
unionHP mh m1 m2 = do
  os <- newEmptyMap
  forEachHP mh m1 (\ k v -> insert k v os)
  forEachHP mh m2 (\ k v -> insert k v os)
  return os

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

{-# NOINLINE unsafeName #-}
unsafeName :: a -> Int
unsafeName x = unsafePerformIO $ do 
   sn <- makeStableName x
   return (hashStableName sn)

--------------------------------------------------------------------------------
-- Version of traverseWithKey_ from Shachaf Ben-Kiki

newtype Traverse_ f = Traverse_ { runTraverse_ :: f () }
instance Applicative f => Monoid (Traverse_ f) where
  mempty = Traverse_ (pure ())
  Traverse_ a `mappend` Traverse_ b = Traverse_ (a *> b)
-- Since the Applicative used is Const (newtype Const m a = Const m), the
-- structure is never built up.
--(b) You can derive traverseWithKey_ from foldMapWithKey, e.g. as follows:
traverseWithKey_ :: Applicative f => (k -> a -> f ()) -> M.Map k a -> f ()
traverseWithKey_ f = runTraverse_ .
                     foldMapWithKey (\k x -> Traverse_ (void (f k x)))
foldMapWithKey :: Monoid r => (k -> a -> r) -> M.Map k a -> r
foldMapWithKey f = getConst . M.traverseWithKey (\k x -> Const (f k x))
