{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.LVar.SLMap
       (
         IMap, 
--         Snapshot(IMapSnap), -- AJT: killed for now
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

import           Data.Concurrent.SkipListMap as SLM
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.LVar.IVar as IV
import qualified Data.Traversable as T

import           Control.Monad
import           Control.LVish
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, putLV_, getLV, freezeLV,
                                                freezeLVAfter, liftIO, addHandler)
import qualified Control.LVish.SchedIdempotent as L
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.StableName (makeStableName, hashStableName)

type QPar = Par QuasiDet 

------------------------------------------------------------------------------
-- IMaps implemented vis SkipListMap
------------------------------------------------------------------------------

-- | We only have one mutable location here, so this is not a scalable implementation.
newtype IMap k s v = IMap (LVar s (SLM.SLMap k v) (k,v))

instance Eq (IMap k s v) where
  IMap lv1 == IMap lv2 = state lv1 == state lv2 

-- Need LVarData2:

-- AJT: killed for now
#if 0
instance LVarData1 (IMap k) where
  newtype Snapshot (IMap k) a = IMapSnap (M.Map k a)
      deriving (Show,Ord,Read,Eq)
  freeze    = fmap IMapSnap . freezeMap
  newBottom = newEmptyMap

  traverseSnap fn (IMapSnap mp) = 
    fmap IMapSnap $ T.traverse fn mp 
#endif

--------------------------------------------------------------------------------

-- | The default number of skiplist levels
defaultLevels :: Int
defaultLevels = 8

-- | Create a fresh map with nothing in it.
newEmptyMap :: Par d s (IMap k s v)
newEmptyMap = newEmptyMap_ defaultLevels

-- | Create a fresh map with nothing in it, with the given number of skiplist
-- levels.
newEmptyMap_ :: Int -> Par d s (IMap k s v)
newEmptyMap_ n = fmap (IMap . WrapLVar) $ WrapPar $ newLV $ SLM.newSLMap n

-- | Create a new map populated with initial elements.
newMap :: M.Map k v -> Par d s (IMap k s v)
newMap m = error "TODO"

-- | Create a new 'IMap' drawing initial elements from an existing list.
newFromList :: (Ord k, Eq v) =>
               [(k,v)] -> Par d s (IMap k s v)
newFromList ls = newFromList_ ls defaultLevels

-- | Create a new 'IMap' drawing initial elements from an existing list, with
-- the given number of skiplist levels.
newFromList_ :: Ord k => [(k,v)] -> Int -> Par d s (IMap k s v)
newFromList_ ls n = do  
  m@(IMap lv) <- newEmptyMap_ n
  forM_ ls $ \(k,v) -> LI.liftIO $ SLM.putIfAbsent (state lv) k $ return v
  return m

-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- value of the provided action.
withCallbacksThenFreeze :: forall k v b s . Eq b =>
                           IMap k s v -> (k -> v -> QPar s ()) -> QPar s b -> QPar s b
withCallbacksThenFreeze (IMap lv) callback action = do
  hp  <- newPool 
  res <- IV.new 
  let deltCB (k,v) = return$ Just$ unWrapPar$ callback k v
      initCB slm = do
        -- The implementation guarantees that all elements will be caught either here,
        -- or by the delta-callback:
        return $ Just $ unWrapPar $ do
          SLM.foldlWithKey (\() k v -> forkHP (Just hp) $ callback k v) () slm
          x <- action -- Any additional puts here trigger the callback.
          IV.put_ res x
  WrapPar $ addHandler (Just hp) (unWrapLVar lv) initCB deltCB
  
  -- We additionally have to quiesce here because we fork the inital set of
  -- callbacks on their own threads:
  quiesce hp
  IV.get res

-- | Add an (asynchronous) callback that listens for all new key/value pairs
-- added to the map, optionally tied to a handler pool.
forEachHP :: Maybe HandlerPool           -- ^ optional pool to enroll in 
          -> IMap k s v                  -- ^ Map to listen to
          -> (k -> v -> Par d s ())      -- ^ callback
          -> Par d s ()
forEachHP mh (IMap (WrapLVar lv)) callb = WrapPar $ 
    addHandler mh lv globalCB (\(k,v) -> return$ Just$ unWrapPar$ callb k v)
  where
    globalCB slm = 
      return $ Just $ unWrapPar $
        SLM.foldlWithKey (\() k v -> forkHP mh $ callb k v) () slm
        
-- | Add an (asynchronous) callback that listens for all new new key/value pairs added to
-- the map
forEach :: IMap k s v -> (k -> v -> Par d s ()) -> Par d s ()
forEach = forEachHP Nothing         

-- | Put a single entry into the map.  (WHNF) Strict in the key and value.
insert :: (Ord k, Eq v) =>
          k -> v -> IMap k s v -> Par d s () 
insert !key !elm (IMap (WrapLVar lv)) = WrapPar$ putLV lv putter
  where putter slm = do
          putRes <- SLM.putIfAbsent slm key $ return elm
          case putRes of
            Added _ -> return $ Just (key, elm)
            Found _ -> error "Multiple puts to one entry in an IMap!"
          
-- | IMap's containing other LVars have some additional capabilities compared to
-- those containing regular Haskell data.  In particular, it is possible to modify
-- existing entries (monotonically).  Further, this `modify` function implicitly
-- inserts a "bottom" element if there is no existing entry for the key.
modify :: forall f a b d s key . (Ord key, LVarData1 f, Show key) =>
          IMap key s (f s a) -> key -> (f s a -> Par d s b) -> Par d s b
modify (IMap (WrapLVar lv)) key fn = do
    act <- WrapPar $ putLV_ lv putter
    act
  where putter slm = do
          putRes <- unWrapPar $ SLM.putIfAbsent slm key newBottom
          case putRes of
            Added v -> return (Just (key,v), fn v)
            Found v -> return (Nothing,      fn v)          
            
-- | Wait for the map to contain a specified key, and return the associated value.
getKey :: Ord k => k -> IMap k s v -> Par d s v
getKey !key (IMap (WrapLVar lv)) = WrapPar$ getLV lv globalThresh deltaThresh
  where
    globalThresh slm _frzn = SLM.find slm key
    deltaThresh (k,v) | k == key  = return $ Just v
                      | otherwise = return Nothing 

-- | Wait until the map contains a certain value (on any key).
waitValue :: (Ord k, Eq v) => v -> IMap k s v -> Par d s ()
waitValue !val (IMap (WrapLVar lv)) = error "TODO"

-- | Wait on the SIZE of the map, not its contents.
waitSize :: Int -> IMap k s v -> Par d s ()
waitSize !sz (IMap (WrapLVar lv)) = WrapPar $
    getLV lv globalThresh deltaThresh
  where
    globalThresh slm _ = do
      snapSize <- SLM.foldlWithKey (\n _ _ -> return $ n+1) 0 slm
      case snapSize >= sz of
        True  -> return (Just ())
        False -> return (Nothing)
    -- Here's an example of a situation where we CANNOT TELL if a delta puts it over
    -- the threshold.a
    deltaThresh _ = globalThresh (L.state lv) False

-- | Get the exact contents of the map  Using this may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
freezeMap :: Ord k => IMap k s v -> QPar s (M.Map k v)
freezeMap (IMap (WrapLVar lv)) = WrapPar $ do
  freezeLV lv
  L.liftIO $ SLM.foldlWithKey (\m k v -> return $ M.insert k v m) M.empty (L.state lv)

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
--   map.  Conflicting entries will result in a multiple put exception.
--   Optionally ties the handlers to a pool.
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

-- AJT: killed this for now
#if 0

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

#endif