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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}

module Data.LVar.SLMap
       (
         IMap,
         newEmptyMap, newMap, newFromList,
         insert, 
         getKey, waitValue, waitSize, modify, freezeMap,

         -- * Iteration and callbacks
         forEach, forEachHP, 
         withCallbacksThenFreeze,

         -- * Higher-level derived operations
         copy, traverseMap, traverseMap_, 
         
         -- * Alternate versions of derived ops that expose HandlerPools they create.
         traverseMapHP, traverseMapHP_, unionHP,
         
         -- * Frozen IMaps
         Map(), lookup, toStdMap, foldWithKeyM, traverseFrzn
       ) where

import           Data.Concurrent.SkipListMap as SLM
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.LVar.IVar as IV
import qualified Data.Traversable as T
import qualified Data.Foldable    as F

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.LVish
import           Control.LVish.DeepFrz.Internal
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, putLV_, getLV, freezeLV,
                                                freezeLVAfter, liftIO, addHandler)
import qualified Control.LVish.SchedIdempotent as L
import           System.Random (randomIO)
import           System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import           System.Mem.StableName (makeStableName, hashStableName)

import GHC.Prim (unsafeCoerce#)

import Prelude hiding (lookup)

type QPar = Par QuasiDet 

------------------------------------------------------------------------------
-- IMaps implemented vis SkipListMap
------------------------------------------------------------------------------

-- | We only have one mutable location here, so this is not a scalable implementation.
data IMap k s v = Ord k => IMap {-# UNPACK #-} !(LVar s (SLM.SLMap k v) (k,v))

-- | A frozen version of an IMap is a pure value.  These are effectively equivalent
-- to the standard Data.Map data structure.
data Map k v = Ord k => FrzMap !(SLM.SLMap k v)

instance Eq (IMap k s v) where
  IMap lv1 == IMap lv2 = state lv1 == state lv2 


{- 
instance LVarData1 (IMap k) where
  newtype Snapshot (IMap k) a = IMapSnap (Map k a)
      deriving (Show,Ord,Eq)
  freeze m@(IMap v) = fmap IMapSnap $ freezeMap m
  
  -- newBottom = newEmptyMap  -- Ergh, this has problems... let's just remove it from the API.
  traverseSnap fn (IMapSnap mp@(FrzMap _)) = 
    fmap IMapSnap $ traverseFrzn fn mp
-}

--------------------------------------------------------------------------------

-- | The default number of skiplist levels
defaultLevels :: Int
defaultLevels = 8

-- | Create a fresh map with nothing in it.
newEmptyMap :: Ord k => Par d s (IMap k s v)
newEmptyMap = newEmptyMap_ defaultLevels

-- | Create a fresh map with nothing in it, with the given number of skiplist
-- levels.
newEmptyMap_ :: Ord k => Int -> Par d s (IMap k s v)
newEmptyMap_ n = fmap (IMap . WrapLVar) $ WrapPar $ newLV $ SLM.newSLMap n

-- | Create a new map populated with initial elements.
newMap :: M.Map k v -> Par d s (IMap k s v)
newMap m = error "TODO - newMap"

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
modify :: forall f a b d s key . (Ord key, LVarData1 f, Show key, Ord a) =>
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
--
-- This is an O(1) operation that doesn't copy the in-memory representation of the
-- IMap.
freezeMap :: Ord k => IMap k s v -> QPar s (IMap k Frzn v)
-- freezeMap (IMap (WrapLVar lv)) = return (IMap (WrapLVar lv))
-- OR we can just do this:

freezeMap x@(IMap (WrapLVar lv)) = WrapPar $ do
  freezeLV lv
  -- For the final deepFreeze at the end of a runpar we can actually skip
  -- the freezeLV part....  
  return (unsafeCoerce# x)

instance DeepFrz a => DeepFrz (IMap k s a) where
  type FrzType (IMap k s a) = IMap k Frzn (FrzType a)
  frz = unsafeCoerce#

-- freezeMap :: Ord k => IMap k s v -> QPar s (Map k v)
-- freezeMap (IMap (WrapLVar lv)) = WrapPar $ do
--   freezeLV lv
--   return (FrzMap (L.state lv))

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

{-
-- Teach it how to freeze WITHOUT the annoying snapshot constructor:
instance DeepFreeze (IMap k s a) (Map k a) where
  type Session (IMap k s a) = s
  deepFreeze iv = do IMapSnap m <- freeze iv
                     return m
-}


--------------------------------------------------------------------------------
-- Operations on frozen Maps
--------------------------------------------------------------------------------

-- | Look up an entry in a frozen map.
lookup :: Ord k => k -> Map k a -> Maybe a
lookup k (FrzMap slm) = unsafeDupablePerformIO (SLM.find slm k)

-- | Convert to the standard Map data structure.  /O(N)/
toStdMap :: Ord k => Map k a -> M.Map k a
toStdMap (FrzMap slm) = unsafeDupablePerformIO $ 
   SLM.foldlWithKey (\m k v -> return $! M.insert k v m) M.empty slm

foldWithKeyM :: MonadIO m => (a -> k -> v -> m a) -> a -> Map k v -> m a
foldWithKeyM fn init (FrzMap slm) = 
   SLM.foldlWithKey (\m k v -> fn m k v) init slm
-- TODO!  Need parallel version!

-- foldWithKeyIO :: (a -> k -> v -> IO a) -> a -> Map k v -> IO a
-- foldWithKeyIO fn init (FrzMap slm) = 
--    SLM.foldlWithKey (\m k v -> fn m k v) init slm



instance (Ord k, Show k, Show a) => Show (Map k a) where
  show mp = show (toStdMap mp)

instance (Ord k, Eq k, Eq a) => Eq (Map k a) where
  m1 == m2 = (toStdMap m1) == (toStdMap m2)

instance (Ord k, Ord a) => Ord (Map k a) where
  compare m1 m2 = compare (toStdMap m1) (toStdMap m2)

-- Note: making these strict for now:
instance F.Foldable (Map k) where
  foldr fn init (FrzMap slm) = unsafeDupablePerformIO$
    SLM.foldlWithKey (\ acc _ v -> return $! fn v acc) init slm
  
-- Traversals
----------------------------------------
-- Here we have a choice.  We could traverse the skip list representation and produce
-- a Data.Map as output, but then we would need to make our frozen Map a sum type.

-- Yet if we want instances of the standard classes, we have to produce a (mutable)
-- Map as output.

traverseFrzn :: (MonadIO m, Ord k) => (a -> m b) -> Map k a -> m (Map k b)
traverseFrzn fn (FrzMap slm) = do
  mp <- LI.liftIO (SLM.newSLMap defaultLevels)
  SLM.foldlWithKey (\() k v -> do
                     putRes <- SLM.putIfAbsentToss mp k (fn v) (LI.liftIO randomIO)
                     case putRes of
                       Added _ -> return ()
                       Found _ -> error "Multiple puts to one entry in a frozen IMap!")
     () slm
  return (FrzMap mp)


{-  
instance Functor (Map k) where
  fmap fn (FrzMap slm) = unsafeDupablePerformIO$ do
    mp <- SLM.map fn slm
    return (FrzMap mp)
-}
  
  {-
instance T.Traversable (Map k) where
-}
