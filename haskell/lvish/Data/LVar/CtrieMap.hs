{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}

{-|

This module provides finite maps that only grow. It is based on a
/Ctrie/ implementation of maps.

This module is usually a more efficient alternative to
`Data.LVar.PureMap`, and provides almost the same interface. However,
it's always good to test multiple data structures if you have a
performance-critical use case.

 -}


module Data.LVar.CtrieMap
       (
         -- * The type and its basic operations
         IMap,
         newEmptyMap, newMap, newFromList,
         insert,
         getKey, waitSize, waitValue,
         modify,

         -- * Generic routines and convenient aliases
         gmodify, getOrInit,

         -- * Quasi-deterministic operations
         freezeMap, fromIMap,
         traverseFrzn_,

         -- * Iteration and callbacks
         forEach, forEachHP,
         withCallbacksThenFreeze,

         -- * Higher-level derived operations
         copy, traverseMap, traverseMap_,

         -- * Alternate versions of derived ops that expose @HandlerPool@s they create
         traverseMapHP, traverseMapHP_, unionHP
       ) where

import           Control.Applicative
import           Control.Exception                      (throw)
import           Control.LVish
import           Control.LVish.DeepFrz.Internal
import           Control.LVish.Internal                 as LI
import           Control.Monad
--import           Control.Monad.IO.Class
import qualified Control.Concurrent.Map                 as CM
import qualified Data.Foldable                          as F
--import           Data.IORef                             (readIORef)
import           Data.List                              (intersperse)
import           Data.LVar.Generic                      as G
import           Data.LVar.Generic.Internal             (unsafeCoerceLVar)
import qualified Data.LVar.IVar                         as IV
import qualified Data.Map.Strict                        as M
import           Data.UtilInternal                      (traverseWithKey_)
import           GHC.Prim                               (unsafeCoerce#)
import           Internal.Control.LVish.SchedIdempotent (freezeLV, getLV, newLV,
                                                         putLV, putLV_)
import qualified Internal.Control.LVish.SchedIdempotent as L
import           Prelude
import           System.IO.Unsafe                       (unsafeDupablePerformIO)

--import Debug.Trace

import           Control.LVish.Unsafe     ()
import qualified Control.Par.Class        as PC
import           Control.Par.Class.Unsafe (internalLiftIO)
--import           Data.Par.Splittable      (mkMapReduce, pmapReduceWith_)
--import qualified Data.Splittable.Class    as Sp
import Data.Hashable (Hashable)

------------------------------------------------------------------------------
-- IMaps implemented vis Ctrie
------------------------------------------------------------------------------

-- | The map datatype itself.  Like all other LVars, it has an @s@ parameter (think
--  `STRef`) in addition to the @a@ parameter that describes the type of elements
-- in the set.
--
-- Performance note: this data structure reduces contention between parallel
-- computations inserting into the map, but all /blocking/ computations are not as
-- scalable.  All continuations waiting for not-yet-present elements will currently
-- share a single queue [2013.09.26].
data IMap k s v = (Ord k, Hashable k) => IMap {-# UNPACK #-} !(LVar s (CM.Map k v) (k,v))

-- | Equality is physical equality, as with @IORef@s.
instance Eq (IMap k s v) where
  IMap lv1 == IMap lv2 = state lv1 == state lv2

-- | An `IMap` can be treated as a generic container LVar.  However, the polymorphic
-- operations are less useful than the monomorphic ones exposed by this module.
instance LVarData1 (IMap k) where
  -- | Get the exact contents of the map.  Using this may cause your
  -- program to exhibit a limited form of nondeterminism: it will never
  -- return the wrong answer, but it may include synchronization bugs
  -- that can (nondeterministically) cause exceptions.
  freeze orig@(IMap (WrapLVar lv)) =
    WrapPar$ do freezeLV lv; return (unsafeCoerceLVar orig)

  -- | We can do better than the default here; this is /O(1)/:
  sortFrzn = AFoldable

  -- | This generic version has the disadvantage that it does not observe the KEY,
  -- only the value.
  addHandler mh (IMap (WrapLVar lv)) callb = WrapPar $
    L.addHandler mh lv globalCB (\(_k,v) -> return$ Just$ unWrapPar$ callb v)
    where
      globalCB cm =
        unWrapPar $
          CM.foldlWithKey LI.liftIO
             (\() _k v -> forkHP mh $ callb v) () cm

-- | The `IMap`s in this module also have the special property that they support an
-- /O(1)/ freeze operation which immediately yields a `Foldable` container
-- (`snapFreeze`).
instance OrderedLVarData1 (IMap k) where
  snapFreeze is = unsafeCoerceLVar <$> freeze is

-- `IMap` values can be returned as the result of a
-- `runParThenFreeze`.  Hence they need a `DeepFrz` instance.
-- @DeepFrz@ is just a type-coercion.  No bits flipped at runtime.
instance DeepFrz a => DeepFrz (IMap k s a) where
  type FrzType (IMap k s a) = IMap k Frzn (FrzType a)
  frz = unsafeCoerceLVar

--------------------------------------------------------------------------------

-- | Create a fresh map with nothing in it.
newEmptyMap :: (Ord k, Hashable k) => Par e s (IMap k s v)
newEmptyMap = newEmptyMap_

-- | Create a fresh map with nothing in it, with the given number of skiplist
-- levels.
newEmptyMap_ :: (Ord k, Hashable k) => Par e s (IMap k s v)
newEmptyMap_ = fmap (IMap . WrapLVar) $ WrapPar $ newLV $ CM.empty

-- | Create a new map populated with initial elements.
newMap :: (Ord k, Hashable k) => M.Map k v -> Par e s (IMap k s v)
newMap mp =
 fmap (IMap . WrapLVar) $ WrapPar $ newLV $ do
  cm <- CM.empty
  traverseWithKey_ (\ k v -> do CM.Added _ <- CM.putIfAbsent cm k (return v)
                                return ()
                   ) mp
  return cm

-- | Create a new map drawing initial elements from an existing list.
newFromList :: (Ord k, Hashable k, Eq v) =>
               [(k,v)] -> Par e s (IMap k s v)
newFromList ls = newFromList_ ls

-- | Create a new map drawing initial elements from an existing list, with
-- the given number of skip list levels.
newFromList_ :: (Ord k, Hashable k) => [(k,v)] -> Par e s (IMap k s v)
newFromList_ ls = do
  m@(IMap lv) <- newEmptyMap_
  -- TODO: May want to consider parallelism here for sufficiently large inputs:
  forM_ ls $ \(k,v) -> LI.liftIO $ CM.putIfAbsent (state lv) k $ return v
  return m

-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- value of the provided action.
withCallbacksThenFreeze :: forall k v b s e . (HasPut e, HasGet e, HasFreeze e, Eq b) =>
                           IMap k s v -> (k -> v -> Par e s ()) -> Par e s b -> Par e s b
withCallbacksThenFreeze (IMap lv) callback action = do
  hp  <- newPool
  res <- IV.new
  let deltCB (k,v) = return$ Just$ unWrapPar$ callback k v
      initCB cm = do
        -- The implementation guarantees that all elements will be caught either here,
        -- or by the delta-callback:
        unWrapPar $ do
          CM.foldlWithKey LI.liftIO
            (\() k v -> forkHP (Just hp) $ callback k v) () cm
          x <- action -- Any additional puts here trigger the callback.
          IV.put_ res x
  WrapPar $ L.addHandler (Just hp) (unWrapLVar lv) initCB deltCB

  -- We additionally have to quiesce here because we fork the inital set of
  -- callbacks on their own threads:
  quiesce hp
  IV.get res

-- | Add an (asynchronous) callback that listens for all new key/value pairs
-- added to the map, optionally tied to a handler pool.
forEachHP :: Maybe HandlerPool           -- ^ optional pool to enroll in
          -> IMap k s v                  -- ^ Map to listen to
          -> (k -> v -> Par e s ())      -- ^ callback
          -> Par e s ()
forEachHP mh (IMap (WrapLVar lv)) callb = WrapPar $
    L.addHandler mh lv globalCB
       -- FIXME: this is bad, it schedules REPEAT callbacks on the same key:
       (\(k,v) -> return$ Just$ unWrapPar$ callb k v)
  where
    gcallb k v = do
      logDbgLn 5 " [CtrieMap] callback from global traversal "
      callb k v
    globalCB cm = do
      unWrapPar $ do
        logDbgLn 5 " [CtrieMap] Beginning fold to check for global-work"
        CM.foldlWithKey LI.liftIO (\() k v -> forkHP mh $ gcallb k v) () cm

-- | Add an (asynchronous) callback that listens for all new new key/value pairs added to
-- the map.
forEach :: IMap k s v -> (k -> v -> Par e s ()) -> Par e s ()
forEach = forEachHP Nothing

-- | Put a single entry into the map.  (WHNF) Strict in the key and value.
insert :: (Ord k, Hashable k, Eq v, HasPut e) =>
          k -> v -> IMap k s v -> Par e s ()
insert !key !elm (IMap (WrapLVar lv)) = WrapPar$ putLV lv putter
  where putter cm = do
          putRes <- CM.putIfAbsent cm key $ return elm
          case putRes of
            CM.Added _ -> return $ Just (key, elm)
            CM.Found _ -> throw$ ConflictingPutExn$ "Multiple puts to one entry in an IMap!"

-- | `IMap`s containing other LVars have some additional capabilities compared to
-- those containing regular Haskell data.  In particular, it is possible to modify
-- existing entries (monotonically).  Further, this `modify` function implicitly
-- inserts a \"bottom\" element if there is no existing entry for the key.
--
modify :: forall f a b e s key . (Ord key, Hashable key, Show key, Ord a, HasPut e) =>
          IMap key s (f s a)
          -> key                  -- ^ The key to lookup.
          -> (Par e s (f s a))    -- ^ Create a new \"bottom\" element whenever an entry is not present.
          -> (f s a -> Par e s b) -- ^ The computation to apply on the right-hand side of the keyed entry.
          -> Par e s b
modify (IMap (WrapLVar lv)) key newBottom fn = do
    act <- WrapPar $ putLV_ lv putter
    act
  where putter cm = do
          putRes <- unWrapPar $ CM.putIfAbsent cm key newBottom
          case putRes of
            CM.Added v -> return (Just (key,v), fn v)
            CM.Found v -> return (Nothing,      fn v)

{-# INLINE gmodify #-}
-- | A generic version of `modify` that does not require a `newBottom` argument,
-- rather, it uses the generic version of that function.
gmodify :: forall f a b e s key . (Ord key, Hashable key, LVarData1 f,
                                   LVarWBottom f, LVContents f a, Show key, Ord a, HasPut e) =>
          IMap key s (f s a)
          -> key                  -- ^ The key to lookup.
          -> (f s a -> Par e s b) -- ^ The computation to apply on the right-hand side of the keyed entry.
          -> Par e s b
gmodify map key fn = modify map key G.newBottom fn


{-# INLINE getOrInit #-}
-- | Return the preexisting value for a key if it exists, and otherwise return
--
--   This is a convenience routine that can easily be defined in terms of `gmodify`
getOrInit :: forall f a b e s key . (Ord key, Hashable key, LVarData1 f, LVarWBottom f,
                                     LVContents f a, Show key, Ord a, HasPut e) =>
          key -> IMap key s (f s a) -> Par e s (f s a)
getOrInit key mp = gmodify mp key return


-- | Wait for the map to contain a specified key, and return the associated value.
getKey :: (HasGet e, Ord k) => k -> IMap k s v -> Par e s v
getKey !key (IMap (WrapLVar lv)) = WrapPar$ getLV lv globalThresh deltaThresh
  where
    globalThresh cm _frzn = CM.lookup key cm
    deltaThresh (k,v) | k == key  = return $ Just v
                      | otherwise = return Nothing

-- | Wait until the map contains a certain value (on any key).
waitValue :: (HasGet e, Ord k, Hashable k, Eq v) => v -> IMap k s v -> Par e s ()
waitValue !val (IMap (WrapLVar lv)) = WrapPar$ getLV lv globalThresh deltaThresh
  where
    deltaThresh (_,v) | v == val  = return$ Just ()
                      | otherwise = return Nothing
    globalThresh ref _frzn = do
      let cm = L.state lv
      let fn Nothing _k v | v == val  = return $! Just ()
                          | otherwise = return $ Nothing
          fn just _ _  = return $! just
      -- This is inefficient.
      -- FIXME: no short-circuit for this fold:
      CM.foldlWithKey id fn Nothing cm

-- | Wait on the SIZE of the map, not its contents.
waitSize :: HasGet e => Int -> IMap k s v -> Par e s ()
waitSize !sz (IMap (WrapLVar lv)) = WrapPar $
    getLV lv globalThresh deltaThresh
  where
    globalThresh cm _ = do
      snapSize <- CM.foldlWithKey id (\n _ _ -> return $ n+1) 0 cm
      case snapSize >= sz of
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
-- This is an /O(1)/ operation that doesn't copy the in-memory representation of the
-- IMap.
freezeMap :: (HasFreeze e, Ord k, Hashable k) => IMap k s v -> Par e s (IMap k Frzn v)
-- freezeMap (IMap (WrapLVar lv)) = return (IMap (WrapLVar lv))
-- OR we can just do this:

freezeMap x@(IMap (WrapLVar lv)) = WrapPar $ do
  freezeLV lv
  -- For the final deepFreeze at the end of a runpar we can actually skip
  -- the freezeLV part....
  return (unsafeCoerce# x)


-- | /O(N)/: Convert from an `IMap` to a plain `Data.Map`.  This is only permitted
--   when the `IMap` has already been frozen.  This is useful for processing the
--   result of `Control.LVish.DeepFrz.runParThenFreeze`, using standard library
--   functions.
fromIMap :: IMap k Frzn a -> M.Map k a
fromIMap (IMap (WrapLVar lv)) = unsafeDupablePerformIO $
  CM.foldlWithKey id
                  (\ acc k v -> return $! M.insert k v acc)
                  M.empty (L.state lv)

-- | Traverse a frozen map for side effect.  This is useful (in comparison with more
-- generic operations) because the function passed in may see the key as well as the
-- value.
traverseFrzn_ :: (Ord k, Hashable k) =>
                 (k -> a -> Par e s ()) -> IMap k Frzn a -> Par e s ()
traverseFrzn_ fn (IMap (WrapLVar lv)) =
  CM.foldlWithKey LI.liftIO
                  (\ () k v -> fn k v)
                  () (L.state lv)

--------------------------------------------------------------------------------
-- Higher level routines that could (mostly) be defined using the above interface.
--------------------------------------------------------------------------------

-- | Establish a monotonic map between the input and output map  Produce a new result
-- based on each element, while leaving the keys the same.
traverseMap :: (Ord k, Hashable k, Eq b, HasPut e) =>
               (k -> a -> Par e s b) -> IMap k s a -> Par e s (IMap k s b)
traverseMap f s = traverseMapHP Nothing f s

-- | An imperative-style, in-place version of 'traverseMap' that takes the output map
-- as an argument.
traverseMap_ :: (Ord k, Hashable k, Eq b, HasPut e) =>
                (k -> a -> Par e s b) -> IMap k s a -> IMap k s b -> Par e s ()
traverseMap_ f s o = traverseMapHP_ Nothing f s o

--------------------------------------------------------------------------------
-- Alternate versions of functions that EXPOSE the HandlerPools
--------------------------------------------------------------------------------

-- | Return a fresh map which will contain strictly more elements than the input.
-- That is, things put in the former go in the latter, but not vice versa.
copy :: (Ord k, Hashable k, Eq v, HasPut e) => IMap k s v -> Par e s (IMap k s v)
copy = traverseMap (\ _ x -> return x)

-- | Variant of `traverseMap` that optionally ties the handlers to a pool.
traverseMapHP :: (Ord k, Hashable k, Eq b, HasPut e) =>
                 Maybe HandlerPool -> (k -> a -> Par e s b) -> IMap k s a ->
                 Par e s (IMap k s b)
traverseMapHP mh fn set = do
  os <- newEmptyMap
  traverseMapHP_ mh fn set os
  return os

-- | Variant of `traverseMap_` that optionally ties the handlers to a pool.
traverseMapHP_ :: (Ord k, Hashable k, Eq b, HasPut e) =>
                  Maybe HandlerPool -> (k -> a -> Par e s b) -> IMap k s a -> IMap k s b ->
                  Par e s ()
traverseMapHP_ mh fn set os = do
  forEachHP mh set $ \ k x -> do
    x' <- fn k x
    insert k x' os

-- | Return a new map which will (ultimately) contain everything in either input
--   map.  Conflicting entries will result in a multiple put exception.
--   Optionally ties the handlers to a pool.
unionHP :: (Ord k, Hashable k, Eq a, HasPut e) => Maybe HandlerPool ->
           IMap k s a -> IMap k s a -> Par e s (IMap k s a)
unionHP mh m1 m2 = do
  os <- newEmptyMap
  forEachHP mh m1 (\ k v -> insert k v os)
  forEachHP mh m2 (\ k v -> insert k v os)
  return os

--------------------------------------------------------------------------------
-- Operations on frozen Maps
--------------------------------------------------------------------------------

-- As with all LVars, after freezing, map elements can be consumed. In
-- the case of this `IMap` implementation, it need only be `Frzn`, not
-- `Trvrsbl`.
instance F.Foldable (IMap k Frzn) where
  -- Note: making these strict for now:
  foldr fn zer (IMap (WrapLVar lv)) =
    -- TODO: this isn't a fold RIGHT, it's a fold left.  Need to fix that:
    unsafeDupablePerformIO $
    CM.foldlWithKey id (\ a _k v -> return (fn v a))
                    zer (L.state lv)

-- Of course, the stronger `Trvrsbl` state is still fine for folding.
instance F.Foldable (IMap k Trvrsbl) where
  foldr fn zer mp = F.foldr fn zer (castFrzn mp)

instance PC.Generator (IMap k Frzn a) where
  type ElemOf (IMap k Frzn a) = (k,a)
  {-# INLINE fold #-}
  fold fn zer (IMap (WrapLVar lv)) =
    unsafeDupablePerformIO $
    CM.foldlWithKey id (\ a k v -> return $! fn a (k,v))
                    zer (L.state lv)

  {-# INLINE foldMP #-}
  -- | More efficient, not requiring unsafePerformIO or risk of duplication.
  foldMP fn zer (IMap (WrapLVar lv)) =
    CM.foldlWithKey internalLiftIO (\ a k v -> fn a (k,v))
                    zer (L.state lv)

instance Show k => PC.ParFoldable (IMap k Frzn a) where
  {-# INLINE pmapFold #-}
  -- Can't split directly but can slice and then split:
  pmapFold mfn rfn initAcc (IMap lv) = do
    let cm = state lv
        doElem k v = undefined
        doSplit = undefined
    internalLiftIO $ putStrLn$  "[DBG] pmapFold on frzn IMap..."
    internalLiftIO $ CM.unsafeTreeTraverse' cm doElem doSplit initAcc

instance (Show k, Show a) => Show (IMap k Frzn a) where
  show (IMap (WrapLVar lv)) =
    "{IMap: " ++
     (concat $ intersperse ", " $
      unsafeDupablePerformIO $
       CM.foldlWithKey id (\ acc k v -> return$ show (k, v) : acc)
                       [] (L.state lv)
     ) ++ "}"

-- | For convenience only; the user could define this.
instance (Show k, Show a) => Show (IMap k Trvrsbl a) where
  show lv = show (castFrzn lv)

--------------------------------------------------------------------------------
