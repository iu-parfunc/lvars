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
{-# LANGUAGE CPP #-}

{-|

  Saturating maps.  These store pure (joinable) values, but when a
  join fails the map fails (saturates), after which it requires only a
  small, constant amount of memory.

-- FIXME: we need effect signatures on all methods! - RRN [2014.10.21]

 -}

module Data.LVar.SatMap
       (
         -- * Basic operations
         SatMap(..), 
         newEmptyMap, newMap, newFromList,
         insert, 

         -- * A new class for join that can fail
         PartialJoinSemiLattice(..),

-- NOT sure about how handlers might work for saturating LVars yet...
-- It's possible that they can operate only with idempotent callbacks
-- and be nondeterministic in whether they guarantee duplicate-suppression.

         -- * Iteration and callbacks
         -- forEach, forEachHP,
         -- withCallbacksThenFreeze,

         -- * Quasi-deterministic operations
         freezeMap, fromIMap,
         -- traverseFrzn_,

         -- * Higher-level derived operations
         -- copy, traverseMap, union,
         
         -- * Alternate versions of derived ops that expose @HandlerPool@s they create
         -- traverseMapHP, traverseMapHP_, unionHP                                        
       ) where

-- import           Algebra.Lattice
-- import           Algebra.Lattice.Dropped
import           Control.LVish.DeepFrz.Internal
import           Control.LVish.DeepFrz (runParThenFreeze)
import           Control.LVish
import           Control.LVish.Internal as LI
import           Internal.Control.LVish.SchedIdempotent (newLV, putLV, putLV_, getLV, freezeLV, freezeLVAfter)
import qualified Internal.Control.LVish.SchedIdempotent as L
import qualified Data.LVar.IVar as IV
import           Data.LVar.Generic as G
-- import           Data.LVar.SatMap.Unsafe
import           Data.UtilInternal (traverseWithKey_)

import           Control.Exception (throw)
import           Data.List (intersperse, intercalate)
import           Data.IORef
import qualified Data.Map.Strict as M
import           System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import           System.Mem.StableName (makeStableName, hashStableName)

import           Control.Applicative ((<$>))
import qualified Data.Foldable as F
import           Data.LVar.Generic.Internal (unsafeCoerceLVar)

-- From here we get a Generator and, in the future, ParFoldable instance for Map:
import Data.Par.Map ()

import qualified Control.Par.Class as PC
import Control.Par.Class.Unsafe (internalLiftIO)
-- import qualified Data.Splittable.Class as Sp
-- import Data.Par.Splittable (pmapReduceWith_, mkMapReduce)

-- | A partial version of "Algebra.Lattice.JoinSemiLattice", this
-- could be made into a complete lattice by the addition of a top
-- element.
class PartialJoinSemiLattice a where
  joinMaybe :: a -> a -> Maybe a

-- -- | Adding a top element makes the partial join total:
-- instance PartialJoinSemiLattice a => JoinSemiLattice (Dropped a) where
--   join a b =
--     case joinMaybe a b of
--       Nothing -> Top
--       Just x  -> Drop x

------------------------------------------------------------------------------
-- DUPLICATED from PureMap:
------------------------------------------------------------------------------

-- | The map datatype itself.  Like all other LVars, it has an @s@ parameter (think
--  `STRef`) in addition to the @a@ parameter that describes the type of elements
-- in the set.
-- 
-- Performance note: There is only /one/ mutable location in this implementation.  Thus
-- it is not a scalable implementation.
newtype SatMap k s v = SatMap (LVar s (IORef (SatMapContents k v)) (k,v))
     -- SatMap { lvar  :: (LVar s (IORef (Maybe (M.Map k v))) (k,v)) 
     --        , onSat :: L.Par () }

type SatMapContents k v = Maybe (M.Map k v, OnSat)

-- | Callback to execute when saturating occurs.
type OnSat = L.Par ()

-- | Equality is physical equality, as with @IORef@s.
instance Eq (SatMap k s v) where
  SatMap lv1 == SatMap lv2 = state lv1 == state lv2 

-- | An `SatMap` can be treated as a generic container LVar.  However, the polymorphic
-- operations are less useful than the monomorphic ones exposed by this module.
instance LVarData1 (SatMap k) where
  freeze orig@(SatMap (WrapLVar lv)) = WrapPar$ do freezeLV lv; return (unsafeCoerceLVar orig)
  -- Unlike the Map-specific forEach variants, this takes only values, not keys.
  addHandler mh mp fn = forEachHP mh mp (\ _k v -> fn v)
  sortFrzn (SatMap lv) = 
    case unsafeDupablePerformIO (readIORef (state lv)) of 
      Nothing -> AFoldable [] -- Map saturated, contents are empty.
      Just (m,_) -> AFoldable m 

-- | The `SatMap`s in this module also have the special property that they support an
-- /O(1)/ freeze operation which immediately yields a `Foldable` container
-- (`snapFreeze`).
instance OrderedLVarData1 (SatMap k) where
  snapFreeze is = unsafeCoerceLVar <$> freeze is

-- | As with all LVars, after freezing, map elements can be consumed. In
-- the case of this `SatMap` implementation, it need only be `Frzn`, not
-- `Trvrsbl`.
instance F.Foldable (SatMap k Frzn) where
  foldr fn zer (SatMap lv) =
    let mp = unsafeDupablePerformIO (readIORef (state lv)) in
    case mp of 
      Nothing -> zer 
      Just (m,_) -> F.foldr fn zer m

-- Of course, the stronger `Trvrsbl` state is still fine for folding.
instance F.Foldable (SatMap k Trvrsbl) where
  foldr fn zer mp = F.foldr fn zer (castFrzn mp)

-- `SatMap` values can be returned as the result of a
--  `runParThenFreeze`.  Hence they need a `DeepFrz` instance.
--  @DeepFrz@ is just a type-coercion.  No bits flipped at runtime.
instance DeepFrz a => DeepFrz (SatMap k s a) where
--   type FrzType (SatMap k s a) = SatMap k Frzn (FrzType a)
  type FrzType (SatMap k s a) = SatMap k Frzn a -- No need to recur deeper.
  frz = unsafeCoerceLVar

instance (Show k, Show a) => Show (SatMap k Frzn a) where
  show (SatMap lv) =
    let mp' = unsafeDupablePerformIO (readIORef (state lv)) 
        contents = case mp' of 
                     Nothing -> "saturated"
                     Just (m,_) -> intercalate ", " $ map show $ M.toList m
    in "{SatMap: " ++ contents ++ "}"

-- | For convenience only; the user could define this.
instance (Show k, Show a) => Show (SatMap k Trvrsbl a) where
  show lv = show (castFrzn lv)


-- | Add an (asynchronous) callback that listens for all new key/value pairs added to
-- the map, optionally enrolled in a handler pool.
forEachHP :: Maybe HandlerPool           -- ^ optional pool to enroll in 
          -> SatMap k s v                  -- ^ Map to listen to
          -> (k -> v -> Par d s ())      -- ^ callback
          -> Par d s ()
forEachHP mh (SatMap (WrapLVar lv)) callb = WrapPar $ do
    L.addHandler mh lv globalCB deltaCB
    return ()
  where
    deltaCB (k,v) = return$ Just$ unWrapPar $ callb k v
    globalCB ref = do
      mp <- L.liftIO $ readIORef ref -- Snapshot
      case mp of 
        Nothing -> return () -- Already saturated, nothing to do.
        Just (m,_) -> unWrapPar $ 
                      traverseWithKey_ (\ k v -> forkHP mh$ callb k v) m

--------------------------------------------------------------------------------

-- | Create a fresh map with nothing in it.
newEmptyMap :: Par d s (SatMap k s v)
newEmptyMap = WrapPar$ fmap (SatMap . WrapLVar) $ newLV$ newIORef (Just (M.empty, return ()))

-- | Create a new map populated with initial elements.
newMap :: M.Map k v -> Par d s (SatMap k s v)
newMap m = WrapPar$ fmap (SatMap . WrapLVar) $ newLV$ newIORef (Just (m,return()))

-- | A convenience function that is equivalent to calling `Data.Map.fromList`
-- followed by `newMap`.
newFromList :: (Ord k, Eq v) =>
               [(k,v)] -> Par d s (SatMap k s v)
newFromList = newMap . M.fromList

-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- value of the provided action.
withCallbacksThenFreeze :: forall k v b s e . (HasPut e, HasGet e, HasFreeze e, Eq b) =>
                           SatMap k s v -> (k -> v -> Par e s ()) -> Par e s b -> Par e s b
withCallbacksThenFreeze (SatMap (WrapLVar lv)) callback action =
    do hp  <- newPool 
       res <- IV.new 
       WrapPar$ freezeLVAfter lv (initCB hp res) deltaCB
       -- We additionally have to quiesce here because we fork the inital set of
       -- callbacks on their own threads:
       quiesce hp
       IV.get res
  where
    deltaCB (k,v) = return$ Just$ unWrapPar $ callback k v
    initCB :: HandlerPool -> IV.IVar s b -> IORef (SatMapContents k v) -> L.Par ()
    initCB hp resIV ref = do
      -- The implementation guarantees that all elements will be caught either here,
      -- or by the delta-callback:
      mp <- L.liftIO $ readIORef ref -- Snapshot
      case mp of 
        Nothing -> return () -- Already saturated, nothing to do.
        Just (m,_) -> unWrapPar $ do 
                    traverseWithKey_ (\ k v -> forkHP (Just hp)$ callback k v) m
                    res <- action -- Any additional puts here trigger the callback.
                    IV.put_ resIV res
        
-- | Add an (asynchronous) callback that listens for all new new key/value pairs added to
-- the map.
forEach :: SatMap k s v -> (k -> v -> Par d s ()) -> Par d s ()
forEach = forEachHP Nothing 

-- | Put a single entry into the map.  Strict (WHNF) in the key and value.
-- 
--   If a key is inserted multiple times, the values had
--   better be compatible @joinMaybe@, or the map becomes "Saturated".
insert :: (Ord k, PartialJoinSemiLattice v, Eq v) =>
          k -> v -> SatMap k s v -> Par d s () 
insert !key !elm (SatMap (WrapLVar lv)) = WrapPar$ do 
    -- OPTIONAL: Take the fast path if it is saturated:
    snap <- L.liftIO (readIORef (L.state lv))
    case snap of 
      Nothing -> return () -- Fizzle.
      Just _  -> putLV_ lv putter
  where -- putter :: _ -> L.Par (Maybe d,b)
        putter ref = do 
          -- TODO: try optimistic CAS version.
          (x,act) <- L.liftIO$ atomicModifyIORef' ref update 
          case act of 
            Nothing -> return ()
            Just a  -> do L.logStrLn 5 $ " [SatMap] insert saturated lvar "++lvid++", running callback."
                          a 
                          L.logStrLn 5 $ " [SatMap] lvar "++lvid++", saturation callback completed."
          return (x,())

        lvid = L.lvarDbgName lv

        delt x = (x,Nothing)
        update Nothing = (Nothing,delt Nothing) -- Ignored on saturated LVar.
        update orig@(Just (mp,onsat)) =
          case M.lookup key mp of  -- A bit painful... double lookup on normal inserts.
            Nothing -> (Just (M.insert key elm mp, onsat),
                        delt (Just (key,elm)))
            Just oldVal -> 
              case joinMaybe elm oldVal of 
                Just newVal -> if newVal == oldVal 
                               then (orig, delt Nothing) -- No change
                               else (Just (M.insert key newVal mp, onsat), 
                                     delt (Just (key,newVal)))
                Nothing -> -- Here we SATURATE!
                           (Nothing, (Nothing, Just onsat))

-- | Register a callback that is only called if the SatMap LVar
-- becomes /saturated/.
whenSat :: SatMap k s v -> Par d s () -> Par d s ()
whenSat (SatMap lv) (WrapPar newact) = WrapPar $ do 
  L.logStrLn 5 " [SatMap] whenSat issuing atomicModifyIORef on state"
  x <- L.liftIO $ atomicModifyIORef' (state lv) fn
  case x of 
    Nothing -> L.logStrLn 5 " [SatMap] whenSat: not yet saturated, registered only."
    Just a  -> do L.logStrLn 5 " [SatMap] whenSat invoking saturation callback..."
                  a 
 where
  fn :: SatMapContents k v -> (SatMapContents k v, Maybe (L.Par ()))
  -- In this case we register newact to execute later:
  fn (Just (mp,onsat)) = let onsat' = onsat >> newact in
                         (Just (mp,onsat'), Nothing)
  -- In this case we execute newact right away:
  fn Nothing = (Nothing, Just newact)

-- | Drive the variable to top.  This is equivalent to an insert of a
-- conflicting binding.
saturate :: SatMap k s v -> Par d s ()
saturate (SatMap lv) = WrapPar $ do
   let lvid = L.lvarDbgName $ unWrapLVar lv
   L.logStrLn 5 $ " [SatMap] saturate: explicity saturating lvar "++lvid
   act <- L.liftIO $ atomicModifyIORef' (state lv) fn
   case act of 
     Nothing -> L.logStrLn 5 $" [SatMap] saturate: done saturating lvar "++lvid++", no callbacks to invoke."
     Just a  -> do L.logStrLn 5 $" [SatMap] saturate: done saturating lvar "++lvid++".  Now invoking callback."
                   a
 where
  fn (Just (mp,onsat)) = (Nothing, Just onsat)
  fn Nothing           = (Nothing,Nothing)


{-

modify :: forall f a b d s key . (Ord key, Show key, Ord a) =>
          SatMap key s (f s a)
          -> key                  -- ^ The key to lookup.
          -> (Par d s (f s a))    -- ^ Create a new \"bottom\" element whenever an entry is not present.
          -> (f s a -> Par d s b) -- ^ The computation to apply on the right-hand side of the keyed entry.
          -> Par d s b

gmodify :: forall f a b d s key . (Ord key, LVarWBottom f, LVContents f a, Show key, Ord a) =>
          SatMap key s (f s a)
          -> key                  -- ^ The key to lookup.
          -> (f s a -> Par d s b) -- ^ The computation to apply on the right-hand side of the keyed entry.
          -> Par d s b

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
freezeMap :: HasFreeze e => SatMap k s v -> Par e s (M.Map k v)
freezeMap (SatMap (WrapLVar lv)) = WrapPar $
   do freezeLV lv
      getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing
-}

traverseMap = error "UNFINISHED"
traverseMap_ = error "UNFINISHED"
traverseMapHP = error "UNFINISHED"
traverseMapHP_ = error "UNFINISHED"
traverseFrzn_ = error "UNFINISHED"
freezeMap = error "UNFINISHED"
union = error "UNFINISHED"
unionHP = error "UNFINISHED"
copy = error "UNFINISHED"

-- | /O(1)/: Convert from an `SatMap` to a plain `Data.Map`.
--   This is only permitted when the `SatMap` has already been frozen.
--   This is useful for processing the result of `Control.LVish.DeepFrz.runParThenFreeze`.    
fromIMap :: SatMap k Frzn a -> Maybe (M.Map k a)
fromIMap (SatMap lv) = unsafeDupablePerformIO $ do 
  x <- readIORef (state lv)
  case x of 
    Just (m,_) -> return (Just m)
    Nothing    -> return Nothing

{-

-- | Traverse a frozen map for side effect.  This is useful (in comparison with more
-- generic operations) because the function passed in may see the key as well as the
-- value.
traverseFrzn_ :: (Ord k) =>
                 (k -> a -> Par d s ()) -> SatMap k Frzn a -> Par d s ()
traverseFrzn_ fn mp =
 traverseWithKey_ fn (fromIMap mp)

--------------------------------------------------------------------------------
-- Higher level routines that could (mostly) be defined using the above interface.
--------------------------------------------------------------------------------

-- | Establish a monotonic map between the input and output sets.
-- Produce a new result based on each element, while leaving the keys
-- the same.
traverseMap :: (Ord k, Eq b) =>
               (k -> a -> Par d s b) -> SatMap k s a -> Par d s (SatMap k s b)
traverseMap f s = traverseMapHP Nothing f s

-- | An imperative-style, in-place version of 'traverseMap' that takes the output set
-- as an argument.
traverseMap_ :: (Ord k, Eq b) =>
                (k -> a -> Par d s b) -> SatMap k s a -> SatMap k s b -> Par d s ()
traverseMap_ f s o = traverseMapHP_ Nothing f s o

-- | Return a new map which will (ultimately) contain everything in either input
-- map.  Conflicting entries will result in a multiple put exception.
union :: (Ord k, Eq a) => SatMap k s a -> SatMap k s a -> Par d s (SatMap k s a)
union = unionHP Nothing

-- TODO: Intersection

--------------------------------------------------------------------------------
-- Alternate versions of functions that EXPOSE the HandlerPools
--------------------------------------------------------------------------------

-- | Return a fresh map which will contain strictly more elements than the input.
-- That is, things put in the former go in the latter, but not vice versa.
copy :: (Ord k, Eq v) => SatMap k s v -> Par d s (SatMap k s v)
copy = traverseMap (\ _ x -> return x)

-- | A variant of `traverseMap` that optionally ties the handlers to a pool.
traverseMapHP :: (Ord k, Eq b) =>
                 Maybe HandlerPool -> (k -> a -> Par d s b) -> SatMap k s a ->
                 Par d s (SatMap k s b)
traverseMapHP mh fn set = do
  os <- newEmptyMap
  traverseMapHP_ mh fn set os  
  return os

-- | A variant of `traverseMap_` that optionally ties the handlers to a pool.
traverseMapHP_ :: (Ord k, Eq b) =>
                  Maybe HandlerPool -> (k -> a -> Par d s b) -> SatMap k s a -> SatMap k s b ->
                  Par d s ()
traverseMapHP_ mh fn set os = do
  forEachHP mh set $ \ k x -> do 
    x' <- fn k x
    insert k x' os

-- | A variant of `union` that optionally ties the handlers in the
-- resulting set to the same handler pool as those in the two input
-- sets.
unionHP :: (Ord k, Eq a) => Maybe HandlerPool ->
           SatMap k s a -> SatMap k s a -> Par d s (SatMap k s a)
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

--------------------------------------------------------------------------------
-- Interfaces for generic programming with containers:


instance PC.Generator (SatMap k Frzn a) where
  type ElemOf (SatMap k Frzn a) = (k,a)
  {-# INLINE fold #-}
  {-# INLINE foldM #-}    
  {-# INLINE foldMP #-}  
  fold   fn zer (SatMap (WrapLVar lv)) = PC.fold   fn zer $ unsafeDupablePerformIO $ readIORef $ L.state lv
  foldM  fn zer (SatMap (WrapLVar lv)) = PC.foldM  fn zer $ unsafeDupablePerformIO $ readIORef $ L.state lv
  foldMP fn zer (SatMap (WrapLVar lv)) = PC.foldMP fn zer $ unsafeDupablePerformIO $ readIORef $ L.state lv

-- TODO: Once containers 0.5.3.2+ is broadly available we can have a real parFoldable
-- instance.  
-- instance Show k => PC.ParFoldable (SatMap k Frzn a) where


-}

----------------------------------------
-- Simple tests
----------------------------------------

t0 :: SatMap String Frzn Int
t0 = runParThenFreeze $ do 
  m <- newEmptyMap
  insert "hi" (32::Int) m
  insert "hi" (34::Int) m
  insert "there" (1::Int) m
  return m

t1 :: SatMap String Frzn Int
t1 = runParThenFreeze $ do 
  m <- newEmptyMap
  insert "hi" (32::Int) m
  insert "hi" (33::Int) m
  return m

instance PartialJoinSemiLattice Int where
  joinMaybe a b 
    | even a && even b = Just (max a b)
    | odd  a && odd  b = Just (max a b)
    | otherwise        = Nothing
