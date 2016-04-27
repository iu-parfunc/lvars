{-# LANGUAGE Unsafe #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE ScopedTypeVariables, ConstraintKinds  #-}

module Data.LVar.PureMap.Unsafe
       (
         -- * Unsafe operations:
         unsafePeekKey,
--         unsafeGetOrInit, unsafeInsertIfAbsent,

         -- * These are here only to reexport downstream:
         IMap(..), forEachHP, fromIMap
       )
       where

import           Control.LVish.DeepFrz.Internal
import           Control.LVish hiding (parIO)
import           Control.LVish.Internal as LI
import           Control.LVish.Internal.SchedIdempotent (freezeLV)
import qualified Control.LVish.Internal.SchedIdempotent as L
import           Data.LVar.Generic as G
import           Data.LVar.Generic.Internal (unsafeCoerceLVar)
import           Data.UtilInternal (traverseWithKey_)

import           Control.Applicative ((<$>))
import           Data.IORef
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import           Data.List (intersperse)
import           System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Control.Par.Class        as PC

import           Data.Par.Map () -- For instances.

------------------------------------------------------------------------------
-- IMaps implemented on top of LVars:
------------------------------------------------------------------------------

-- | The map datatype itself.  Like all other LVars, it has an @s@ parameter (think
--  `STRef`) in addition to the @a@ parameter that describes the type of elements
-- in the set.
--
-- Performance note: There is only /one/ mutable location in this implementation.  Thus
-- it is not a scalable implementation.
newtype IMap k s v = IMap (LVar s (IORef (M.Map k v)) (k,v))

-- | Equality is physical equality, as with @IORef@s.
instance Eq (IMap k s v) where
  IMap lv1 == IMap lv2 = state lv1 == state lv2

-- | An `IMap` can be treated as a generic container LVar.  However, the polymorphic
-- operations are less useful than the monomorphic ones exposed by this module.
instance LVarData1 (IMap k) where
  freeze orig@(IMap (WrapLVar lv)) = WrapPar$ do freezeLV lv; return (unsafeCoerceLVar orig)
  -- Unlike the Map-specific forEach variants, this takes only values, not keys.
  addHandler mh mp fn = forEachHP mh mp (\ _k v -> fn v)
  sortFrzn (IMap lv) = AFoldable$ unsafeDupablePerformIO (readIORef (state lv))

-- | The `IMap`s in this module also have the special property that they support an
-- /O(1)/ freeze operation which immediately yields a `Foldable` container
-- (`snapFreeze`).
instance OrderedLVarData1 (IMap k) where
  snapFreeze is = unsafeCoerceLVar <$> freeze is

-- As with all LVars, after freezing, map elements can be consumed. In
-- the case of this `IMap` implementation, it need only be `Frzn`, not
-- `Trvrsbl`.
instance F.Foldable (IMap k Frzn) where
  foldr fn zer (IMap lv) =
    let set = unsafeDupablePerformIO (readIORef (state lv)) in
    F.foldr fn zer set

-- Of course, the stronger `Trvrsbl` state is still fine for folding.
instance F.Foldable (IMap k Trvrsbl) where
  foldr fn zer mp = F.foldr fn zer (castFrzn mp)

-- `IMap` values can be returned as the result of a
--  `runParThenFreeze`.  Hence they need a `DeepFrz` instance.
--  @DeepFrz@ is just a type-coercion.  No bits flipped at runtime.
instance DeepFrz a => DeepFrz (IMap k s a) where
  type FrzType (IMap k s a) = IMap k Frzn (FrzType a)
  frz = unsafeCoerceLVar

instance (Show k, Show a) => Show (IMap k Frzn a) where
  show (IMap lv) =
    let mp' = unsafeDupablePerformIO (readIORef (state lv)) in
    "{IMap: " ++
    (concat $ intersperse ", " $ map show $
     M.toList mp') ++ "}"

-- | For convenience only; the user could define this.
instance (Show k, Show a) => Show (IMap k Trvrsbl a) where
  show lv = show (castFrzn lv)


-- | Add an (asynchronous) callback that listens for all new key/value pairs added to
-- the map, optionally enrolled in a handler pool.
forEachHP :: Maybe HandlerPool           -- ^ optional pool to enroll in
          -> IMap k s v                  -- ^ Map to listen to
          -> (k -> v -> Par e s ())      -- ^ callback
          -> Par e s ()
forEachHP mh (IMap (WrapLVar lv)) callb = WrapPar $ do
    L.addHandler mh lv globalCB deltaCB
    return ()
  where
    -- FIXME: require idempotence or make sure this does NOT launch
    -- repeated callbacks for the same key:
    deltaCB (k,v) = return$ Just$ unWrapPar $ callb k v
    globalCB ref = do
      mp <- L.liftIO $ readIORef ref -- Snapshot
      unWrapPar $
        traverseWithKey_ (\ k v -> forkHP mh$ callb k v) mp

------------------------------------------------------------------------------


-- | An unsafe, nonblocking version of `getKey`.  This reveals whether
unsafePeekKey :: Ord k => k -> IMap k s v -> Par e s (Maybe v)
unsafePeekKey key (IMap (WrapLVar lv)) = do
    mp <- liftIO$ readIORef (L.state lv)
    return$! M.lookup key mp

-- | A generic initialize proceedure that returns a preexisting value, if it exists,
-- otherwise filling in a new "bottom" value and returning it.
--
-- The boolean return value is @True@ iff a new, fresh entry was created.
unsafeGetOrInit :: forall f a b e s key . (Ord key, LVarWBottom f, LVContents f a, Show key, Ord a) =>
          key -- ^ The key to lookup or populate.
          -> IMap key s (f s a)
          -> Par e s (Bool, f s a)
unsafeGetOrInit key (IMap (WrapLVar lv)) = go1
 where
  -- go1 is OPTIONAL optimization.  Could skip right to go2.
  -- The tension here is that we can't do IO during an atomicModifyIORef.
  go1 = do
    let mpref = (L.state lv)
    mp <- liftIO$ readIORef mpref
    case M.lookup key mp of
      Just x -> return (False,x)
      Nothing -> go2
  go2 = do
           bot <- G.newBottom
           liftIO$ atomicModifyIORef' (L.state lv) $ \ mp ->
             -- Here we pay the cost of a SECOND lookup.  Ouch!
             case M.lookup key mp of
               Nothing -> (M.insert key bot mp,(True,bot))
               -- Oops! it appeared in the meantime.  Our allocation was still wasted:
               Just x  -> (mp,(False,x))

-- FIXME: need a delta-thresh!
--      act <- putLV_ (unWrapLVar lv) putter


{-

-- | An unsafe way to race to insert.  Returns Nothing if the insert is successful,
-- and the found value otherwise.
unsafeInsertIfAbsent :: (HasBump e, Ord k) => k -> v -> IMap k s v -> Par e s (Maybe v)a
unsafeInsertIfAbsent key val (IMap (WrapLVar lv)) = liftIO$
  atomicModifyIORef' (L.state lv) $ \ mp ->
    case M.lookup key mp of
      Nothing -> (M.insert key val mp,Nothing)
      -- Oops! it appeared in the meantime.  Our allocation was still wasted:
      x@(Just _) -> (mp,x)

-- FIXME: need a delta-thresh!
--      act <- putLV_ (unWrapLVar lv) putter
-}

--------------------------------------------------------------------------------

-- | /O(1)/: Convert from an `IMap` to a plain `Data.Map`.
--   This is only permitted when the `IMap` has already been frozen.
--   This is useful for processing the result of `Control.LVish.DeepFrz.runParThenFreeze`.
fromIMap :: IMap k Frzn a -> M.Map k a
fromIMap (IMap lv) = unsafeDupablePerformIO (readIORef (state lv))

instance PC.Generator (IMap k Frzn a) where
  type ElemOf (IMap k Frzn a) = (k,a)
  {-# INLINE fold #-}
  fold fn zer mp = PC.fold fn zer (fromIMap mp)

  {-# INLINE foldMP #-}
  -- | More efficient, not requiring unsafePerformIO or risk of duplication.
  -- foldMP fn zer mp = foldMP fn zer (fromIMap mp)
