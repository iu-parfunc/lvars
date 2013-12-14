{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}

-- | An I-Structure, also known as an array of IVars, implemented using a boxed vector.

module Data.LVar.IStructure
       (
         IStructure,
         
         -- * Basic operations         
         newIStructure, newIStructureWithCallback,
         put, put_, get, getLength,

         -- * Iteration and callbacks
         forEachHP
         -- forEach,         
       ) where

import Data.Vector as V

import           Control.DeepSeq (NFData)
import           Control.Applicative
import           Data.Maybe (fromJust, isJust)
import qualified Data.LVar.IVar as IV
import           Data.LVar.IVar (IVar(IVar))
import qualified Data.Foldable as F
import           Data.List (intersperse)
-- import qualified Data.Traversable as T

import           Control.LVish as LV hiding (put,put_,get)
import           Control.LVish.DeepFrz.Internal
import           Control.LVish.Internal as LI
import           Control.LVish.Sched (newLV, putLV, getLV, freezeLV,
                                                freezeLVAfter, liftIO)
import           Data.LVar.Generic as G
import           Data.LVar.Generic.Internal (unsafeCoerceLVar)

------------------------------------------------------------------------------

-- | An I-Structure, also known as an array of IVars.
newtype IStructure s a = IStructure (V.Vector (IV.IVar s a))

-- unIStructure (IStructure lv) = lv

instance Eq (IStructure s v) where
  IStructure vec1 == IStructure vec2 = vec1 == vec2

-- | An `IStructure` can be treated as a generic container LVar.  However, the
-- polymorphic operations are less useful than the monomorphic ones exposed by this
-- module (e.g., @forEachHP@ vs. @addHandler@).
instance LVarData1 IStructure where
  freeze orig@(IStructure vec) = WrapPar$ do
    -- No new alloc here, just time:
    V.forM_ vec $ \ (IVar (WrapLVar lv)) -> freezeLV lv 
    return (unsafeCoerceLVar orig)

  -- | We can do better than the default here; this is /O(1)/:    
  sortFrzn = AFoldable
                     
  -- Unlike the IStructure-specific forEach, this takes only values, not indices.
  addHandler mh is fn = forEachHP mh is (\ _k v -> fn v)

-- | The `IStructure`s in this module also have the special property that they
-- support a freeze operation which immediately yields a `Foldable` container
-- without any sorting (see `snapFreeze`).
instance OrderedLVarData1 IStructure where
  -- No extra work here...  
  snapFreeze is = unsafeCoerceLVar <$> G.freeze is

-- As with all LVars, after freezing, map elements can be consumed. In
-- the case of this @IStructure@ implementation, it need only be
-- `Frzn`, not `Trvrsbl`.
instance F.Foldable (IStructure Frzn) where
  foldr fn zer (IStructure vec) = 
    F.foldr (\ iv acc ->
              case IV.fromIVar iv of
                Nothing -> acc
                Just x  -> fn x acc)
             zer vec

-- Of course, the stronger `Trvrsbl` state is still fine for folding.
instance F.Foldable (IStructure Trvrsbl) where
  foldr fn zer mp = F.foldr fn zer (castFrzn mp)

-- @IStructure@ values can be returned as the result of a
-- `runParThenFreeze`.  Hence they need a `DeepFrz` instance.
-- @DeepFrz@ is just a type-coercion.  No bits flipped at runtime.
instance DeepFrz a => DeepFrz (IStructure s a) where
  type FrzType (IStructure s a) = IStructure Frzn (FrzType a)
  frz = unsafeCoerceLVar

instance (Show a) => Show (IStructure Frzn a) where
  show (IStructure vec) =
  -- individual elements are showable, and show returns a string, so
  -- we want to concatenate those.
    "{IStructure: " Prelude.++
    (Prelude.concat $ intersperse ", " $ Prelude.map show $ V.toList vec) Prelude.++
    "}"

-- | For convenience only; the user could define this.
instance Show a => Show (IStructure Trvrsbl a) where
  show = show . castFrzn

------------------------------------------------------------------------------

-- | Retrieve the number of slots in the `IStructure`.
getLength :: IStructure s a -> Par d s Int
getLength (IStructure vec) = return $! V.length vec

-- Physical identity, just as with IORefs.
-- instance Eq (IStructure s v) where
--   IStructure lv1 == IStructure lv2 = state lv1 == state lv2 

-- | Create a new, empty, monotonically growing 'IStructure' of a given size.
--   All entries start off as zero, which must be \"bottom\".
newIStructure :: Int -> Par d s (IStructure s elt)
newIStructure len = fmap IStructure $
                    V.generateM len (\_ -> IV.new)

-- | Register handlers on each internal IVar as it is created.
--   This operation should be more efficient than `newIStructure` followed by `forEachHP`.
newIStructureWithCallback :: Int -> (Int -> elt -> Par d s ()) -> Par d s (IStructure s elt)
newIStructureWithCallback len fn =
  fmap IStructure $
   V.generateM len $ \ix -> do 
      iv <- IV.new
      IV.whenFull Nothing iv (fn ix)
      return iv

-- | /O(N)/ complexity, unfortunately. This implementation of `IStructure`s requires
-- freezing each of the individual IVars stored in the array.
freezeIStructure :: IStructure s a -> LV.Par QuasiDet s (V.Vector (Maybe a))
freezeIStructure (IStructure vec) = do
  v <- V.mapM IV.freezeIVar vec
  return v

{-# INLINE forEachHP #-}
-- | Add an (asynchronous) callback that listens for all new elements added to
-- the `IStructure`, optionally enrolled in a handler pool.
forEachHP :: -- (Eq a) =>
             Maybe HandlerPool           -- ^ pool to enroll in, if any
          -> IStructure s a              -- ^ `IStructure` to listen to
          -> (Int -> a -> Par d s ())    -- ^ callback
          -> Par d s ()
forEachHP hp (IStructure vec) callb =
  -- F.traverse_ (\iv -> IV.addHandler hp iv callb) vec
  for_ (0, V.length vec) $ \ ix ->
    IV.whenFull hp (V.unsafeIndex vec ix) (callb ix)

{-

{-# INLINE forVec #-}
-- | Simple for-each loops over vector elements.
forVec :: Storable a =>
          M.IOVector a -> (Int -> a -> Par d s ()) -> Par d s ()
forVec vec fn = loop 0 
  where
    len = M.length vec
    loop i | i == len = return ()
           | otherwise = do elm <- LI.liftIO$ M.unsafeRead vec i
                            fn i elm
                            loop (i+1)

{-# INLINE forEach #-}
-- | Add an (asynchronous) callback that listens for all new elements added to
-- the set
forEach :: (Num a, Storable a, Eq a) =>
           NatArray s a -> (Int -> a -> Par d s ()) -> Par d s ()
forEach = forEachHP Nothing
-}




{-# INLINE put #-}

-- | Put a single element in the `IStructure` at a given index.  That index must be previously empty.  (WHNF)
-- Strict in the element being put in the set.
put_ :: Eq elt => IStructure s elt -> Int -> elt -> Par d s ()
put_ (IStructure vec) !ix !elm = IV.put_ (vec ! ix) elm

-- | Put a single element in the `IStructure` at a given index.  This variant is deeply strict (`NFData`).
put :: (NFData elt, Eq elt) => IStructure s elt -> Int -> elt -> Par d s ()
put (IStructure vec) !ix !elm = IV.put (vec ! ix) elm

{-# INLINE get #-}
-- | Wait for the indexed entry to contain a value, and return that value.
get :: Eq elt => IStructure s elt -> Int -> Par d s elt
get (IStructure vec) !ix = IV.get (vec ! ix)
