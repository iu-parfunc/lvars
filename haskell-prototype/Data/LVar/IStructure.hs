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

-- | An I-Structure, aka an Array of IVars.

module Data.LVar.IStructure
       (
         -- * Basic operations
         IStructure,
         -- Snapshot(IStructSnap),
         
         newIStructure, newIStructureWithCallback,
         put, put_, get,

         -- * Iteration and callbacks
         -- forEach,
         forEachHP
       ) where

import Data.Vector as V

import           Control.DeepSeq (NFData)
import           Control.Monad (void)
import           Data.IORef
import           Data.Maybe (fromMaybe)
import qualified Data.LVar.IVar as IV
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import           Control.LVish as LV hiding (addHandler)
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV,
                                                freezeLVAfter, liftIO)
import qualified Control.LVish.SchedIdempotent as L

------------------------------------------------------------------------------

-- | An I-Structure, aka an Array of IVars.
--   For now this really is a simple vector of IVars.
newtype IStructure s a = IStructure (V.Vector (IV.IVar s a))

unIStructure (IStructure lv) = lv

-- | Physical identity, just as with IORefs.
-- instance Eq (IStructure s v) where
--   IStructure lv1 == IStructure lv2 = state lv1 == state lv2 

instance LVarData1 IStructure where
  newtype Snapshot IStructure a = IStructSnap (V.Vector (Maybe a))
    deriving (Show,Ord,Read,Eq)
  freeze :: IStructure s a -> LV.Par QuasiDet s (Snapshot IStructure a)  
  freeze = unsafeConvert . fmap IStructSnap . freezeIStructure
  -- newBottom :: Par d s (IStructSnap s a)
  -- newBottom = error "makes no sense for an IStructure, need size"
  -- traverseSnap f (IStructSnap m) = fmap IVarSnap $ traverse f m


-- | Create a new, empty, monotonically growing 'IStructure' of a given size.
--   All entries start off as zero, which must be BOTTOM.
newIStructure :: Int -> Par d s (IStructure s elt)
newIStructure len = fmap IStructure $
                    V.generateM len (\_ -> IV.new)

-- | This registers handlers on each internal IVar as it is created.
newIStructureWithCallback :: Int -> (Int -> elt -> Par d s ()) -> Par d s (IStructure s elt)
newIStructureWithCallback len fn =
  fmap IStructure $
   V.generateM len $ \ix -> do 
      iv <- IV.new
      IV.addHandler Nothing iv (fn ix)
      return iv

freezeIStructure :: IStructure s a -> LV.Par QuasiDet s (V.Vector (Maybe a))
freezeIStructure (IStructure vec) =
  V.mapM IV.freezeIVar vec

instance DeepFreeze (IStructure s a) (V.Vector (Maybe a)) where
  type Session (IStructure s a) = s 
  deepFreeze iv = 
      do IStructSnap m <- LV.freeze iv
         return m

{-# INLINE forEachHP #-}
-- | Add an (asynchronous) callback that listens for all new elements added to
-- the IStructure, optionally enrolled in a handler pool
forEachHP :: (Eq a) =>
             Maybe HandlerPool           -- ^ pool to enroll in, if any
          -> IStructure s a              -- ^ IStructure to listen to
          -> (Int -> a -> Par d s ())    -- ^ callback
          -> Par d s ()
forEachHP hp (IStructure vec) callb =
  -- F.traverse_ (\iv -> IV.addHandler hp iv callb) vec
  for_ (0, V.length vec) $ \ ix ->
    IV.addHandler hp (V.unsafeIndex vec ix) (callb ix)

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
-- | Put a single element in the array.  That slot must be previously empty.  (WHNF)
-- Strict in the element being put in the set.
put_ :: Eq elt => IStructure s elt -> Int -> elt -> Par d s ()
put_ (IStructure vec) !ix !elm = IV.put_ (vec ! ix) elm

put :: (NFData elt, Eq elt) => IStructure s elt -> Int -> elt -> Par d s ()
put (IStructure vec) !ix !elm = IV.put (vec ! ix) elm

{-# INLINE get #-}
-- | Wait for the indexed entry to contain a value.
get :: Eq elt => IStructure s elt -> Int -> Par d s elt
get (IStructure vec) !ix = IV.get (vec ! ix)

