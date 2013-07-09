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

-- | An I-structure (array) of positive numbers.
--
-- Experimental.

module Data.LVar.NatArray
       (
         -- * Basic operations
         NatArray,
         -- Snapshot(NatArraySnap),
         
         newEmptyNatArray, put, get,

         -- -- * Iteration and callbacks
         forEach, forEachHP

         -- -- * Quasi-deterministic operations
         -- freezeSetAfter, withCallbacksThenFreeze, freezeSet,

         -- -- * Higher-level derived operations
         -- copy, traverseSet, traverseSet_, union, intersection,
         -- cartesianProd, cartesianProds, 

         -- -- * Alternate versions of derived ops that expose HandlerPools they create.
         -- forEachHP, traverseSetHP, traverseSetHP_,
         -- cartesianProdHP, cartesianProdsHP
       ) where

-- import qualified Data.Vector.Unboxed as U
-- import qualified Data.Vector.Unboxed.Mutable as M

import qualified Data.Vector.Storable as U
import qualified Data.Vector.Storable.Mutable as M
import Foreign.Marshal.MissingAlloc (callocBytes)
import Foreign.Marshal.Alloc (finalizerFree)
import Foreign.Storable (sizeOf, Storable)
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import qualified Foreign.Ptr as P
import qualified Data.Bits.Atomic as B
import Data.Bits ((.&.))

import           Control.Monad (void)
import           Data.IORef
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.LVar.IVar as IV
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import           Control.LVish as LV hiding (addHandler)
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV,
                                                freezeLVAfter, liftIO)
import qualified Control.LVish.SchedIdempotent as L

------------------------------------------------------------------------------
-- Toggles

#define USE_CALLOC
-- A low-level optimization below.

------------------------------------------------------------------------------

-- | An array of bit-fields with a monotonic OR operation.  This can be used to model
--   a set of Ints by setting the vector entries to zero or one, but it can also
--   model other finite lattices for each index.
newtype NatArray s a = NatArray (LVar s (M.IOVector a) (Int,a))

unNatArray (NatArray lv) = lv

-- | Physical identity, just as with IORefs.
-- instance Eq (NatArray s v) where
--   NatArray lv1 == NatArray lv2 = state lv1 == state lv2 

{-

instance LVarData1 NatArray where
  newtype Snapshot NatArray a = ISetSnap (S.Set a)
      deriving (Show,Ord,Read,Eq)
  freeze    = fmap ISetSnap . freezeSet
  newBottom = newEmptySet

  -- TODO: traverseSnap

-}


-- | Create a new, empty, monotonically growing 'NatArray' of a given size.
--   All entries start off as zero, which must be BOTTOM.
newEmptyNatArray :: forall elt d s . (Storable elt, Num elt) =>
                     Int -> Par d s (NatArray s elt)
newEmptyNatArray len = WrapPar $ fmap (NatArray . WrapLVar) $ newLV $ do
#ifdef USE_CALLOC
  let bytes = sizeOf (undefined::elt) * len
  mem <- callocBytes bytes
  fp <- newForeignPtr finalizerFree mem
  return $! M.unsafeFromForeignPtr0 fp len
#else
  M.replicate len 0
#endif

--------------------------------------------------------------------------------

{-# INLINE forEachHP #-}
-- | Add an (asynchronous) callback that listens for all new elements added to
-- the array, optionally enrolled in a handler pool.
forEachHP :: (Storable a, Eq a, Num a) =>
             Maybe HandlerPool           -- ^ pool to enroll in, if any
          -> NatArray s a                -- ^ Set to listen to
          -> (Int -> a -> Par d s ())           -- ^ callback
          -> Par d s ()
forEachHP hp (NatArray (WrapLVar lv)) callb = WrapPar $ do
    L.addHandler hp lv globalCB deltaCB
    return ()
  where
    deltaCB (ix,x) = return$ Just$ unWrapPar$ callb ix x
    globalCB vec = return$ Just$ unWrapPar$
      -- FIXME / TODO: need a better (parallel) for loop:
      forVec vec $ \ ix elm ->
        -- FIXME: When it starts off, it is SPARSE... there must be a good way to
        -- avoid testing each position for zero.
        if elm == 0
        then return ()                
        else forkHP hp $ callb ix elm

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


{-# INLINE put #-}
-- | Put a single element in the array.  That slot must be previously empty.  (WHNF)
-- Strict in the element being put in the set.
put :: forall s d elt . (Storable elt, B.AtomicBits elt, Num elt, Show elt) =>
       NatArray s elt -> Int -> elt -> Par d s ()
put _ !ix 0 = error$ "NatArray: violation!  Attempt to put zero to index: "++show ix
put (NatArray (WrapLVar lv)) !ix !elm = WrapPar$ putLV lv (putter ix)
  where putter ix vec@(M.MVector offset fptr) =
          withForeignPtr fptr $ \ ptr -> do 
            let offset = sizeOf (undefined::elt) * ix
            -- ARG, if it weren't for the idempotency requirement we could use fetchAndAdd here:
            -- orig <- B.fetchAndAdd (P.plusPtr ptr offset) elm                          
            orig <- B.compareAndSwap (P.plusPtr ptr offset) 0 elm
            case orig of
              0 -> return (Just (ix, elm))
              i | i == elm  -> return Nothing -- Allow repeated, equal puts.
                | otherwise -> error$"Multiple puts to index of a NatArray: "++
                                     show ix++" new/old : "++show elm++"/"++show orig

{-# INLINE get #-}
-- | Wait for an indexed entry to contain a non-zero value.
-- 
-- Warning: this is inefficient if it needs to block, because the deltaThresh must
-- monitor EVERY new addition.
get :: forall s d elt . (Storable elt, B.AtomicBits elt, Num elt) =>
       NatArray s elt -> Int -> Par d s elt
get (NatArray (WrapLVar lv)) !ix  = WrapPar $
    getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do      
      elm <- M.read ref ix 
      if elm == 0
        then return Nothing
        else return (Just elm)
    -- FIXME: we don't actually want to call the deltaThresh on every element...
      -- We want more locality than that...
    deltaThresh (ix2,e2) | ix == ix2 = return$! Just e2
                         | otherwise = return Nothing 


{-
parFor :: (ParFuture iv p) => InclusiveRange -> (Int -> p ()) -> p ()
parFor (InclusiveRange start end) body =
 do
    let run (x,y) = for_ x (y+1) body
        range_segments = splitInclusiveRange (4*numCapabilities) (start,end)

    vars <- M.forM range_segments (\ pr -> spawn_ (run pr))
    M.mapM_ get vars
    return ()

splitInclusiveRange :: Int -> (Int, Int) -> [(Int, Int)]
splitInclusiveRange pieces (start,end) =
  map largepiece [0..remain-1] ++
  map smallpiece [remain..pieces-1]
 where
   len = end - start + 1 -- inclusive [start,end]
   (portion, remain) = len `quotRem` pieces
   largepiece i =
       let offset = start + (i * (portion + 1))
       in (offset, offset + portion)
   smallpiece i =
       let offset = start + (i * portion) + remain
       in (offset, offset + portion - 1)

data InclusiveRange = InclusiveRange Int Int
-}
