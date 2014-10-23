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

-- UNFINISHED!!!

-- | An array of bit-fields (numbers) with a monotonic OR operation.

module Data.LVar.BoundedNatSet
       (
         -- * Basic operations
         BitArray,
         -- Snapshot(BitArraySnap),
         
         newEmptyBitArray, putBits,
         -- waitElem, waitSize, 

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
import           Internal.Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV,
                                                freezeLVAfter, liftIO)
import qualified Internal.Control.LVish.SchedIdempotent as L

------------------------------------------------------------------------------

-- | An array of bit-fields with a monotonic OR operation.  This can be used to model
--   a set of Ints by setting the vector entries to zero or one, but it can also
--   model other finite lattices for each index.
newtype BitArray s a = BitArray (LVar s (M.IOVector a) (Int,a))

unBitArray (BitArray lv) = lv

-- | Physical identity, just as with IORefs.
-- instance Eq (BitArray s v) where
--   BitArray lv1 == BitArray lv2 = state lv1 == state lv2 

{-

instance LVarData1 BitArray where
  newtype Snapshot BitArray a = ISetSnap (S.Set a)
      deriving (Show,Ord,Read,Eq)
  freeze    = fmap ISetSnap . freezeSet
  newBottom = newEmptySet

  -- TODO: traverseSnap

-}


-- | Create a new, empty, monotonically growing 'BitArray' of a given size.
--   All entries start off as zero, which must be BOTTOM.
newEmptyBitArray :: forall elt d s . Storable elt =>
                    Int -> Par d s (BitArray s elt)
newEmptyBitArray len = WrapPar $ fmap (BitArray . WrapLVar) $ newLV $ do 
  let bytes = sizeOf (undefined::elt) * len
  mem <- callocBytes bytes
  fp <- newForeignPtr finalizerFree mem
  return $! M.unsafeFromForeignPtr0 fp len

--------------------------------------------------------------------------------
-- Quasi-deterministic ops:
--------------------------------------------------------------------------------

{-

-- | Freeze an 'BitArray' after a specified callback/handler is done running.  This
-- differs from withCallbacksThenFreeze by not taking an additional action to run in
-- the context of the handlers.
--
--    (@'freezeSetAfter' 's' 'f' == 'withCallbacksThenFreeze' 's' 'f' 'return ()' @)
freezeSetAfter :: BitArray s a -> (a -> QPar s ()) -> QPar s ()
freezeSetAfter s f = withCallbacksThenFreeze s f (return ())
  
-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- value of the provided action.
withCallbacksThenFreeze :: Eq b => BitArray s a -> (a -> QPar s ()) -> QPar s b -> QPar s b
withCallbacksThenFreeze (BitArray (WrapLVar lv)) callback action =
    do
       hp  <- newPool 
       res <- IV.new -- TODO, specialize to skip this when the init action returns ()
       WrapPar$ 
         freezeLVAfter lv (initCB hp res) deltCB
       -- We additionally have to quiesce here because we fork the inital set of
       -- callbacks on their own threads:
       quiesce hp
       IV.get res
  where
    deltCB x = return$ Just$ unWrapPar$ callback x
    initCB hp resIV ref = do
      -- The implementation guarantees that all elements will be caught either here,
      -- or by the delta-callback:
      set <- readIORef ref -- Snapshot
      return $ Just $ unWrapPar $ do
        F.foldlM (\() v -> forkHP (Just hp)$ callback v) () set -- Non-allocating traversal.
        res <- action -- Any additional puts here trigger the callback.
        IV.put_ resIV res

-- | Get the exact contents of the set.  Using this may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
freezeSet :: BitArray s a -> QPar s (S.Set a)
freezeSet (BitArray (WrapLVar lv)) = WrapPar $ 
   do freezeLV lv
      getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing

--------------------------------------------------------------------------------
-}

{-# INLINE forEachHP #-}
-- | Add an (asynchronous) callback that listens for all new elements added to
-- the set, optionally enrolled in a handler pool
forEachHP :: Storable a =>
             Maybe HandlerPool           -- ^ pool to enroll in, if any
          -> BitArray s a                -- ^ Set to listen to
          -> (Int -> a -> Par d s ())           -- ^ callback
          -> Par d s ()
forEachHP hp (BitArray (WrapLVar lv)) callb = WrapPar $ do
    L.addHandler hp lv globalCB deltaCB
    return ()
  where
    deltaCB (ix,x) = return$ Just$ unWrapPar$ callb ix x
    globalCB vec = return$ Just$ unWrapPar$
      -- FIXME / TODO: need a better (parallel) for loop:
      forVec vec $ \ ix elm ->
        forkHP hp $ callb ix elm

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
forEach :: Storable a => BitArray s a -> (Int -> a -> Par d s ()) -> Par d s ()
forEach = forEachHP Nothing


-- | Put a single element in the set.  (WHNF) Strict in the element being put in the
-- set.     
putBits :: forall s d elt . (Storable elt, B.AtomicBits elt, Num elt) =>
           Int -> elt -> BitArray s elt -> Par d s ()
putBits !ix !elm (BitArray (WrapLVar lv)) = WrapPar$ putLV lv (putter ix)
  where putter ix vec@(M.MVector offset fptr) =
          withForeignPtr fptr $ \ ptr -> do 
            let offset = sizeOf (undefined::elt) * ix 
            orig <- B.fetchAndOr (P.plusPtr ptr offset) elm
            if orig .&. elm == 0 -- If those bits were not already set....
--              then return (Just (ix,elm))
              then return (Just (ix, elm .|. orig))
              else return Nothing

-- | Wait for an indexed entry to contain ANY of a certain set of bits.
waitBits :: forall s d elt . (Storable elt, B.AtomicBits elt, Num elt) =>
            Int -> a -> BitArray s a -> Par d s ()
waitBits !elm (BitArray (WrapLVar lv)) = WrapPar $
    getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      set <- readIORef ref
      case S.member elm set of
        True  -> return (Just ())
        False -> return (Nothing)
    deltaThresh e2 | e2 == elm = return $ Just ()
                   | otherwise  = return Nothing 

-- Wait for it to contain ALL of a certain set of bits.
-- waitBitsAnd

{-

-- | Wait on the SIZE of the set, not its contents.
waitSize :: Int -> BitArray s a -> Par d s ()
waitSize !sz (BitArray lv) = WrapPar$
    getLV (unWrapLVar lv) globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      set <- readIORef ref
      case S.size set >= sz of
        True  -> return (Just ())
        False -> return (Nothing)
    -- Here's an example of a situation where we CANNOT TELL if a delta puts it over
    -- the threshold.a
    deltaThresh _ = globalThresh (state lv) False

--------------------------------------------------------------------------------
-- Higher level routines that could be defined using the above interface.
--------------------------------------------------------------------------------

-- | Return a fresh set which will contain strictly more elements than the input set.
-- That is, things put in the former go in the latter, but not vice versa.
copy :: Ord a => BitArray s a -> Par d s (BitArray s a)
copy = traverseSet return

-- | Establish monotonic map between the input and output sets.
traverseSet :: Ord b => (a -> Par d s b) -> BitArray s a -> Par d s (BitArray s b)
traverseSet f s = traverseSetHP Nothing f s

-- | An imperative-style, inplace version of 'traverseSet' that takes the output set
-- as an argument.
traverseSet_ :: Ord b => (a -> Par d s b) -> BitArray s a -> BitArray s b -> Par d s ()
traverseSet_ f s o = void $ traverseSetHP_ Nothing f s o

-- | Return a new set which will (ultimately) contain everything in either input set.
union :: Ord a => BitArray s a -> BitArray s a -> Par d s (BitArray s a)
union s1 s2 = do
  os <- newEmptySet
  forEach s1 (`putInSet` os)
  forEach s2 (`putInSet` os)
  return os

-- | Build a new set which will contain the intersection of the two input sets.
intersection :: Ord a => BitArray s a -> BitArray s a -> Par d s (BitArray s a)
-- Can we do intersection with only the public interface?  It should be monotonic.
-- Well, for now we cheat and use liftIO:
intersection s1 s2 = do
  os <- newEmptySet
  forEach s1 (fn os s2)
  forEach s2 (fn os s1)
  return os
 where  
  fn outSet other@(BitArray lv) elm = do
    -- At this point 'elm' has ALREADY been added to "us", we check "them":    
    peek <- LI.liftIO$ readIORef (state lv)
    if S.member elm peek 
      then putInSet elm outSet
      else return ()

-- | Cartesian product of two sets.
cartesianProd :: (Ord a, Ord b) => BitArray s a -> BitArray s b -> Par d s (BitArray s (a,b))
cartesianProd s1 s2 = cartesianProdHP Nothing s1 s2 
  
-- | Takes the cartesian product of several sets.
cartesianProds :: Ord a => [BitArray s a] -> Par d s (BitArray s [a])
cartesianProds ls = cartesianProdsHP Nothing ls

--------------------------------------------------------------------------------
-- Alternate versions of functions that EXPOSE the HandlerPools
--------------------------------------------------------------------------------

-- TODO: unionHP, intersectionHP...

-- | Variant that optionally ties the handlers to a pool.
traverseSetHP :: Ord b => Maybe HandlerPool -> (a -> Par d s b) -> BitArray s a ->
                 Par d s (BitArray s b)
traverseSetHP mh fn set = do
  os <- newEmptySet
  traverseSetHP_ mh fn set os  
  return os

-- | Variant that optionally ties the handlers to a pool.
traverseSetHP_ :: Ord b => Maybe HandlerPool -> (a -> Par d s b) -> BitArray s a -> BitArray s b ->
                  Par d s ()
traverseSetHP_ mh fn set os = do
  forEachHP mh set $ \ x -> do 
    x' <- fn x
    putInSet x' os

--------------------------------------------------------------------------------
-- Set specific DeepFreeze instances:
--------------------------------------------------------------------------------

-- Teach it how to freeze WITHOUT the annoying snapshot constructor:

instance DeepFreeze (BitArray s a) (S.Set a) where
  type Session (BitArray s a) = s
  deepFreeze iv = do ISetSnap m <- freeze iv
                     return m

------------------------------------------------------------
-- Compromise to avoid overlap
------------------------------------------------------------
instance (LVarData1 f, DeepFreeze (f s0 a) b, Ord b) =>
         DeepFreeze (BitArray s0 (f s0 a)) (S.Set b)  where
    type Session (BitArray s0 (f s0 a)) = s0
    deepFreeze from = do
      x <- freezeSet from
      let fn :: f s0 a -> S.Set b -> QPar s0 (S.Set b)
          fn elm acc = do elm' <- deepFreeze elm
                          return (S.insert elm' acc)
      y <- F.foldrM fn S.empty x 
      return y      

 -}


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
