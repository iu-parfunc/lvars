{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DoAndIfThenElse            #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Internal module exposing multiple implementations.
--   Don't use this unless you have a good reason.

module Control.Par.MergeSort.Internal
       ( mergeSort_int32
       , mergeSort_int64
       , SMerge(..)
       , SSort(..)
       , mkMergeSort

       -- * Parallel C sort
       , c_cilksort

       -- * For testing only:
       , findSplit'
       )
  where

import           Control.LVish as LVishSched
-- TODO(osa): Disabling these until we implement instances:
-- import qualified Control.Monad.Par.Scheds.Direct as DirectSched
-- import qualified Control.Monad.Par.Scheds.Sparks as SparksSched
-- import qualified Control.Monad.Par.Scheds.Trace  as TraceSched

import           Control.Par.Class (ParThreadSafe ())
import qualified Control.Par.Class as PC
import           Control.Par.Class.Unsafe (internalLiftIO)
import           Control.Par.ST

import           Control.Monad
import           Control.Monad.ST (ST)
-- import qualified Control.Monad.State.Strict   as SS
import           Data.Int

-- TODO(osa): Removed all variants to make test program working, maybe move each
-- specialzed version to it's own module.

#define VFlp SFlp
#define MVectorFlp SVectorFlp
import qualified Control.Par.ST.StorableVec2 as V
import qualified Data.Vector.Storable.Mutable as MV

-- [2013.11.15] Adding new variant:
import qualified Data.Vector.Algorithms.Intro as VI
import qualified Data.Vector.Algorithms.Merge as VA

-- FFI stuff for Cilk parts
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr

--------------------------------------------------------------------------------

-- | Select which sequential merge implementation to use.
data SMerge = CMerge | HSMerge
  deriving (Show, Read)

-- | Select which sequential sort implementation to use.
data SSort = CSort | VAMSort | VAISort
  deriving (Show, Read)

--------------------------------------------------------------------------------
-- Specialized sorts.

{-# INLINE mergeSort_int32 #-}
mergeSort_int32 :: (ParThreadSafe p,
                      HasGet e, HasPut e, PC.ParFuture p)
                  => Int -- ^ Sequential sort threshold
                  -> Int -- ^ Sequential merge threshold
                  -> SSort
                  -> SMerge
                  -> V.ParVec2T s1 Int32 Int32 p e s ()
mergeSort_int32 !st !mt !sa !ma =
  mkMergeSort st mt
     (case sa of
       VAMSort -> seqSortL
       VAISort -> seqSortL2
       CSort   -> cilkSeqSort_int32)
     (case ma of
        CMerge  -> cilkSeqMerge_int32
        HSMerge -> seqmerge)


{-# INLINE mergeSort_int64 #-}
mergeSort_int64 :: (ParThreadSafe p,
                      HasGet e, HasPut e, PC.ParFuture p)
                  => Int -- ^ Sequential sort threshold
                  -> Int -- ^ Sequential merge threshold
                  -> SSort
                  -> SMerge
                  -> V.ParVec2T s1 Int64 Int64 p e s ()
mergeSort_int64 !st !mt !sa !ma =
  mkMergeSort st mt
     (case sa of
       VAMSort -> seqSortL
       VAISort -> seqSortL2
       CSort   -> cilkSeqSort_int64)
     (case ma of
        CMerge  -> cilkSeqMerge_int64
        HSMerge -> seqmerge)

--------------------------------------------------------------------------------
-- Generic sort routines

-- | Given a vector in left position, and an available buffer of equal
-- size in the right position, sort the left vector.
mkMergeSort :: forall p e s s1 elt .
               (ParThreadSafe p,
                HasGet e, HasPut e, PC.ParFuture p, Ord elt)
            => Int -- ^ Sequential sort threshold
            -> Int -- ^ Sequential merge threshold
            -> SeqSortM elt p e s
            -> SeqMergeM elt p e s
            -> V.ParVec2T s1 elt elt p e s ()
mkMergeSort !st !mt seqSort seqMerge =
 --  loop
 -- where
 --  loop :: forall s . V.ParVec2T s1 elt elt p e s ()
 --  loop =
   do len <- V.lengthL
      unless (len <= 1) $
        if len < st then do
          seqSort
        else do
          let sp = len `quot` 2
          void $ forkSTSplit (sp,sp)
            (do _ <- forkSTSplit (sp,sp)
                       (mkMergeSort st mt seqSort seqMerge)
                       (mkMergeSort st mt seqSort seqMerge)
                mergeTo2 sp mt seqMerge)
            (do _ <- forkSTSplit (sp,sp)
                       (mkMergeSort st mt seqSort seqMerge)
                       (mkMergeSort st mt seqSort seqMerge)
                mergeTo2 sp mt seqMerge)
          mergeTo1 sp mt seqMerge
-- where
--  loop = mkMergeSort st mt seqSort ma



-- mergeSortOutPlace ::
--             (ParThreadSafe p,
--               PC.ParFuture p, Ord e1, Show e1) =>
--              Int -> Int -> SSort -> SMerge ->
--              V.ParVec2T s1 e1 e1 p e s ()
-- mergeSortOutPlace !st !mt !sa !ma = do
--   error "mergeOutPlace"
{-
 where
  cpuMergeSort t cpuMS vec =
    if V.length vec <= t
    then cpuMS vec
    else do
      let n = (V.length vec) `div` 2
      let (lhalf, rhalf) = V.splitAt n vec
      ileft <- spawn_ (cpuMergeSort t cpuMS lhalf)
      right <-         cpuMergeSort t cpuMS rhalf
      left  <- get ileft
      merge t left right
-}

-- | Call a sequential in-place sort on the left vector.
{-# INLINE seqSortL #-}
seqSortL :: (Ord eL, ParThreadSafe p) => V.ParVec2T s1 eL eR p e s ()
seqSortL = do
  STTup2 (VFlp vecL) (VFlp _) <- reify
  liftST $ VA.sort vecL

{-# INLINE seqSortL2 #-}
seqSortL2 :: (Ord eL, ParThreadSafe p) => V.ParVec2T s1 eL eR p e s ()
seqSortL2 = do
  STTup2 (VFlp vecL) (VFlp _) <- reify
  liftST $ VI.sort vecL

---------------

-- | Outer wrapper for a function that merges input vectors in the
-- left position into the vector in right position.
{-# INLINE mergeTo2 #-}
mergeTo2 :: (ParThreadSafe p, Ord elt,
             PC.ParFuture p, HasGet e, HasPut e) =>
            Int -> Int -> SeqMergeM elt p e s -> V.ParVec2T s1 elt elt p e s ()
mergeTo2 sp threshold ma = do
  -- convert the state from (Vec, Vec) to ((Vec, Vec), Vec) then call normal parallel merge
  transmute (morphToVec21 sp) (pMergeTo2 threshold ma)

  -- error "mergeTo2"


-- | Type alias for a ParST state of ((Vec,Vec), Vec)
type ParVec21T s1 e1 p e s a =
     ParST (STTup2 (STTup2 (MVectorFlp e1) (MVectorFlp e1)) (MVectorFlp e1) s1) p e s a

-- | Parallel merge kernel.
{-# INLINABLE pMergeTo2 #-}
pMergeTo2 :: (ParThreadSafe p, Ord elt,
              PC.ParFuture p, HasGet e, HasPut e) =>
             Int -> SeqMergeM elt p e s -> ParVec21T s1 elt p e s ()
pMergeTo2 threshold seqMerge = do
  STTup2 (STTup2 (VFlp v1) (VFlp v2)) _ <- reify
  let l1 = MV.length v1
      l2 = MV.length v2

  if l1 == 0 && l2 == 0 then
    return ()
  else if l1 < threshold || l2 < threshold || l1 <= 1 || l2 <= 1 then do
    seqMerge
  else do
    (splitL, splitR) <- findSplit
    let mid = splitL + splitR
    void $ forkSTSplit ((splitL, splitR), mid)
      (pMergeTo2 threshold seqMerge)
      (pMergeTo2 threshold seqMerge)


-- | Merging from right-to-left works by swapping the states before
-- and after calling the left-to-right merge.
{-# INLINE mergeTo1 #-}
mergeTo1 :: (ParThreadSafe p, Ord elt,
             PC.ParFuture p, HasGet e, HasPut e) =>
            Int -> Int -> SeqMergeM elt p e s -> V.ParVec2T s1 elt elt p e s ()
mergeTo1 sp threshold seqMerge = do
  V.swapState
  mergeTo2 sp threshold seqMerge
  V.swapState

-- NOTE(osa): This function assumes that at least one of the vectors are not
-- empty. Otherwise it fails with an error like:
-- *** Exception: ./Data/Vector/Generic/Mutable.hs:591 (read): index out of bounds (0,0)
--
{-# INLINE findSplit #-}
findSplit :: forall s1 e1 p e s .
             (ParThreadSafe p, Ord e1,
              PC.ParMonad p) =>
             ParVec21T s1 e1 p e s (Int, Int)
findSplit = do
  STTup2 (STTup2 (VFlp vl) (VFlp vr)) _ <- reify
  let lLen = MV.length vl
      rLen = MV.length vr
  liftST $ findSplit' vl vr 0 lLen 0 rLen

findSplit' :: (MV.Storable a, Ord a) =>
              MV.MVector s a -> MV.MVector s a -> Int -> Int -> Int -> Int -> ST s (Int, Int)
findSplit' vl vr lLow lHigh rLow rHigh = do
  let lIndex = (lLow + lHigh) `div` 2
      rIndex = (rLow + rHigh) `div` 2

  left <- MV.read vl lIndex
  right <- MV.read vr rIndex
  if (lIndex == 0) && (rIndex == 0) then do
    return (lIndex, rIndex)
  else if lIndex == 0 then do
    rightSub1 <- MV.read vr (rIndex - 1)
    if rightSub1 <= left
       then return (lIndex, rIndex)
       else findSplit' vl vr 0 0 rLow rIndex
--           else trace ((show rIndex) ++ " " ++ (show lIndex) ++ "sp") $ split 0 0 rLow rIndex
  else if rIndex == 0 then do
    leftSub1 <- MV.read vl (lIndex - 1)
    if (leftSub1 <= right)
      then return (lIndex, rIndex)
      else findSplit' vl vr lLow lIndex 0 0
--          else trace ((show rIndex) ++ " " ++ (show lIndex) ++ "spl") $ split lLow lIndex 0 0
  else do
    rightSub1 <- MV.read vr (rIndex - 1)
    leftSub1 <- MV.read vl (lIndex - 1)
    if (leftSub1 <= right) && (rightSub1 <= left)
      then return (lIndex, rIndex)
      else if (leftSub1 <= right)
        then findSplit' vl vr lIndex lHigh rLow rIndex
        else findSplit' vl vr lLow lIndex rIndex rHigh

-----

-- TODO: Move me into a library of safe (alias free) state transformations.

{-# INLINE morphToVec21 #-}
-- | Slice the left vector at the given split point, leaving a pair of two vectors.
morphToVec21 :: Int
             -> STTup2 (SVectorFlp v1) (SVectorFlp v2) s
             -> STTup2 (STTup2 (SVectorFlp v1) (SVectorFlp v1)) (SVectorFlp v2) s
morphToVec21 sp (STTup2 (VFlp vec1) vec2) =
  let l1 = MV.slice 0 sp vec1
      r1 = MV.slice sp (MV.length vec1 - sp) vec1 in
  STTup2 (STTup2 (VFlp l1) (VFlp r1)) vec2

-----

{-# INLINE seqmerge #-}
seqmerge :: forall s1 e1 p e s . (ParThreadSafe p, Ord e1) => ParVec21T s1 e1 p e s ()
seqmerge = do
  STTup2 (STTup2 (VFlp left) (VFlp right)) (VFlp dest) <- reify

  let lenL = MV.length left
      lenR = MV.length right
      len = lenL + lenR

  let copyRemainingRight :: Int -> e1 -> Int -> ST s1 ()
      copyRemainingRight !ri !rx !di =
        if ri < (lenR-1) then do
          MV.write dest di rx
          let ri' = ri + 1
          rx' <- MV.read right ri'
          copyRemainingRight ri' rx' (di + 1)
        else when (di < len) $ do
          MV.write dest di rx

      copyRemainingLeft :: Int -> e1 -> Int -> ST s1 ()
      copyRemainingLeft !li !lx !di =
        if li < (lenL-1) then do
          MV.write dest di lx
          let li' = li + 1
          lx' <- MV.read left li'
          copyRemainingLeft li' lx' (di + 1)
        else when (di < len) $ do
          MV.write dest di lx

  let loop !li !lx !ri !rx !di =
        let di' = di+1 in
        if lx < rx then do
          MV.write dest di lx
          let li' = li + 1
          if li' == lenL then
            -- copy the rest of right into dest
            copyRemainingRight ri rx di'
          else when (di' < len) $ do
             lx' <- MV.read left li'
             loop li' lx' ri rx di'
        else do
          MV.write dest di rx
          let ri' = ri + 1
          if ri' == lenR then
            -- copy the rest of left into dest
            copyRemainingLeft li lx di'
          else when (di' < len) $ do
            rx' <- MV.read right ri'
            loop li lx ri' rx' di'

  if len == 0 then
    return ()
  else if lenL == 0 then liftST $ do
    fstR <- MV.read right 0
    copyRemainingRight 0 fstR 0
  else if lenR == 0 then liftST $ do
    fstL <- MV.read left 0
    copyRemainingLeft 0 fstL 0
  else liftST $ do
    fstL <- MV.read left 0
    fstR <- MV.read right 0
    loop 0 fstL 0 fstR 0

--------------------------------------------------------------------------------

-- Requires that we selected STORABLE vectors above!

-- | Raw foreign sort type.
type SeqSort e = Ptr e -> CLong -> IO (Ptr e)

-- | A sequential sort computation that operates on the state.
type SeqSortM elt p e s =
  forall s1 . V.ParVec2T s1 elt elt p e s ()

-- | Raw foreign merge type.
type SeqMerge e = Ptr e -> CLong -> Ptr e -> CLong -> Ptr e -> IO ()

-- | Merge computation against a tuple-of-array state.
type SeqMergeM elt p e s = forall s1 . ParVec21T s1 elt p e s ()

--------------------------------------------------------------------------------

foreign import ccall unsafe "wrap_seqquick_int32"
  c_seqquick_int32 :: SeqSort Int32

foreign import ccall unsafe "wrap_seqquick_int64"
  c_seqquick_int64 :: SeqSort Int64

foreign import ccall unsafe "wrap_seqmerge_int32"
  c_seqmerge_int32 ::  SeqMerge Int32

foreign import ccall unsafe "wrap_seqmerge_int64"
  c_seqmerge_int64 ::  SeqMerge Int64

--------------------------------------------------------------------------------

cilkSeqSort_int32 :: (ParThreadSafe p, PC.ParMonad p)
                  =>  V.ParVec2T s1 Int32 Int32 p e s ()
cilkSeqSort_int32 = mkCilkSeqSort c_seqquick_int32

cilkSeqSort_int64 :: (ParThreadSafe p, PC.ParMonad p)
                  =>  V.ParVec2T s1 Int64 Int64 p e s ()
cilkSeqSort_int64 = mkCilkSeqSort c_seqquick_int64

-- | Sequential Cilk sort, on the left vector, inplace.
--
--   UNSAFE version - must be used at the right type.
mkCilkSeqSort :: (Ord eL, ParThreadSafe p, PC.ParMonad p)
              => SeqSort eL
              -> V.ParVec2T s1 eL eL p e s ()
-- This has the same signature & contract as seqSortL.
mkCilkSeqSort c_seqquick  = do
  STTup2 (VFlp vecL) (VFlp _) <- reify
  internalLiftIO $ do
    let len = MV.length vecL
    let (fptr,_) = MV.unsafeToForeignPtr0 vecL
    withForeignPtr fptr $ \ vptr -> do
      -- No allocation, C operates on this memory in-place:
      _ <- c_seqquick (castPtr vptr) (fromIntegral len)
      return ()
    return ()

--------------------------------------------------------------------------------

cilkSeqMerge_int32 :: (ParThreadSafe p, PC.ParMonad p) => ParVec21T s1 Int32 p e s ()
cilkSeqMerge_int32 = mkCilkSeqMerge c_seqmerge_int32

cilkSeqMerge_int64 :: (ParThreadSafe p, PC.ParMonad p) => ParVec21T s1 Int64 p e s ()
cilkSeqMerge_int64 = mkCilkSeqMerge c_seqmerge_int64

{-# INLINE mkCilkSeqMerge #-}
-- | UNSAFE operation -- better use it at the right type.
mkCilkSeqMerge :: (Ord e1, ParThreadSafe p, PC.ParMonad p)
               => SeqMerge e1 -> ParVec21T s1 e1 p e s ()
mkCilkSeqMerge c_seqmerge = do
  STTup2 (STTup2 (VFlp v1) (VFlp v2)) (VFlp v3) <- reify
  let (fptr1,_) = MV.unsafeToForeignPtr0 v1
      (fptr2,_) = MV.unsafeToForeignPtr0 v2
      (fptr3,_) = MV.unsafeToForeignPtr0 v3

  internalLiftIO $ do
    let len1 = MV.length v1
        len2 = MV.length v2
    withForeignPtr fptr1 $ \vptr1 ->
     withForeignPtr fptr2 $ \vptr2 ->
      withForeignPtr fptr3 $ \vptr3 ->
        c_seqmerge (castPtr vptr1) (fromIntegral len1)
                   (castPtr vptr2) (fromIntegral len2)
                   (castPtr vptr3)

#if 0
foreign import ccall unsafe "wrap_cilksort"
  c_cilksort ::  Ptr Int32 -> Ptr Int32 -> CLong -> IO CLong
#else
c_cilksort ::  Ptr Int32 -> Ptr Int32 -> CLong -> IO CLong
c_cilksort = error "c_cilksort: cilk versions not loaded"
#endif

--------------------------------------------------------------------------------
-- Helpers for manipulating ParVec12T and ParVec21T

indexL1 :: (ParThreadSafe p) => Int -> ParVec21T s1 e1 p e s e1
indexL1 index = do
  STTup2 (STTup2 (VFlp l1) _) _ <- reify
  liftST $ MV.read l1 index

indexR1 :: (ParThreadSafe p) => Int -> ParVec21T s1 e1 p e s e1
indexR1 index = do
  STTup2 (STTup2 _ (VFlp r1)) _ <- reify
  liftST $ MV.read r1 index

write2 :: (ParThreadSafe p) => Int -> e1 -> ParVec21T s1 e1 p e s ()
write2 index value = do
  STTup2 _ (VFlp v2) <- reify
  liftST $ MV.write v2 index value


lengthLR1 :: (ParThreadSafe p) => ParVec21T s1 e1 p e s (Int, Int)
lengthLR1 = do
  STTup2 (STTup2 (VFlp vecL) (VFlp vecR)) _ <- reify
  let lenL = MV.length vecL
      lenR = MV.length vecR
  return (lenL, lenR)


--------------------------------------------------------------------------------
-- Currently unused:
--------------------------------------------------------------------------------

-- | Sequential merge kernel.
_sMergeTo2 :: (ParThreadSafe p, Ord e1) => ParVec21T s1 e1 p e s ()
_sMergeTo2 = do
  (lenL, lenR) <- lengthLR1
  sMergeTo2K 0 lenL 0 lenR 0

sMergeTo2K :: (Ord a, ParThreadSafe p) =>
               Int -> Int -> Int -> Int -> Int -> ParST
               (STTup2 (STTup2 (SVectorFlp a) (SVectorFlp a)) (SVectorFlp a) s1) p e s ()
sMergeTo2K !lBot !lLen !rBot !rLen !index
  | lBot == lLen && rBot < rLen = do
    value <- indexR1 rBot
    write2 index value
    sMergeTo2K lBot lLen (rBot + 1) rLen (index + 1)

  | rBot >= rLen && lBot < lLen = do
    value <- indexL1 lBot
    write2 index value
    sMergeTo2K (lBot + 1) lLen rBot rLen (index + 1)

  | index >= (lLen + rLen) = do
    return ()

  | otherwise = do
    left <- indexL1 lBot
    right <- indexR1 rBot
    if left < right then do
      write2 index left
      sMergeTo2K (lBot + 1) lLen rBot rLen (index + 1)
     else do
      write2 index right
      sMergeTo2K lBot lLen (rBot + 1) rLen (index + 1)

_morphToVec12 :: Int
              -> STTup2 (SVectorFlp v1) (SVectorFlp v2) s
              -> STTup2 (SVectorFlp v1) (STTup2 (SVectorFlp v2) (SVectorFlp v2)) s
{-# INLINE _morphToVec12 #-}
_morphToVec12 sp (STTup2 (VFlp vec1) (VFlp vec2)) =
  let l2 = MV.slice 0 sp vec2
      r2 = MV.slice sp (MV.length vec2 - sp) vec2 in
  STTup2 (VFlp vec1) (STTup2 (VFlp l2) (VFlp r2))

--------------------------------------------------------------------------------

{-

indexL2 :: (ParThreadSafe p) => Int -> ParVec12T s1 e1 p e s e1
indexL2 index = do
  STTup2 _ (STTup2 (VFlp l2) _) <- reify
  liftST $ MV.read l2 index

indexR2 :: (ParThreadSafe p) => Int -> ParVec12T s1 e1 p e s e1
indexR2 index = do
  STTup2 _ (STTup2 _ (VFlp r2)) <- reify
  liftST $ MV.read r2 index

write1 :: (ParThreadSafe p) => Int -> e1 -> ParVec12T s1 e1 p e s ()
write1 index value = do
  STTup2 (VFlp v1) _ <- reify
  liftST $ MV.write v1 index value

length2 :: (ParThreadSafe p) => ParVec21T s1 e1 p e s Int
length2 = do
  STTup2 _ (VFlp vec2) <- reify
  return $ MV.length vec2

length1 :: (ParThreadSafe p) => ParVec12T s1 e1 p e s Int
length1 = do
  STTup2 (VFlp v1) _ <- reify
  return $ MV.length v1

lengthLR2 :: (ParThreadSafe p) => ParVec12T s1 s1 p e s (Int, Int)
lengthLR2 = do
  STTup2 _ (STTup2 (VFlp vecL) (VFlp vecR)) <- reify
  let lenL = MV.length vecL
      lenR = MV.length vecR
  return (lenL, lenR)

-- | Type alias for a ParST state of (Vec, (Vec, Vec))
type ParVec12T s1 e1 p e s a =
     ParST (STTup2 (MVectorFlp e1) (STTup2 (MVectorFlp e1) (MVectorFlp e1)) s1) p e s a

-}
