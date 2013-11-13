{-# LANGUAGE MultiParamTypeClasses #-}                                                         
{-# LANGUAGE ScopedTypeVariables #-}                                                           
{-# LANGUAGE GeneralizedNewtypeDeriving #-}                                                    
{-# LANGUAGE TypeFamilies #-}                                                                  
{-# LANGUAGE ConstraintKinds #-}                                                               
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}                                                             
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Main where

--module MergeSort 
--       (
--         runTests
--       )
--       where

import Control.LVish      as LV
import Control.Par.ST
import Control.Par.Class.Unsafe (ParThreadSafe(), internalLiftIO)
import qualified Control.Par.Class as PC

import Control.Monad
import Control.Monad.ST        (ST)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State.Strict as SS
import Data.Word
import Data.Time.Clock

#ifdef BOXED
-- No reason to use the boxed version moving forward:
import Data.Vector.Mutable as MV
import qualified Data.Vector as IMV
import qualified Control.Par.ST.Vec2 as V
import Data.Vector (freeze)
#elif defined(UNBOXED)
#warning "Using Unboxed Vectors."
#define VFlp UFlp
#define MVectorFlp UVectorFlp
import qualified Data.Vector.Unboxed as IMV
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Control.Par.ST.UVec2 as V
import Data.Vector.Unboxed (freeze)
#else
#warning "Using Storable Vectors."
#define VFlp SFlp
#define MVectorFlp SVectorFlp
import qualified Data.Vector.Storable as IMV
import qualified Data.Vector.Storable.Mutable as MV
import qualified Control.Par.ST.StorableVec2 as V
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array (allocaArray)
#endif

import qualified Data.Vector.Algorithms.Merge as VA
import Prelude hiding (read, length)
import qualified Prelude

import System.Random.MWC (create, uniformR) -- uniformVector,
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf


--------------------------------------------------------------------------------

#define SAFE
#ifndef SAFE
thawit  x     = IMV.unsafeThaw   x
newMV   x     = MV.unsafeNew   x
readMV  x y   = MV.unsafeRead  x y
writeMV x y z = MV.unsafeWrite x y z
sliceMV x y z = MV.unsafeSlice x y z
copyMV  x y   = MV.unsafeCopy  x y 
#else
thawit  x     = IMV.thaw   x
newMV   x     = MV.new   x
readMV  x y   = MV.read  x y
writeMV x y z = MV.write x y z
sliceMV x y z = MV.slice x y z
copyMV  x y   = MV.copy  x y 
#endif
{-# INLINE thawit #-}
{-# INLINE newMV #-}
{-# INLINE readMV #-}
{-# INLINE writeMV #-}
{-# INLINE sliceMV #-}
{-# INLINE copyMV #-}

--------------------------------------------------------------------------------

runTests :: Bool
runTests = True

-- | Generate a random vector of length N and sort it using parallel
-- in-place merge sort. 
wrapper :: Int -> String
wrapper size = LV.runPar $ V.runParVec2T (0,size) $ do

  -- test setup: 
  randVec <- liftST$ mkRandomVec size    
  
  STTup2 (VFlp left) (VFlp right) <- SS.get
  SS.put (STTup2 (VFlp randVec) (VFlp right))
  
  start <- internalLiftIO$ getCurrentTime  
  -- post condition: left array is sorted
  mergeSort  
  end <- internalLiftIO$ getCurrentTime
  
  let runningTime = ((fromRational $ toRational $ diffUTCTime end start) :: Double)
    
  (rawL, rawR) <- V.reify
  frozenL <- liftST$ IMV.freeze rawL
  
  internalLiftIO$ putStrLn$ "Is Sorted?: "++show (checkSorted frozenL)
  internalLiftIO$ printf "Sorting vector took %0.2f sec.\n" runningTime
  internalLiftIO$ printf "SELFTIMED: %0.3f\n" runningTime  
  
  return "done"
--  return$ show frozenL

seqsortThresh :: Int
seqsortThresh = 2048

seqmergeThresh :: Int
seqmergeThresh = seqsortThresh

-- | Given a vector in left position, and an available buffer of equal
-- size in the right position, sort the left vector.
mergeSort :: (ParThreadSafe parM, PC.FutContents parM (),
              PC.ParFuture parM, Ord elt, Show elt) => 
             V.ParVec2T s1 elt elt parM ()  
mergeSort = do
  len <- V.lengthL

-- SET THRESHOLD
  if len < seqsortThresh then do
    seqSortL
   else do  
    let sp = (len `quot` 2)              
    forkSTSplit (sp,sp)
      (do len <- V.lengthL
          let sp = (len `quot` 2)
          forkSTSplit (sp,sp)
            mergeSort
            mergeSort
          mergeTo2 sp seqmergeThresh)
      (do len <- V.lengthL                                                                    
          let sp = (len `quot` 2)                                                              
          forkSTSplit (sp,sp)
            mergeSort         
            mergeSort
          mergeTo2 sp seqmergeThresh)
    mergeTo1 sp seqmergeThresh

-- | Call a sequential in-place sort on the left vector.
seqSortL :: (Ord eltL, ParThreadSafe parM) => V.ParVec2T s eltL eltR parM ()
seqSortL = do
  STTup2 (VFlp vecL) (VFlp vecR) <- SS.get
  liftST$ VA.sort vecL

main :: IO ()
main = do
  args <- getArgs
  let sz = case args of
            []   -> 2^16
            [sz] -> 2^(Prelude.read sz)
  putStrLn $ wrapper sz

-- | Create a vector containing the numbers [0,N) in random order.
mkRandomVec :: Int -> ST s (MV.STVector s Int)
mkRandomVec len =  
  -- Annoyingly there is no MV.generate:
  do g <- create
     v <- IMV.thaw $ IMV.generate len fromIntegral
     loop 0 v g
     return v
 where 
  -- Note: creating 2^24 elements takes 1.6 seconds under -O2 but 36
  -- seconds under -O0.  This sorely needs optimization!
  loop n vec g | n == len  = return vec
	       | otherwise = do 
--    let (offset,g') = randomR (0, len - n - 1) g
    offset <- uniformR (0, len - n - 1) g
    MV.swap vec n (n + offset)
    loop (n+1) vec g
    
checkSorted :: IMV.Vector Int -> Bool
--checkSorted vec = and $ IMV.zipWith (<=) vec (IMV.drop 1 vec)
checkSorted vec = IMV.foldl' (\acc elem -> acc && elem) True $ IMV.imap (\i elem -> (i == elem)) vec

  

---------------

-- | Outer wrapper for a function that merges input vectors in the
-- left position into the vector in right position.
mergeTo2 :: (ParThreadSafe parM, Ord elt, Show elt, PC.FutContents parM (),
             PC.ParFuture parM) => 
            Int -> Int -> V.ParVec2T s elt elt parM ()
mergeTo2 sp threshold = do
  -- convert the state from (Vec, Vec) to ((Vec, Vec), Vec) then call normal parallel merge
  transmute (morphToVec21 sp) (pMergeTo2 threshold)
                  
-- | Parallel merge kernel.
pMergeTo2 :: (ParThreadSafe parM, Ord elt, Show elt, PC.FutContents parM (),
              PC.ParFuture parM) =>
             Int -> ParVec21T s elt parM ()
pMergeTo2 threshold = do
  len <- length2
  if len < threshold then
    sMergeTo2
   else do
    (splitL, splitR) <- findSplit indexL1 indexR1
    let mid = splitL + splitR
    forkSTSplit ((splitL, splitR), mid)
      (pMergeTo2 threshold)
      (pMergeTo2 threshold)
    return ()
      
-- | Sequential merge kernel.      
sMergeTo2 :: (ParThreadSafe parM, Ord elt, Show elt) =>       
            ParVec21T s elt parM ()
sMergeTo2 = do
  (lenL, lenR) <- lengthLR1
  sMergeTo2K 0 lenL 0 lenR 0
      
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
    
-- | Mergeing from right-to-left works by swapping the states before
-- and after calling the left-to-right merge.
mergeTo1 sp threshold = do
  V.swapState
  (mergeTo2 sp threshold)
  V.swapState

-- NOTE: THIS FUNCTION IS BORKED! It has issues with very small input
-- lengths and gets stuck. It might be a general problem with the
-- parallel merge algorithm, but is most likely something wrong about
-- how this function operates. We are being *very dangerous* and just
-- not using a small threshold that triggers this issue. t=4 breaks
-- it, but t=8 seems to work.
--         
findSplit :: (ParThreadSafe parM, Ord elt, Show elt,
              PC.ParMonad parM) => 
             (Int -> ParVec21T s elt parM elt) ->
             (Int -> ParVec21T s elt parM elt)->
             ParVec21T s elt parM (Int, Int)

findSplit indexLeft indexRight = do                                 
  
  (lLen, rLen) <- lengthLR1    
    
  split 0 lLen 0 rLen
      where
        split lLow lHigh rLow rHigh = do
          let lIndex = (lLow + lHigh) `div` 2
              rIndex = (rLow + rHigh) `div` 2
              
          left <- indexLeft lIndex
          right <- indexRight rIndex
          if (lIndex == 0) && (rIndex == 0) then do
            return (lIndex, rIndex)                    
          else if (lIndex == 0) then do
            rightSub1 <- indexRight (rIndex - 1)
            if (rightSub1 <= left)
               then return (lIndex, rIndex)
               else split 0 0 rLow rIndex
          else if (rIndex == 0) then do
            leftSub1 <- indexLeft (lIndex - 1)
            if (leftSub1 <= right)
              then return (lIndex, rIndex)
              else split lLow lIndex 0 0
          else do
            rightSub1 <- indexRight (rIndex - 1)
            leftSub1 <- indexLeft (lIndex - 1)
            if (leftSub1 <= right) && (rightSub1 <= left)
              then return (lIndex, rIndex)
              else if (leftSub1 <= right)
                then split lIndex lHigh rLow rIndex
                else split lLow lIndex rIndex rHigh

-- | Type alias for a ParST state of ((Vec,Vec), Vec)             
type ParVec21T s elt parM ans = ParST (STTup2 
                                       (STTup2 (MVectorFlp elt) 
                                               (MVectorFlp elt))
                                       (MVectorFlp elt) s)
                                      parM ans
                                              
-- | Type alias for a ParST state of (Vec, (Vec, Vec))                                
type ParVec12T s elt parM ans = ParST (STTup2 
                                       (MVectorFlp elt)
                                       (STTup2 (MVectorFlp elt) 
                                               (MVectorFlp elt)) s)
                                      parM ans
    
                       
-- | Helpers for manipulating ParVec12T and ParVec21T                                
                                
indexL1 :: (ParThreadSafe parM) => Int -> ParVec21T s elt parM elt
indexL1 index = do
  STTup2 (STTup2 (VFlp l1) (VFlp r1)) (VFlp v2) <- SS.get
  liftST$ MV.read l1 index

indexR1 :: (ParThreadSafe parM) => Int -> ParVec21T s elt parM elt
indexR1 index = do
  STTup2 (STTup2 (VFlp l1) (VFlp r1)) (VFlp v2) <- SS.get
  liftST$ MV.read r1 index

indexL2 :: (ParThreadSafe parM) => Int -> ParVec12T s elt parM elt
indexL2 index = do
  STTup2 (VFlp v1) (STTup2 (VFlp l2) (VFlp r2)) <- SS.get
  liftST$ MV.read l2 index

indexR2 :: (ParThreadSafe parM) => Int -> ParVec12T s elt parM elt
indexR2 index = do
  STTup2 (VFlp v1) (STTup2 (VFlp l2) (VFlp r2)) <- SS.get
  liftST$ MV.read r2 index

write1 :: (ParThreadSafe parM) => Int -> elt -> ParVec12T s elt parM ()
write1 index value = do
  STTup2 (VFlp v1) (STTup2 (VFlp l2) (VFlp r2)) <- SS.get
  liftST$ MV.write v1 index value
             
write2 :: (ParThreadSafe parM) => Int -> elt -> ParVec21T s elt parM ()
write2 index value = do
  STTup2 (STTup2 (VFlp l1) (VFlp r1)) (VFlp v2) <- SS.get
  liftST$ MV.write v2 index value

length2 :: (ParThreadSafe parM) => ParVec21T s elt parM Int      
length2 = do
  STTup2 (STTup2 (VFlp vecL) (VFlp vecR)) (VFlp vec2) <- SS.get
  return $ MV.length vec2
        
length1 :: (ParThreadSafe parM) => ParVec12T s elt parM Int
length1 = do
  STTup2 (VFlp v1) (STTup2 (VFlp l2) (VFlp r2)) <- SS.get
  return$ MV.length v1
      
lengthLR1 :: (ParThreadSafe parM) => ParVec21T s elt parM (Int,Int)
lengthLR1 = do
  STTup2 (STTup2 (VFlp vecL) (VFlp vecR)) (VFlp vec2) <- SS.get
  let lenL = MV.length vecL
      lenR = MV.length vecR
  return$ (lenL, lenR)

lengthLR2 :: (ParThreadSafe parM) => ParVec12T s elt parM (Int,Int)
lengthLR2 = do
  STTup2 (VFlp v1) (STTup2 (VFlp vecL) (VFlp vecR)) <- SS.get
  let lenL = MV.length vecL
      lenR = MV.length vecR
  return$ (lenL, lenR)

-----
  
morphToVec21 sp (STTup2 (VFlp vec1) (VFlp vec2)) =
  let l1 = MV.slice 0 sp vec1
      r1 = MV.slice sp (MV.length vec1 - sp) vec1 in
  STTup2 (STTup2 (VFlp l1) (VFlp r1)) (VFlp vec2)

morphToVec12 sp (STTup2 (VFlp vec1) (VFlp vec2)) =
  let l2 = MV.slice 0 sp vec2
      r2 = MV.slice sp (MV.length vec2 - sp) vec2 in
  STTup2 (VFlp vec1) (STTup2 (VFlp l2) (VFlp r2))

-----

vec2printState :: (ParThreadSafe parM, Show elt, PC.ParFuture parM,
                   PC.FutContents parM ()) =>
                  String -> V.ParVec2T s elt elt parM ()
vec2printState str = do
  STTup2 (VFlp v1) (VFlp v2) <- SS.get
  f1 <- liftST$ IMV.freeze v1
  f2 <- liftST$ IMV.freeze v2
  internalLiftIO$ putStrLn$ str ++ " " ++ show f1 ++ " " ++ show f2
  
vec21printState :: (ParThreadSafe parM, Show elt, PC.ParMonad parM) =>
                  String -> ParVec21T s elt parM ()
vec21printState str = do
  STTup2 (STTup2 (VFlp v1) (VFlp v2)) (VFlp v3) <- SS.get
  f1 <- liftST$ IMV.freeze v1
  f2 <- liftST$ IMV.freeze v2
  f3 <- liftST$ IMV.freeze v3
  internalLiftIO$ putStrLn$ str ++ " (" ++ show f1 ++ ", " ++ show f2 ++ "), " ++ show f3 ++ ")"


--------------------------------------------------------------------------------
#define CILK_SEQ
#ifdef CILK_SEQ
-- Requires that we selected STORABLE vectors above!
    
foreign import ccall unsafe "wrap_seqquick"
  c_seqquick :: Ptr CElmT -> CLong -> IO (Ptr CElmT)

-- | Sequential Cilk sort
cilkSeqSort :: IMV.Vector ElmT -> Par d s (IMV.Vector ElmT)
cilkSeqSort v = internalLiftIO $ do
  mutv <- IMV.thaw v
  MV.unsafeWith mutv $ \vptr ->
    c_seqquick (castPtr vptr) (fromIntegral $ IMV.length v)
  IMV.unsafeFreeze mutv

foreign import ccall unsafe "wrap_cilksort"
  c_cilksort ::  Ptr CElmT -> Ptr CElmT -> CLong -> IO CLong

foreign import ccall unsafe "wrap_seqmerge"
  c_seqmerge ::  Ptr CElmT -> CLong -> Ptr CElmT -> CLong -> Ptr CElmT -> IO ()

cilkSeqMerge :: IMV.Vector ElmT -> IMV.Vector ElmT -> IMV.Vector ElmT
cilkSeqMerge v1 v2 = unsafePerformIO $ do
    mutv1 <- thawit v1
    mutv2 <- thawit v2
    let len1 = IMV.length v1
	len2 = IMV.length v2
--    IMV.create $ do 
    do
       dest <- newMV (len1 + len2)
--       dest <- MIMV.unsafeNew (len1 + len2)
       MV.unsafeWith mutv1 $ \vptr1 ->
	MV.unsafeWith mutv2 $ \vptr2 ->         
	 MV.unsafeWith dest $ \vdest ->
	    c_seqmerge (castPtr vptr1) (fromIntegral len1) 
		       (castPtr vptr2) (fromIntegral len2) 
		       (castPtr vdest)
--       return dest
       IMV.unsafeFreeze dest

-- Element type being sorted:
type ElmT  = Word32
type CElmT = CUInt
#endif
-- End CILK block.
  
