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
{-# LANGUAGE CPP #-}



module Main where

--module MergeSort 
--       (
--         runTests
--       )
--       where

import Control.LVish      as LV
import qualified Data.LVar.IVar as IV
import Control.Par.StateT as S

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.ST        (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Control.Monad.Trans (lift)
import Control.Monad.State (get,put)

import Control.Concurrent (threadDelay)

import Data.STRef
#ifdef UNBOXED

#else
import Data.Vector.Mutable as MV
import Data.Vector       (freeze)
import qualified Data.Vector as IMV
#endif

import qualified Data.Vector.Algorithms.Intro as VA
import Prelude hiding (read, length)
import System.IO.Unsafe (unsafePerformIO)

import GHC.Prim (RealWorld)

import qualified Control.Par.Class as PC

import Test.HUnit (Assertion, assert, assertEqual, assertBool, Counts(..))
import Test.Framework.Providers.HUnit
import Test.Framework -- (Test, defaultMain, testGroup)
import Test.Framework.TH (defaultMainGenerator)

import qualified Control.Par.ST.Vec2 as V
import Control.Par.ST

import qualified Control.Monad.State.Strict as SS

import Control.Par.Class.Unsafe (ParThreadSafe(unsafeParIO), internalLiftIO)

import System.Random.MWC (create, uniformVector, uniformR)

import Debug.Trace

runTests :: Bool
runTests = True

wrapper :: Int -> String
wrapper size = LV.runPar $ V.runParVec2T (0,size) $ do

  -- test setup: 
  randVec <- liftST$ mkRandomVec size
  
  STTup2 (VFlp left) (VFlp right) <- SS.get
  SS.put (STTup2 (VFlp randVec) (VFlp right))
  
  -- post condition: left array is sorted
  mergeSort
    
  (rawL, rawR) <- V.reify
  frozenL <- liftST$ freeze rawL
  
  return$ show frozenL

mergeSort :: (ParThreadSafe parM, PC.FutContents parM (),
              PC.ParFuture parM, Ord elt, Show elt) => 
             V.ParVec2T s1 elt elt parM ()  
mergeSort = do
--  vec2printState "start ms"
  len <- V.lengthL
  
  if len < 2 then do
    seqSortL
   else do  
    let sp = (len `quot` 2)              
--    trace "fork outer" $ forkSTSplit (sp,sp)
    forkSTSplit (sp,sp)
      (do len <- V.lengthL
          let sp = (len `quot` 2)
--          trace "!fork inner" $ forkSTSplit (sp,sp)
          forkSTSplit (sp,sp)
            mergeSort
            mergeSort
--          trace "mt2" $ mergeTo2 sp 8)
          mergeTo2 sp 8)
      (do len <- V.lengthL                                                                    
          let sp = (len `quot` 2)                                                              
--          trace "!fork inner" $ forkSTSplit (sp,sp)                             
          forkSTSplit (sp,sp)
            mergeSort         
            mergeSort
--          trace "mt2" $mergeTo2 sp 8)                           
          mergeTo2 sp 8)
--    trace "mt1" mergeTo1 sp 8
    mergeTo1 sp 8
       --  vec2printState "end ms"
--    mergeTo2 sp 8
    

seqSortL :: (Ord eltL, ParThreadSafe parM) => V.ParVec2T s eltL eltR parM ()
seqSortL = do
  STTup2 (VFlp vecL) (VFlp vecR) <- SS.get
  liftST$ VA.sort vecL

main = putStrLn $ wrapper 32

-- | Create a vector containing the numbers [0,N) in random order.
--mkRandomVec :: Int -> IO (MV.IOVector Float)
mkRandomVec :: Int -> ST s (MV.STVector s Float)
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

---------------

mergeTo2 :: (ParThreadSafe parM, Ord elt, Show elt, PC.FutContents parM (),
             PC.ParFuture parM) => 
            Int -> Int -> V.ParVec2T s elt elt parM ()
mergeTo2 sp threshold = do
  -- convert the state from (Vec, Vec) to ((Vec, Vec), Vec) then call normal parallel merge
--  vec2printState "mt2"
  transmute (morphToVec21 sp) (pMergeTo2 threshold)
--  vec2printState "mt2f"
                  
pMergeTo2 :: (ParThreadSafe parM, Ord elt, Show elt, PC.FutContents parM (),
              PC.ParFuture parM) =>
             Int -> ParVec21T s elt parM ()
pMergeTo2 threshold = do
--  vec21printState "start"
  -- threshold check here  
  len <- length2
  if len < threshold then
    sMergeTo2
   else do
    -- find the split points
    (splitL, splitR) <- findSplit indexL1 indexR1
    let mid = splitL + splitR
--    trace "split returned" $ return ()
    
    forkSTSplit ((splitL, splitR), mid)
      (pMergeTo2 threshold)
      (pMergeTo2 threshold)
--      (trace "pmerge" pMergeTo2 threshold)
--      (trace "pmerge" pMergeTo2 threshold)
    return ()
      
sMergeTo2 :: (ParThreadSafe parM, Ord elt, Show elt) =>       
            ParVec21T s elt parM ()
sMergeTo2 = do
  (lenL, lenR) <- lengthLR1
  sMergeTo2K 0 lenL 0 lenR 0
      
sMergeTo2K lBot lLen rBot rLen index 
  | lBot == lLen && rBot < rLen = do
    value <- indexR1 rBot
    write2 index value
    sMergeTo2K lBot lLen (rBot + 1) rLen (index + 1)      

sMergeTo2K lBot lLen rBot rLen index 
  | rBot >= rLen && lBot < lLen = do
    value <- indexL1 lBot
    write2 index value
    sMergeTo2K (lBot + 1) lLen rBot rLen (index + 1)
    
sMergeTo2K lBot lLen rBot rLen index     
  | index >= (lLen + rLen) = do
    return ()

sMergeTo2K lBot lLen rBot rLen index = do
  left <- indexL1 lBot
  right <- indexR1 rBot
  if left < right then do
    write2 index left
    sMergeTo2K (lBot + 1) lLen rBot rLen (index + 1)
   else do
    write2 index right
    sMergeTo2K lBot lLen (rBot + 1) rLen (index + 1)    
    
mergeTo1 sp threshold = do
--  vec2printState "mt1"
  V.swapState
--  vec2printState "mt1m"
  (mergeTo2 sp threshold)
  V.swapState
--  vec2printState "mt1f"
    

-- NOTE: THIS FUNCTION IS BORKED! It has issues with very small input
-- lengths and gets stuck. It might be a general problem with the
-- parallel merge algorithm, but is most likely something wrong about
-- how this function operates. We are being *very dangerous* and just
-- not using a small threshold that triggers this issue.
--         
findSplit :: (ParThreadSafe parM, Ord elt, Show elt,
              PC.ParMonad parM) => 
             (Int -> ParVec21T s elt parM elt) ->
             (Int -> ParVec21T s elt parM elt)->
             ParVec21T s elt parM (Int, Int)

findSplit indexLeft indexRight = do                                 
  
  (lLen, rLen) <- lengthLR1    
    
--  trace "splits fault" $ split 0 lLen 0 rLen 
  split 0 lLen 0 rLen
      where
        split lLow lHigh rLow rHigh = do
          let lIndex = (lLow + lHigh) `div` 2
              rIndex = (rLow + rHigh) `div` 2
              
--          vec21printState ("(" ++ show lIndex ++ ", " ++ show rIndex ++ ")")
            
--          leftSub1 <- indexLeft (lIndex - 1)
          left <- indexLeft lIndex
--          rightSub1 <- indexRight (rIndex - 1)
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
             
type ParVec21T s elt parM ans = ParST (STTup2 
                                       (STTup2 (MVectorFlp elt) 
                                               (MVectorFlp elt))
                                       (MVectorFlp elt) s)
                                      parM ans
                                              
type ParVec12T s elt parM ans = ParST (STTup2 
                                       (MVectorFlp elt)
                                       (STTup2 (MVectorFlp elt) 
                                               (MVectorFlp elt)) s)
                                      parM ans
    
                       
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
  f1 <- liftST$ freeze v1
  f2 <- liftST$ freeze v2
  internalLiftIO$ putStrLn$ str ++ " " ++ show f1 ++ " " ++ show f2
  
vec21printState :: (ParThreadSafe parM, Show elt, PC.ParMonad parM) =>
                  String -> ParVec21T s elt parM ()
vec21printState str = do
  STTup2 (STTup2 (VFlp v1) (VFlp v2)) (VFlp v3) <- SS.get
  f1 <- liftST$ freeze v1
  f2 <- liftST$ freeze v2
  f3 <- liftST$ freeze v3
  internalLiftIO$ putStrLn$ str ++ " (" ++ show f1 ++ ", " ++ show f2 ++ "), " ++ show f3 ++ ")"


