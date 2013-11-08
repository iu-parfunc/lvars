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
import Data.Vector.Mutable as MV
import Data.Vector       (freeze)
import qualified Data.Vector.Storable as IMV
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
import qualified Control.Par.ST as PST

import qualified Control.Monad.State.Strict as SS

import Control.Par.Class.Unsafe (ParThreadSafe(unsafeParIO))

import System.Random.MWC (create, uniformVector, uniformR)

import Debug.Trace

runTests :: Bool
runTests = True

wrapper :: String
wrapper = LV.runPar $ V.runParVec2T (17,17) $ do
  -- hack: put our input vector into the state
--  vecR <- PST.liftST$ MV.new $ length vecL
--  SS.put (PST.STTup2 (PST.VFlp vecL) (PST.VFlp vecR))
  

  
  V.setL 1.0
  V.setR 0.1
  
  -- toy vector to start with
  V.writeL 0 120.0
  V.writeL 1 5.0
  V.writeL 2 3.0
  V.writeL 3 0.222
  
  -- input: unsorted array in Left position
  -- output: sorted array in Left position
  mergeSort
    
  (rawL, rawR) <- V.reify
  frozenL <- PST.liftST$ freeze rawL
  
  return$ show frozenL

mergeSort :: (ParThreadSafe parM, PC.FutContents parM (),
              PC.ParFuture parM, Ord elt, Show elt) => 
             V.ParVec2T s1 elt elt parM ()  
mergeSort = do
  len <- V.lengthL
  
  if len < 4 then do
    seqSortL
   else do  
    let sp = (len `quot` 2)              
    PST.forkSTSplit (sp,sp)
      (do len <- V.lengthL
          let sp = (len `quot` 2)
          PST.forkSTSplit (sp,sp)
            mergeSort
            mergeSort
          merge sp len)
      (do len <- V.lengthL                                                                    
          let sp = (len `quot` 2)                                                              
          PST.forkSTSplit (sp,sp)                             
            mergeSort         
            mergeSort
          merge sp len)                           
    merge2 sp len
    
merge sp len = do
  (vecL, vecR) <- V.reify
  lf <- PST.liftST$ freeze vecL
  rf <- PST.liftST$ freeze vecR
  trace ("merge input: " ++ show lf ++ " " ++ show rf) return ()
  trace (show sp ++ " " ++ show len) $ mergeK 0 sp sp (len - 1) 0 -- output: sorted Right
  (vecL, vecR) <- V.reify                                                                      
  lf <- PST.liftST$ freeze vecL                                                                
  rf <- PST.liftST$ freeze vecR
  trace ("merge output: " ++ show lf ++ " " ++ show rf) return ()     
      
mergeK lBot rBot sp top index
  | lBot == sp && rBot <= top = do
    value <- V.readL rBot
    V.writeR index value
    trace "@" mergeK lBot (rBot + 1) sp top (index + 1)

mergeK lBot rBot sp top index 
  | rBot > top && lBot < sp = do -- just copy over left
    value <- V.readL lBot
    V.writeR index value
    trace (">" ++ show lBot ++ " " ++ show rBot ++ " " ++ show index) $ mergeK (lBot + 1) rBot sp top (index + 1)

mergeK lBot rBot sp top index
  | index > top = do
    -- there is no more copying to do, so we are done
    trace "< returning >" $ return ()
  
mergeK lBot rBot sp top index = do
  -- given two slices of a vec1, merge them into vec2
  left <- V.readL lBot
  right <- V.readL rBot
  if left < right then do
    V.writeR index left
    trace ("#" ++ show lBot ++ " " ++ show rBot) $ mergeK (lBot + 1) rBot sp top (index + 1)
   else do
    V.writeR index right
    trace ("!" ++ show lBot ++ " " ++ show rBot) $ mergeK lBot (rBot + 1) sp top (index + 1)
    
merge2 sp len = do
  (vecL, vecR) <- V.reify
  lf <- PST.liftST$ freeze vecL
  rf <- PST.liftST$ freeze vecR
  trace ("merge input: " ++ show lf ++ " " ++ show rf) return ()
  trace (show sp ++ " " ++ show len) $ mergeK2 0 sp sp (len - 1) 0 -- output: sorted Right
  (vecL, vecR) <- V.reify                                                                      
  lf <- PST.liftST$ freeze vecL                                                                
  rf <- PST.liftST$ freeze vecR
  trace ("merge output: " ++ show lf ++ " " ++ show rf) return ()     
      
mergeK2 lBot rBot sp top index
  | lBot == sp && rBot <= top = do
    value <- V.readR rBot
    V.writeL index value
    trace "@" mergeK2 lBot (rBot + 1) sp top (index + 1)

mergeK2 lBot rBot sp top index 
  | rBot > top && lBot < sp = do -- just copy over left
    value <- V.readR lBot
    V.writeL index value
    trace (">" ++ show lBot ++ " " ++ show rBot ++ " " ++ show index) $ mergeK2 (lBot + 1) rBot sp top (index + 1)

mergeK2 lBot rBot sp top index
  | index > top = do
    -- there is no more copying to do, so we are done
    trace "< returning >" $ return ()
  
mergeK2 lBot rBot sp top index = do
  -- given two slices of a vec1, merge them into vec2
  left <- V.readR lBot
  right <- V.readR rBot
  if left < right then do
    V.writeL index left
    trace ("#" ++ show lBot ++ " " ++ show rBot) $ mergeK2 (lBot + 1) rBot sp top (index + 1)
   else do
    V.writeL index right
    trace ("!" ++ show lBot ++ " " ++ show rBot) $ mergeK2 lBot (rBot + 1) sp top (index + 1)

    
    
seqSortL :: (Ord eltL, ParThreadSafe parM) => V.ParVec2T s eltL eltR parM ()
seqSortL = do
  PST.STTup2 (PST.VFlp vecL) (PST.VFlp vecR) <- SS.get
  PST.liftST$ VA.sort vecL

main = putStrLn wrapper