{-# LANGUAGE TemplateHaskell, ParallelListComp #-}

-- | Test only the memoization functionality, corresponds to the @Data.LVar.Memo*@
-- modules.

module MemoTests where
import Control.LVish

import Data.LVar.CycGraph

import qualified Data.LVar.IVar as IV
import Data.Set as S
import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Framework.TH (testGroupGenerator)
import Test.Framework    (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase) -- For macro-expansion.

import Prelude as P

--------------------------------------------------------------------------------
-- Unit Tests
--------------------------------------------------------------------------------

-- This has changed a bunch, update it: [2013.10.23]
{-
cyc02 :: IO String
cyc02 = runParIO $ exploreGraph
          (\33 -> return [33])
          (\cyc k nbrs ->
            return ("key "++show k++" cyc "++show cyc++" nbrs "++show (P.map fst nbrs)))
          (33::Int)

cyc03 :: IO String
cyc03 = runParIO $ exploreGraph fn1 fn2 33
 where
   fn1 33 = return [44]
   fn1 44 = return [33]   
   fn2 cyc k nbrs = return ("key "++show k++" cyc "++show cyc++" nbrs "++show (P.map fst nbrs))

cyc04 :: IO String
cyc04 = runParIO $ exploreGraph fn1 hndlr 33
 where
   fn1 33 = return [44]
   fn1 44 = return [55]
   fn1 55 = return [33]

   hndlr True 55 nbrs = return "stop-at-55"
   hndlr cyc k nbrs = do
     vals <- mapM (IV.get . snd) nbrs
     return ("key="++show k++" cyc:"++show cyc++" nbrs:("++
             concat [ show k++","++str++" " | (k,_) <- nbrs | str <- vals ] ++")")
-}
   
-----------------------------------------------
-- Test the sequential cycle-detection approach
-----------------------------------------------

case_02seq :: Assertion
case_02seq = assertEqual "direct, one-node cycle, DFS" "cycle-33" cyc02seq
cyc02seq :: String
cyc02seq = runPar $ exploreGraph_seq
                   (\33 -> return$ Request 33 (\a -> return$ Done$ "33 finished: "++a))
                   (\k -> return$ "cycle-"++show k)
                   33

case_03seq :: Assertion
case_03seq = assertEqual "2-way cycle, DFS" "44 finished: cycle-33"  cyc03seq
cyc03seq :: String
cyc03seq = runPar $ exploreGraph_seq fn (\k -> return ("cycle-"++show k)) 44
 where
   fn 33 = return (Request 44 (\a -> return (Done$ "33 finished: "++a)))
   fn 44 = return (Request 33 (\a -> return (Done$ "44 finished: "++a)))

case_04seq :: Assertion
case_04seq = assertEqual "3-way cycle, DFS"
             "33 complete: 44 complete: cycle-55" cyc04seq

cyc04seq :: String
cyc04seq = runPar $ exploreGraph_seq fn (\k -> return ("cycle-"++show k)) 33
 where
   fn 33 = return (Request 44 (\a -> return (Done$ "33 complete: "++a)))
   fn 44 = return (Request 55 (\a -> return (Done$ "44 complete: "++a)))
   fn 55 = return (Request 33 (\a -> return (Done$ "55 complete: "++a)))

cyc05seq :: String
cyc05seq = runPar $ exploreGraph_seq fn (\k -> return ("cycle-"++show k)) 33
 where
   fn 33 = return (Request 44 (\a -> return (Done$ "33 complete: "++a)))
   fn 44 = return (Request 55 (\a -> return (Done$ "44 complete: "++a)))
   fn 55 = return (Request 33 (\a -> return (Done$ "55 complete: "++a)))


--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]
