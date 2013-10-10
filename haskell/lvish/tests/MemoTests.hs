{-# LANGUAGE TemplateHaskell #-}

-- | Test only the memoization functionality, corresponds to the @Data.LVar.Memo*@
-- modules.

module MemoTests where
import Control.LVish
import Data.LVar.MemoCyc
import Data.Set as S
import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Framework.TH (testGroupGenerator)
import Test.Framework    (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase) -- For macro-expansion.

--------------------------------------------------------------------------------
-- Unit Tests
--------------------------------------------------------------------------------

cyc02 :: String
cyc02 = runPar $ do
  m <- makeMemoFixedPoint (\_ -> return (Request 33 (\_ -> return (Done "nocycle"))))
                          (\_ -> return "cycle")
  getMemo m 33

cyc03 :: (String, String, S.Set Int, S.Set Int)
cyc03 = runPar $ do
  m  <- makeMemoFixedPoint fn (\_ -> return "cycle")
  s1 <- getMemo m 33
  s2 <- getMemo m 44
  r1 <- getReachable m 33
  r2 <- getReachable m 44
  return (s1, s2, r1, r2)
 where
   fn 33 = return (Request 44 (\_ -> return (Done "33 finished, no cycle")))
   fn 44 = return (Request 33 (\_ -> return (Done "44 finished, no cycle")))

cyc04 :: (String, S.Set Int, S.Set Int, S.Set Int)
cyc04 = runPar $ do
  m <- makeMemoFixedPoint fn (\_ -> return "cycle")
  bl <- getMemo m 33
  r1 <- getReachable m 33
  r2 <- getReachable m 44
  r3 <- getReachable m 55
  return (bl, r1, r2, r3)
 where
   fn 33 = return (Request 44 (\_ -> return (Done "33 complete")))
   fn 44 = return (Request 55 (\_ -> return (Done "44 complete")))
   fn 55 = return (Request 33 (\_ -> return (Done "55 complete")))

-----------------------------------------------
-- Test the sequential cycle-detection approach
-----------------------------------------------

case_02B :: Assertion
case_02B = assertEqual "direct, one-node cycle, DFS" "cycle-33" cyc02B
cyc02B :: String
cyc02B = runPar $ makeMemoFixedPoint_seq
                   (\33 -> return$ Request 33 (\a -> return$ Done$ "33 finished: "++a))
                   (\k -> return$ "cycle-"++show k)
                   33

case_03B :: Assertion
case_03B = assertEqual "2-way cycle, DFS" "44 finished: cycle-33"  cyc03B
cyc03B :: String
cyc03B = runPar $ makeMemoFixedPoint_seq fn (\k -> return ("cycle-"++show k)) 44
 where
   fn 33 = return (Request 44 (\a -> return (Done$ "33 finished: "++a)))
   fn 44 = return (Request 33 (\a -> return (Done$ "44 finished: "++a)))

case_04B :: Assertion
case_04B = assertEqual "3-way cycle, DFS"
             "33 complete: 44 complete: cycle-55" cyc04B

cyc04B :: String
cyc04B = runPar $ makeMemoFixedPoint_seq fn (\k -> return ("cycle-"++show k)) 33
 where
   fn 33 = return (Request 44 (\a -> return (Done$ "33 complete: "++a)))
   fn 44 = return (Request 55 (\a -> return (Done$ "44 complete: "++a)))
   fn 55 = return (Request 33 (\a -> return (Done$ "55 complete: "++a)))

-- main = print cyc02

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]
