{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module STTests (tests, runTests) where

import           Control.LVish                  as LV
import qualified Control.Par.ST                 as PST
import qualified Control.Par.ST.Vec             as V
-- import qualified Control.Par.ST.Vec2              as VV

import           Control.Monad
import qualified Control.Monad.State.Strict     as S
import           Data.STRef
import           Data.Vector                    (freeze)

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH              (testGroupGenerator)
import           Test.HUnit                     (Assertion, assertEqual)

--------------------------------------------------------------------------------

case_v_t0 :: Assertion
case_v_t0 = assertEqual "basic forkSTSplit usage"
            "fromList [5,0,0,0,0,120,0,0,0,0]" t0

t0 :: String
t0 = LV.runPar $ V.runParVecT 10 p0

p0 :: (HasGet e, HasPut e) => PST.ParST (PST.MVectorFlp Int s1) Par e s String
p0 = do
  V.set 0
  void $ V.forkSTSplit 5 (V.write 0 5) (V.write 0 120)
  raw <- V.reify
  frozen <- PST.liftST $ freeze raw
  return $ show frozen

-- case_v_t1 :: Assertion
-- case_v_t1 = assertEqual "testing transmute"
--             "fromList [0]fromList [0]" t1
--
-- t1 :: String
-- t1 = LV.runPar $ V.runParVecT 1 p1
--
-- p1 :: forall s e .
--       (HasGet e, HasPut e) => PST.ParST (PST.MVectorFlp Int s1) Par e s String
-- p1 = do
--   V.set 0
--   PST.transmute undefined undefined
--   -- flip PST.transmute (\v -> PST.STTup2 v v) $ undefined
--     -- PST.STTup2 rawL rawR <- V.reify
--     -- frozenL <- PST.liftST $ freeze rawL
--     -- frozenR <- PST.liftST $ freeze rawR
--     -- return $ show frozenL ++ show frozenR

-- case_v_t2 :: Assertion
-- case_v_t2 = assertEqual "testing transmute with effects"
--                  "fromList [120,5] fromList [120,5]fromList [120,5]" t2
--
-- t2 :: String
-- t2 = LV.runPar $ V.runParVecT 2 p2
--
-- -- | FIXME: This is an example of what we should NOT be allowed to do.
-- --   Arbitrary transmute can't be allowed, it allows aliasing.
-- --   However, controlled zooming in and out will be allowed.
-- p2 :: V.ParVecT s1 Int (LV.Par e s0) String
-- p2 = do
--   V.set 0
--   str <- PST.transmute (\v -> PST.STTup2 v v)
--     (do
--         VV.writeL 0 120
--         VV.writeR 1 5
--         (rawL,rawR) <- VV.reify
--         frozenL <- PST.liftST$ freeze rawL
--         frozenR <- PST.liftST$ freeze rawR
--         return$ show frozenL ++ show frozenR)
--
--   raw <- V.reify
--   frozen <- PST.liftST$ freeze raw
--   let result = show frozen ++ " " ++ str
--   return result

--------------------------------------------------------------------------------

data Tree a s = Empty
              | Node (STRef s a) (Tree a s) (Tree a s)

instance PST.STSplittable (Tree a) where
  type SplitIdx (Tree a) = ()

  splitST () Empty = error "splitST: cannot split empty tree!"
  splitST () (Node _ left right) = (left, right)

p2 :: forall s ss e.
      (HasPut e, HasGet e) =>
      PST.ParST (Tree Integer ss) Par e s String
p2 = do
  x <- PST.liftST $ newSTRef 10
  y <- PST.liftST $ newSTRef 20
  z <- PST.liftST $ newSTRef 30
  S.put (Node x (Node y Empty Empty) (Node z Empty Empty))
  void $ V.forkSTSplit ()
    (do Node r _ _ <- S.get
        PST.liftST $ writeSTRef r 99)
    (do Node r _ _ <- S.get
        PST.liftST $ writeSTRef r 101)

  (Node _ (Node y' _ _) (Node z' _ _)) <- S.get
  a1 <- PST.liftST $ readSTRef y'
  a2 <- PST.liftST $ readSTRef z'
  return $ show (a1,a2)

case_treeSplit :: Assertion
case_treeSplit =
  assertEqual "splitting binary tree" "(99,101)" (runPar $ PST.runParST undefined p2)

--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]
