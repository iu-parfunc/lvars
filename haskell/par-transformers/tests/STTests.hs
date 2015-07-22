{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes #-}

module STTests (tests, runTests) where

import           Control.LVish              as LV
import qualified Control.Par.ST             as PST
import qualified Control.Par.ST.Vec         as V
-- import qualified Control.Par.ST.Vec2              as VV

import           Control.Monad
-- import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.Reader       as R
import           Data.STRef
import           Data.Vector                (freeze, toList)
import           Test.Tasty
import           Test.Tasty.HUnit

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "ST tests"
  [ basicST
  , treeSplit
  , testCase "runParST test with recipe" $
    assertEqual "" 33 $
    LV.runPar $ PST.runParST PST.STUnitRecipe p3
  ]

runTests :: IO ()
runTests = defaultMain tests

--------------------------------------------------------------------------------

basicST :: TestTree
basicST = testCase "basic formSTSplit usage" $
  assertEqual "basic forkSTSplit usage" [5,0,0,0,0,120,0,0,0,0] t0

t0 :: [Int]
t0 = LV.runPar $ V.runParVecT 10 p0

p0 :: (HasGet e, HasPut e) => PST.ParST (PST.MVectorFlp Int s1) Par e s [Int]
p0 = do
  V.set 0
  void $ V.forkSTSplit 5 (V.write 0 5) (V.write 0 120)
  raw <- V.reify
  frozen <- PST.liftST $ freeze raw
  return $ toList frozen

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

p3 :: PST.ParST (PST.STUnit s0) Par e s Int
p3 = do PST.STUnit <- PST.reify
        PST.unsafeInstall PST.STUnit
        return 33

--------------------------------------------------------------------------------

data Tree a s = Empty
              | Node (STRef s a) (Tree a s) (Tree a s)

instance PST.STSplittable (Tree a) where
  type SplitIdx (Tree a) = ()

  splitST () Empty = error "splitST: cannot split empty tree!"
  splitST () (Node _ left right) = (left, right)

  -- | With these the practice is to simply pass a tree, and the STRefs get "freshened":
  data BuildRecipe (Tree a) = TreeRecipe (forall s . Tree a s)

  instantiateRecipe (TreeRecipe tr) = PST.cloneState tr

  cloneState Empty = return Empty
  cloneState (Node ref l r) =
    do val <- readSTRef ref
       x   <- newSTRef val
       l'  <- PST.cloneState l
       r'  <- PST.cloneState r
       return $ Node x l' r'


p2 :: forall s ss e.
      (HasPut e, HasGet e) =>
      PST.ParST (Tree Int ss) Par e s (Int, Int)
p2 = do
  x <- PST.liftST $ newSTRef 10
  y <- PST.liftST $ newSTRef 20
  z <- PST.liftST $ newSTRef 30
  PST.unsafeInstall (Node x (Node y Empty Empty) (Node z Empty Empty))
  void $ V.forkSTSplit ()
    (do Node r _ _ <- R.ask
        PST.liftST $ writeSTRef r 99)
    (do Node r _ _ <- R.ask
        PST.liftST $ writeSTRef r 101)

  (Node _ (Node y' _ _) (Node z' _ _)) <- R.ask
  a1 <- PST.liftST $ readSTRef y'
  a2 <- PST.liftST $ readSTRef z'
  return (a1,a2)

treeSplit :: TestTree
treeSplit = testCase "Splitting binary tree" $
  assertEqual "" (99,101) (runPar $ PST.runParST (TreeRecipe Empty) p2)
