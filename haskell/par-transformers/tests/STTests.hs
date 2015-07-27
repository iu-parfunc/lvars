{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes #-}

module STTests (tests, runTests) where

import           Control.LVish as LV
import           Control.Par.ST as PST
import qualified Control.Par.ST.Vec as V
-- import qualified Control.Par.ST.Vec2              as VV

import           Control.Monad
import           Control.Monad.ST
-- import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.Reader as R
import           Data.STRef
import           Data.Vector (freeze, toList)
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "ST tests"
  [ basicST
  , treeSplit
  , testCase "runParST test with recipe" $
    assertEqual "" 33 $
    LV.runPar $ runParST STUnitRecipe p4
  , testCase "runParSTCopy with a vec of refs" $
    assertEqual "" [99,88] t5
  ]

runTests :: IO ()
runTests = defaultMain tests

--------------------------------------------------------------------------------

basicST :: TestTree
basicST = testCase "basic formSTSplit usage" $
  assertEqual "basic forkSTSplit usage" [5,0,0,0,0,120,0,0,0,0] t0

t0 :: [Int]
t0 = LV.runPar $ V.runParVecT 10 p0

p0 :: (HasGet e, HasPut e) => ParST (MVectorFlp Int s1) Par e s [Int]
p0 = do
  V.set 0
  void $ V.forkSTSplit 5 (V.write 0 5) (V.write 0 120)
  raw <- V.reify
  frozen <- liftST $ freeze raw
  return $ toList frozen

-- case_v_t1 :: Assertion
-- case_v_t1 = assertEqual "testing transmute"
--             "fromList [0]fromList [0]" t1
--
-- t1 :: String
-- t1 = LV.runPar $ V.runParVecT 1 p1
--
-- p1 :: forall s e .
--       (HasGet e, HasPut e) => ParST (MVectorFlp Int s1) Par e s String
-- p1 = do
--   V.set 0
--   transmute undefined undefined
--   -- flip transmute (\v -> STTup2 v v) $ undefined
--     -- STTup2 rawL rawR <- V.reify
--     -- frozenL <- liftST $ freeze rawL
--     -- frozenR <- liftST $ freeze rawR
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
--   str <- transmute (\v -> STTup2 v v)
--     (do
--         VV.writeL 0 120
--         VV.writeR 1 5
--         (rawL,rawR) <- VV.reify
--         frozenL <- liftST$ freeze rawL
--         frozenR <- liftST$ freeze rawR
--         return$ show frozenL ++ show frozenR)
--
--   raw <- V.reify
--   frozen <- liftST$ freeze raw
--   let result = show frozen ++ " " ++ str
--   return result


--------------------------------------------------------------------------------

data Tree a s = Empty
              | Node (STRef s a) (Tree a s) (Tree a s)

instance STSplittable (Tree a) where
  type SplitIdx (Tree a) = ()

  splitST () Empty = error "splitST: cannot split empty tree!"
  splitST () (Node _ left right) = (left, right)

  -- | With these the practice is to simply pass a tree, and the STRefs get "freshened":
  data BuildRecipe (Tree a) = TreeRecipe (forall s . Tree a s)

  instantiateRecipe (TreeRecipe tr) = cloneState tr

  cloneState Empty = return Empty
  cloneState (Node ref l r) =
    do val <- readSTRef ref
       x   <- newSTRef val
       l'  <- cloneState l
       r'  <- cloneState r
       return $ Node x l' r'


p3 :: forall s ss e.
      (HasPut e, HasGet e) =>
      ParST (Tree Int ss) Par e s (Int, Int)
p3 = do
  x <- liftST $ newSTRef 10
  y <- liftST $ newSTRef 20
  z <- liftST $ newSTRef 30
  unsafeInstall (Node x (Node y Empty Empty) (Node z Empty Empty))
  void $ V.forkSTSplit ()
    (do Node r _ _ <- R.ask
        liftST $ writeSTRef r 99)
    (do Node r _ _ <- R.ask
        liftST $ writeSTRef r 101)

  (Node _ (Node y' _ _) (Node z' _ _)) <- R.ask
  a1 <- liftST $ readSTRef y'
  a2 <- liftST $ readSTRef z'
  return (a1,a2)

treeSplit :: TestTree
treeSplit = testCase "Splitting binary tree" $
  assertEqual "" (99,101) (runPar $ runParST (TreeRecipe Empty) p3)


p4 :: ParST (STUnit s0) Par e s Int
p4 = do STUnit <- reify
        unsafeInstall STUnit
        return 33


----------------------------------------


newtype VecOfIntRef s = VecOfIntRef (MV.MVector s (STRef s Int))

mapMV :: (a -> ST s b) -> MV.MVector s a -> ST s (MV.MVector s b)
mapMV fn vec = do
 newV <- MV.new (MV.length vec)
 forM_ [0 .. MV.length vec - 1] $ \ ix ->
   do val <- MV.read vec ix
      val' <- fn val
      MV.write newV ix val'
 return newV

-- | A correct instance at this type must copy the *elements*,
--   removing any sharing.
instance STSplittable VecOfIntRef where
  type SplitIdx (VecOfIntRef) = Int

  splitST pos (VecOfIntRef vec) =
    let (VFlp v1, VFlp v2) = splitST pos (VFlp vec)
    in (VecOfIntRef v1, VecOfIntRef v2)

  data BuildRecipe VecOfIntRef = VecOfIntRefRecipe (forall s . ArrayRecipe s Int)

--  instantiateRecipe (VecOfIntRefRecipe (ArrayRecipe len fn)) =
--    undefined

  cloneState (VecOfIntRef vec) =
    do v' <- mapMV (\ref ->
                     do v <- readSTRef ref
                        newSTRef v)
                   vec
       return $ VecOfIntRef v'

p5 :: (HasGet e, HasPut e) => ParST (VecOfIntRef s1) Par e s [Int]
p5 =
  do (x,y) <- forkSTSplit 5
               (do VecOfIntRef left <- PST.reify
                   ref <- liftST $ MV.read left 0
                   liftST $ writeSTRef ref 99
                   liftST $ readSTRef ref)
               (do VecOfIntRef right <- PST.reify
                   ref <- liftST $ MV.read right 0
                   liftST $ writeSTRef ref 88
                   liftST $ readSTRef ref)
     return [x,y]

t5 :: [Int]
t5 = runPar $ runParSTCopy mkInit p5
 where
  mkInit = do vec  <- MV.new 10
              ref  <- newSTRef 11
              MV.set vec ref
              return (VecOfIntRef vec)

{-
  -- This, correctly, gets a type error:
  p5 :: (HasGet e, HasPut e) => ParST (MVectorFlp (STRef s1 Int) s1) Par e s [Int]
  p5 = do
    sr <- liftST $ newSTRef (33::Int)
    V.set sr
    liftPar $ fork $ return ()
    void $ V.forkSTSplit 5
       (do sr' <- V.read 0
           liftST $ writeSTRef sr' 88)
       (do sr' <- V.read 0
           liftST $ writeSTRef sr' 99)
    return []
 -}
