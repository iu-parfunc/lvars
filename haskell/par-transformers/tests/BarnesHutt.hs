
{-# LANGUAGE TypeFamilies #-}

import Control.LVish as LV
import Control.Par.ST
import qualified Control.Monad.State.Strict as S
import Data.STRef

-------------------------------------------------------------------------------

data Tree a s = Empty
              | Node (STRef s a) (Tree a s) (Tree a s)

instance STSplittable (Tree a) where
  -- | This is a binary tree
  type SplitIdx (Tree a) = ()
  splitST () Empty = error "splitST: cannot split empty tree!"
  splitST () (Node root left right) = (left,right)

-- A generic, safe library would have to take an inialization procedure of type
-- (forall s . ST s a), then it could use that, together with a description of the
-- shape of tree desired, to safely produce a tree populated with initial values.
-- The structure description process could be something like a zipper, operating only
-- "with gloves on".

p0 :: ParST (Tree Int s0) (LV.Par d s1) String
p0 = do

  x <- liftST $ newSTRef 10
  y <- liftST $ newSTRef 20
  z <- liftST $ newSTRef 30
  S.put (Node x (Node y Empty Empty) (Node z Empty Empty))
  forkSTSplit ()
    (do Node r _ _ <- S.get
        liftST$ writeSTRef r 99
        return ())
    (do Node r _ _ <- S.get
        liftST$ writeSTRef r 101
        return ())

  (Node x' (Node y' _ _) (Node z' _ _)) <- S.get 
  a1 <- liftST$ readSTRef y'
  a2 <- liftST$ readSTRef z'
  return$ show (a1,a2)

main :: IO ()
main = putStrLn $ runPar$ runParST undefined p0 

