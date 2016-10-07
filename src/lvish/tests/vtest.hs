{-@ LIQUID "--totality"        @-}
{-@ LIQUID "--higherorder"     @-}
{-@ LIQUID "--prune-unsorted"  @-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Semigroup
import Data.VerifiedSemigroup
import Data.VerifiedMonoid
import Language.Haskell.Liquid.ProofCombinators
import Control.LVish
import Data.LVar.PureMap
import Data.Time.Clock
import System.IO.Unsafe
import Data.Par.Map ()
import Control.Par.Class
import Control.Par.Class.Unsafe (internalLiftIO)

{-@ newtype Prod = Prod { unProd :: Int } @-}
newtype Prod = Prod { unProd :: Int }
  deriving (Show, Eq, Ord)

{-@ axiomatize unProd @-}

{-@ assume unProdBeta :: x:Int -> { unProd (Prod x) == x } @-}
unProdBeta :: Int -> Proof
unProdBeta _ = simpleProof

{-@ assume prodEta :: x:Prod -> { Prod (unProd x) == x } @-}
prodEta :: Prod -> Proof
prodEta _ = simpleProof

{-@ axiomatize add @-}
add :: Prod -> Prod -> Prod
add x y = Prod (unProd x + unProd y)

{-@ addAssoc :: x:Prod -> y:Prod -> z:Prod
             -> {add x (add y z) == add (add x y) z} @-}
addAssoc :: Prod -> Prod -> Prod -> Proof
addAssoc x y z =   add x (add y z)
               ==. Prod (unProd x + unProd (Prod (unProd y + unProd z)))
               ==. Prod (unProd x + (unProd y + unProd z)) ? unProdBeta (unProd y + unProd z)
               ==. Prod ((unProd x + unProd y) + unProd z)
               ==. Prod (unProd (Prod (unProd x + unProd y)) + unProd z) ? unProdBeta (unProd x + unProd y)
               ==. add (add x y) z
               *** QED

vSemigroupProd :: VerifiedSemigroup Prod
vSemigroupProd = VerifiedSemigroup add addAssoc

{-@ axiomatize zero @-}
{-@ zero :: Prod @-}
zero :: Prod
zero = Prod 0

{-@ oneLident :: x:Prod -> {add zero x == x} @-}
oneLident :: Prod -> Proof
oneLident x =   add zero x
            ==. Prod (unProd (Prod 0) + unProd x)
            ==. Prod (0 + unProd x) ? unProdBeta 0
            ==. Prod (unProd x)
            ==. x ? prodEta x
            *** QED

{-@ oneRident :: x:Prod -> {add x zero == x} @-}
oneRident :: Prod -> Proof
oneRident x =   add x zero
            ==. Prod (unProd x + unProd (Prod 0))
            ==. Prod (unProd x + 0) ? unProdBeta 0
            ==. Prod (unProd x)
            ==. x ? prodEta x
            *** QED

vMonoidProd :: VerifiedMonoid Prod
vMonoidProd = VerifiedMonoid zero vSemigroupProd oneLident oneRident

size :: Int
size = 10000000

main :: IO ()
main = do
  !start <- getCurrentTime
  ans <- runParNonDet $ isND $ do
    mp <- newFromList $ zip [1..size] [1..size]
    fmp <- freezeMap mp
    vpmapFold (\(_ , v) -> return $ Prod v)
              vMonoidProd fmp
  !end <- getCurrentTime
  putStrLn $ show ans
  putStrLn $ (show ((realToFrac $ diffUTCTime end start) * 1000.0 ::Double)) ++ " ms"
  return ()
