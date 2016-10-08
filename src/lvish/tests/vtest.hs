{-@ LIQUID "--totality"        @-}
{-@ LIQUID "--higherorder"     @-}
{-@ LIQUID "--prune-unsorted"  @-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.VerifiedSemigroup
import Data.VerifiedMonoid
import Language.Haskell.Liquid.ProofCombinators
import Control.LVish
import Data.LVar.PureMap as PM
import Data.Map
import Data.Par.Map ()
import Control.Par.Class
import Criterion.Main

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

create_map :: Int -> IO (Map Int Int)
create_map sz = return $ fromList $ zip [1..sz] [1..sz]

main :: IO ()
main = do
  !mp1 <- create_map 10000
  !mp2 <- create_map 100000
  !mp3 <- create_map 1000000
  !_ <- vpmapFoldtest mp1
  !_ <- vpmapFoldtest mp2
  !_ <- vpmapFoldtest mp3
  defaultMain [
    bgroup "vpmapFoldtest"
    [ bench "10000"    $ nfIO $ vpmapFoldtest mp1
    , bench "100000"   $ nfIO $ vpmapFoldtest mp2
    , bench "1000000"  $ nfIO $ vpmapFoldtest mp3
    ],
    bgroup "pmapFoldtest"
    [ bench "10000"    $ nfIO $ pmapFoldtest mp1
    , bench "100000"   $ nfIO $ pmapFoldtest mp2
    , bench "1000000"  $ nfIO $ pmapFoldtest mp3
    ]
    ]

pmapFoldtest :: Map Int Int -> IO (Int)
pmapFoldtest mp = runParNonDet $ isND $ do
  !pmp <- PM.newMap mp
  !fmp <- PM.freezeMap pmp
  pmapFold (\(_ , v) -> return v) (\v1 v2 -> return $ v1 + v2) 0 fmp

vpmapFoldtest :: Map Int Int -> IO (Int)
vpmapFoldtest mp = runParNonDet $ isND $ do
  !pmp <- PM.newMap mp
  !fmp <- PM.freezeMap pmp
  !ans <- vpmapFold (\(_ , v) -> return $ Prod v) vMonoidProd fmp
  return $ unProd ans
