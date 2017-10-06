{-@ LIQUID "--totality"        @-}
{-@ LIQUID "--higherorder"     @-}
{-@ LIQUID "--prune-unsorted"  @-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sum (Sum(..), vMonoidSum, add, zero) where

import Control.DeepSeq
import Data.VerifiedMonoid
import Data.VerifiedSemigroup
import Language.Haskell.Liquid.ProofCombinators

{-@ newtype Sum = Sum { getSum :: Int } @-}
newtype Sum = Sum { getSum :: Int }
  deriving (Eq, NFData)

{-@ assume getSumBeta :: x:Int -> { getSum (Sum x) == x } @-}
getSumBeta :: Int -> Proof
getSumBeta _ = simpleProof

{-@ assume sumEta :: x:Sum -> { Sum (getSum x) == x } @-}
sumEta :: Sum -> Proof
sumEta _ = simpleProof

{-@ axiomatize add @-}
add :: Sum -> Sum -> Sum
add x y = Sum (getSum x + getSum y)

{-@ addAssoc :: x:Sum -> y:Sum -> z:Sum
             -> {add x (add y z) == add (add x y) z} @-}
addAssoc :: Sum -> Sum -> Sum -> Proof
addAssoc x y z =   add x (add y z)
               ==. Sum (getSum x + getSum (Sum (getSum y + getSum z)))
               ==. Sum (getSum x + (getSum y + getSum z)) ? getSumBeta (getSum y + getSum z)
               ==. Sum ((getSum x + getSum y) + getSum z)
               ==. Sum (getSum (Sum (getSum x + getSum y)) + getSum z) ? getSumBeta (getSum x + getSum y)
               ==. add (add x y) z
               *** QED

vSemigroupSum :: VerifiedSemigroup Sum
vSemigroupSum = VerifiedSemigroup add addAssoc

{-@ axiomatize zero @-}
{-@ zero :: Sum @-}
zero :: Sum
zero = Sum 0

{-@ oneLident :: x:Sum -> {add zero x == x} @-}
oneLident :: Sum -> Proof
oneLident x =   add zero x
            ==. Sum (getSum (Sum 0) + getSum x)
            ==. Sum (0 + getSum x) ? getSumBeta 0
            ==. Sum (getSum x)
            ==. x ? sumEta x
            *** QED

{-@ oneRident :: x:Sum -> {add x zero == x} @-}
oneRident :: Sum -> Proof
oneRident x =   add x zero
            ==. Sum (getSum x + getSum (Sum 0))
            ==. Sum (getSum x + 0) ? getSumBeta 0
            ==. Sum (getSum x)
            ==. x ? sumEta x
            *** QED

vMonoidSum :: VerifiedMonoid Sum
vMonoidSum = VerifiedMonoid zero vSemigroupSum oneLident oneRident
