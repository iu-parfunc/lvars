{-@ LIQUID "--totality"        @-}
{-@ LIQUID "--higherorder"     @-}
{-@ LIQUID "--prune-unsorted"  @-}
{-@ LIQUID "--automatic-instances=liquidinstances" @-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sum (Sum(..), vMonoidSum, add, zero) where

import Control.DeepSeq
import Data.VerifiedMonoid
import Data.VerifiedSemigroup
import Language.Haskell.Liquid.ProofCombinators

{-@ newtype Sum = Sum { getSum :: Int } @-}
newtype Sum = Sum { getSum :: Int }
  deriving (Eq, NFData)

{-@ assume sumEta :: x:Sum -> { Sum (getSum x) == x } @-}
sumEta :: Sum -> Proof
sumEta _ = simpleProof

{-@ axiomatize add @-}
add :: Sum -> Sum -> Sum
add x y = Sum (getSum x + getSum y)

{-@ addAssoc :: x:Sum -> y:Sum -> z:Sum
             -> {add x (add y z) == add (add x y) z} @-}
addAssoc :: Sum -> Sum -> Sum -> Proof
addAssoc x y z = simpleProof

vSemigroupSum :: VerifiedSemigroup Sum
vSemigroupSum = VerifiedSemigroup add addAssoc

{-@ axiomatize zero @-}
{-@ zero :: Sum @-}
zero :: Sum
zero = Sum 0

{-@ oneLident :: x:Sum -> {add zero x == x} @-}
oneLident :: Sum -> Proof
oneLident = sumEta

{-@ oneRident :: x:Sum -> {add x zero == x} @-}
oneRident :: Sum -> Proof
oneRident = sumEta

vMonoidSum :: VerifiedMonoid Sum
vMonoidSum = VerifiedMonoid zero vSemigroupSum oneLident oneRident
