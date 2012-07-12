{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Main where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Algebra.Lattice (joinLeq)
import Language.LambdaPar.Common

import Tests

main = $(defaultMainGenerator)


--------------------------------------------------------------------------------
-- QuickCheck properties:

-- prop_reverse xs = reverse (reverse xs) == (xs::[Elt])

--------------------------------------------------------------------------------
-- HUnit tests:

case_1 =   True  @=? joinLeq  Empty   (Full 3)
case_2 =   True  @=? joinLeq (Full 3) (Full 3)
case_3 =   False @=? joinLeq (Full 3) (Full 4)

case_p0 =  True  @=? testOne p0
case_p1 =  True  @=? testOne p1
case_p2 =  True  @=? testOne p2
case_p3 =  True  @=? testOne p3
case_p4 =  True  @=? testOne p4

