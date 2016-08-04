module Main where

import qualified CancelTests
import qualified STTests
import           Test.Tasty

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain alltests

alltests :: TestTree
alltests = testGroup "par-transformers tests"
       [ STTests.tests
       , CancelTests.tests
       ]
