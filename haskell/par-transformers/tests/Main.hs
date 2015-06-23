module Main where

import qualified CancelTests
import qualified STTests
import           Test.Framework

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain alltests

alltests :: [Test]
alltests =
       [ STTests.tests
       , CancelTests.tests
       ]
