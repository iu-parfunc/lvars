
module Main where

import qualified STTests
import qualified CancelTests
import Test.Framework 

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain alltests

alltests :: [Test]
alltests = 
       [ STTests.tests
       , CancelTests.tests
       ]
