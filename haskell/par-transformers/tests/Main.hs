
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
-- [2014.09.29] Temp, disabling while refactoring the effect system:
--       , CancelTests.tests
       ]
