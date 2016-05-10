{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Tests for the Data.LVar.PureMap and Data.LVar.SLMap modules.

module CtrieMapTests where

-- import qualified Data.LVar.SLSet as IS
import qualified Data.LVar.CtrieMap as IM

#include "CommonMapTests.hs"

type TheMap k s v = IM.IMap k s v 

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "" [tests_here, tests_common ]

tests_here :: TestTree
tests_here = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain tests

{-

------------------------------------------------------------------------------------------
-- Show instances
------------------------------------------------------------------------------------------

-- | It happens that these come out in the opposite order from the Pure one:
_case_show02 :: Assertion
_case_show02 = assertEqual "show for SLMap" "{IMap: (\"key2\",44), (\"key1\",33)}" show02
show02 :: String
show02 = show$ runParThenFreeze $ do
  mp <- IM.newEmptyMap
  SM.insert "key1" (33::Int) mp
  SM.insert "key2" (44::Int) mp  
  return mp

-}
