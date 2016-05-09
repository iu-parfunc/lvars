
-- This is NOT a full Haskell module.
-- This is a slice of source code that is #included into multiple files.

-- ASSUMES: module "IM" refers to the Map implementation.

-- This code is for testing write-only operations:

import Test.Tasty.HUnit 
import Test.Tasty (TestTree, testGroup, defaultMain)
--import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Tasty.TH (testGroupGenerator)
import qualified Test.HUnit as HU
import TestHelpers2 as T
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forM)
import Data.Traversable (traverse)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Word
import Data.IORef
import System.Random
import Test.QuickCheck
import Test.Tasty.QuickCheck (testProperty)

-- Some maps need Hashable instead of Ord:
import Data.Hashable

import           Control.LVish
import           Control.LVish.DeepFrz (DeepFrz(..), Frzn, NonFrzn, Trvrsbl,
                                        runParThenFreeze, runParThenFreezeIO)
import qualified Control.LVish.Internal as I
import qualified Control.Par.Class as PC
import qualified Data.LVar.IVar as IV
import Data.LVar.Generic ( LVarData1(sortFrzn) , AFoldable(..) )
import GHC.Conc (numCapabilities)

--------------------------------------------------------------------------------
-- Quickcheck properties:

-- Build simple properties that amount to the identity function, but perform
-- conversions in the middle.
mkSimpleIdentityProp ::
  (Ord v, Ord k, Hashable k, F.Foldable t, DeepFrz a, FrzType a ~ t v) =>
  (TheMap k NonFrzn v -> Par (Ef P G NF B NI) NonFrzn a) -> [(k, v)] -> Bool
mkSimpleIdentityProp trans prs =
  (L.sort$ L.nub$ map snd prs) == 
  (L.sort$ L.nub $ F.toList $
   runParThenFreeze $ isDet $ do
     mp0 <- IM.newFromList prs
     trans mp0)

prop_tofrom :: [Int] -> Bool 
prop_tofrom ls = mkSimpleIdentityProp return (zip ls ls)

tests_writeOnly :: TestTree
tests_writeOnly = testGroup "Common" [ $(testGroupGenerator) ] 

--------------------------------------------------------------------------------


fillNFreezeChunks :: [(Int, Int)] -> TheMap Int Frzn Int
-- fmap IM.freezeMap $ 
fillNFreezeChunks chunks = runParThenFreeze $ do
  mp <- IM.newEmptyMap 
  forM chunks $ \ (start,end) -> do
    fork $ do
      T.for_ (start, end)$ \n -> IM.insert n n mp
  return mp

fillNFreeze :: Int -> TheMap Int Frzn Int
fillNFreeze sz = fillNFreezeChunks (splitRange numCapabilities (0,sz-1))

case_fillFreeze1K :: Assertion
case_fillFreeze1K = assertEqual "fill and then freeze"
    (sum [0..sz-1])
    (case sortFrzn (fillNFreeze sz) of AFoldable x -> F.foldl' (+) 0 x)
 where sz = 1000
