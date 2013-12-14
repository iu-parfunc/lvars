
-- | A module with helper functions that are used elsewhere in the LVish repository.

module Data.UtilInternal
       (
         traverseWithKey_,
         Traverse_(..)
       )
       where

import           Control.Applicative
import           Control.Monad (void)
import           Data.Monoid (Monoid(..))
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- Helper code.
--------------------------------------------------------------------------------
   
-- Version of traverseWithKey_ from Shachaf Ben-Kiki
-- (See thread on Haskell-cafe.)
-- Avoids O(N) allocation when traversing for side-effect.

newtype Traverse_ f = Traverse_ { runTraverse_ :: f () }
instance Applicative f => Monoid (Traverse_ f) where
  mempty = Traverse_ (pure ())
  Traverse_ a `mappend` Traverse_ b = Traverse_ (a *> b)
-- Since the Applicative used is Const (newtype Const m a = Const m), the
-- structure is never built up.
--(b) You can derive traverseWithKey_ from myfoldMapWithKey, e.g. as follows:

{-# INLINE traverseWithKey_ #-}
traverseWithKey_ :: Applicative f => (k -> a -> f ()) -> M.Map k a -> f ()
traverseWithKey_ f = runTraverse_ .
                     myfoldMapWithKey (\k x -> Traverse_ (void (f k x)))

{-# INLINE myfoldMapWithKey #-}
myfoldMapWithKey :: Monoid r => (k -> a -> r) -> M.Map k a -> r
myfoldMapWithKey f = getConst . M.traverseWithKey (\k x -> Const (f k x))
