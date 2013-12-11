{-# LANGUAGE BangPatterns #-}

{-|

This module provides sets that allow both addition and removal of
elements.  This is possible because, under the hood, it's represented
with two monotonically growing sets, one for additions and one for
removals.  It is inspired by /2P-Sets/ from the literature on
/conflict-free replicated data types/.

 -}
module Data.LVar.AddRemoveSet
       (
         AddRemoveSet,
         newEmptySet, newSet, newFromList,
         insert, waitAddedElem, waitAddedSize,
         remove, waitRemovedElem, waitRemovedSize,
         
       ) where
import qualified Data.Set as S
import           Control.LVish
import           Control.LVish.Internal
import qualified Data.LVar.SLSet as SLS


-- | The set datatype.
data AddRemoveSet s a =
     AddRemoveSet !(SLS.ISet s a)
                  !(SLS.ISet s a)

-- | Create a new, empty `AddRemoveSet`.
newEmptySet :: Ord a => Par d s (AddRemoveSet s a)
newEmptySet = undefined

-- | Create a new `AddRemoveSet` populated with initial elements.
newSet :: Ord a => S.Set a -> Par d s (AddRemoveSet s a)
newSet set = undefined

-- | A simple convenience function.  Create a new 'ISet' drawing
-- initial elements from an existing list.
newFromList :: Ord a => [a] -> Par d s (AddRemoveSet s a)
newFromList ls = undefined

-- | Put a single element in the set.  (WHNF) Strict in the element
-- being put in the set.
insert :: Ord a => a -> AddRemoveSet s a -> Par d s ()
insert !elm (AddRemoveSet added removed) = undefined

-- | Wait for the set to contain a specified element.
waitAddedElem :: Ord a => a -> AddRemoveSet s a -> Par d s ()
waitAddedElem !elm (AddRemoveSet added removed) = undefined

-- | Wait on the size of the set of added elements.
waitAddedSize :: Int -> AddRemoveSet s a -> Par d s ()
waitAddedSize !sz (AddRemoveSet added removed) = undefined

-- | Remove a single element from the set.
remove :: Ord a => a -> AddRemoveSet s a -> Par d s ()
remove !elm (AddRemoveSet added removed) = undefined

-- | Wait for a single element to be removed from the set.
waitRemovedElem :: Ord a => a -> AddRemoveSet s a -> Par d s ()
waitRemovedElem !elm (AddRemoveSet added removed) = undefined

-- | Wait on the size of the set of removed elements.
waitRemovedSize :: Int -> AddRemoveSet s a -> Par d s ()
waitRemovedSize !sz (AddRemoveSet added removed) = undefined
