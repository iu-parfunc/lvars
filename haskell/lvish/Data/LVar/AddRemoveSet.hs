{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

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

         freezeSet
         
       ) where
import qualified Data.Set as S
import           Control.LVish
import           Control.LVish.Internal
import qualified Data.LVar.PureSet as PS
import           Control.Applicative

-- | The set datatype.
data AddRemoveSet s a =
     AddRemoveSet !(PS.ISet s a)
                  !(PS.ISet s a)

-- | Create a new, empty `AddRemoveSet`.
newEmptySet :: Ord a => Par d s (AddRemoveSet s a)
newEmptySet = newSet S.empty

-- | Create a new `AddRemoveSet` populated with initial elements.
newSet :: Ord a => S.Set a -> Par d s (AddRemoveSet s a)
-- Here we're creating two new PureSets, one from the provided initial
-- elements (the "add" set) and one empty (the "remove" set), and
-- then, since both of those return `Par` computations, we're using
-- our friends `<$>` and `<*>`.
newSet set = AddRemoveSet <$> (PS.newSet set) <*> PS.newEmptySet
-- Alternate version that works if we import `Control.Monad`:
-- newSet set = ap (fmap AddRemoveSet (PS.newSet set)) PS.newEmptySet
  
-- | A simple convenience function.  Create a new 'ISet' drawing
-- initial elements from an existing list.
newFromList :: Ord a => [a] -> Par d s (AddRemoveSet s a)
newFromList ls = newSet (S.fromList ls)

-- | Put a single element in the set.  (WHNF) Strict in the element
-- being put in the set.
insert :: Ord a => a -> AddRemoveSet s a -> Par d s ()
-- Because the two sets inside an AddRemoveSet are already PureSets,
-- we really just have to call the provided `insert` method for
-- PureSet.  We don't need to call `putLV` or anything like that!
insert !elm (AddRemoveSet added removed) = PS.insert elm added

-- | Wait for the set to contain a specified element.
waitAddedElem :: Ord a => a -> AddRemoveSet s a -> Par d s ()
-- And similarly here, we don't have to call `getLV` ourselves.
waitAddedElem !elm (AddRemoveSet added removed) = PS.waitElem elm added

-- | Wait on the size of the set of added elements.
waitAddedSize :: Int -> AddRemoveSet s a -> Par d s ()
-- You get the idea...
waitAddedSize !sz (AddRemoveSet added removed) = PS.waitSize sz added

-- | Remove a single element from the set.
remove :: Ord a => a -> AddRemoveSet s a -> Par d s ()
-- We remove an element by adding it to the `removed` set!
remove !elm (AddRemoveSet added removed) = PS.insert elm removed

-- | Wait for a single element to be removed from the set.
waitRemovedElem :: Ord a => a -> AddRemoveSet s a -> Par d s ()
waitRemovedElem !elm (AddRemoveSet added removed) = PS.waitElem elm removed

-- | Wait on the size of the set of removed elements.
waitRemovedSize :: Int -> AddRemoveSet s a -> Par d s ()
waitRemovedSize !sz (AddRemoveSet added removed) = do 
   logDbgLn 2 "waitRemovedSize: about to block."
   PS.waitSize sz removed
   logDbgLn 2 "waitRemovedSize: unblocked, returning."

-- | Get the exact contents of the set.  As with any
-- quasi-deterministic operation, using `freezeSet` may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
freezeSet :: (Ord a, HasFreeze e) => AddRemoveSet s a -> Par e s (S.Set a)
-- Freezing takes the set difference of added and removed elements.
freezeSet (AddRemoveSet added removed) =
  liftA2 S.difference (PS.freezeSet added) (PS.freezeSet removed)
