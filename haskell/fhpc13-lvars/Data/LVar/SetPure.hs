{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Data.LVar.SetPure
       (
         ISet(), newEmptySet, newEmptySetWithCallBack, putInSet, putInSet_,
         waitForSet, waitForSetSize, consumeSet,
         
         -- For debugging only!
         unsafePeekSet, reallyUnsafePeekSet
         ) where
import LVarTracePure
import           Control.DeepSeq
import           Data.IORef
import qualified Data.Set as S
import           System.IO.Unsafe (unsafePerformIO)
import Algebra.Lattice (JoinSemiLattice(..))

------------------------------------------------------------------------------
-- ISets and setmap implemented on top of LVars:
------------------------------------------------------------------------------

-- Abstract data type:
-- newtype ISet a = ISet (LVar (S.Set a))
newtype ISet a = ISet (LVar (ISetContents a))

newtype ISetContents a = ISetContents { unContents :: S.Set a }
  deriving JoinSemiLattice

instance NFData (ISetContents a) where
  
  
newEmptySet :: Par (ISet a)
newEmptySet = fmap (ISet) $ newLV (ISetContents S.empty)

-- | Extended lambdaLVar (callbacks).  Create an empty set, but establish a callback
-- that will be invoked (in parallel) on each element added to the set.
newEmptySetWithCallBack :: forall a . Ord a => (a -> Par ()) -> Par (ISet a)
newEmptySetWithCallBack callb = fmap ISet $ newLVWithCallback (ISetContents S.empty) cb
 where -- Every time the set is updated we fork callbacks on new elements:
   cb :: ISetContents a -> ISetContents a -> Trace
   cb (ISetContents old) (ISetContents new) =
     -- Unfortunately we need to do a set diff every time.
     let fresh = S.difference new old 
         -- Spawn in parallel all new callbacks:
         trcs = map runCallback (S.toList fresh)
         runCallback :: a -> Trace
         -- Run each callback with an empty continuation:
         runCallback elem = runCont (callb elem) (\_ -> Done)
     in
     -- Would be nice if this were a balanced tree:      
     foldl Fork Done trcs

-- | Put a single element in the set.  (WHNF) Strict in the element being put in the
-- set.
putInSet_ :: Ord a => a -> ISet a -> Par () 
putInSet_ !elem (ISet lv) = putLV lv (ISetContents$ S.singleton elem)

putInSet :: (NFData a, Ord a) => a -> ISet a -> Par ()
putInSet e s = deepseq e (putInSet_ e s)

-- | Wait for the set to contain a specified element.
waitForSet :: Ord a => a -> ISet a -> Par ()
waitForSet !elem (ISet lv) = getLV lv fn
  where
    fn (ISetContents set)
      | S.member elem set = Just ()
      | otherwise         = Nothing

-- | Wait on the SIZE of the set, not its contents.
waitForSetSize :: Int -> ISet a -> Par ()
waitForSetSize sz (ISet lv) = getLV lv fn
  where
    fn (ISetContents set)
           | S.size set >= sz = Just ()
           | otherwise        = Nothing 

-- | Get the exact contents of the set.  Using this may cause your
-- program exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
consumeSet :: ISet a -> Par (S.Set a)
consumeSet (ISet lv) = fmap unContents $ consumeLV lv

unsafePeekSet :: ISet a -> Par (S.Set a)
unsafePeekSet (ISet lv) = fmap unContents $ unsafePeekLV lv

reallyUnsafePeekSet :: ISet a -> (S.Set a)
reallyUnsafePeekSet (ISet (LVar {lvstate})) =
  unsafePerformIO $ do
    LVarContents {current=ISetContents x} <- readIORef lvstate
    return x

