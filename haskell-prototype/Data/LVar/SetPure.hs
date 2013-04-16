{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns, BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Data.LVar.SetPure
       (
         -- * Example 3: Monotonically growing sets.
         ISet(), newEmptySet, newEmptySetWithCallBack, putInSet, putInSet_,
         waitForSet, waitForSetSize, consumeSet,
         
         -- * DEBUGGING only:
         unsafePeekSet, reallyUnsafePeekSet
         ) where
import LVarTracePure
import           Control.DeepSeq
import           Data.IORef
import qualified Data.Set as S
import           System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------------
-- ISets and setmap implemented on top of LVars:
------------------------------------------------------------------------------

-- Abstract data type:
newtype ISet a = ISet (LVar (S.Set a))

newEmptySet :: Par (ISet a)
newEmptySet = fmap ISet $ newLV S.empty

-- | Extended lambdaLVar (callbacks).  Create an empty set, but establish a callback
-- that will be invoked (in parallel) on each element added to the set.
newEmptySetWithCallBack :: forall a . Ord a => (a -> Par ()) -> Par (ISet a)
newEmptySetWithCallBack callb = fmap ISet $ newLVWithCallback S.empty cb
 where -- Every time the set is updated we fork callbacks on new elements:
   cb :: S.Set a -> S.Set a -> Trace
   cb old new =
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
putInSet_ !elem (ISet lv) = putLV lv (S.singleton elem)

putInSet :: (NFData a, Ord a) => a -> ISet a -> Par ()
putInSet e s = deepseq e (putInSet_ e s)

-- | Wait for the set to contain a specified element.
waitForSet :: Ord a => a -> ISet a -> Par ()
waitForSet !elem (ISet lv) = getLV lv fn
  where
    fn set | S.member elem set = Just ()
           | otherwise         = Nothing

-- | Wait on the SIZE of the set, not its contents.
waitForSetSize :: Int -> ISet a -> Par ()
waitForSetSize sz (ISet lv) = getLV lv fn
  where
    fn set | S.size set >= sz = Just ()
           | otherwise        = Nothing 

-- | Get the exact contents of the set.  Using this may cause your
-- program exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
consumeSet :: ISet a -> Par (S.Set a)
consumeSet (ISet lv) = consumeLV lv

unsafePeekSet :: ISet a -> Par (S.Set a)
unsafePeekSet (ISet lv) = unsafePeekLV lv

reallyUnsafePeekSet :: ISet a -> (S.Set a)
reallyUnsafePeekSet (ISet (LVar {lvstate})) =
  unsafePerformIO $ do
    LVarContents {current} <- readIORef lvstate
    return current

