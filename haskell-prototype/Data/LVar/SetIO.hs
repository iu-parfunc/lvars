{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns, BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Data.LVar.SetIO
       (
         -- * Example 3: Monotonically growing sets.
         ISet(), newEmptySet, newEmptySetWithCallBack, putInSet,
         waitForSet, waitForSetSize, consumeSet,
         
         -- * DEBUGGING only:
         unsafePeekSet, reallyUnsafePeekSet
         ) where
import LVarTraceIO
import           Data.IORef
import qualified Data.Set as S
import           System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------------
-- ISets and setmap implemented on top of LVars:
------------------------------------------------------------------------------

newtype ISet a = ISet (LVar (IORef (S.Set a)))

newEmptySet :: Par (ISet a)
newEmptySet = fmap ISet $ newLV$ newIORef S.empty

-- | Extended lambdaLVar (callbacks).  Create an empty set, but
-- establish a callback that will be invoked (in parallel) on each
-- element added to the set.
newEmptySetWithCallBack :: forall a . Ord a => (a -> Par ()) -> Par (ISet a)
newEmptySetWithCallBack callb = fmap ISet $ newLVWithCallback io
 where -- Every time the set is updated we fork callbacks:
   io = do
     alreadyCalled <- newIORef S.empty
     contents <- newIORef S.empty   
     let fn :: IORef (S.Set a) -> IO Trace
         fn _ = do
           curr <- readIORef contents
           old <- atomicModifyIORef alreadyCalled (\set -> (curr,set))
           let new = S.difference curr old
           -- Spawn in parallel all new callbacks:
           let trcs = map runCallback (S.toList new)
           -- Would be nice if this were a balanced tree:           
           return (foldl Fork Done trcs)

         runCallback :: a -> Trace
         -- Run each callback with an etpmyt continuation:
         runCallback elem = runCont (callb elem) (\_ -> Done)
         
     return (contents, fn)

-- | Put a single element in the set.  (WHNF) Strict in the element being put in the
-- set.     
putInSet :: Ord a => a -> ISet a -> Par () 
putInSet !elem (ISet lv) = putLV lv putter
  where
    putter ref = atomicModifyIORef ref (\set -> (S.insert elem set, ()))

-- | Wait for the set to contain a specified element.
waitForSet :: Ord a => a -> ISet a -> Par ()
waitForSet elem (ISet lv@(LVar ref _ _)) = getLV lv getter
  where
    getter = do
      set <- readIORef ref
      case S.member elem set of
        True  -> return (Just ())
        False -> return (Nothing)

-- | Wait on the SIZE of the set, not its contents.
waitForSetSize :: Int -> ISet a -> Par ()
waitForSetSize sz (ISet lv@(LVar ref _ _)) = getLV lv getter
  where
    getter = do
      set <- readIORef ref
      if S.size set >= sz
         then return (Just ())
         else return Nothing     

-- | Get the exact contents of the set.  Using this may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
consumeSet :: ISet a -> Par (S.Set a)
consumeSet (ISet lv) = consumeLV lv readIORef

unsafePeekSet :: ISet a -> Par (S.Set a)
unsafePeekSet (ISet lv) = unsafePeekLV lv readIORef

reallyUnsafePeekSet :: ISet a -> (S.Set a)
reallyUnsafePeekSet (ISet (LVar {lvstate})) =
  unsafePerformIO $ do
    current <- readIORef lvstate
    return current

