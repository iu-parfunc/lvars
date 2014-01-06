module Data.Concurrent.Bag(Bag, Token, new, put, remove, foreach) where

import           Control.Monad
import           Control.Concurrent
import           System.IO.Unsafe (unsafePerformIO)
import           Data.IORef
import qualified Data.IntMap as M

------------------------------------------------------------------------------
-- A nonscalable implementation of a concurrent bag
------------------------------------------------------------------------------

type UID     = Int
type Token a = (Bag a, UID)
type Bag a   = IORef (M.IntMap a)

-- Return the old value.  Could replace with a true atomic op.
atomicIncr :: IORef Int -> IO Int
atomicIncr cntr = atomicModifyIORef' cntr (\c -> (c+1,c))

{-# NOINLINE uidCntr #-}
uidCntr :: IORef UID
uidCntr = unsafePerformIO (newIORef 0)

getUID :: IO UID
getUID =  atomicIncr uidCntr

-- | Create an empty bag
new :: IO (Bag a)
new = newIORef (M.empty)

-- | Add an element to a bag, returning a token that can later be used to remove
-- that element.
put :: Bag a -> a -> IO (Token a)
put b x = do
  uid <- getUID
  atomicModifyIORef' b $ \m -> (M.insert uid x m, ())  
  return (b, uid)

-- | foreach b f will traverse b (concurrently with updates), applying f to each
-- encountered element, together with a token that can be used to remove the
-- element.
foreach :: Bag a -> (a -> Token a -> IO ()) -> IO ()
foreach b f = do
  m <- readIORef b
  let invoke (k, a) = f a (b, k)
  mapM_ invoke $ M.toList m

-- | Remove the element associated with a given token.  Repeated removals are
-- permitted.
remove :: Token a -> IO ()
remove (b, uid) = atomicModifyIORef' b $ \m -> (M.delete uid m, ())
