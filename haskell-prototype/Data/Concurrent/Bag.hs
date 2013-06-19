module Data.Concurrent.Bag(Bag, Token, new, put, remove, foreach) where

import           Control.Monad
import           Control.Concurrent
import           System.IO.Unsafe (unsafePerformIO)
import           Data.IORef
import qualified Data.Map as M

------------------------------------------------------------------------------
-- A nonscalable implementation of a concurrent bag
------------------------------------------------------------------------------

type UID     = Int
type Token a = (Bag a, UID)
type Bag a   = IORef (M.Map UID a)

-- Return the old value.  Could replace with a true atomic op.
atomicIncr :: IORef Int -> IO Int
atomicIncr cntr = atomicModifyIORef cntr (\c -> (c+1,c))

uidCntr :: IORef UID
uidCntr = unsafePerformIO (newIORef 0)

getUID :: IO UID
getUID =  atomicIncr uidCntr

new :: IO (Bag a)
new = newIORef (M.empty)

put :: Bag a -> a -> IO (Token a)
put b x = do
  uid <- getUID
  atomicModifyIORef b $ \m -> (M.insert uid x m, ())  
  return (b, uid)

-- | iter b f will traverse b (concurrently with updates), applying f to each
-- encountered element.
foreach :: Bag a -> (a -> Token a -> IO ()) -> IO ()
foreach = error "todo"

remove :: Token a -> IO ()
remove = error "todo"