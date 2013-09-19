{-# LANGUAGE NamedFieldPuns, BangPatterns #-}

-- | A concurrent finite map represented as a single linked list.  
--
-- In contrast to standard maps, this one only allows lookups and insertions,
-- not modifications or removals.  While modifications would be fairly easy to
-- add, removals would significantly complicate the logic, and aren't needed for
-- the primary application -- LVars.
--
-- The interface is also somewhat low-level: rather than a standard insert
-- function, @tryInsert@ takes a "token" (i.e. a pointer into the linked list)
-- and attempts to insert at that location (but may fail).  Tokens are acquired
-- through the @find@ function, which yields a token in the case that a key is
-- *not* found; the token represents the location in the list where the key
-- *should* go.  This low-level interface is intended for use in higher-level
-- data structures, e.g. SkipListMap.

module Data.Concurrent.LinkedMap (
  LMap(), newLMap, Token(), value, find, FindResult(..), tryInsert,
  foldlWithKey, map, reverse)
where
  
import Data.IORef
import Data.Atomics  
import Control.Reagent -- AT: not yet using this, but would be nice to refactor
                       -- to use it.
import Control.Monad.IO.Class
import Prelude hiding (reverse, map)

-- | A concurrent finite map, represented as a linked list
data LMList k v = 
    Node k v {-# UNPACK #-} !(IORef (LMList k v))
  | Empty 

type LMap k v = IORef (LMList k v)

-- | Create a new concurrent map
newLMap :: IO (LMap k v)
newLMap = newIORef Empty
  
-- | A position in the map into which a key/value pair can be inserted          
data Token k v = Token {
  keyToInsert :: k,                   -- ^ what key were we looking up?
  value       :: Maybe v,             -- ^ the value at this position in the map
  nextRef     :: IORef (LMList k v),  -- ^ the reference at which to insert
  nextTicket  :: Ticket (LMList k v)  -- ^ a ticket for the old value of nextRef
}

-- | Either the value associated with a key, or else a token at the position
-- where that key should go.
data FindResult k v =
    Found v
  | NotFound (Token k v)

-- | Attempt to locate a key in the map
{-# INLINE find #-}
find :: Ord k => LMap k v -> k -> IO (FindResult k v)
find m k = findInner m Nothing 
  where 
    findInner m v = do
      nextTicket <- readForCAS m
      let stopHere = NotFound $ Token {keyToInsert = k, value = v, nextRef = m, nextTicket}
      case peekTicket nextTicket of
        Empty -> return stopHere
        Node k' v' next -> 
          case compare k k' of
            LT -> return stopHere
            EQ -> return $ Found v'
            GT -> findInner next (Just v')
      
-- | Attempt to insert a key/value pair at the given location (where the key is
-- given by the token).  NB: tryInsert will *always* fail after the first attempt.
-- If successful, returns a (mutable!) view of the map beginning at the given key.            
{-# INLINE tryInsert #-}            
tryInsert :: Token k v -> v -> IO (Maybe (LMap k v))
tryInsert Token { keyToInsert, nextRef, nextTicket } v = do
  newRef <- newIORef $ peekTicket nextTicket
  (success, _) <- casIORef nextRef nextTicket $ Node keyToInsert v newRef
  return $ if success then Just nextRef else Nothing

-- | Concurrently fold over all key/value pairs in the map within the given
-- monad, in increasing key order.  Inserts that arrive concurrently may or may
-- not be included in the fold.
foldlWithKey :: MonadIO m => (a -> k -> v -> m a) -> a -> LMap k v -> m a
foldlWithKey f a m = do
  n <- liftIO $ readIORef m
  case n of
    Empty -> return a
    Node k v next -> do
      a' <- f a k v
      foldlWithKey f a' next


-- | Map over a snapshot of the list.  Inserts that arrive concurrently may or may
-- not be included.  This does not affect keys, so the physical structure remains the
-- same.
map :: MonadIO m => (a -> b) -> LMap k a -> m (LMap k b)
map fn mp = do 
 tmp <- foldlWithKey (\ acc k v -> do
                      r <- liftIO (newIORef acc)
                      return$! Node k (fn v) r)
                     Empty mp
 tmp' <- liftIO (newIORef tmp)
 -- Here we suffer a reverse to avoid blowing the stack. 
 reverse tmp'

-- | Create a new linked map that is the reverse order from the input.
reverse :: MonadIO m => LMap k v -> m (LMap k v)
reverse mp = liftIO . newIORef =<< loop Empty mp
  where
    loop !acc mp = do
      n <- liftIO$ readIORef mp
      case n of
        Empty -> return acc
        Node k v next -> do
          r <- liftIO (newIORef acc)
          loop (Node k v r) next
