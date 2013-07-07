{-# LANGUAGE NamedFieldPuns #-}

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
  LMap(), newLMap, Token(), value, find, FindResult(..), tryInsert, forPairs)
where
  
import Data.IORef
import Data.Atomics  
import Control.Reagent -- AT: not yet using this, but would be nice to refactor
                       -- to use it.
import Control.Monad.IO.Class

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
tryInsert :: Token k v -> v -> IO (Maybe (LMap k v))
tryInsert Token { keyToInsert, nextRef, nextTicket } v = do
  newRef <- newIORef $ peekTicket nextTicket
  (success, _) <- casIORef nextRef nextTicket $ Node keyToInsert v newRef
  return $ if success then Just nextRef else Nothing

-- | Concurrently iterate over all key/value pairs in the map within the given
-- monad.  Inserts that arrive concurrently may or may not be included in the
-- iteration.
forPairs :: MonadIO m => LMap k v -> (k -> v -> m ()) -> m ()
forPairs m f = do
  n <- liftIO $ readIORef m
  case n of
    Empty -> return ()
    Node k v next -> do
      f k v
      forPairs next f