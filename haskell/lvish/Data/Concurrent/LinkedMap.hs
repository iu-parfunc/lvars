{-# LANGUAGE NamedFieldPuns, BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

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
  LMap(), LMList(..),
  newLMap, Token(), value, find, FindResult(..), tryInsert,
  foldlWithKey, map, reverse, head, toList, fromList, findIndex,
  
  -- * Utilities for splitting/slicing
  halve, halve', dropUntil
  )
where
  
import Data.IORef
import Data.Atomics  
import Control.Reagent -- AT: not yet using this, but would be nice to refactor
                       -- to use it.
import Control.Monad.IO.Class
import Control.Exception (assert)
import Prelude hiding (reverse, map, head)

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
--
-- Strict in the accumulator.  
foldlWithKey :: Monad m => (forall x . IO x -> m x) ->
                (a -> k -> v -> m a) -> a -> LMap k v -> m a
foldlWithKey liftIO f !a !m = do
  n <- liftIO$ readIORef m
  case n of
    Empty -> return a
    Node k v next -> do
      a' <- f a k v
      foldlWithKey liftIO f a' next


-- | Map over a snapshot of the list.  Inserts that arrive concurrently may or may
-- not be included.  This does not affect keys, so the physical structure remains the
-- same.
map :: MonadIO m => (a -> b) -> LMap k a -> m (LMap k b)
map fn mp = do 
 tmp <- foldlWithKey liftIO
                     (\ acc k v -> do
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

head :: LMap k v -> IO (Maybe k)
head lm = do
  x <- readIORef lm
  case x of
    Empty      -> return Nothing
    Node k _ _ -> return $! Just k

-- | Convert to a list
toList :: LMap k v -> IO [(k,v)]
toList lm = do
  x <- readIORef lm
  case x of
    Empty       -> return []
    Node k v tl -> do
      ls <- toList tl
      return $! (k,v) : ls 

-- | Convert from a list.
fromList :: [(k,v)] -> IO (LMap k v)
fromList ls = do
  let loop [] = return Empty
      loop ((k,v):tl) = do
        tl' <- loop tl
        ref <- newIORef tl'
        return $! Node k v ref
  lm <- loop ls
  newIORef lm


halve' :: Ord k => Maybe k -> LMap k v -> IO (Maybe (LMap k v, LMap k v))
halve' mend lm = do 
  lml <- readIORef lm
  res <- halve mend lml
  case res of
    Nothing -> return Nothing
    Just (len1,_len2,tailhd) -> do
      ls <- toList lm
      l' <- fromList (take len1 ls)
      r' <- newIORef tailhd
      return $! Just $! (l',r')
          

-- | Attempt to split into two halves.
--    
--   This optionally takes an upper bound key, which is treated as an alternate
--   end-of-list signifier.
--
--   Result: If there is only one element, then return Nothing.  If there are more,
--   return the number of elements in the first and second halves, plus a pointer to
--   the beginning of the second half.  It is a contract of this function that the
--   two Ints returned are non-zero.
--
halve :: Ord k => Maybe k -> LMList k v -> IO (Maybe (Int, Int, LMList k v))
{-# INLINE halve #-}
halve mend ls = loop 0 ls ls
  where
    isEnd Empty = True
    isEnd (Node k _ _) =
       case mend of
         Just end -> k >= end
         Nothing -> False
    emptCheck (0,l2,t) = return Nothing
    emptCheck !x       = return $! Just x

    loop len tort hare | isEnd hare =
      emptCheck (len, len, tort)
    loop len tort@(Node _ _ next1) (Node k v next2) = do 
      next2' <- readIORef next2
      case next2' of
        x | isEnd x -> emptCheck (len, len+1, tort)
        Node _ _ next3 -> do next1' <- readIORef next1
                             next3' <- readIORef next3
                             loop (len+1) next1' next3'

-- | Drop from the front of the list until the first key is equal or greater than the
-- given key.
dropUntil :: Ord k => k -> LMList k v -> IO (LMList k v)
dropUntil _ Empty = return Empty
dropUntil stop nd@(Node k v tl)
  | stop <= k = return nd
  | otherwise = do tl' <- readIORef tl
                   dropUntil stop tl' 

-- | Given a pointer into the middle of the list, find how deep it is.
-- findIndex :: Eq k => LMList k v -> LMList k v -> IO (Maybe Int)
findIndex :: Eq k => LMList k v -> LMList k v -> IO (Maybe Int)                   
findIndex ls1 ls2 =
  error "FINISHME - LinkedMap.findIndex"

