{-# LANGUAGE ExistentialQuantification, GADTs #-}

-- | An implementation of concurrent finite maps based on skip lists.  Only
-- supports lookup and insertions, not modifications or removals.
--
-- Skip lists are a probabilistic data structure that roughly approximate
-- balanced trees.  At the bottom layer is a standard linked list representation
-- of a finite map.  Above this is some number of "index" lists that provide
-- shortcuts to the layer below them.  When a key/value pair is added, it is
-- always added to the bottom layer, and is added with exponentially decreasing
-- probability to each index layer above it.
--
-- Skip lists are a very good match for lock-free programming, since the
-- linearization point can be taken as insertion into the bottom list, and index
-- nodes can be added *afterward* in a best-effort style (i.e., if there is
-- contention to add them, we can simply walk away, with the effect that the
-- probability of appearing in an index is partly a function of contention.)
-- 
-- To implement skip lists in Haskell, we use a GADT to represent the layers,
-- each of which has a different type (since it indexes the layer below it).

module Data.Concurrent.SkipListMap (
  SLMap(), newSLMap, find, PutResult(..), putIfAbsent, putIfAbsentToss, foldlWithKey, counts
  -- map: is not exposed, because it has that FINISHME for now... [2013.10.01]
  )
where
  
import System.Random  

import Control.Applicative ((<$>))
import Control.Monad  
import Control.Monad.IO.Class
import Control.LVish.MonadToss
import Control.LVish (Par)

import Control.LVish.Unsafe () -- FOR MonadIO INSTANCE!  FIXME.  We can't keep this from escaping.
import Data.IORef
import Data.Atomics
import qualified Data.Concurrent.LinkedMap as LM
import Prelude hiding (map)


-- | The GADT representation.  The type @t@ gives the type of nodes at a given
-- level in the skip list.
data SLMap_ k v t where
  Bottom :: LM.LMap k v -> SLMap_ k v (LM.LMap k v)
  Index  :: LM.LMap k (t, v) -> SLMap_ k v t -> SLMap_ k v (LM.LMap k (t, v))

-- The complete multi-level SLMap always keeps a pointer to the bottom level (the
-- second field).
data SLMap k v = forall t. SLMap (SLMap_ k v t) (LM.LMap k v)

-- | Physical identity
instance Eq (SLMap k v) where
  SLMap _ lm1 == SLMap _ lm2 = lm1 == lm2

-- | Create a new skip list with the given number of levels.
newSLMap :: Int -> IO (SLMap k v)
newSLMap 0 = do
  lm <- LM.newLMap
  return $ SLMap (Bottom lm) lm
newSLMap n = do 
  SLMap slm lmBottom <- newSLMap (n-1)
  lm <- LM.newLMap
  return $ SLMap (Index lm slm) lmBottom

-- | Attempt to locate a key in the map.
find :: Ord k => SLMap k v -> k -> IO (Maybe v)      
find (SLMap slm _) k = find_ slm Nothing k

-- Helper for @find@.
find_ :: Ord k => SLMap_ k v t -> Maybe t -> k -> IO (Maybe v)

-- At the bottom level: just lift the find from LinkedMap
find_ (Bottom m) shortcut k = do
  searchResult <- LM.find (maybe m id shortcut) k
  case searchResult of
    LM.Found v      -> return $ Just v
    LM.NotFound tok -> return Nothing
    
-- At an indexing level: attempt to use the index to shortcut into the level
-- below.  
find_ (Index m slm) shortcut k = do 
  searchResult <- LM.find (maybe m id shortcut) k
  case searchResult of 
    LM.Found (_, v) -> 
      return $ Just v   -- the key is in the index itself; we're outta here
    LM.NotFound tok -> case LM.value tok of
      Just (m', _) -> find_ slm (Just m') k     -- there's an index node
                                                -- preceeding our key; use it to
                                                -- shortcut into the level below.
      
      Nothing      -> find_ slm Nothing k       -- no smaller key in the index,
                                                -- so start at the beginning of
                                                -- the level below.
      
data PutResult v = Added v | Found v

{-# SPECIALIZE  putIfAbsent :: (Ord k) => SLMap k v -> k -> Par d s v -> Par d s (PutResult v)  #-}

-- | Adds a key/value pair if the key is not present, all within a given monad.
-- Returns the value now associated with the key in the map.
putIfAbsent :: (Ord k, MonadIO m, MonadToss m) => 
               SLMap k v         -- ^ The map
               -> k              -- ^ The key to lookup/insert
               -> m v            -- ^ A computation of the value to insert
               -> m (PutResult v)
putIfAbsent (SLMap slm _) k vc = 
  putIfAbsent_ slm Nothing k vc toss $ \_ _ -> return ()

{-# SPECIALIZE  putIfAbsentToss :: (Ord k) => 
     SLMap k v -> k -> Par d s v -> Par d s Bool -> Par d s (PutResult v)  #-}

-- | Adds a key/value pair if the key is not present, all within a given monad.
-- Returns the value now associated with the key in the map.
putIfAbsentToss :: (Ord k, MonadIO m) =>  SLMap k v -- ^ The map
                -> k             -- ^ The key to lookup/insert
                -> m v           -- ^ A computation of the value to insert
                -> m Bool        -- ^ An explicit, thread-local coin to toss
                -> m (PutResult v)
putIfAbsentToss (SLMap slm _) k vc coin = 
  putIfAbsent_ slm Nothing k vc coin $ \_ _ -> return () 
                                               
-- Helper for putIfAbsent
putIfAbsent_ :: (Ord k, MonadIO m) => 
                SLMap_ k v t    -- ^ The map    
                -> Maybe t      -- ^ A shortcut into this skiplist level
                -> k             -- ^ The key to lookup/insert
                -> m v           -- ^ A computation of the value to insert
                -> m Bool        -- ^ A (thread-local) coin tosser
                -> (t -> v -> m ())  -- ^ A thunk for inserting into the higher
                                     -- levels of the skiplist
                -> m (PutResult v)
                
-- At the bottom level, we use a retry loop around the find/tryInsert functions
-- provided by LinkedMap
putIfAbsent_ (Bottom m) shortcut k vc coin install = retryLoop vc where 
  -- The retry loop; ensures that vc is only executed once
  retryLoop vc = do
    searchResult <- liftIO $ LM.find (maybe m id shortcut) k
    case searchResult of
      LM.Found v      -> return $ Found v
      LM.NotFound tok -> do
        v <- vc
        maybeMap <- liftIO $ LM.tryInsert tok v
        case maybeMap of
          Just m' -> do
            install m' v                  -- all set on the bottom level, now try indices
            return $ Added v
          Nothing -> retryLoop $ return v -- next time around, remember the value to insert
          
-- At an index level; try to shortcut into the level below, while remembering
-- where we were so that we can insert index nodes later on
putIfAbsent_ (Index m slm) shortcut k vc coin install = do          
  searchResult <- liftIO $ LM.find (maybe m id shortcut) k
  case searchResult of 
    LM.Found (_, v) -> return $ Found v -- key is in the index; bail out
    LM.NotFound tok -> 
      let install' mBelow v = do        -- to add an index node here,
            shouldAdd <- coin           -- first, see if we (probabilistically) should
            when shouldAdd $ do 
              maybeHere <- liftIO $ LM.tryInsert tok (mBelow, v)  -- then, try it!
              case maybeHere of
                Just mHere -> install mHere v  -- if we succeed, keep inserting
                                               -- into the levels above us
                              
                Nothing -> return ()    -- otherwise, oh well; we tried.
      in case LM.value tok of
        Just (m', _) -> putIfAbsent_ slm (Just m') k vc coin install'
        Nothing      -> putIfAbsent_ slm Nothing   k vc coin install'

-- | Concurrently fold over all key/value pairs in the map within the given
-- monad, in increasing key order.  Inserts that arrive concurrently may or may
-- not be included in the fold.
foldlWithKey :: MonadIO m => (a -> k -> v -> m a) -> a -> SLMap k v -> m a
foldlWithKey f a (SLMap _ lm) = LM.foldlWithKey f a lm

-- | Create an identical copy of an (unchanging) SLMap with the keys unchanged and
-- the values replaced by the result of applying the provided function.
-- map :: MonadIO m => (a -> b) -> SLMap k a -> m (SLMap k b)
map :: MonadIO m => (a -> a) -> SLMap k a -> m (SLMap k a)
map fn (SLMap (Bottom lm) lm2) = do
  lm'  <- LM.map fn lm
  return$! SLMap (Bottom lm') lm'

map fn (SLMap (Index lm slm) lmbot) = do
  SLMap slm2 bot2 <- map fn (SLMap slm lmbot)
  lm2  <- LM.map (\(t,a) -> (t,fn a)) lm
  error "FINISHME -- SkipListMap.map"
--  return$! SLMap (Index lm2 slm2) bot2


-- | Returns the sizes of the skiplist levels; for performance debugging.
counts :: SLMap k v -> IO [Int]
counts (SLMap slm _) = counts_ slm

counts_ :: SLMap_ k v t -> IO [Int]
counts_ (Bottom m)    = do
  c <- LM.foldlWithKey (\n _ _ -> return (n+1)) 0 m
  return [c]
counts_ (Index m slm) = do
  c  <- LM.foldlWithKey (\n _ _ -> return (n+1)) 0 m
  cs <- counts_ slm
  return $ c:cs


-- TODO: provide a balanced traversal / fold:
