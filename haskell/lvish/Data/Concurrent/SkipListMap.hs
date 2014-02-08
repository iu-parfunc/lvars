{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-} -- for debugging

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
  SLMap(), newSLMap, find, PutResult(..), putIfAbsent, putIfAbsentToss, foldlWithKey, counts,
  -- map: is not exposed, because it has that FINISHME for now... [2013.10.01]
  debugShow, 

  -- * Slicing SLMaps
  SLMapSlice(Slice), toSlice, splitSlice, sliceSize
  )
where
  
import System.Random  

import Control.Applicative ((<$>))
import Control.Monad  
import Control.Monad.IO.Class
import Control.Exception (assert)
import Control.LVish.MonadToss
import Control.LVish (Par)

import Control.LVish.Unsafe () -- FOR MonadIO INSTANCE!  FIXME.  We can't keep this from escaping.
import Data.Maybe (fromMaybe)
import Data.IORef
import Data.Atomics
import qualified Data.Concurrent.LinkedMap as LM
import Prelude hiding (map)
import qualified Prelude as P


-- | The GADT representation.  The type @t@ gives the type of nodes at a given
-- level in the skip list.
data SLMap_ k v t where
  Bottom :: LM.LMap k v                      -> SLMap_ k v (LM.LMap k v)
  Index  :: LM.LMap k (t, v) -> SLMap_ k v t -> SLMap_ k v (LM.LMap k (t, v))

-- The complete multi-level SLMap always keeps a pointer to the bottom level (the
-- second field).
data SLMap k v = forall t. SLMap (SLMap_ k v t) (LM.LMap k v)

-- | A portion of an SLMap between two keys.  If the upper-bound is missing, that
--   means "go to the end".  The optional lower bound is used to "lazily" prune the
--   fronts each layer.  The reason for this is that we don't want to reallocate an
--   IORef spine and prematurely prune all lower layers IF we're simply going to
--   split again before actually enumerating the contents.
data SLMapSlice k v = Slice (SLMap k v)
                      !(Maybe k) -- Lower bound.  
                      !(Maybe k) -- Upper bound.

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
  searchResult <- LM.find (fromMaybe m shortcut) k
  case searchResult of
    LM.Found v      -> return $ Just v
    LM.NotFound tok -> return Nothing
    
-- At an indexing level: attempt to use the index to shortcut into the level
-- below.  
find_ (Index m slm) shortcut k = do 
  searchResult <- LM.find (fromMaybe m shortcut) k
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

{-# SPECIALIZE  putIfAbsent :: (Ord k) => SLMap k v -> k -> Par e s v -> Par e s (PutResult v)  #-}

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
     SLMap k v -> k -> Par e s v -> Par e s Bool -> Par e s (PutResult v)  #-}

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
    searchResult <- liftIO $ LM.find (fromMaybe m shortcut) k
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
  searchResult <- liftIO $ LM.find (fromMaybe m shortcut) k
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
--
-- Strict in the accumulator.        
foldlWithKey :: Monad m => (forall x . IO x -> m x) ->
                (a -> k -> v -> m a) -> a -> SLMap k v -> m a
foldlWithKey liftIO f !a (SLMap _ !lm) = LM.foldlWithKey liftIO f a lm

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
  c <- LM.foldlWithKey id (\n _ _ -> return (n+1)) 0 m
  return [c]
counts_ (Index m slm) = do
  c  <- LM.foldlWithKey id (\n _ _ -> return (n+1)) 0 m
  cs <- counts_ slm
  return $ c:cs


-- | Create a slice corresponding to the entire (non-empty) map.
toSlice :: SLMap k v -> SLMapSlice k v
toSlice mp = Slice mp Nothing Nothing


instance Show (LM.LMap k v) where
  show _ = "<LinkedMap>"

instance Show (LM.LMList k v) where
  show _ = "<LinkedMapList>"  

-- | Attempt to split a slice of an SLMap.  If there are not enough elements to form
-- two slices, this retruns Nothing.
splitSlice :: forall k v . (Show k, Ord k) =>
              SLMapSlice k v -> IO (Maybe (SLMapSlice k v, SLMapSlice k v))
splitSlice sl0@(Slice (SLMap index lmbot) mstart mend) = do
  res <- loop index
  case res of
    Just (x,y) -> do sz1 <- fmap (P.map fst) $ sliceToList sl0
                     sz2 <- fmap (P.map fst) $ sliceToList x
                     sz3 <- fmap (P.map fst) $ sliceToList y                      
                     putStrLn $ "Splitslice! size " ++(show sz1) ++" out szs "++(show (sz2,sz3))
                                ++ " mstart/end "++show (mstart,mend)
    Nothing -> return ()
  return res    
  where    
    loop :: SLMap_ k v t -> IO (Maybe (SLMapSlice k v, SLMapSlice k v))
    loop (Bottom lm) = do
      putStrLn "AT BOT"
      lm' <- readIORef lm
      lm'' <- case mstart of
                Nothing -> return lm'
                Just strtK -> LM.dropUntil strtK lm'
      res <- LM.halve mend lm''

      -- DEBUG:
      putStrLn $ "halve RES -> "++show res
      
      case res of
        Nothing -> return Nothing
        Just x -> dosplit (SLMap (Bottom lm) lm)
                          (\ tlboxed -> SLMap (Bottom tlboxed) tlboxed) x

    loop orig@(Index m slm) = do
      indm <- readIORef m
      indm' <- case mstart of
                Nothing -> return indm
                Just strtK -> LM.dropUntil strtK indm      
      -- Halve *this* level of the index, and use that as a fast way to split everything below.
      res <- LM.halve mend indm'
      case res of
        -- Case 1: This level isn't big enough to split, keep going down.  Note that we don't
        -- reconstruct the higher level, for splitting we don't care about it:
        Nothing -> loop slm
        -- Case 2: Do the split but use the full lmbot on the right
        -- (lazy pruning of the head elements):
        Just x -> dosplit (SLMap orig lmbot)
                          (\ tlboxed -> SLMap (Index tlboxed slm) lmbot) x 

    -- Create the left and right slices when halving is successful.
    dosplit :: SLMap k v -> (LM.LMap k tmp -> SLMap k v) -> 
               (Int, Int, LM.LMList k tmp) ->
               IO (Maybe (SLMapSlice k v, SLMapSlice k v))
    dosplit lmap mkRight (lenL, lenR, tlseg) =
      assert (lenL > 0) $ assert (lenR > 0) $ do
          putStrLn $ "Halved lengths "++show (lenL,lenR)
          -- We don't really want to allocate just for slicing... but alas we need new 
          -- IORef boxes here.  We lazily prune the head of the lower levels, but we
          -- don't want to throw away the work we've done traversing to this point in "loop":
          tlboxed <- newIORef tlseg
          tmp <- fmap length $ LM.toList tlboxed          
          let (LM.Node tlhead _ _) = tlseg
              rmap   = mkRight tlboxed 
              rslice = Slice rmap (Just tlhead) mend
              lslice = Slice lmap Nothing (Just tlhead)
          return $! Just $! (lslice, rslice)


-- | /O(N)/ measure the length of the bottom tier.
sliceSize :: Ord k => SLMapSlice k v -> IO Int
sliceSize slc = do
   ls <- sliceToList slc
   return $! length ls

sliceToList :: Ord k => SLMapSlice k v -> IO [(k,v)]
sliceToList (Slice (SLMap _ lmbot) mstart mend) = do
   ls <- LM.toList lmbot
   -- We SHOULD use the index layers to shortcut to a start, then stop at the end.
   return $! [ pr | pr@(k,v) <- ls, strtCheck k, endCheck k ] -- Inefficient!
  where
    strtCheck = case mstart of
                 Just strt -> \ k -> k >= strt
                 Nothing   -> \ _ -> True
    endCheck = case mend of
                 Just end -> \ k -> k < end
                 Nothing  -> \ _ -> True    



-- | Print a slice with each layer on a line.
debugShow :: forall k v . (Ord k, Show k, Show v) => SLMapSlice k v -> IO String
debugShow (Slice (SLMap index lmbot) mstart mend) =
  do lns <- loop index
     let len = length lns
     return $ unlines [ "["++show i++"]  "++l | l <- lns | i <- reverse [0::Int ..len-1] ]
  where
    startCheck = case mstart of
                  Just start -> \ k -> k >= start
                  Nothing  -> \ _ -> True    
    endCheck = case mend of
                 Just end -> \ k -> k < end
                 Nothing  -> \ _ -> True

    loop :: SLMap_ k v t -> IO [String]
    loop (Bottom lm) = do
      ls <- LM.toList lm
      return [ unwords $ [ if endCheck k && startCheck k
                           then show i++":"++show k++","++show v
                           else "_"
                         | i <- [0::Int ..]
                         | (k,v) <- ls
--                         , startCheck k
                         ] ]
    loop (Index indm slm) = do
      ls <- LM.toList indm
      strs <- forM [ (i,tup) | i <- [0::Int ..] | tup@(k,_) <- ls ] $ -- , startCheck k
              \ (ix, (key, (shortcut::t, val))) -> do
        -- Peek at the next layer down:
{-        
        case (slm::SLMap_ k v t) of
          Index (nxt::LM.LMap k (t2,v)) _ -> do
--    Could not deduce (t3 ~ IORef (LM.LMList k (t3, v)))            
            lmlst <- readIORef nxt
            LM.findIndex lmlst lmlst            
--        Bottom x  -> x
-}
         if endCheck key && startCheck key
          then return $ show ix++":"++show key++","++show val
          else return "_"
      rest <- loop slm
      return $ unwords strs : rest
