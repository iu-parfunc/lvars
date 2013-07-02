{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.LVar.Map
       (
         IMap, newEmptyMap, insert, withCallbacksThenFreeze,
         getKey, waitValue, waitSize, modify, freezeMap
       ) where

import           Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.LVar.IVar as IV

import           Control.LVish

------------------------------------------------------------------------------
-- IMaps implemented on top of LVars:
------------------------------------------------------------------------------

-- | We only have one mutable location here, so this is not a scalable implementation.
newtype IMap k v = IMap (LVar (IORef (M.Map k v)) (k,v))

instance Eq (IMap k v) where
  IMap lv1 == IMap lv2 = state lv1 == state lv2 


-- Need LVarData2:

instance LVarData1 (IMap k) where
  newtype Snapshot (IMap k) a = IMapSnap (M.Map k a)
  freeze    = fmap IMapSnap . freezeMap
  newBottom = newEmptyMap


newEmptyMap :: Par (IMap k v)
newEmptyMap = fmap IMap $ newLV$ newIORef M.empty

-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- valueof the Map variable.
withCallbacksThenFreeze :: forall k v b . Eq b =>
                           IMap k v -> (k -> v -> Par ()) -> Par b -> Par b
withCallbacksThenFreeze (IMap lv) callback action =
    do
       res <- IV.new -- TODO, specialize to skip this when the init action returns ()
       freezeLVAfter lv (initCB res) (\(k,v) -> return$ Just$ callback k v)
       -- freezeSet lv -- This does nothing, but it gives us the value.
       IV.get res
  where
    initCB :: IV.IVar b -> (IORef (M.Map k v)) -> IO (Maybe (Par ()))    
    initCB resIV ref = do
      -- The implementation guarantees that all elements will be caught either here,
      -- or by the delta-callback:
      mp <- readIORef ref -- Snapshot
      return $ Just $ do 
        -- Data.Foldable should give us a non-copying way to iterate:
        -- But it's actually insufficient because it only exposes the values:
        -- F.foldlM (\() v -> fork$ callback undefined v) () mp
        mapM_ (\(k,v) -> fork$ callback k v) (M.toList mp)
        
        res <- action -- Any additional puts here trigger the callback.
        IV.put_ resIV res


-- | Put a single entry into the map.  (WHNF) Strict in the key and value.
-- 
insert :: (Ord k, Eq v) =>
          k -> v -> IMap k v -> Par () 
insert !key !elm (IMap lv) = putLV lv putter
  where putter ref  = atomicModifyIORef ref update
        update mp =
          let mp' = M.insertWith fn key elm mp
              fn v1 v2 | v1 == v2  = v1
                       | otherwise = error "Multiple puts to one entry in an IMap!"
          in
          -- Here we do a constant time check to see if we actually changed anything:
          -- For idempotency it is important that we return Nothing if not.
          if M.size mp' > M.size mp
          then (mp',Just (key,elm))
          else (mp, Nothing)

-- | IMap's containing other LVars have some additional capabilities compared to
-- those containing regular Haskell data.  In particular, it is possible to modify
-- existing entries (monotonically).  Further, this `modify` function implicitly
-- inserts a "bottom" element if there is no existing entry for the key.
modify :: forall f a b key . (Ord key, LVarData1 f) =>
          key -> IMap key (f a) -> (f a -> Par b) -> Par b
modify key (IMap lv) fn = do 
  let ref = state lv 
  mp  <- liftIO$ readIORef ref
  case M.lookup key mp of
    Just lv2 -> fn lv2
    Nothing -> do 
      bot <- newBottom :: Par (f a)
      act <- liftIO$ atomicModifyIORef ref $ \ mp2 ->
               case M.lookup key mp2 of
                 Just lv2 -> (mp2, fn lv2)
                 Nothing  -> (M.insert key bot mp2, fn bot)
      act

-- | Wait for the map to contain a specified key, and return the associated value.
getKey :: Ord k => k -> IMap k v -> Par v
getKey !key (IMap lv) = getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      mp <- readIORef ref
      return (M.lookup key mp)
    deltaThresh (k,v) | k == key  = return$ Just v
                      | otherwise = return Nothing 

-- | Wait until the map contains a certain value (on any key).
waitValue :: (Ord k, Eq v) => v -> IMap k v -> Par ()
waitValue !val (IMap lv) = getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      mp <- readIORef ref
      -- This is very inefficient:
      let fn Nothing v | v == val  = Just ()
                       | otherwise = Nothing
          fn just _  = just
      -- FIXME: no short-circuit for this fold:
      return $! M.foldl fn Nothing mp
    deltaThresh (_,v) | v == val  = return$ Just ()
                      | otherwise = return Nothing 


-- | Wait on the SIZE of the map, not its contents.
waitSize :: Int -> IMap k v -> Par ()
waitSize !sz (IMap lv) = getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      mp <- readIORef ref
      case M.size mp >= sz of
        True  -> return (Just ())
        False -> return (Nothing)
    -- Here's an example of a situation where we CANNOT TELL if a delta puts it over
    -- the threshold.a
    deltaThresh _ = globalThresh (state lv) False

-- | Get the exact contents of the map  Using this may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
freezeMap :: IMap k v -> Par (M.Map k v)
freezeMap (IMap lv) =
   do freezeLV lv
      getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing
