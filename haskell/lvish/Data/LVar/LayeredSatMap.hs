{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}

module Data.LVar.LayeredSatMap
    ( LayeredSatMap
    , forEachHP
    , newEmptyMap
    , newMap
    , newFromList
    , withCallbacksThenFreeze
    , forEach
    , insert
    , whenSat
    , saturate
    , test0
    )
    where

import Data.LVar.SatMap(PartialJoinSemiLattice(..))

import           Control.LVish.DeepFrz.Internal
import           Control.LVish.DeepFrz (runParThenFreeze)
import           Control.LVish.Internal as LI
import           Internal.Control.LVish.Sched (newLV, putLV, putLV_, getLV, freezeLV, freezeLVAfter)
import qualified Internal.Control.LVish.Sched as L
import qualified Data.LVar.IVar as IV
import           Data.LVar.Generic as G
import           Data.LVar.Generic.Internal (unsafeCoerceLVar)
import           Data.UtilInternal (traverseWithKey_)

import Control.LVish

import Data.IORef
import qualified Data.Map as M
import qualified Data.Foldable as F
import           GHC.Prim (unsafeCoerce#)
import System.IO.Unsafe (unsafeDupablePerformIO)

data LayeredSatMap k s v = LayeredSatMap (LVar s (LSMContents k v) (k, v))

data LSMContents k v = LSMContents [IORef (Maybe (M.Map k v), OnSat)]

type OnSat = L.Par ()

forEachHP :: Maybe HandlerPool -- ^ optional pool to enroll in
          -> LayeredSatMap k s v -- ^ Map to listen to
          -> (k -> v -> Par d s ()) -- ^ callback
          -> Par d s ()
forEachHP mh (LayeredSatMap (WrapLVar lv)) callback = WrapPar $ do
    L.addHandler mh lv globalCB deltaCB
    return ()
    where
      deltaCB (k, v) = return $ Just $ unWrapPar $ callback k v
      globalCB (LSMContents (mpRef:mps)) = do
        (mp, _) <- L.liftIO $ readIORef mpRef
        case mp of
          Nothing -> return ()
          Just m -> unWrapPar $ traverseWithKey_ (\k v -> forkHP mh $ callback k v) m

newEmptyMap :: Par d s (LayeredSatMap k s v)
newEmptyMap = newMap M.empty

newMap :: M.Map k v -> Par d s (LayeredSatMap k s v)
newMap m = WrapPar $ fmap (LayeredSatMap . WrapLVar) $ newLV $ do
  ref <- newIORef (Just m, return ())
  return $ LSMContents [ref]

newFromList :: (Ord k, Eq v) =>
               [(k,v)] -> Par d s (LayeredSatMap k s v)
newFromList = newMap . M.fromList

withCallbacksThenFreeze :: forall k v b s e . (HasPut e, HasGet e, HasFreeze e, Eq b) =>
                           LayeredSatMap k s v -> (k -> v -> Par e s ()) -> Par e s b -> Par e s b
withCallbacksThenFreeze (LayeredSatMap (WrapLVar lv)) callback action = do
  hp <- newPool
  res <- IV.new
  WrapPar $ freezeLVAfter lv (initCB hp res) deltaCB
  quiesce hp
  IV.get res
      where
        deltaCB (k, v) = return $ Just $ unWrapPar $ callback k v
        initCB hp resIV (LSMContents (mpRef:mps)) = do
          mp <- L.liftIO $ readIORef mpRef
          case mp of
            (Nothing, _) -> return ()
            (Just m, _) -> unWrapPar $ do
               traverseWithKey_ (\k v -> forkHP (Just hp) $ callback k v) m
               res <- action
               IV.put_ resIV res

-- | Flatten this stack of LVars, merging all layers together
-- according the the insert operation.
flatten :: (PartialJoinSemiLattice v, Ord k) => LayeredSatMap k Frzn v -> Maybe (M.Map k v)
flatten lsm@(LayeredSatMap lv) = unsafeDupablePerformIO $ do
  let LSMContents (mpRef:mps) = state lv
  (mp, onsat) <- readIORef mpRef
  case mp of
    Nothing -> return Nothing
    Just m -> do
      result <- flatten' m mps
      return result
  where
    flatten' acc [] = return $ Just acc
    flatten' acc (p:ps) = do
      parent <- readIORef p
      case parent of
        (Nothing, _) -> return Nothing
        (Just pMap, _) -> case merge acc pMap of
          Nothing -> return Nothing
          Just m -> flatten' m ps
    merge acc m = M.foldWithKey go (Just acc) m
    go _ _ Nothing = Nothing
    go k v (Just m) = case M.lookup k m of
      Nothing -> Just $ M.insert k v m
      Just old -> case joinMaybe old v of
        Nothing -> Nothing
        Just newVal -> Just $ M.insert k newVal m

forEach :: LayeredSatMap k s v -> (k -> v -> Par d s ()) -> Par d s ()
forEach = forEachHP Nothing

insert :: (Ord k, PartialJoinSemiLattice v, Eq v) =>
          k -> v -> LayeredSatMap k s v -> Par d s ()
insert !key !elm (LayeredSatMap (WrapLVar lv)) = WrapPar $ do
    let LSMContents (mpRef:mps) = L.state lv
    mp <- L.liftIO $ readIORef mpRef
    case mp of
      (Nothing, _) -> return ()
      (Just _, _) -> putLV_ lv putter
    where
      putter (LSMContents (mpRef:mps)) = do
        (x, act) <- L.liftIO $ atomicModifyIORef' mpRef update
        case act of
          Nothing -> return ()
          (Just a) -> do L.logStrLn 5 $ " [LayeredSatMap] insert saturated lvar" ++ lvid ++ ", running callback."
                         a
                         L.logStrLn 5 $ " [LayeredSatMap] lvar" ++ lvid ++ ", saturation callback completed."
        return (x, ())
      lvid = L.lvarDbgName lv
      delt x = (x, Nothing)
      update n@(Nothing, _) = (n, delt Nothing)
      update orig@(Just m, onsat) = case M.lookup key m of
        Nothing -> ((Just $ M.insert key elm m, onsat), delt $ Just (key, elm))
        Just oldVal -> case joinMaybe elm oldVal of
          Just newVal -> if newVal == oldVal
                         then (orig, delt Nothing)
                         else ((Just $ M.insert key newVal m, onsat), delt $ Just (key, newVal))
          Nothing -> ((Nothing, onsat), (Nothing, Just onsat))

-- | Create a new LVar by pushing a layer onto the existing contents
-- of this map.
pushLayer :: LayeredSatMap k s v -> Par d s (LayeredSatMap k s v)
pushLayer orig@(LayeredSatMap (WrapLVar lv)) = do
  let LSMContents mps = L.state lv
  ref <- WrapPar $ L.liftIO $ newIORef (Just M.empty, return ())
  WrapPar $ fmap (LayeredSatMap . WrapLVar) $ newLV $ return $ LSMContents $ ref:mps

whenSat :: LayeredSatMap k s v -> Par d s () -> Par d s ()
whenSat (LayeredSatMap lv) (WrapPar newact) = WrapPar $ do
    L.logStrLn 5 " [LayeredSatMap] whenSat issuing atomicModifyIORef on state"
    let LSMContents (mpRef:mps) = state lv
    x <- L.liftIO $ atomicModifyIORef' mpRef fn
    case x of
      Nothing -> L.logStrLn 5 " [LayeredSatMap] whenSat: not yet saturated, registered only."
      Just a -> L.logStrLn 5 " [LayeredSatMap] whenSat invoking saturation callback" >> a
    where
      fn n@(Nothing, _) = (n, Just newact)
      fn (Just m, onsat) = let onsat' = onsat >> newact in
        ((Just m, onsat'), Nothing)

saturate :: LayeredSatMap k s v -> Par d s ()
saturate (LayeredSatMap lv) = WrapPar $ do
    let lvid = L.lvarDbgName $ unWrapLVar lv
        LSMContents (mpRef:mps) = state lv
    L.logStrLn 5 $ " [LayeredSatMap] saturate: explicity saturating lvar " ++ lvid
    act <- L.liftIO $ atomicModifyIORef' mpRef fn
    case act of
      Nothing -> L.logStrLn 5 $ " [LayeredSatMap] saturate: done saturating lvar " ++ lvid ++ ", no callbacks to invoke."
      Just a -> L.logStrLn 5 (" [SatMap] saturate: done saturating lvar " ++ lvid ++ ".  Now invoking callback.") >> a
    where
      fn n@(Nothing, _) = (n, Nothing)
      -- Should this be returning (Nothing, return ()) or something? We don't need the callback anymore
      fn (Just m, onsat) = ((Nothing, onsat), Just onsat)

instance DeepFrz a => DeepFrz (LayeredSatMap k s a) where
 type FrzType (LayeredSatMap k s a) = LayeredSatMap k Frzn (FrzType a) -- No need to recur deeper.
 frz = unsafeCoerce# -- Can't use unsafeCoerceLVar due to LVarData1 constraint

test0 :: [Maybe (M.Map String Int)]
test0 = map flatten $ runParThenFreeze $ do
  m <- newEmptyMap
  insert "foo" (0 :: Int) m
  newLayer1 <- pushLayer m
  newLayer2 <- pushLayer m
  insert "foo" 2 newLayer1
  insert "foo" 1 newLayer2 -- should fail
  insert "bar" 48 m
  return [m, newLayer1, newLayer2]
