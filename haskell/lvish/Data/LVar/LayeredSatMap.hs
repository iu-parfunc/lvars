{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.LVar.LayeredSatMap
    ( forEachHP
    , newEmptyMap
    , newMap
    , newFromList
    , withCallbacksThenFreeze
    , forEach
    , insert
    , whenSat
    , saturate
    , fromIMap
    , t0
    ) where

import Data.LVar.SatMap(PartialJoinSemiLattice(..))

import           Control.LVish.DeepFrz.Internal
import           Control.LVish.DeepFrz (runParThenFreeze)
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, putLV_, getLV, freezeLV, freezeLVAfter)
import qualified Control.LVish.SchedIdempotent as L
import qualified Data.LVar.IVar as IV
import           Data.LVar.Generic as G
import           Data.LVar.Generic.Internal (unsafeCoerceLVar)
import           Data.UtilInternal (traverseWithKey_)

import Control.LVish

import Data.IORef
import qualified Data.Map as M
import qualified Data.Foldable as F
import System.IO.Unsafe (unsafeDupablePerformIO)

data LayeredSatMap k s v = LayeredSatMap (LVar s (IORef (LayeredSatMapContents k v)) (k, v))

data LayeredSatMapContents k v = Contents (M.Map k v) (Maybe (IORef (LayeredSatMapContents k v))) OnSat
                               | Saturated

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
      globalCB ref = do
        mp <- L.liftIO $ readIORef ref
        case mp of
          Saturated -> return ()
          Contents m _ _ -> unWrapPar $ traverseWithKey_ (\k v -> forkHP mh $ callback k v) m

newEmptyMap :: Par d s (LayeredSatMap k s v)
newEmptyMap = WrapPar $ fmap (LayeredSatMap . WrapLVar) $ newLV $ newIORef $ Contents M.empty Nothing $ return ()

newMap :: M.Map k v -> Par d s (LayeredSatMap k s v)
newMap m = WrapPar $ fmap (LayeredSatMap . WrapLVar) $ newLV $ newIORef $ Contents m Nothing $ return ()

newFromList :: (Ord k, Eq v) =>
               [(k,v)] -> Par d s (LayeredSatMap k s v)
newFromList = newMap . M.fromList

withCallbacksThenFreeze :: forall k v b s . Eq b =>
                           LayeredSatMap k s v -> (k -> v -> QPar s ()) -> QPar s b -> QPar s b
withCallbacksThenFreeze (LayeredSatMap (WrapLVar lv)) callback action = do
  hp <- newPool
  res <- IV.new
  WrapPar $ freezeLVAfter lv (initCB hp res) deltaCB
  quiesce hp
  IV.get res
      where
        deltaCB (k, v) = return $ Just $ unWrapPar $ callback k v
        initCB hp resIV ref = do
          mp <- L.liftIO $ readIORef ref
          case mp of
            Saturated -> return ()
            c@(Contents m parent _) -> unWrapPar $ do
               traverseWithKey_ (\k v -> forkHP (Just hp) $ callback k v) m
               res <- action
               -- TODO: when do we flatten?
               IV.put_ resIV res

-- | Flatten this stack of LVars, merging all layers together
-- according the the insert operation. To be called when the map is
-- frozen.
flatten :: (Ord k, PartialJoinSemiLattice v, Eq v) => LayeredSatMap k s v -> Par d s ()
flatten lsm@(LayeredSatMap (WrapLVar lv)) = do
  contents <- WrapPar $ L.liftIO $ readIORef $ L.state lv
  flatten' contents
    where
      flatten' contents = case contents of
        Saturated -> return ()
        Contents _ Nothing _ -> return () -- the bottom layer of the map
        Contents m (Just parent) onsat -> do -- recursively flatten, then merge
          parentMap <- WrapPar $ L.liftIO $ readIORef parent
          flatten' parentMap
          parentMap' <- WrapPar $ L.liftIO $ readIORef parent
          case parentMap' of
            Saturated -> return ()
            -- TODO: This may need to be replaced with a forEach so
            -- that we see later updates to the parent map. If we
            -- guarantee that flatten only occurs after freezing this
            -- is fine; otherwise this will take a bit of type
            -- rearranging.
            Contents m _ _ -> traverseWithKey_ (\k v -> insert k v lsm) m

forEach :: LayeredSatMap k s v -> (k -> v -> Par d s ()) -> Par d s ()
forEach = forEachHP Nothing

insert :: (Ord k, PartialJoinSemiLattice v, Eq v) =>
          k -> v -> LayeredSatMap k s v -> Par d s ()
insert !key !elm (LayeredSatMap (WrapLVar lv)) = WrapPar $ do
    contents <- L.liftIO (readIORef (L.state lv))
    case contents of
      Saturated -> return ()
      Contents _ _ _ -> putLV_ lv putter
    where
      putter ref = do
        (x, act) <- L.liftIO $ atomicModifyIORef' ref update
        case act of
          Nothing -> return ()
          (Just a) -> do L.logStrLn 5 $ " [LayeredSatMap] insert saturated lvar" ++ lvid ++ ", running callback."
                         a
                         L.logStrLn 5 $ " [LayeredSatMap] lvar" ++ lvid ++ ", saturation callback completed."
        return (x, ())
      lvid = L.lvarDbgName lv
      delt x = (x, Nothing)
      update Saturated = (Saturated, delt Nothing)
      update orig@(Contents mp parent onsat) =
          case M.lookup key mp of
            Nothing -> (Contents (M.insert key elm mp) parent onsat,
                        delt $ Just (key, elm))
            Just oldVal ->
                case joinMaybe elm oldVal of
                  Just newVal -> if newVal == oldVal
                                 then (orig, delt Nothing)
                                 else (Contents (M.insert key newVal mp) parent onsat,
                                       delt $ Just (key, newVal))
                  Nothing -> (Saturated, (Nothing, Just onsat))

-- | Create a new LVar by pushing a layer onto the existing contents
-- of this map.
pushLayer :: LayeredSatMap k s v -> Par d s (LayeredSatMap k s v)
pushLayer orig@(LayeredSatMap (WrapLVar lv)) = do
    contents <- WrapPar $ L.liftIO $ readIORef $ L.state lv
    case contents of
      -- if the map is already saturated, just return it
      Saturated -> return orig
      c@(Contents mp parent onsat) -> do
          parentRef <- WrapPar $ L.liftIO $ newIORef c
          let ref = newIORef $ Contents M.empty (Just parentRef) (return ())
          new <- WrapPar $ fmap (LayeredSatMap . WrapLVar) $ newLV ref
          whenSat orig $ saturate new
          return new

whenSat :: LayeredSatMap k s v -> Par d s () -> Par d s ()
whenSat (LayeredSatMap lv) (WrapPar newact) = WrapPar $ do
    L.logStrLn 5 " [LayeredSatMap] whenSat issuing atomicModifyIORef on state"
    x <- L.liftIO $ atomicModifyIORef' (state lv) fn
    case x of
      Nothing -> L.logStrLn 5 " [LayeredSatMap] whenSat: not yet saturated, registered only."
      Just a -> L.logStrLn 5 " [LayeredSatMap] whenSat invoking saturation callback" >> a
    where
      fn :: LayeredSatMapContents k v -> (LayeredSatMapContents k v, Maybe (L.Par ()))
      fn Saturated = (Saturated, Just newact)
      fn (Contents m ref onsat) = let onsat' = onsat >> newact
                                  in (Contents m ref onsat', Nothing)
--   FN :: LayeredSatMapContents k v -> (LayeredSatMapContents k v, Maybe (L.Par ()))

saturate :: LayeredSatMap k s v -> Par d s ()
saturate (LayeredSatMap lv) = WrapPar $ do
    let lvid = L.lvarDbgName $ unWrapLVar lv
    L.logStrLn 5 $ " [LayeredSatMap] saturate: explicity saturating lvar " ++ lvid
    act <- L.liftIO $ atomicModifyIORef' (state lv) fn
    case act of
      Nothing -> L.logStrLn 5 $ " [LayeredSatMap] saturate: done saturating lvar " ++ lvid ++ ", no callbacks to invoke."
      Just a -> L.logStrLn 5 (" [SatMap] saturate: done saturating lvar " ++ lvid ++ ".  Now invoking callback.") >> a
    where
      fn Saturated = (Saturated, Nothing)
      fn (Contents _ _ onsat) = (Saturated, Just onsat)

fromIMap :: LayeredSatMap k Frzn v -> Maybe (M.Map k v)
fromIMap (LayeredSatMap lv) = unsafeDupablePerformIO $ do
    x <- readIORef $ state lv
    -- Note that this requires us to ensure that a frozen LSM is
    -- always flattened. Maybe add a sanity check that the parent
    -- layer IORef is Nothing?
    case x of
      Contents m _ _ -> return $! Just m
      Saturated -> return Nothing

instance LVarData1 (LayeredSatMap k) where
    freeze orig@(LayeredSatMap (WrapLVar lv)) = WrapPar $ do
        -- Hm... need a (PartialJoinSemilattice v) constraint here in
        -- order to flatten on freeze.
        -- unWrapPar $ flatten orig
        freezeLV lv
        return $ unsafeCoerceLVar orig
    addHandler mh mp fn = forEachHP mh mp (\_ v -> fn v)
    sortFrzn (LayeredSatMap lv) =
        case unsafeDupablePerformIO $ readIORef $ state lv of
          Saturated -> AFoldable []
          Contents m _ _ -> AFoldable m

instance F.Foldable (LayeredSatMap k Frzn) where
  foldr fn zer (LayeredSatMap lv) =
    let mp = unsafeDupablePerformIO (readIORef (state lv)) in
    case mp of
      Saturated -> zer
      Contents m _ _ -> F.foldr fn zer m

instance F.Foldable (LayeredSatMap k Trvrsbl) where
  foldr fn zer mp = F.foldr fn zer (castFrzn mp)

instance DeepFrz a => DeepFrz (LayeredSatMap k s a) where
 type FrzType (LayeredSatMap k s a) = LayeredSatMap k Frzn (FrzType a) -- No need to recur deeper.
 frz = unsafeCoerceLVar

-- TO ADD:
------------------------

-- (1) pushLayer -- done

-- (2) a way for the merging to happen UPON FREEZE,
--  That will require at least an internal mechanism for post-freeze callbacks.

t0 = (\(a, b) -> (fromIMap a, fromIMap b)) $ runParThenFreeze $ do
  m <- newEmptyMap
  insert "hi" (32::Int) m
  insert "hi" (34::Int) m
  insert "there" (1::Int) m
  newLayer <- pushLayer m
  insert "hi" (46 :: Int) newLayer
  flatten newLayer
  -- current issue: ("foo", 0) is not seen by newLayer.
  insert "foo" 0 m
  flatten newLayer
  return (m, newLayer)
