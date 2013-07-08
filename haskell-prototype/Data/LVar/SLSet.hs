{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Data.LVar.SLSet
       (
         -- * Basic operations
         ISet, 
--         Snapshot(ISetSnap),
         newEmptySet, newSet, newFromList,
         putInSet, waitElem, waitSize, 

         -- * Iteration and callbacks
         forEach, addHandler,

         -- * Quasi-deterministic operations
         freezeSetAfter, withCallbacksThenFreeze, freezeSet,

         -- * Higher-level derived operations
         copy, traverseSet, traverseSet_, union, intersection,
         cartesianProd, cartesianProds, 

         -- * Alternate versions of derived ops that expose HandlerPools they create.
         forEachHP, traverseSetHP, traverseSetHP_,
         cartesianProdHP, cartesianProdsHP
       ) where 

import qualified Data.Foldable as F
import           Data.Concurrent.SkipListMap as SLM
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.LVar.IVar as IV
import           Control.Monad
import           Control.LVish as LV hiding (addHandler)
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV,
                                                freezeLVAfter, liftIO)
import qualified Control.LVish.SchedIdempotent as L

------------------------------------------------------------------------------
-- ISets implemented via SkipListMap
------------------------------------------------------------------------------

newtype ISet s a = ISet (LVar s (SLM.SLMap a ()) a)

unISet (ISet lv) = lv

-- | Physical identity, just as with IORefs.
instance Eq (ISet s v) where
  ISet slm1 == ISet slm2 = state slm1 == state slm2
  
-- AJT: I'm not sure what to do with the following, given that freezeSet needs
-- an Ord contraint...
  
-- instance LVarData1 ISet where
--   newtype Snapshot ISet a = ISetSnap (S.Set a)
--       deriving (Show,Ord,Read,Eq)
--   freeze    = fmap ISetSnap . freezeSet
--   newBottom = newEmptySet

  -- TODO: traverseSnap

-- | The default number of skiplist levels
defaultLevels :: Int
defaultLevels = 8

-- | Create a new, empty, monotonically growing 'ISet'.
newEmptySet :: Par d s (ISet s a)
newEmptySet = newEmptySet_ defaultLevels

-- | Create a new, empty, monotonically growing 'ISet', with the given number of
-- skiplist levels.
newEmptySet_ :: Int -> Par d s (ISet s a)
newEmptySet_ n = fmap (ISet . WrapLVar) $ WrapPar $ newLV $ SLM.newSLMap n

-- | Create a new set populated with initial elements.
newSet :: S.Set a -> Par d s (ISet s a)
newSet s = error "TODO"

-- | Create a new 'ISet' drawing initial elements from an existing list.
newFromList :: Ord a => [a] -> Par d s (ISet s a)
newFromList ls = newFromList_ ls defaultLevels

-- | Create a new 'ISet' drawing initial elements from an existing list, with
-- the given number of skiplist levels.
newFromList_ :: Ord a => [a] -> Int -> Par d s (ISet s a)
newFromList_ ls n = do  
  s@(ISet lv) <- newEmptySet_ n
  forM_ ls $ \x -> LI.liftIO $ SLM.putIfAbsent (state lv) x $ return ()
  return s

-- (Todo: in production you might want even more ... like going from a Vector)

--------------------------------------------------------------------------------
-- Quasi-deterministic ops:
--------------------------------------------------------------------------------

type QPar = Par QuasiDet 

-- | Freeze an 'ISet' after a specified callback/handler is done running.  This
-- differs from withCallbacksThenFreeze by not taking an additional action to run in
-- the context of the handlers.
--
--    (@'freezeSetAfter' 's' 'f' == 'withCallbacksThenFreeze' 's' 'f' 'return ()' @)
freezeSetAfter :: ISet s a -> (a -> QPar s ()) -> QPar s ()
freezeSetAfter s f = withCallbacksThenFreeze s f (return ())
  
-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- value of the provided action.
withCallbacksThenFreeze :: Eq b => ISet s a -> (a -> QPar s ()) -> QPar s b -> QPar s b
withCallbacksThenFreeze (ISet lv) callback action = do
  hp  <- newPool 
  res <- IV.new -- TODO, specialize to skip this when the init action returns ()
  let deltCB x = return$ Just$ unWrapPar$ callback x
      initCB slm = do
        -- The implementation guarantees that all elements will be caught either here,
        -- or by the delta-callback:
        return $ Just $ unWrapPar $ do
          SLM.foldlWithKey (\() v () -> forkInPool hp $ callback v) () slm
          x <- action -- Any additional puts here trigger the callback.
          IV.put_ res x
  WrapPar $ L.addHandler hp (unWrapLVar lv) initCB deltCB
  
  -- We additionally have to quiesce here because we fork the inital set of
  -- callbacks on their own threads:
  quiesce hp
  IV.get res

-- | Get the exact contents of the set.  Using this may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
freezeSet :: Ord a => ISet s a -> QPar s (S.Set a)
freezeSet (ISet (WrapLVar lv)) = WrapPar $ do
  freezeLV lv
  L.liftIO $ SLM.foldlWithKey (\s elm () -> return $ S.insert elm s) S.empty (L.state lv)

--------------------------------------------------------------------------------

-- | Add an (asynchronous) callback that listens for all new elements added to the set.
addHandler :: HandlerPool                 -- ^ pool to enroll in 
           -> ISet s a                    -- ^ Set to listen to
           -> (a -> Par d s ())           -- ^ callback
           -> Par d s ()
addHandler hp (ISet lv) callb = WrapPar $ 
    L.addHandler hp (unWrapLVar lv) globalCB (\x -> return$ Just$ unWrapPar$ callb x)
  where
    globalCB slm = 
      return $ Just $ unWrapPar $
        SLM.foldlWithKey (\() v () -> forkInPool hp $ callb v) () slm

-- | Shorthandfor creating a new handler pool and adding a single handler to it.
forEach :: ISet s a -> (a -> Par d s ()) -> Par d s HandlerPool
forEach = forEachHP Nothing

-- | Put a single element in the set.  (WHNF) Strict in the element being put in the
-- set.     
putInSet :: Ord a => a -> ISet s a -> Par d s ()
putInSet !elm (ISet lv) = WrapPar$ putLV (unWrapLVar lv) putter
  where putter slm = do
          putRes <- SLM.putIfAbsent slm elm $ return ()
          case putRes of
            Added _ -> return $ Just elm
            Found _ -> return Nothing 

-- | Wait for the set to contain a specified element.
waitElem :: Ord a => a -> ISet s a -> Par d s ()
waitElem !elm (ISet (WrapLVar lv)) = WrapPar $
    getLV lv globalThresh deltaThresh
  where
    globalThresh slm _frzn = SLM.find slm elm
    deltaThresh e2 | e2 == elm = return $ Just ()
                   | otherwise = return Nothing

-- | Wait on the SIZE of the set, not its contents.
waitSize :: Int -> ISet s a -> Par d s ()
waitSize !sz (ISet (WrapLVar lv)) = WrapPar$
    getLV lv globalThresh deltaThresh
  where
    globalThresh slm _ = do
      snapSize <- SLM.foldlWithKey (\n _ _ -> return $ n+1) 0 slm
      case snapSize >= sz of
        True  -> return (Just ())
        False -> return (Nothing)
    -- Here's an example of a situation where we CANNOT TELL if a delta puts it over
    -- the threshold.a
    deltaThresh _ = globalThresh (L.state lv) False

--------------------------------------------------------------------------------
-- Higher level routines that could be defined using the above interface.
--------------------------------------------------------------------------------

-- | Return a fresh set which will contain strictly more elements than the input set.
-- That is, things put in the former go in the latter, but not vice versa.
copy :: Ord a => ISet s a -> Par d s (ISet s a)
copy = traverseSet return 

-- | Establish monotonic map between the input and output sets.
traverseSet :: Ord b => (a -> Par d s b) -> ISet s a -> Par d s (ISet s b)
traverseSet f s = fmap snd $ traverseSetHP Nothing f s

-- | An imperative-style, inplace version of 'traverseSet' that takes the output set
-- as an argument.
traverseSet_ :: Ord b => (a -> Par d s b) -> ISet s a -> ISet s b -> Par d s ()
traverseSet_ f s o = void $ traverseSetHP_ Nothing f s o

-- | Return a new set which will (ultimately) contain everything in either input set.
union :: Ord a => ISet s a -> ISet s a -> Par d s (ISet s a)
union s1 s2 = do
  hp <- newPool
  os <- newEmptySet
  addHandler hp s1 (`putInSet` os)
  addHandler hp s2 (`putInSet` os)
  return os

-- | Build a new set which will contain the intersection of the two input sets.
intersection :: Ord a => ISet s a -> ISet s a -> Par d s (ISet s a)
-- Can we do intersection with only the public interface?  It should be monotonic.
--   AJT: You could do it using cartesian product...
-- Well, for now we cheat and use liftIO:
intersection s1 s2 = do
  hp <- newPool
  os <- newEmptySet
  addHandler hp s1 (fn os s2)
  addHandler hp s2 (fn os s1)
  return os
 where  
  fn outSet other@(ISet lv) elm = do
    -- At this point 'elm' has ALREADY been added to "us", we check "them":    
    peek <- LI.liftIO $ SLM.find (state lv) elm
    case peek of
      Just _  -> putInSet elm outSet
      Nothing -> return ()

-- | Cartesian product of two sets.
cartesianProd :: (Ord a, Ord b) => ISet s a -> ISet s b -> Par d s (ISet s (a,b))
cartesianProd s1 s2 =
  fmap snd $ 
  cartesianProdHP Nothing s1 s2 
  
-- | Takes the cartesian product of several sets.
cartesianProds :: Ord a => [ISet s a] -> Par d s (ISet s [a])
cartesianProds ls =
  fmap snd $ 
  cartesianProdsHP Nothing ls

--------------------------------------------------------------------------------
-- Alternate versions of functions that EXPOSE the HandlerPools
--------------------------------------------------------------------------------

-- TODO: unionHP, intersectionHP...

-- | Variant of 'forEach' that optionally uses an existing handler pool.
forEachHP :: Maybe HandlerPool -> ISet s a -> (a -> Par d s ()) -> Par d s HandlerPool
forEachHP mh is cb = do 
   hp <- fromMaybe newPool (fmap return mh)
   addHandler hp is cb
   return hp

-- | Variant that optionally uses an existing handler pool.
traverseSetHP :: Ord b => Maybe HandlerPool -> (a -> Par d s b) -> ISet s a ->
                 Par d s (HandlerPool, ISet s b)
traverseSetHP mh fn set = do
  os <- newEmptySet
  hp <- traverseSetHP_ mh fn set os  
  return (hp,os)

-- | Variant that optionally uses an existing handler pool.
traverseSetHP_ :: Ord b => Maybe HandlerPool -> (a -> Par d s b) -> ISet s a -> ISet s b ->
                  Par d s HandlerPool
traverseSetHP_ mh fn set os = do
  forEachHP mh set $ \ x -> do 
    x' <- fn x
    putInSet x' os

-- | Variant of 'cartesianProd' that exposes the handler pool for quiescing and
-- optionally uses an existing, input handler pool.
cartesianProdHP :: (Ord a, Ord b) => Maybe HandlerPool -> ISet s a -> ISet s b ->
                   Par d s (HandlerPool, ISet s (a,b))
cartesianProdHP mh s1 s2 = do
  -- This is implemented much like intersection:
  hp <- fromMaybe newPool (fmap return mh)
  os <- newEmptySet
  addHandler hp s1 (fn os s2 (\ x y -> (x,y)))
  addHandler hp s2 (fn os s1 (\ x y -> (y,x)))
  return (hp,os)
 where
  -- This is expensive, but we've got to do it from both sides to counteract races:
  fn outSet other@(ISet lv) cmbn elm1 = 
    SLM.foldlWithKey (\() elm2 () -> putInSet (cmbn elm1 elm2) outSet) () (state lv)

-- | Variant of 'cartesianProds' that exposes the handler pool for quiescing and
-- optionally uses an existing, input handler pool.
cartesianProdsHP :: Ord a => Maybe HandlerPool -> [ISet s a] ->
                    Par d s (HandlerPool, ISet s [a])
cartesianProdsHP mh [] = do s <- newEmptySet
                            h <- fromMaybe newPool (fmap return mh) -- Pointless!
                            return (h,s)
cartesianProdsHP mh ls = do
  -- We use one handler pool for the whole network of related sets we create:
  hp <- fromMaybe newPool (fmap return mh)  
#if 1
  -- Case 1: recursive definition in terms of pairwise products:
  -- It would be best to create a balanced tree of these, I believe:
  let loop [lst]     = traverseSetHP (Just hp) (\x -> return [x]) lst -- Inefficient!
      loop (nxt:rst) = do
        (_, partial) <- loop rst
        (_,p1) <- cartesianProdHP (Just hp) nxt partial
        traverseSetHP (Just hp) (\ (x,tl) -> return (x:tl)) p1 -- Inefficient!!
  (_, x) <- loop ls
  return (hp, x)
#else
  os <- newEmptySet
  let loop done [] acc = acc
      loop done (nxt:rest) acc =
        addHandler hp nxt (fn os done rest)
        
--  forM_ ls $ \ inSet -> do 
--    addHandler hp s1 (fn os s2 (\ x y -> (x,y)))

  return os
 where
  fn outSet left right newElm = do
    peeksL <- liftIO$ mapM (readIORef . state . unISet) left
    peeksR <- liftIO$ mapM (readIORef . state . unISet) right

--    F.foldlM (\() elm2 -> putInSet (cmbn elm1 elm2) outSet) () peek
    return undefined
#endif

--------------------------------------------------------------------------------
-- Set specific DeepFreeze instances:
--------------------------------------------------------------------------------

-- AJT: killed this for now
#if 0

-- Teach it how to freeze WITHOUT the annoying snapshot constructor:

instance DeepFreeze (ISet s a) (S.Set a) where
  type Session (ISet s a) = s
  deepFreeze iv = do ISetSnap m <- freeze iv
                     return m

#if 0
------------------------------------------------------------
-- Most general, but causes overlapping instances
------------------------------------------------------------    
instance (DeepFreeze a b, Ord b, Session a ~ s0) =>
         DeepFreeze (ISet s0 a) (S.Set b)
  where
    type Session (ISet s0 a) = s0
    deepFreeze = freezer     
      
freezer :: forall a b s0 . (DeepFreeze a b, Ord b, Session a ~ s0) =>
           (ISet s0 a) -> Par QuasiDet s0 (S.Set b)
freezer from = do
      x <- freezeSet from
      let fn :: a -> S.Set b -> QPar s0 (S.Set b)
          fn elm acc = do elm' <- deepFreeze elm
                          return (S.insert elm' acc)
      y <- F.foldrM fn S.empty x
      return y

#else
------------------------------------------------------------
-- Compromise to avoid overlap
------------------------------------------------------------
instance (LVarData1 f, DeepFreeze (f s0 a) b, Ord b) =>
         DeepFreeze (ISet s0 (f s0 a)) (S.Set b)  where
    type Session (ISet s0 (f s0 a)) = s0
    deepFreeze from = do
      x <- freezeSet from
      let fn :: f s0 a -> S.Set b -> QPar s0 (S.Set b)
          fn elm acc = do elm' <- deepFreeze elm
                          return (S.insert elm' acc)
      y <- F.foldrM fn S.empty x 
      return y      
#endif

#endif