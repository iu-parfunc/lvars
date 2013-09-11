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
{-# LANGUAGE GADTs #-}

module Data.LVar.SLSet
       (
         -- * Basic operations
         ISet, 
         newEmptySet, newSet, newFromList,
         putInSet, waitElem, waitSize, 

         -- * Iteration and callbacks
         forEach, forEachHP,

         -- * Quasi-deterministic operations
         freezeSetAfter, withCallbacksThenFreeze, 

         -- * Higher-level derived operations
         copy, traverseSet, traverseSet_, union, intersection,
         cartesianProd, cartesianProds, 

         -- * Alternate versions of derived ops that expose HandlerPools they create.
         traverseSetHP, traverseSetHP_,
         cartesianProdHP, cartesianProdsHP
       ) where 

import qualified Data.Foldable as F
import           Data.Concurrent.SkipListMap as SLM
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.LVar.IVar as IV
import           Control.Monad
import           Control.LVish as LV
import           Control.LVish.DeepFrz  as DF
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV,
                                                freezeLVAfter, liftIO, addHandler)
import qualified Control.LVish.SchedIdempotent as L
import           System.IO.Unsafe (unsafeDupablePerformIO)

------------------------------------------------------------------------------
-- ISets implemented via SkipListMap
------------------------------------------------------------------------------


data ISet s a = Ord a => ISet {-# UNPACK #-}!(LVar s (SLM.SLMap a ()) a)
-- TODO: Address the possible inefficiency of carrying Ord dictionaries at runtime.

unISet (ISet lv) = lv

-- | Physical identity, just as with IORefs.
instance Eq (ISet s v) where
  ISet slm1 == ISet slm2 = state slm1 == state slm2
  
-- In order to make freeze an O(1) operation, ideally the Snapshot type function
-- would basically just be a cast from the mutable to the immutable form of the data
-- structure.

instance LVarData1 ISet where
  newBottom = newEmptySet -- ARGH!

  freeze orig@(ISet (WrapLVar lv)) =
    WrapPar$ do freezeLV lv; return (unsafeCoerceLVar orig)

  -- | Get the exact contents of the set.  Using this may cause your
  -- program to exhibit a limited form of nondeterminism: it will never
  -- return the wrong answer, but it may include synchronization bugs
  -- that can (nondeterministically) cause exceptions.
  sortFreeze (ISet (WrapLVar lv)) = WrapPar $ do
    -- freezeSet :: Ord a => ISet s a -> QPar s (ISet Frzn a)    
    freezeLV lv
    set <- L.liftIO $ SLM.foldlWithKey
             (\s elm () -> return $ S.insert elm s) S.empty (L.state lv)
    return (AFoldable set)

-- TODO: add member
member :: a -> ISet Frzn a -> Bool
member = error "FINISHME: SLSet member"

instance F.Foldable (ISet Trvrsbl) where
  foldr fn zer (ISet (WrapLVar lv)) =
    unsafeDupablePerformIO $
    SLM.foldlWithKey (\ a k _v -> return (fn k a))
                           zer (L.state lv)

-- | The default number of skiplist levels
defaultLevels :: Int
defaultLevels = 8

-- | Create a new, empty, monotonically growing 'ISet'.
newEmptySet :: Ord a => Par d s (ISet s a)
newEmptySet = newEmptySet_ defaultLevels

-- | Create a new, empty, monotonically growing 'ISet', with the given number of
-- skiplist levels.
newEmptySet_ :: Ord a => Int -> Par d s (ISet s a)
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
          SLM.foldlWithKey (\() v () -> forkHP (Just hp) $ callback v) () slm
          x <- action -- Any additional puts here trigger the callback.
          IV.put_ res x
  WrapPar $ L.addHandler (Just hp) (unWrapLVar lv) initCB deltCB
  
  -- We additionally have to quiesce here because we fork the inital set of
  -- callbacks on their own threads:
  quiesce hp
  IV.get res


--------------------------------------------------------------------------------

-- | Add an (asynchronous) callback that listens for all new elements added to
-- the set, optionally enrolled in a handler pool.
forEachHP :: Maybe HandlerPool            -- ^ optional pool to enroll in 
           -> ISet s a                    -- ^ Set to listen to
           -> (a -> Par d s ())           -- ^ callback
           -> Par d s ()
forEachHP hp (ISet (WrapLVar lv)) callb = WrapPar $ 
    L.addHandler hp lv globalCB (\x -> return$ Just$ unWrapPar$ callb x)
  where
    globalCB slm = 
      return $ Just $ unWrapPar $
        SLM.foldlWithKey (\() v () -> forkHP hp $ callb v) () slm

-- | Add an (asynchronous) callback that listens for all new elements added to
-- the set
forEach :: ISet s a -> (a -> Par d s ()) -> Par d s ()
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
traverseSet f s = traverseSetHP Nothing f s

-- | An imperative-style, inplace version of 'traverseSet' that takes the output set
-- as an argument.
traverseSet_ :: Ord b => (a -> Par d s b) -> ISet s a -> ISet s b -> Par d s ()
traverseSet_ f s o = traverseSetHP_ Nothing f s o

-- | Return a new set which will (ultimately) contain everything in either input set.
union :: Ord a => ISet s a -> ISet s a -> Par d s (ISet s a)
union s1 s2 = do
  os <- newEmptySet
  forEach s1 (`putInSet` os)
  forEach s2 (`putInSet` os)
  return os

-- | Build a new set which will contain the intersection of the two input sets.
intersection :: Ord a => ISet s a -> ISet s a -> Par d s (ISet s a)
-- Can we do intersection with only the public interface?  It should be monotonic.
--   AJT: You could do it using cartesian product...
-- Well, for now we cheat and use liftIO:
intersection s1 s2 = do
  os <- newEmptySet
  forEach s1 (fn os s2)
  forEach s2 (fn os s1)
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
cartesianProd s1 s2 = cartesianProdHP Nothing s1 s2 
  
-- | Takes the cartesian product of several sets.
cartesianProds :: Ord a => [ISet s a] -> Par d s (ISet s [a])
cartesianProds ls = cartesianProdsHP Nothing ls

--------------------------------------------------------------------------------
-- Alternate versions of functions that EXPOSE the HandlerPools
--------------------------------------------------------------------------------

-- TODO: unionHP, intersectionHP...

-- | Variant that optionally ties the handlers to a pool.
traverseSetHP :: Ord b => Maybe HandlerPool -> (a -> Par d s b) -> ISet s a ->
                 Par d s (ISet s b)
traverseSetHP mh fn set = do
  os <- newEmptySet
  traverseSetHP_ mh fn set os  
  return os

-- | Variant that optionally ties the handlers to a pool.
traverseSetHP_ :: Ord b => Maybe HandlerPool -> (a -> Par d s b) -> ISet s a -> ISet s b ->
                  Par d s ()
traverseSetHP_ mh fn set os = do
  forEachHP mh set $ \ x -> do 
    x' <- fn x
    putInSet x' os

-- | Variant of 'cartesianProd' that optionally ties the handlers to a pool.
cartesianProdHP :: (Ord a, Ord b) => Maybe HandlerPool -> ISet s a -> ISet s b ->
                   Par d s (ISet s (a,b))
cartesianProdHP mh s1 s2 = do
  -- This is implemented much like intersection:
  os <- newEmptySet
  forEachHP mh s1 (fn os s2 (\ x y -> (x,y)))
  forEachHP mh s2 (fn os s1 (\ x y -> (y,x)))
  return os
 where
  -- This is expensive, but we've got to do it from both sides to counteract races:
  fn outSet other@(ISet lv) cmbn elm1 = 
    SLM.foldlWithKey (\() elm2 () -> putInSet (cmbn elm1 elm2) outSet) () (state lv)

-- | Variant of 'cartesianProds' that optionally ties the handlers to a pool.
cartesianProdsHP :: Ord a => Maybe HandlerPool -> [ISet s a] ->
                    Par d s (ISet s [a])
cartesianProdsHP mh [] = newEmptySet
cartesianProdsHP mh ls = do
#if 1
  -- Case 1: recursive definition in terms of pairwise products:
  -- It would be best to create a balanced tree of these, I believe:
  let loop [lst]     = traverseSetHP mh (\x -> return [x]) lst -- Inefficient!
      loop (nxt:rst) = do
        partial <- loop rst
        p1 <- cartesianProdHP mh nxt partial
        traverseSetHP mh (\ (x,tl) -> return (x:tl)) p1 -- Inefficient!!
  loop ls
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

