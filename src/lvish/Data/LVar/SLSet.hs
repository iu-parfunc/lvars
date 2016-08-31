{-# LANGUAGE Trustworthy #-}
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

{-|

This module provides sets that only grow.  It is based on a
/concurrent skip list/ representation of sets.

This module is usually a more efficient alternative to
"Data.LVar.PureSet", and provides almost the same interface.  However,
it's always good to test multiple data structures if you have a
performance-critical use case.

 -}

module Data.LVar.SLSet
       (
         -- * Basic operations
         ISet, 
         newEmptySet, newSet, newFromList,
         insert, waitElem, waitSize, 
         member,
         
         -- * Iteration and callbacks
         forEach, forEachHP,

         -- * Freezing and quasi-deterministic operations
         freezeSetAfter, withCallbacksThenFreeze, 
         fromISet,

         -- * Higher-level derived operations
         copy, traverseSet, traverseSet_, union, intersection,
         cartesianProd, cartesianProds, 

         -- * Alternate versions of derived ops that expose @HandlerPool@s they create
         traverseSetHP, traverseSetHP_,
         cartesianProdHP, cartesianProdsHP
       ) where 

import qualified Data.Foldable as F
import           Data.Concurrent.SkipListMap as SLM
import           Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.LVar.IVar as IV
import           Data.LVar.Generic
import           Data.LVar.Generic.Internal (unsafeCoerceLVar)
import           Control.Monad
import           Control.LVish as LV
import           Control.LVish.DeepFrz.Internal
import           Control.LVish.Internal as LI
import           Control.LVish.Internal.SchedIdempotent (newLV, putLV, getLV, freezeLV)
import qualified Control.LVish.Internal.SchedIdempotent as L
import           System.IO.Unsafe (unsafeDupablePerformIO)

------------------------------------------------------------------------------
-- ISets implemented via SkipListMap
------------------------------------------------------------------------------

-- | The set datatype itself.  Like all other LVars, it has an @s@ parameter (think
-- `STRef`) in addition to the @a@ parameter that describes the type of elements
-- in the set.
--
-- Performance note: this data structure reduces contention between parallel
-- computations inserting into the map, but all /blocking/ computations are not as
-- scalable.  All continuations waiting for not-yet-present elements will currently
-- share a single queue [2013.09.26].
data ISet s a = Ord a => ISet {-# UNPACK #-}!(LVar s (SLM.SLMap a ()) a)
-- TODO: Address the possible inefficiency of carrying Ord dictionaries at runtime.

-- | Physical identity, just as with `IORef`s.
instance Eq (ISet s v) where
  ISet slm1 == ISet slm2 = state slm1 == state slm2
  
-- | An `ISet` can be treated as a generic container LVar.
instance LVarData1 ISet where
  -- In order to make freeze an O(1) operation, freeze is just a cast from the
  -- mutable to the immutable form of the data structure.
  freeze orig@(ISet (WrapLVar lv)) =
    WrapPar$ do freezeLV lv; return (unsafeCoerceLVar orig)
  addHandler = forEachHP                
  -- | We can do better than the default here; this is /O(1)/:  
  sortFrzn (is :: ISet Frzn a) = AFoldable is

instance LVarWBottom ISet where
  type LVContents ISet a = (Ord a)
  newBottom = newEmptySet

-- | The `ISet`s in this module also have the special property that they support an
-- /O(1)/ freeze operation which immediately yields a `Foldable` container
-- (`snapFreeze`).
instance OrderedLVarData1 ISet where
  snapFreeze is = unsafeCoerceLVar <$> freeze is

-- `ISet` values can be returned as the result of a
--  `runParThenFreeze`.  Hence they need a `DeepFrz` instance.
--  @DeepFrz@ is just a type-coercion.  No bits flipped at runtime.
instance DeepFrz a => DeepFrz (ISet s a) where
  type FrzType (ISet s a) = ISet Frzn (FrzType a)
  frz = unsafeCoerceLVar

instance Show a => Show (ISet Frzn a) where
  show lv = "{ISet: " ++
     (concat $ intersperse ", " $ map show $ 
      F.foldr (\ elm ls -> elm : ls) []
      (unsafeCoerceLVar lv :: ISet Trvrsbl a)) ++ "}"

-- | For convenience only; the user could define this.
instance Show a => Show (ISet Trvrsbl a) where
  show lv = show (castFrzn lv)

--------------------------------------------------------------------------------

-- | Test whether an element is in a frozen image of a set.
member :: a -> ISet Frzn a -> Bool
member elm (ISet (WrapLVar lv)) =
  case unsafeDupablePerformIO (SLM.find (L.state lv) elm) of
    Just () -> True
    Nothing -> False

-- As with all LVars, after freezing, map elements can be consumed. In
-- the case of this `ISet` implementation, it need only be `Frzn`, not
-- `Trvrsbl`.
instance F.Foldable (ISet Frzn) where
  foldr fn zer (ISet (WrapLVar lv)) =
    unsafeDupablePerformIO $
    SLM.foldlWithKey id (\ a k _v -> return (fn k a))
                        zer (L.state lv)

-- Of course, the stronger `Trvrsbl` state is still fine for folding.
instance F.Foldable (ISet Trvrsbl) where
  foldr fn zer mp = F.foldr fn zer (castFrzn mp)


-- | The default number of skiplist levels
defaultLevels :: Int
defaultLevels = 8

-- | Create a new, empty, monotonically growing set.
newEmptySet :: Ord a => Par e s (ISet s a)
newEmptySet = newEmptySet_ defaultLevels

-- | Tuning: Create a new, empty, monotonically growing set, with the given number
-- of skip list levels.
newEmptySet_ :: Ord a => Int -> Par e s (ISet s a)
newEmptySet_ n = fmap (ISet . WrapLVar) $ WrapPar $ newLV $ SLM.newSLMap n

-- | Create a new `ISet` populated with initial elements.
newSet :: Ord a => S.Set a -> Par e s (ISet s a)
newSet set = 
 fmap (ISet . WrapLVar) $ WrapPar $ newLV $ do
  slm <- SLM.newSLMap defaultLevels
  F.foldlM (\ () elm -> do
              SLM.Added _ <- SLM.putIfAbsent slm elm (return ())
              return ()
           ) () set
  return slm

-- | A simple convenience function.   Create a new 'ISet' drawing initial elements from an existing list.
newFromList :: Ord a => [a] -> Par e s (ISet s a)
newFromList ls = newFromList_ ls defaultLevels

-- | Create a new 'ISet' drawing initial elements from an existing list, with
-- the given number of skiplist levels.
newFromList_ :: Ord a => [a] -> Int -> Par e s (ISet s a)
newFromList_ ls n = do  
  s@(ISet lv) <- newEmptySet_ n
  LI.liftIO $ forM_ ls $ \x ->
    SLM.putIfAbsent (state lv) x $ return ()
  return s

-- (Todo: in production you might want even more ... like going from a Vector)

--------------------------------------------------------------------------------
-- Quasi-deterministic ops:
--------------------------------------------------------------------------------

-- | Freeze an 'ISet' after a specified callback/handler is done running.  This
-- differs from `withCallbacksThenFreeze` by not taking an additional action to run in
-- the context of the handlers.
--
--    (@'freezeSetAfter' 's' 'f' == 'withCallbacksThenFreeze' 's' 'f' 'return ()' @)
freezeSetAfter :: (HasPut e, HasGet e, HasFreeze e) => 
                  ISet s a -> (a -> Par e s ()) -> Par e s ()
freezeSetAfter s f = withCallbacksThenFreeze s f (return ())
  
-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- value of the provided action.
withCallbacksThenFreeze :: (HasPut e, HasGet e, HasFreeze e, Eq b) => ISet s a -> (a -> Par e s ()) -> Par e s b -> Par e s b
withCallbacksThenFreeze (ISet lv) callback action = do
  hp  <- newPool 
  res <- IV.new -- TODO, specialize to skip this when the init action returns ()
  let deltCB x = return$ Just$ unWrapPar$ callback x
      initCB slm unlockSet = 
        -- The implementation guarantees that all elements will be caught either here,
        -- or by the delta-callback:
        unWrapPar $ do
          SLM.foldlWithKey LI.liftIO
            (\() v () -> forkHP (Just hp) $ callback v) () slm
          LI.liftIO unlockSet
          x <- action -- Any additional puts here trigger the callback.
          IV.put_ res x
  WrapPar $ L.addHandler (Just hp) (unWrapLVar lv) initCB deltCB
  
  -- We additionally have to quiesce here because we fork the inital set of
  -- callbacks on their own threads:
  quiesce hp
  IV.get res


-- | /O(N)/: Convert from an `ISet` to a plain `Data.Set`.
--   This is only permitted when the `ISet` has already been frozen.
--   This is useful for processing the result of `Control.LVish.DeepFrz.runParThenFreeze`. 
fromISet :: Ord a => ISet Frzn a -> S.Set a 
fromISet set = F.foldr S.insert S.empty set

--------------------------------------------------------------------------------

-- | Add an (asynchronous) callback that listens for all new elements added to
-- the set, optionally enrolled in a handler pool.
forEachHP :: Maybe HandlerPool            -- ^ optional pool to enroll in 
           -> ISet s a                    -- ^ Set to listen to
           -> (a -> Par e s ())           -- ^ callback
           -> Par e s ()
forEachHP hp (ISet (WrapLVar lv)) callb = WrapPar $ 
    L.addHandler hp lv globalCB (\x -> return$ Just$ unWrapPar$ callb x)
  where
    globalCB slm unlockSet = 
      unWrapPar $ do 
        SLM.foldlWithKey LI.liftIO
           (\() v () -> forkHP hp $ callb v) () slm
        LI.liftIO unlockSet

-- | Add an (asynchronous) callback that listens for all new elements added to
-- the set.
forEach :: ISet s a -> (a -> Par e s ()) -> Par e s ()
forEach = forEachHP Nothing

-- | Put a single element in the set.  (WHNF) Strict in the element being put in the
-- set.     
insert :: Ord a => a -> ISet s a -> Par e s ()
insert !elm (ISet lv) = WrapPar$ putLV (unWrapLVar lv) putter
  where putter slm = do
          putRes <- SLM.putIfAbsent slm elm $ return ()
          case putRes of
            Added _ -> return $ Just elm
            Found _ -> return Nothing 

-- | Wait for the set to contain a specified element.
waitElem :: Ord a => a -> ISet s a -> Par e s ()
waitElem !elm (ISet (WrapLVar lv)) = WrapPar $
    getLV lv globalThresh deltaThresh
  where
    globalThresh slm _frzn = SLM.find slm elm
    deltaThresh e2 | e2 == elm = return $ Just ()
                   | otherwise = return Nothing

-- | Wait on the /size/ of the set, not its contents.
waitSize :: Int -> ISet s a -> Par e s ()
waitSize !sz (ISet (WrapLVar lv)) = WrapPar$
    getLV lv globalThresh deltaThresh
  where
    globalThresh slm _ = do
      snapSize <- SLM.foldlWithKey id (\n _ _ -> return $ n+1) 0 slm
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
copy :: Ord a => ISet s a -> Par e s (ISet s a)
copy = traverseSet return 

-- | Establish a monotonic map between the input and output sets.
traverseSet :: Ord b => (a -> Par e s b) -> ISet s a -> Par e s (ISet s b)
traverseSet f s = traverseSetHP Nothing f s

-- | An imperative-style, in-place version of 'traverseSet' that takes the output set
-- as an argument.
traverseSet_ :: Ord b => (a -> Par e s b) -> ISet s a -> ISet s b -> Par e s ()
traverseSet_ f s o = traverseSetHP_ Nothing f s o

-- | Return a new set which will (ultimately) contain everything in either input set.
union :: Ord a => ISet s a -> ISet s a -> Par e s (ISet s a)
union = unionHP Nothing

-- | Build a new set which will contain the intersection of the two input sets.
intersection :: Ord a => ISet s a -> ISet s a -> Par e s (ISet s a)
intersection = intersectionHP Nothing

-- | Take the cartesian product of two sets.
cartesianProd :: (Ord a, Ord b) => ISet s a -> ISet s b -> Par e s (ISet s (a,b))
cartesianProd s1 s2 = cartesianProdHP Nothing s1 s2 
  
-- | Take the cartesian product of several sets.
cartesianProds :: Ord a => [ISet s a] -> Par e s (ISet s [a])
cartesianProds ls = cartesianProdsHP Nothing ls

--------------------------------------------------------------------------------
-- Alternate versions of functions that EXPOSE the HandlerPools
--------------------------------------------------------------------------------

-- | Variant of `traverseSet` that optionally ties the handlers to a pool.
traverseSetHP :: Ord b => Maybe HandlerPool -> (a -> Par e s b) -> ISet s a ->
                 Par e s (ISet s b)
traverseSetHP mh fn set = do
  os <- newEmptySet
  traverseSetHP_ mh fn set os  
  return os

-- | Variant of `traverseSet_` that optionally ties the handlers to a pool.
traverseSetHP_ :: Ord b => Maybe HandlerPool -> (a -> Par e s b) -> ISet s a -> ISet s b ->
                  Par e s ()
traverseSetHP_ mh fn set os = do
  forEachHP mh set $ \ x -> do 
    x' <- fn x
    insert x' os

-- | Variant that optionally ties the handlers in the resulting set to the same
-- handler pool as those in the two input sets.
unionHP :: Ord a => Maybe HandlerPool -> ISet s a -> ISet s a -> Par e s (ISet s a)
unionHP mh s1 s2 = do
  os <- newEmptySet
  forEachHP mh s1 (`insert` os)
  forEachHP mh s2 (`insert` os)
  return os

-- | Variant that optionally ties the handlers in the resulting set to the same
-- handler pool as those in the two input sets.
intersectionHP :: Ord a => Maybe HandlerPool -> ISet s a -> ISet s a -> Par e s (ISet s a)
-- Can we do intersection with only the public interface?  It should be monotonic.
--   AJT: You could do it using cartesian product...
-- Well, for now we cheat and use liftIO:
intersectionHP mh s1 s2 = do
  os <- newEmptySet
  forEachHP mh s1 (fn os s2)
  forEachHP mh s2 (fn os s1)
  return os
 where  
  fn outSet (ISet lv) elm = do
    -- At this point 'elm' has ALREADY been added to "us", we check "them":    
    peek <- LI.liftIO $ SLM.find (state lv) elm
    case peek of
      Just _  -> insert elm outSet
      Nothing -> return ()

-- | Variant of 'cartesianProd' that optionally ties the handlers to a pool.
cartesianProdHP :: (Ord a, Ord b) => Maybe HandlerPool -> ISet s a -> ISet s b ->
                   Par e s (ISet s (a,b))
cartesianProdHP mh s1 s2 = do
  -- This is implemented much like intersection:
  os <- newEmptySet
  forEachHP mh s1 (fn os s2 (\ x y -> (x,y)))
  forEachHP mh s2 (fn os s1 (\ x y -> (y,x)))
  return os
 where
  -- This is expensive, but we've got to do it from both sides to counteract races:
  fn outSet (ISet lv) cmbn elm1 = 
    SLM.foldlWithKey LI.liftIO
       (\() elm2 () -> insert (cmbn elm1 elm2) outSet) () (state lv)

-- | Variant of 'cartesianProds' that optionally ties the handlers to a pool.
cartesianProdsHP :: Ord a => Maybe HandlerPool -> [ISet s a] ->
                    Par e s (ISet s [a])
cartesianProdsHP _  [] = newEmptySet
cartesianProdsHP mh ls = do
#if 1
  -- Case 1: recursive definition in terms of pairwise products:
  -- It would be best to create a balanced tree of these, I believe:
  let loop [lst]     = traverseSetHP mh (\x -> return [x]) lst -- Inefficient!
      loop (nxt:rst) = do
        partial <- loop rst
        p1 <- cartesianProdHP mh nxt partial
        traverseSetHP mh (\ (x,tl) -> return (x:tl)) p1 -- Inefficient!!
      loop [] = undefined -- impossible
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

--    F.foldlM (\() elm2 -> insert (cmbn elm1 elm2) outSet) () peek
    return (error "FINISHME: set cartesianProdHP")
#endif

