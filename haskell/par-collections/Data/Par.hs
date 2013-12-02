


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| 

A collection of useful parallel combinators based on top of a 'Par' monad.
Specifically, this module provides higher order functions for traversing data
structures in parallel.

-}

module Data.Par
  (
    -- * Efficient, balanced parallel traversals for splittable structures                     

    -- | These operations require an instance of `Data.Splittable.Split`, but in
    -- return they can perform more balanced traversals that are more tolerant of
    -- fine-granularity.
    pmapReduce, pmapReduce_, pforEach,
                             
    -- * Naive parallel maps on traversable structures.
    
    -- | Because these operations assume only `Traversable`, the best they can do is
    -- to fork one parallel computation per element.
    pmap, ptraverse, ptraverse_
  )
where 

import Control.DeepSeq
import Data.Traversable
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import GHC.Conc (numCapabilities)

import Control.Par.Class
import Data.Splittable.Class as Sp (Split(..), Generator(..)) 


-- -----------------------------------------------------------------------------
-- Parallel maps over Traversable data structures
-- -----------------------------------------------------------------------------

-- | Applies the given function to each element of a data structure
-- in parallel (fully evaluating the results), and returns a new data
-- structure containing the results.
--
-- > pmap f xs = mapM (spawnP . f) xs >>= mapM get
--
-- @pmap@ is commonly used for lists, where it has this specialised type:
--
-- > pmap :: NFData b => (a -> b) -> [a] -> Par [b]
--
-- But note that for efficient parallelism you want balanced task trees, not "one at
-- a time" parallel tasks.  Thus look at `pmapReduce` and friends.
pmap :: (Traversable t, NFData b, ParFuture p, FutContents p b) =>
          (a -> b) -> t a -> p (t b)
{-# INLINE pmap #-}
pmap f xs = mapM (spawnP . f) xs >>= mapM get


-- | Like 'pmap', but the function is a @Par@ monad operation.
--
-- > ptraverse f xs = mapM (spawn . f) xs >>= mapM get
--
ptraverse :: (Traversable t, NFData b, ParFuture p, FutContents p b) =>
           (a -> p b) -> t a -> p (t b)
{-# INLINE ptraverse #-}           
ptraverse f xs = mapM (spawn . f) xs >>= mapM get

-- | A variant that only evaluates to weak-head-normal-form.
ptraverse_ :: (Traversable t, ParFuture p, FutContents p b) =>
           (a -> p b) -> t a -> p (t b)
{-# INLINE ptraverse_ #-}
ptraverse_ f xs = mapM (spawn_ . f) xs >>= mapM get


-- TODO: parBuffer -- enable signaling with a counter?

-- -----------------------------------------------------------------------------
-- Parallel maps over splittable data structures
-- -----------------------------------------------------------------------------

-- | Computes a binary map\/reduce over a data source.  The input is recursively
-- split into two, the result for each half is computed in parallel, and then the two
-- results are combined. A sequential loop is used if the splitting bottoms out.
--
-- For example, the following is a parallel implementation of
--
-- >  foldl (+) 0 (map (^2) [1..10^6])
--
-- > import Data.Par.Range (irange)
-- >  ...
-- > parMapReduce (irange 1 (10^6))
-- >        (\x -> return (x^2))
-- >        (\x y -> return (x+y))
-- >        0
--
-- If an automatic threshold is being used in the underlying ranges, answers may vary
-- on different machines if the reduce function is not associative.  But even then,
-- platform portability does NOT require a commutative reduce function.  This combinator
-- will always fold earlier results on the left and later on the right.
pmapReduce :: forall c e m a t .
      (Split c, Generator c, ParFuture m, FutContents m a, NFData a)
      => c                 -- ^ element generator to consume
      -> (ElemOf c -> m a) -- ^ compute one result
      -> (a -> a -> m a)   -- ^ combine two results 
      -> a                 -- ^ initial accumulator value
      -> m a
{-# INLINE pmapReduce #-}      
pmapReduce = mkMapReduce spawn

-- | A version of `pmapReduce` that is only weak-head-normal-form (WHNF) strict in
-- the folded accumulators.
pmapReduce_ :: forall c e m a t .
      (Split c, Generator c, ParFuture m, FutContents m a)
      => c                 -- ^ element generator to consume
      -> (ElemOf c -> m a) -- ^ compute one result
      -> (a -> a -> m a)   -- ^ combine two results 
      -> a                 -- ^ initial accumulator value
      -> m a
{-# INLINE pmapReduce_ #-}      
pmapReduce_ = mkMapReduce spawn_
  
-- | Execute a side-effect in parallel for each element generated.  This is
--   synchronous; that is, it does not return until all of the actions are executed.
pforEach :: (Split c, Generator c, ParFuture m, FutContents m ())
      => c                  -- ^ element generator to consume
      -> (ElemOf c -> m ()) -- ^ compute one result
      -> m ()
{-# INLINE pforEach #-}
pforEach gen mp = pmapReduce_ gen mp (\ () () -> return ()) ()

-- | Make a parallel map-reduce function given a custom
--   function for spawning work.
mkMapReduce 
   :: forall c e m a t .
      (Split c, Generator c, ParFuture m, FutContents m a)
      => (m a -> m (Future m a)) -- ^ Spawn function
      -> c                 -- ^ element generator to consume
      -> (ElemOf c -> m a) -- ^ compute one result
      -> (a -> a -> m a)   -- ^ combine two results 
      -> a                 -- ^ initial accumulator value
      -> m a
{-# INLINE mkMapReduce #-}
mkMapReduce spawner genc fn binop init = loop genc
 where
  mapred :: ElemOf c -> a -> m a 
  mapred b ac = do x <- fn b;
                   result <- ac `binop` x
                   return result
  loop :: c -> m a
  loop gen =
    case split gen of
      -- Sequential case:
      [seqchunk] -> Sp.foldrM mapred init seqchunk
        -- foldM mapred init [min..max]
      [a,b] -> do iv <- spawner$ loop a
                  res2 <- loop b
                  res1 <- get iv
                  binop res1 res2
      ls@(_:_:_) ->
        do ivs <- mapM (spawner . loop) ls
           foldM (\ acc iv -> get iv >>= binop acc) init ivs
      [] -> return init

