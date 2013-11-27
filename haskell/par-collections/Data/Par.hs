


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| 
    A collection of useful parallel combinators based on top of a 'Par' monad.

    In particular, this module provides higher order functions for
     traversing data structures in parallel.  

-}

module Data.Par
  (
    -- * Naive parallel maps on traversable structures.
    
    -- | Because these operations assume only `Traversable`, the best they can do is
    -- to fork one parallel computation per element.
    parMap, parMapM, parMapM_

    -- * More efficient, balanced parallel traversals for splittable structures                     

    -- | These operations require an instance of `Data.Splittable.Split`, but in
    -- return they can perform more balanced traversals that are more tolerant of
    -- fine-granularity.
                     
    -- TODO: 
  )
where 

import Control.DeepSeq
import Data.Traversable
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import GHC.Conc (numCapabilities)

import Control.Par.Class
import Data.Splittable as Sp (Split(..), Generator(..)) 


-- -----------------------------------------------------------------------------
-- Parallel maps over Traversable data structures
-- -----------------------------------------------------------------------------

-- | Applies the given function to each element of a data structure
-- in parallel (fully evaluating the results), and returns a new data
-- structure containing the results.
--
-- > parMap f xs = mapM (spawnP . f) xs >>= mapM get
--
-- @parMap@ is commonly used for lists, where it has this specialised type:
--
-- > parMap :: NFData b => (a -> b) -> [a] -> Par [b]
--
parMap :: (Traversable t, NFData b, ParFuture p, FutContents p b) =>
          (a -> b) -> t a -> p (t b)
{-# INLINE parMap #-}
parMap f xs = mapM (spawnP . f) xs >>= mapM get

-- --  A variant that only evaluates to weak-head-normal-form.
-- parMap_ :: (Traversable t, ParFuture p, FutContents p b) =>
--           (a -> b) -> t a -> p (t b)
-- {-# INLINE parMap #-}
-- parMap_ f xs = mapM (spawnP . f) xs >>= mapM get


-- | Like 'parMap', but the function is a @Par@ monad operation.
--
-- > parMapM f xs = mapM (spawn . f) xs >>= mapM get
--
parMapM :: (Traversable t, NFData b, ParFuture p, FutContents p b) =>
           (a -> p b) -> t a -> p (t b)
{-# INLINE parMapM #-}           
parMapM f xs = mapM (spawn . f) xs >>= mapM get

-- | A variant that only evaluates to weak-head-normal-form.
parMapM_ :: (Traversable t, ParFuture p, FutContents p b) =>
           (a -> p b) -> t a -> p (t b)
{-# INLINE parMapM_ #-}
parMapM_ f xs = mapM (spawn_ . f) xs >>= mapM get


-- TODO: parBuffer -- enable signaling with a counter?

-- -----------------------------------------------------------------------------
-- Parallel maps over splittable data structures
-- -----------------------------------------------------------------------------

-- pmap :: (Splittable a) => (a -> b) -> a -> b

-- pmap :: (Functor f, Splittable (f a)) => (a -> b) -> f a -> f b

-- pmapSplit

{-
pmapReduce_
   :: (ParFuture p, FutContents p a)
      => Range   -- ^ iteration range over which to calculate
      -> (Int -> p a)     -- ^ compute one result
      -> (a -> a -> p a)  -- ^ combine two results 
      -> a                -- ^ initial result
      -> p a
pmapReduce_ = mkMapReduce spawn_
-}

{-# INLINE mkMapReduce #-}
-- mkMapReduce :: (Generator c e, ParFuture m) =>
--                (m t -> m (Future m t)) ->
--                Range -> (Int -> m t) -> (t -> t -> m t) -> t -> m t

-- | Computes a binary map\/reduce over a data source.  The input is recursively
-- split into two, the result for each half is computed in parallel, and then the two
-- results are combined. A sequential loop is used if the splitting bottoms out.
--
-- For example, the following is a parallel implementation of
--
-- >  foldl (+) 0 (map (^2) [1..10^6])
--
-- > parMapReduce (irange 1 (10^6))
-- >        (\x -> return (x^2))
-- >        (\x y -> return (x+y))
-- >        0
--
-- If an automatic threshold is being used in the underlying ranges, answers may vary
-- on different machines.  But note that this function does NOT require a commutative
-- combining function.  Commutativity, however, is not required.  This will always
-- combine lower iteration results on the left and higher on the right.
pmapReduce :: forall c e m a t .
      (Split c, Generator c e, ParFuture m, FutContents m a, NFData a)
      => c                -- ^ element generator to consume
      -> (e -> m a)       -- ^ compute one result
      -> (a -> a -> m a)  -- ^ combine two results 
      -> a                -- ^ initial accumulator value
      -> m a
pmapReduce = mkMapReduce spawn

pmapReduce_ :: forall c e m a t .
      (Split c, Generator c e, ParFuture m, FutContents m a)
      => c                -- ^ element generator to consume
      -> (e -> m a)       -- ^ compute one result
      -> (a -> a -> m a)  -- ^ combine two results 
      -> a                -- ^ initial accumulator value
      -> m a
pmapReduce_ = mkMapReduce spawn_
  


-- | Make a parallel map-reduce function given a custom
--   function for spawning work.
mkMapReduce 
   :: forall c e m a t .
      (Split c, Generator c e, ParFuture m, FutContents m a)
      => (m a -> m (Future m a)) -- ^ Spawn function
      -> c                -- ^ element generator to consume
      -> (e -> m a)       -- ^ compute one result
      -> (a -> a -> m a)  -- ^ combine two results 
      -> a                -- ^ initial accumulator value
      -> m a
mkMapReduce spawner genc fn binop init = loop genc
 where
  mapred :: a -> e -> m a 
  mapred ac b = do x <- fn b;
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

