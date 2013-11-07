


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

{-| 
    A collection of useful parallel combinators based on top of a 'Par' monad.

    In particular, this module provides higher order functions for
     traversing data structures in parallel.  

-}

module Data.Par
  (
    -- * Naive parallel maps on traversable structures.
    parMap, parMapM, parMapM_

    -- * More efficient, balanced parallel traversals for splittable structures
    -- TODO 
  )
where 

import Control.DeepSeq
import Data.Traversable
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import GHC.Conc (numCapabilities)

import Control.Par.Class


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




-- pmapSplit
