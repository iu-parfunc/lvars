


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| 

A collection of useful parallel combinators based on top of a 'Par' monad together
with "Data.Splittable".

-}

module Data.Par.Splittable
  (
    -- * Efficient, balanced parallel traversals for splittable structures                     

    -- | These operations require an instance of `Data.Splittable.Split`, but in
    -- return they can perform more balanced traversals that are more tolerant of
    -- fine-granularity.
    pmapReduce, pmapReduce_, pmapReduceWith_,
    pforEach,

    -- * The underlying, general version of which the above are specializations
    mkMapReduce
  )
where 

import Control.DeepSeq
import Data.Traversable
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import GHC.Conc (numCapabilities)

import Control.Par.Class     as PC
import Data.Splittable.Class (Split(..)) 

import Debug.Trace

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
pmapReduce = mkMapReduce split PC.foldM spawn

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
pmapReduce_ = mkMapReduce split PC.foldM spawn_

-- | A version of `pmapReduce_` that uses a custom splitting function.
pmapReduceWith_ :: forall c e m a t .
      (Generator c, ParFuture m, FutContents m a)
      => (c -> [c])        -- ^ splitting function.
      -> c                 -- ^ element generator to consume
      -> (ElemOf c -> m a) -- ^ compute one result
      -> (a -> a -> m a)   -- ^ combine two results 
      -> a                 -- ^ initial accumulator value
      -> m a
{-# INLINE pmapReduceWith_ #-}      
pmapReduceWith_ split = mkMapReduce split PC.foldM spawn_

-- | Execute a side-effect in parallel for each element generated.
-- 
--  This is SYNCHRONOUS; that is, it does not return until all of the actions are
--  executed.
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
      (ParFuture m, FutContents m a)
      => (c -> [c])              -- ^ splitting function
      -> ((a -> e -> m a) -> a -> c -> m a) -- ^ sequential fold function
      -> (m a -> m (Future m a)) -- ^ spawn function
      -> c                       -- ^ element generator to consume
      -> (e -> m a)              -- ^ compute one result
      -> (a -> a -> m a)         -- ^ combine two results 
      -> a                       -- ^ initial accumulator value
      -> m a
{-# INLINE mkMapReduce #-}
mkMapReduce splitter seqfold spawner genc fn binop init = loop genc
 where
  mapred :: a -> e -> m a 
  mapred ac b = do x <- fn b;
                   result <- ac `binop` x
                   return result
  loop :: c -> m a
  loop gen =
    -- trace ("[DBG] Looping around mkMapReduce...") $ 
    case splitter gen of
      -- Sequential case, use Generator class:
      [seqchunk] -> -- trace ("[DBG]   Bottoming out to sequential fold..") $ 
                    seqfold mapred init seqchunk
        -- foldM mapred init [min..max]
      [a,b] -> do iv <- spawner$ loop a
                  res2 <- loop b
                  res1 <- get iv
                  binop res1 res2
      ls@(_:_:_) ->
        do ivs <- mapM (spawner . loop) ls
           M.foldM (\ acc iv -> get iv >>= binop acc) init ivs
      [] -> return init

