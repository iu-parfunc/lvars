{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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
    pforEach, asyncForEach,

    -- * The underlying, general version of which the above are specializations
    mkMapReduce
  )
where

import Control.DeepSeq
-- import Control.Par.EffectSigs
import Control.Monad          as M hiding (join, mapM, sequence)
import Control.Par.Class
import Data.Traversable
import Prelude                hiding (head, mapM, read, sequence, tail)

import Control.Par.Class      as PC
import Control.Par.EffectSigs
import Data.Splittable.Class  (Split (..))

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
pmapReduce :: forall c e s m a .
      (Split c, Generator c, ParFuture m, HasPut e, HasGet e, NFData a)
      => c                 -- ^ element generator to consume
      -> (ElemOf c -> m e s a) -- ^ compute one result
      -> (a -> a -> m e s a)   -- ^ combine two results
      -> a                 -- ^ initial accumulator value
      -> m e s a
{-# INLINE pmapReduce #-}
pmapReduce = mkMapReduce split PC.foldM spawn

-- | A version of `pmapReduce` that is only weak-head-normal-form (WHNF) strict in
-- the folded accumulators.
pmapReduce_ :: forall c e s m a .
      (Split c, Generator c, HasPut e, HasGet e, ParFuture m)
      => c                     -- ^ element generator to consume
      -> (ElemOf c -> m e s a) -- ^ compute one result
      -> (a -> a -> m e s a)   -- ^ combine two results
      -> a                 -- ^ initial accumulator value
      -> m e s a
{-# INLINE pmapReduce_ #-}
pmapReduce_ = mkMapReduce split PC.foldM spawn_

-- | A version of `pmapReduce_` that uses a custom splitting function.
pmapReduceWith_ :: forall c e s m a .
      (Generator c, ParFuture m, HasPut e, HasGet e)
      => (c -> [c])        -- ^ splitting function.
      -> c                 -- ^ element generator to consume
      -> (ElemOf c -> m e s a) -- ^ compute one result
      -> (a -> a -> m e s a)   -- ^ combine two results
      -> a                 -- ^ initial accumulator value
      -> m e s a
{-# INLINE pmapReduceWith_ #-}
pmapReduceWith_ split = mkMapReduce split PC.foldM spawn_

-- | Execute a side-effect for each element generated.  Use the `Split` instance to
-- determine the degree of parallelism (granularity).
--
--  This is SYNCHRONOUS; that is, it does not return until all of the actions are
--  executed.
pforEach :: (Split c, Generator c, ParFuture m, HasPut e, HasGet e)
      => c                  -- ^ element generator to consume
      -> (ElemOf c -> m e s ()) -- ^ compute one result
      -> m e s ()
{-# INLINE pforEach #-}
pforEach gen mp = pmapReduce_ gen mp (\ () () -> return ()) ()

-- | Non-blocking version of pforEach.
asyncForEach :: (Split c, Generator c, ParFuture m)
      => c                  -- ^ element generator to consume
      -> (ElemOf c -> m e s ()) -- ^ compute one result
      -> m e s ()
-- asyncForEach = asyncForEachHP Nothing
asyncForEach gen fn =
  case split gen of
    [seqchunk] -> PC.forM_ seqchunk fn
    ls -> M.forM_ ls $ \ gen_i ->
            fork $
              PC.forM_ gen_i fn

-- | Make a parallel map-reduce function given a custom
--   function for spawning work.
mkMapReduce
   :: forall c e f s m a .
      (ParFuture m, HasPut f, HasGet f)
      => (c -> [c])
      -- ^ splitting function
      -> ((a -> e -> m f s a) -> a -> c -> m f s a)
      -- ^ sequential fold function
      -> (m f s a -> m f s (Future m s a))
      -- ^ spawn function
      -> c
      -- ^ element generator to consume
      -> (e -> m f s a)
      -- ^ compute one result
      -> (a -> a -> m f s a)
      -- ^ combine two results
      -> a
      -- ^ initial accumulator value
      -> m f s a
{-# INLINE mkMapReduce #-}
mkMapReduce splitter seqfold spawner genc fn binop initAcc = loop genc
 where
  mapred :: a -> e -> m f s a
  mapred ac b = do x <- fn b;
                   result <- ac `binop` x
                   return result
  loop :: c -> m f s a
  loop gen =
    -- trace ("[DBG] Looping around mkMapReduce...") $
    case splitter gen of
      -- Sequential case, use Generator class:
      [seqchunk] -> -- trace ("[DBG]   Bottoming out to sequential fold..") $
                    seqfold mapred initAcc seqchunk
        -- foldM mapred initAcc [min..max]
      [a,b] -> do iv <- spawner $ loop a
                  res2 <- loop b
                  res1 <- read iv
                  binop res1 res2
      ls@(_:_:_) ->
        do ivs <- mapM (spawner . loop) ls
           M.foldM (\ acc iv -> read iv >>= binop acc) initAcc ivs
      [] -> return initAcc

