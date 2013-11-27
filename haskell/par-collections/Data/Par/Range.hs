{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

{-| 
    A collection of useful parallel combinators based on top of a 'Par' monad.

    In particular, this module provides higher order functions for
     traversing data structures in parallel.  

-}

module Data.Par.Range
  (
    -- * Describing ranges of integers
    InclusiveRange(..), range, irange, zrange, fullpar,

    -- * Combined MapReduce operations on ranges
    pmapReduce
--    parFor
  )
where 

import Control.DeepSeq
import Data.Traversable ()
import Control.Monad as M hiding (mapM, sequence, join)
import GHC.Conc (numCapabilities, getNumProcessors)

import Control.Par.Class
import Data.Splittable
import Prelude hiding (init,max,sequence, head,tail)

import System.IO.Unsafe (unsafePerformIO)
import Data.Splittable

-- --------------------------------------------------------------------------------

-- | An iteration space expressed as an inclusive range of integers,
-- i.e. `InclusiveRange 1 3` includes both `1` and `3`.
--
data InclusiveRange =
     InclusiveRange
     { startInd  :: {-# UNPACK #-} !Int -- ^ Start, inclusive
     , endInd    :: {-# UNPACK #-} !Int -- ^ End, inclusive
     , seqThresh :: {-# UNPACK #-} !Int -- ^ For ranges less than or equal to this, switch to sequential execution.
     }
     deriving (Eq,Ord,Show,Read)

instance Split InclusiveRange where
  {-# INLINE split #-}
  split = splitPlease 2
  {-# INLINE splitPlease #-}  
  splitPlease pieces rng@(InclusiveRange start end thresh) 
     | len <= thresh = [rng] 
     | len < pieces  = [ InclusiveRange i i thresh | i <- [start .. end]]
     | otherwise = chunks
    where 
    len = end - start + 1 
    chunks =
      map largepiece [0..remain-1] ++
      map smallpiece [remain..pieces-1]
    (portion, remain) = len `quotRem` pieces
    largepiece i =
        let offset = start + (i * (portion + 1))
        in (InclusiveRange offset (offset + portion) thresh)
    smallpiece i =
        let offset = start + (i * portion) + remain
        in (InclusiveRange offset (offset + portion - 1) thresh)


-- | A range of integers.  This follows the standard for for-loops in imperative
-- languages: inclusive start, exclusive end.
-- data Range = Range {-# UNPACK #-} !Int {-# UNPACK #-}!Int

-- | A simple shorthand for ranges from `n` to `m-1` (inclusive,exclusive).
-- 
-- Note that this function, as well as `irange` and `zrange`, by default produce
-- \"auto-sequentializing\" iteration spaces that choose a threshold for bottoming
-- out to sequential execution based on the size of the range and the number of
-- processors.
range :: Int -> Int -> InclusiveRange
range s e = mkInclusiveRange s (e-1)
{-# INLINE range #-}

-- | A simple shorthand for inclusive ranges from `n` to `m`.
irange :: Int -> Int -> InclusiveRange
irange s e = mkInclusiveRange s e
{-# INLINE irange #-}

-- | A simple shorthand for ranges from `0` to `n-1`
zrange :: Int -> InclusiveRange
zrange n = mkInclusiveRange 0 (n-1)
{-# INLINE zrange #-}

-- | Tweak an iteration range to exploit all parallelism; never bottom-out to
-- sequential loops.
fullpar :: InclusiveRange -> InclusiveRange
fullpar (InclusiveRange s e _) = InclusiveRange s e 1
{-# INLINE fullpar #-}

-- By default we produce a range that bottoms out to sequential.
mkInclusiveRange :: Int -> Int -> InclusiveRange
mkInclusiveRange s e = InclusiveRange s e thresh
  where
    thresh = min max_iterations_seq chunksize
    chunksize = len `quot` (auto_partition_factor * num_procs)
    len = e - s + 1 
{-# INLINE mkInclusiveRange #-}

--------------------------------------------------------------------------------
-- Parallel granularity heuristics... this could be replaced with auto-tuning.
--------------------------------------------------------------------------------

-- How many tasks per process should we aim for?  Higher numbers
-- improve load balance but put more pressure on the scheduler.
auto_partition_factor :: Int
auto_partition_factor = 4

-- Running large numbers of iterations will take time even if the work is trivial.
-- Thus we put an upper bound on how large we will make a sequential slab of work.
max_iterations_seq :: Int
max_iterations_seq = 4000 

-- | We used to be able to use `numCapabilities` for this.  But now, with
-- `numCapabilities` changing at runtime, it will become a source of nondeterminism.
num_procs :: Int
-- num_procs = numCapabilities -- When will this constant version be deprecated/removed?
num_procs = unsafePerformIO getNumProcessors



--------------------------------------------------------------------------------

-- | Computes a binary map\/reduce over a finite range.  The range is recursively
-- split into two, the result for each half is computed in parallel, and then the two
-- results are combined.  The thresholding behavior for the range is obeyed, and a
-- sequential loop is used if the splitting bottoms out.
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
pmapReduce
   :: (NFData a, ParFuture p, FutContents p a)
      => InclusiveRange   -- ^ iteration range over which to calculate
      -> (Int -> p a)     -- ^ compute one result
      -> (a -> a -> p a)  -- ^ combine two results 
      -> a                -- ^ initial result
      -> p a
pmapReduce  = mkMapReduce spawn
 -- irng fn binop init = loop irng
 -- where
 --  spawner = spawn 
 --  mapred ac b = do x <- fn b;
 --                   result <- ac `binop` x
 --                   return result                            
 --  loop rng =
 --    case split rng of
 --      -- Sequential case:
 --      [InclusiveRange st en _] -> forAcc_ st en init mapred 
 --        -- foldM mapred init [min..max]
 --      [a,b] -> do iv <- spawner$ loop a
 --                  res2 <- loop b
 --                  res1 <- get iv
 --                  binop res1 res2
 --      ls -> do ivs <- mapM (spawner . loop) ls
 --               foldM (\ acc iv -> get iv >>= binop acc) init ivs
 --      [] -> return init

-- | A version of `pmapReduce` that is only weak-head-normal-form (WHNF) strict in
-- the folded accumulators.
pmapReduce_
   :: (ParFuture p, FutContents p a)
      => InclusiveRange   -- ^ iteration range over which to calculate
      -> (Int -> p a)     -- ^ compute one result
      -> (a -> a -> p a)  -- ^ combine two results 
      -> a                -- ^ initial result
      -> p a
pmapReduce_ = mkMapReduce spawn_

{-# INLINE mkMapReduce #-}
mkMapReduce :: ParFuture m => (m t -> m (Future m t)) ->
               InclusiveRange -> (Int -> m t) -> (t -> t -> m t) -> t -> m t
mkMapReduce spawner irng fn binop init = loop irng
 where
  mapred ac b = do x <- fn b;
                   result <- ac `binop` x
                   return result                            
  loop rng =
    case split rng of
      -- Sequential case:
      [InclusiveRange st en _] -> forAcc_ st en init mapred 
        -- foldM mapred init [min..max]
      [a,b] -> do iv <- spawner$ loop a
                  res2 <- loop b
                  res1 <- get iv
                  binop res1 res2
      ls -> do ivs <- mapM (spawner . loop) ls
               foldM (\ acc iv -> get iv >>= binop acc) init ivs
      [] -> return init



{-

-- TODO: A version that works for any splittable input domain.  In this case
-- the "threshold" is a predicate on inputs.
-- parMapReduceRangeGeneric :: (inp -> Bool) -> (inp -> Maybe (inp,inp)) -> inp ->




-- | Parallel for-loop over an inclusive range.  Semantically similar to:
--
-- > parFor (InclusiveRange n m) f = forM_ (randomize_order [n..m]) f
--         
-- The implementation will split the work into an unspecified number of subtasks in an
-- attempt to gain parallelism.  The exact number of subtasks is chosen at runtime,
-- and is probably a small multiple of the available number of processors.
--
-- Strictly speaking the semantics of 'parFor' depends on the number of processors,
-- and its behaviour, while deterministic, is a function of which machine it is run
-- on.  (Note that this is true even for sequential Haskell programs, because the
-- size of `Int` varies between platforms.)
--
-- The a correct use of `parFor` not have any iteration block on any other iteration.
-- (As if using a sequential for-loop but one with a random order.)
--
parFor :: (ParFuture iv p) => InclusiveRange -> (Int -> p ()) -> p ()
parFor (InclusiveRange start end) body =
 do
    let run (x,y) = for_ x (y+1) body
        range_segments = splitInclusiveRange (4*numCapabilities) (start,end)

    vars <- M.forM range_segments (\ pr -> spawn_ (run pr))
    M.mapM_ get vars
    return ()



{-# INLINE parForSimple #-}
-- | The least-sophisticated form of parallel loop.  Fork all iterations,
-- immediately, as individual parallel tasks.
--
-- When using this kind of loop, it is safe for iterations to do depend on eachother
-- and communicate via blocking reads.  As long as there are no cycles, the runtime
-- will figure out what order to execute the tasks to satisify their data dependency.
parForEach :: (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForEach range fn = for_ range $ \i -> fork (fn i) 

-}

-- data LRange = 
{-

{-# INLINE parForL #-}
-- | Left-biased parallel for loop.  As worker threads beyond the first are added,
-- this hews closer to the sequential iteration order than an unbiased parallel loop.
--
-- Takes a range as inclusive-start, exclusive-end.
parForL :: (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForL (start,end) _ | start > end = error$"parForL: start is greater than end: "++show (start,end)
parForL (start,end) body = do
  -- logStrLn$ " initial iters: "++show (end-start)
  loop 0 (end - start) 1
 where
   loop offset remain chunk
     | remain <= 0     = return () 
     | remain <= chunk = parForSimple (offset, offset+remain) body
     | otherwise       = do
         let nxtstrt = offset+chunk
         -- logStrLn$ "loop:  .. "++show (offset, remain, chunk)
         fork$ parForSimple (offset, nxtstrt) body
         loop nxtstrt (remain-chunk) (2*chunk)


-- | Divide the iteration space recursively, but ultimately run every iteration in
-- parallel.  That is, the loop body is permitted to block on other iterations.
parForTree :: (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForTree (start,end) _
  | start > end = error$"parForTree: start is greater than end: "++show (start,end)
parForTree (start,end) body = do
  loop 0 (end - start)
 where
   loop offset remain 
     | remain == 1     = body offset
     | otherwise       = do
         let (half,rem) = remain `quotRem` 2
         fork$ loop offset half
         loop (offset+half) (half+rem)


-- | Split the work into a number of tiles, and fork it in a tree topology.
parForTiled :: Int -> (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForTiled otiles (start,end) body = do 
  loop 0 (end - start) otiles
 where
   loop offset remain tiles
     | remain == 1     = body offset
     | tiles  == 1     = for_ (offset,offset+remain) body
     | otherwise       = do
         let (half,rem)   = remain `quotRem` 2
             (halfT,remT) = tiles `quotRem` 2
         fork$ loop offset half halfT
         loop (offset+half) (half+rem) (halfT+remT)


-- | A simple for loop for numeric ranges (not requiring deforestation
-- optimizations like `forM`).  Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => (Int, Int) -> (Int -> m ()) -> m ()
for_ (start, end) _fn | start > end = error "for_: start is greater than end"
for_ (start, end) fn = loop start
  where
  loop !i | i == end  = return ()
          | otherwise = do fn i; loop (i+1)

-}


-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ start end _fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start
  where
   loop !i | i == end  = return ()
	   | otherwise = do fn i; loop (i+1)

{-# INLINE forAcc_ #-}
forAcc_ :: Monad m => Int -> Int -> acc -> (acc -> Int -> m acc) -> m acc
forAcc_ start end _ _fn | start > end = error "for_: start is greater than end"
forAcc_ start end acc fn = loop acc start 
  where
   loop !acc !i
     | i == end  = return acc
     | otherwise = do acc' <- fn acc i
                      loop acc (i+1)


