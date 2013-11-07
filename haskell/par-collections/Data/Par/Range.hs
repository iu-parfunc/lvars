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
    InclusiveRange(..), range, irange, zrange,

    -- * Combined MapReduce operations on ranges
    parMapReduceThresh, parMapReduce
--    parFor
  )
where 

import Control.DeepSeq
import Data.Traversable
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import GHC.Conc (numCapabilities)

import Control.Par.Class
import Prelude hiding (init,min,max)

-- --------------------------------------------------------------------------------

-- |  An inclusive range of integers, i.e. `InclusiveRange 1 3` includes both `1` and `3`.
data InclusiveRange = InclusiveRange {-# UNPACK #-} !Int {-# UNPACK #-}!Int

-- | A range of integers.  This follows the standard for for-loops in imperative
-- languages: inclusive start, exclusive end.
-- data Range = Range {-# UNPACK #-} !Int {-# UNPACK #-}!Int

-- | A simple shorthand for ranges from `n` to `m-1` (inclusive,exclusive).
range :: Int -> Int -> InclusiveRange
range s e = InclusiveRange s (e-1)

-- | A simple shorthand for inclusive ranges from `n` to `m`.
irange :: Int -> Int -> InclusiveRange
irange s e = InclusiveRange s e

-- | A simple shorthand for ranges from `0` to `n-1`
zrange :: Int -> InclusiveRange
zrange n = InclusiveRange 0 (n-1)


-- | Computes a binary map\/reduce over a finite range.  The range is
-- recursively split into two, the result for each half is computed in
-- parallel, and then the two results are combined.  When the range
-- reaches the threshold size, the remaining elements of the range are
-- computed sequentially.
--
-- For example, the following is a parallel implementation of
--
-- >  foldl (+) 0 (map (^2) [1..10^6])
--
-- > parMapReduceThresh 100 (InclusiveRange 1 (10^6))
-- >        (\x -> return (x^2))
-- >        (\x y -> return (x+y))
-- >        0
--
-- Note that this function does NOT require an commutative or associative combining
-- function.
parMapReduceThresh
   :: (NFData a, ParFuture p, FutContents p a)
      => Int                          -- ^ threshold
      -> InclusiveRange               -- ^ range over which to calculate
      -> (Int -> p a)                 -- ^ compute one result
      -> (a -> a -> p a)              -- ^ combine two results 
      -> a                            -- ^ initial result
      -> p a
parMapReduceThresh threshold (InclusiveRange min max) fn binop init
 = loop min max
 where
  loop min max
    | max - min <= threshold =
	let mapred a b = do x <- fn b;
			    result <- a `binop` x
			    return result                            
	in
         -- TODO: verify deforestation here:
         foldM mapred init [min..max]

    | otherwise  = do
	let mid = min + ((max - min) `quot` 2)
	rght <- spawn $ loop (mid+1) max
	l  <- loop  min    mid
	r  <- get rght
	l `binop` r

-- How many tasks per process should we aim for?  Higher numbers
-- improve load balance but put more pressure on the scheduler.
auto_partition_factor :: Int
auto_partition_factor = 4

-- | We used to be able to use `numCapabilities` for this.  But now, with
-- `numCapabilities` changing at runtime, it will become a source of nondeterminism.
numProcs :: Int
numProcs = numCapabilities -- When will this constant version be deprecated/removed?
-- numProcs = unsafePerformIO getNumProcessors


-- | \"Auto-partitioning\" version of 'parMapReduceThresh' that chooses the threshold based on
--    the size of the range and the number of processors.
--
-- For consistent results across different machines, the fold function should be
-- associative.  Commutativity, however, is not required.  This will always combine
-- lower iteration results on the left and higher on the right.
parMapReduce :: (NFData a, ParFuture p, FutContents p a) => 
		     InclusiveRange
                     -> (Int -> p a)
                     -> (a -> a -> p a)   -- ^ Combine results: should be ASSOCIATIVE.
                     -> a -> p a
parMapReduce (InclusiveRange start end) fn binop init =
   loop (length origsegs) origsegs
  where
  origsegs = splitInclusiveRange (auto_partition_factor * numProcs) (start,end)
  loop 1 [(st,en)] =
     let mapred a b = do x <- fn b;
			 result <- a `binop` x
			 return result
     in foldM mapred init [st..en]
  loop n segs =
     let half = n `quot` 2
	 (left,right) = splitAt half segs in
     do l  <- spawn$ loop half left
        r  <- loop (n-half) right
	l' <- get l
	l' `binop` r

-- Internal helper:
-- TODO: replace with a Split class instancce:
splitInclusiveRange :: Int -> (Int, Int) -> [(Int, Int)]
splitInclusiveRange pieces (start,end) =
  map largepiece [0..remain-1] ++
  map smallpiece [remain..pieces-1]
 where
   len = end - start + 1 -- inclusive [start,end]
   (portion, remain) = len `quotRem` pieces
   largepiece i =
       let offset = start + (i * (portion + 1))
       in (offset, offset + portion)
   smallpiece i =
       let offset = start + (i * portion) + remain
       in (offset, offset + portion - 1)


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


-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ start end _fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start
  where
   loop !i | i == end  = return ()
	   | otherwise = do fn i; loop (i+1)

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

