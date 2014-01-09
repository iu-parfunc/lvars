{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | EXPERIMENTAL version which eventually should be made generic across Par monads
-- (i.e. a BulkRetryT transformer), and should thus be extended to transparently
-- catch any attempts by a thread to block, not just the special non-blocking calls
-- provided by *this* library.

module Control.LVish.BulkRetry

       where

import qualified Data.Bits.Atomic as B
import Foreign.Storable (sizeOf, Storable)
import Control.Monad (unless) 
import Control.LVish
import Control.LVish.Internal (unsafeDet)
import Control.Par.Class (LVarSched(returnToSched))
-- import Data.LVar.NatArray
import Data.LVar.NatArray.Unsafe (NatArray, unsafePeek)

import Data.Par.Splittable (pforEach)
import Data.Par.Range (range)

import qualified Data.Foldable as F
import qualified Data.Set as S
import           Data.LVar.PureSet as IS

-- import Data.Par.Range

--------------------------------------------------------------------------------

-- | The point where users send abort messages.
data RetryHub s = RetryHub (ISet s Int) -- ^ This stores the iterations that fail.
                           Int -- ^ This is the current iteration

-- -- | Non-blocking get on a `NatArray`.
-- getNB :: forall s d elt . (Storable elt, B.AtomicBits elt, Num elt) =>
--          RetryHub s -> NatArray s elt -> Int -> Par d s elt
-- -- LVarSched (Par d s)         
-- getNB (RetryHub fails) arr ind = do
--   x <- unsafePeek arr ind
--   -- if empty, don't block, do this:
--   case x of
--     Nothing  -> do logDbgLn 4 $ " [dbg-lvish] getNB: iteration failed, enqueue for retry: "++show ind
--                    insert ind fails
--                    returnToSched
--     Just res -> return res


-- | Non-blocking get on a `NatArray`.  In this prototype we require that the user
-- manually CPS the computation, so that the delimited continuation between this get
-- and the end of the loop iteration is passed explicitly as an argument.
--
-- The current reason for this compromise is that the HandlerPool mechanism is not
-- robust to us dropping the current continuation with `returnToSched`.  We would
-- need a version of HandlerPool's that interoperates with a user-level callCC, that is
-- we would need something like bracket/dynamic-wind for our continuation monad.
getNB_cps :: forall s d elt . (Storable elt, B.AtomicBits elt, Num elt) =>
         RetryHub s
         -> NatArray s elt      -- ^ Array to dereference
         -> Int                 -- ^ Which index to get
         -> (elt -> Par d s ()) -- ^ Delimited continuation.
         -> Par d s ()
-- LVarSched (Par d s)         
getNB_cps (RetryHub fails thisiter) arr ind cont = do
  x <- unsafePeek arr ind
  -- if empty, don't block, do this:
  case x of
    Nothing  -> do logDbgLn 4 $ " [dbg-lvish] getNB: iteration "++ show thisiter
                                ++" failed, due to get on index "++show ind
                   insert thisiter fails
                   return ()
    Just res -> do logDbgLn 4 $ " [dbg-lvish] getNB: result available, calling continuation (iter "++show thisiter++")"
                   cont res
{-# INLINE getNB_cps #-}

desired_tasks :: Int
desired_tasks = 16 -- FIXME: num procs * overpartition

-- | A parallel for-loop which aborts and retries failed iterations in bulk, rather
-- than allowing them to "block" and suffering the overhead of capturing and storing
-- their continuations.
-- 
-- `forSpeculative` continues retrying until ALL iterations have completed.  It is
-- thus a *synchronous* parallel for loop.
forSpeculative :: (Int, Int)  -- ^ Inclusive/Exclusive range to run.
                  -> (RetryHub s -> Int -> Par d s ()) -- ^ Body of the loop
                  -> Par d s ()
-- forSpeculative :: (Int, Int) -> (RetryHub s -> Int -> Par QuasiDet s ()) -> Par QuasiDet s ()
-- TODO: Requires idempotency!!
forSpeculative (st,end) bodyfn = do
  logDbgLn 2 $ " [dbg-lvish] Begin forSpeculative, bounds "++show (st,end)
  let sz = end - st
      -- Even in a trivial loop, 2000 iters per task should be enough:
      prefix = min sz (2000 * desired_tasks)
      -- TODO: automatic strategies for tuning the input prefix size would be helpful.
      -- One approach that might make sense would be to auto-tune based on the
      -- time/iteration observed.  That is, gradually increase to try to approximate a
      -- minimum reasonable task size and no bigger.  

      body' = bodyfn
      -- body' retry ix = bodyfn retry ix
  
  let flush leftover fails = 
        -- unless (S.null leftover) $ do
          -- TODO: need parallel fold, this is sequential...
          F.foldlM (\ () ix -> do
                       logDbgLn 3 $ " [dbg-lvish] forSpeculative: flushing iter "++show ix
                       body' (RetryHub fails ix) ix)
                   () leftover
  let flushLoop leftover =  do
        fails <- newEmptySet
        -- FIXME:
        flush leftover fails -- Sequential...        
        snap <- unsafeDet $ freezeSet fails
        logDbgLn 3 $ " [dbg-lvish] forSpeculative: did one sequential flush, remaining: "++show snap
        unless (S.null snap) $
          -- error$ "forSpeculative: failures not flushed with a sequential run!:\n "++show snap
          flushLoop snap
      
  -- Outer loop of "rounds", in which we try a prefix of the iteration space.  
  let loop !round leftover offset 0 = do
        logDbgLn 3 $ " [dbg-lvish] forSpeculative: got to the end, only failures left."
        flushLoop leftover
        
      loop !round leftover offset remain = do
        logDbgLn 3 $ " [dbg-lvish] forSpeculative starting round "++
                     show round++": offset "++show offset++", remaining "++show remain
        -- Set of iterations that failed in THIS upcoming round:
        fails <- newEmptySet
        let chunkend = offset + (min prefix remain)        

        hp <- newPool
        -- Here we keep the failed iterations "to the left" of the new batch, i.e. we
        -- fork them first.
        
        -- FINISHME: need Split instance:
        -- pforEach leftover $ bodyfn (RetryHub fails)
        logDbgLn 4 $ " [dbg-lvish] forSpeculative RElaunching failures: "++show leftover
        F.foldrM (\ ix () -> forkHP (Just hp) (body' (RetryHub fails ix) ix)) () leftover
        -- TODO: if we keep failing it's better to expand the prefix.  That way we
        -- end up with a logarithmic number of retries for each iterate in the worst
        -- case, rather than linear (making the whole loop unnecessarily quadratic).

        logDbgLn 4 $ " [dbg-lvish] forSpeculative launching new batch: "++show (offset,chunkend)
        asyncForEachHP (Just hp) (range offset chunkend) $ \ ix -> 
          body' (RetryHub fails ix) ix
        logDbgLn 4 $ " [dbg-lvish] forSpeculative: return from par for-loop; now quiesce."
        quiesce hp
        logDbgLn 4 $ " [dbg-lvish] forSpeculative: quiesce finished, next freeze failed set."
        snap <- unsafeDet $ freezeSet fails
        logDbgLn 4 $ " [dbg-lvish] forSpeculative finish round; failed iterates: "++show snap
        loop (round+1) snap chunkend (remain - (chunkend - offset))
  loop 0 S.empty 0 sz       
  -- After the last quiesce, we're done.
