{-# LANGUAGE ScopedTypeVariables#-}

-- | EXPERIMENTAL version which eventually should be made generic across Par monads
-- (i.e. a BulkRetryT transformer), and should thus be extended to transparently
-- catch any attempts by a thread to block, not just the special non-blocking calls
-- provided by *this* library.

module Control.LVish.BulkRetry

       where

import qualified Data.Bits.Atomic as B
import Foreign.Storable (sizeOf, Storable)

import Control.LVish
import Control.Par.Class (LVarSched(returnToSched))
-- import Data.LVar.NatArray
import Data.LVar.NatArray.Unsafe (NatArray, unsafePeek)

import Data.LVar.PureSet as IS

-- import Data.Par.Range

--------------------------------------------------------------------------------

-- | The point where users send abort messages.
newtype RetryHub s = RetryHub (ISet s Int) -- ^ This stores the iterations that fail.

-- | Non-blocking get on a `NatArray`.
getNB :: forall s d elt . (Storable elt, B.AtomicBits elt, Num elt) =>
         RetryHub s -> NatArray s elt -> Int -> Par d s elt
-- LVarSched (Par d s)         
getNB (RetryHub fails) arr ind = do
  x <- unsafePeek arr ind
  -- if empty, don't block, do this:
  case x of
    Nothing  -> do insert ind fails
                   returnToSched
    Just res -> return res

desired_tasks :: Int
desired_tasks = 16 -- FIXME: num procs * overpartition

-- | Aborts and retries failed iterations in bulk.
--   `forSpeculative` continues retrying until all iterations have completed.
forSpeculative :: (Int, Int) -> (RetryHub s -> Int -> Par d s ()) -> Par d s ()
-- TODO: Requires idempotency!!
forSpeculative (st,end) bodyfn = do
  let sz = end - st
      -- Even in a trivial loop, 2000 iters per task should be enough:
      prefix = min sz (2000 * desired_tasks)
      -- TODO: automatic strategies for tuning the input prefix size would be helpful.
      -- One approach that might make sense would be to auto-tune based on the
      -- time/iteration observed.  That is, gradually increase to try to approximate a
      -- minimum reasonable task size and no bigger.
  pool <- newPool  
  -- Outer loop of "rounds", in which we try a prefix of the iteration space.
  let loop offset remain = do
        -- Set of iterations that failed THIS round
        fails <- newEmptySet
        let chunk = offset + (min prefix remain)
        -- TODO: need a way to properly divide-and-conquer the frozen set.

--        parForTiled (Just pool)
        parForTiled desired_tasks (offset,chunk) $ \ix -> do
          bodyfn (RetryHub fails) ix
        quiesce pool
        loop 0 0 
  loop 0 sz       
  error "FINISHME - forSpeculative"
  
