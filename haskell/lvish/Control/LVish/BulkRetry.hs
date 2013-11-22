{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Par.Class (LVarSched(returnToSched))
-- import Data.LVar.NatArray
import Data.LVar.NatArray.Unsafe (NatArray, unsafePeek)

import qualified Data.Foldable as F
import qualified Data.Set as S
import           Data.LVar.PureSet as IS

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
forSpeculative :: (Int, Int) -> (RetryHub s -> Int -> Par QuasiDet s ()) -> Par QuasiDet s ()
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

  let flush leftover fails =
        -- unless (S.null leftover) $ do
          -- TODO: need parallel fold, this is sequential...
          F.foldrM (\ ix () -> bodyfn (RetryHub fails) ix)
                   () leftover        
  
  let loop leftover offset 0 = do
        fails <- newEmptySet
        flush leftover fails -- Sequential, no failures...
      loop leftover offset remain = do  
        -- Set of iterations that failed in THIS upcoming round:
        fails <- newEmptySet
        let chunkend = offset + (min prefix remain)        
                
        parForTiled (Just pool) desired_tasks (offset,chunkend) $ \ix -> do
          bodyfn (RetryHub fails) ix
        quiesce pool
        snap <- freezeSet fails
        loop snap chunkend (remain - (chunkend - offset))
  loop S.empty 0 sz       
  -- After the last quiesce, we're done.
