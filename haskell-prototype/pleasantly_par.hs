{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- Embarassingly parallel.
-- If this doesn't get a speedup nothing will!

-- Note: This program is an example of a dependence on "put" being
-- strict.  If it were not the real work would be deferred until after
-- the parallel computation is finished!

-- Note: This code doesn't rely on any special aspects of LVars, as
-- opposed to IVars.  It's only here to exercise the scheduler in the
-- LVar library, to make sure that it's actually parallelizing.

#ifdef PURE
#warning "Using the PURE version"
import LVarTracePure (Par, spawn_, get, runPar)
#else
import LVarTraceInternal (Par, spawn_, get, runPar)
#endif

import GHC.Conc (numCapabilities, myThreadId)
import Debug.Trace
import Control.Monad
-- import Control.Monad.IO.Class (liftIO)

import Control.Exception (evaluate)
import System.Environment
import qualified Control.Monad.State.Strict as S 
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (yield)

-- For benchmarking
import Criterion.Main
import Criterion.Config

-- Compute sum_n(1/n)
work :: Int -> Int -> Double -> Double
work offset 0 n = n
work offset i n | i `mod` 10000 == 0 = 
  unsafePerformIO $ do yield
--		       putStr "."
		       return (work offset (i-1) (n + 1 / fromIntegral (i+offset)))
work offset (!i) (!n) = work offset (i-1) (n + 1 / fromIntegral (i+offset))

runit :: Int ->  IO ()
runit total = evaluate $ runPar $ 
   do 
      prnt$ "Running embarassingly parallel benchmark."
      prnt$ "Running "++ show total ++" total iterations"
      prnt$ "Spawning "++ show numCapabilities ++" tasks..."
      results <- mapM (spawn_ . kernel) [0 .. numCapabilities-1] 
      prnt "Done initializing."

      final <- foldM (\ acc (ind,iv) -> 
		      do prnt$ "  Retrieving output "++ show ind ++": "
			 n <- get iv
			 prnt$ show n
			 return (acc + n))
		  0.0 (zip [0..] results)
      prnt$ "Final Output: " ++ show final
 where  
  oneshare = total `quot` numCapabilities
  kernel jid =
     do 
        tid <- io myThreadId 
	prnt$ (show tid++" job "++show jid++":  About to do a chunk of work ("++ show oneshare ++" iterations)...")
        res <- io$ evaluate $ work (oneshare * jid) oneshare 0.0
	prnt$ (show tid++"   job "++show jid++":  done with work (result "++ show res ++")")
        return res

prnt :: String -> Par ()
prnt str = trace str $ return ()

io :: IO a -> Par a
io act = let x = unsafePerformIO act in 
         seq x (return x)
         
myConfig = defaultConfig {
  cfgSamples = ljust 100,
  cfgPerformGC = ljust True
}
         
main :: IO ()
main = do
  defaultMainWith myConfig (return ()) [
         bgroup "pleasantly_par" [
           bench "50 million iterations" $ runit (50*1000*1000)
         ]
         ]
  
  
