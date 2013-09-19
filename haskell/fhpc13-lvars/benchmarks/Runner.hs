{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Runner where 

-- This is used with three different benchmark implementations:
--   PURE        -- use LVarTracePure
--   STRATEGIES  -- use Strategies, no LVars
--   PAR         -- use Par, no LVars
--
-- Run-time options:
--   Wrk = work to do per vertex
--   depthK = max hops of the connected component to explore
--   (OR N = target vertices to visit (will overshoot))

import           Control.Exception (evaluate)
import           Control.Monad (forM_, when)
import           Data.Word
import           Data.IORef
import           Data.List as L
import           Data.List.Split (chunksOf)
import qualified Data.IntSet as IS
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import           Text.Printf (printf)
import           System.Mem (performGC)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Environment (getEnvironment,getArgs)
import           System.CPUTime.Rdtsc (rdtsc)
import           System.CPUTime  (getCPUTime)

-- For representing graphs
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- Vector representation of graphs: the list (or set) at index k is
-- node k's neighbors.
type Graph = V.Vector [Node]
type Graph2 = V.Vector IS.IntSet

type Node = Int

-- Optimized version:
mkGraphFromFile :: String -> IO Graph
mkGraphFromFile file = do
  putStrLn $ "* Begin loading graph from " ++ file ++ "..." 
  t0    <- getCurrentTime
  inStr <- B.readFile file
  let -- Returns a list of edges:
      loop1 [] = []
      loop1 (b1:b2:rst) = do
        case (B.readInt b1, B.readInt b2) of
          (Just (src, _), Just (dst, _)) -> (src, dst) : loop1 rst
          _ -> error $ "Failed parse of bytestrings: " ++ show (B.unwords[b1, b2])
      loop1 _ = error "Odd number of integers in graph file!"

  let edges = case B.words inStr of
               ("EdgeArray":rst) -> loop1 rst
      mx = foldl' (\mx (s,d) -> mx `max` s `max` d) 0 edges
  mg <- MV.replicate (mx+1) []
  forM_ edges $ \ (src,dst) -> do
    -- Interpret this as a DIRECTED graph:    
    ls <- MV.read mg src
    MV.write mg src (dst:ls)
  g <- V.freeze mg
  -- Just to make SURE it's computed:
  putStrLn $ " * Graph loaded: " 
    ++ show(V.length g) ++ " vertices.  Neighbors of vertex 0: "
    ++ show (nbrs g 0)
  t1 <- getCurrentTime
  putStrLn $ " * Time reading/parsing data: " ++ show(diffUTCTime t1 t0)
  return g

-- Neighbors of a node with a given label
nbrs :: Graph -> Int -> [Int]
nbrs g lbl = g V.! lbl

-- For debugging
printGraph :: Graph -> IO ()
printGraph g = do
  let ls = V.toList g
  putStrLn (show ls)
  return ()
    
-- Iterates the sin function n times on its input and returns the sum
-- of all iterations.
sin_iter :: Word64 -> Float -> Float
sin_iter 0  x = x
sin_iter n !x = sin_iter (n - 1) (x + sin x)

type WorkRet = (Float, Node)
type WorkFn = (Node -> WorkRet)

theEnv :: [(String,String)]
theEnv = unsafePerformIO getEnvironment

checkEnv :: Read a => String -> a -> a 
checkEnv v def =
  case lookup v theEnv of
    Just "" -> def
    Just s  -> read s    
    Nothing -> def

verbose :: Bool
verbose = checkEnv "VERBOSE" False

dbg :: Bool
-- dbg = checkEnv "DEBUG" False
dbg = False -- Let it inline, DCE.

type Starter = Int       -- iteration counter
               -> Graph2 -- graph
               -> Int    -- start node
               -> WorkFn -- function to be applied to each node
               -> IO ()

{-# INLINE makeMain #-}
makeMain :: Starter -> IO ()
makeMain start_traverse = do
  -- Fetch runtime parameters:
  -- First, defaults:
  let graphFile_ :: String
      graphFile_ = "/tmp/grid_125000"
  
  let k_ :: Int
      k_ = 25    -- Number of hops to explore
      
  let w_ :: Word64
      w_ = 20000 -- Amount of work (iterations of sin)

  args <- getArgs
  
  -- LK: this way of writing the type annotations is the only way I
  -- can get emacs to not think this is a parse error! :(
  let (graphFile,depthK,wrk) = 
        case args of
          []                   -> (graphFile_, k_, w_)
          [graphFiles]         -> (graphFiles, k_, w_)
          [graphFiles, ks]     -> (graphFiles, read ks, w_)
          [graphFiles, ks, ws] -> (graphFiles, read ks, read ws :: Word64)
  
  gr <- mkGraphFromFile graphFile

  let startNode = 0
      gr2 = V.map IS.fromList gr
  evaluate (gr2 V.! 0)
  
  let graphThunk :: WorkFn -> IO ()
      graphThunk fn = do
        start_traverse depthK gr2 0 fn
        putStrLn "All done."
  
  -- Takes a node ID (which is just an int) and returns it paired with
  -- a floating-point number that's the value of iterating the sin
  -- function wrk times on that node ID.
  let sin_iter_count :: WorkFn
      sin_iter_count x = let f = fromIntegral x in
                         (sin_iter wrk f, x)

  -- Reeeealllllly want to audit this code. -- LK
  freq <- measureFreq
  clocks_per_kilosin <- measureSin 1000
  let clocks_per_micro, kilosins_per_micro :: Rational 
      clocks_per_micro = (fromIntegral freq) / (1000 * 1000)
      kilosins_per_micro = clocks_per_micro / (fromIntegral clocks_per_kilosin)
      numSins :: Rational       
      numSins = (fromIntegral wrk) * kilosins_per_micro * 1000
      numSins' = round numSins

      busy_waiter :: WorkFn
      busy_waiter n = unsafePerformIO $
--        wait_microsecs (wrk * clocks_per_micro) n
        wait_sins numSins' n

  printf "CPU Frequency: %s, clocks per microsecond %s\n"
           (commaint freq) (commaint (round clocks_per_micro))

  printf "Time for 1K sins %s, Ksins per micro %s, numSins %s = %s\n"
         (show clocks_per_kilosin) 
         (show (fromRational kilosins_per_micro ::Double))
         (show numSins) (show numSins')

  printf "* Beginning benchmark with depthK=%d and wrk=%d\n" depthK wrk
  
  performGC
  t0 <- getCurrentTime
  startT <- rdtsc  
--  graphThunk sin_iter_count
  graphThunk busy_waiter  
  t1 <- getCurrentTime
  putStrLn $ "SELFTIMED " ++ show (diffUTCTime t1 t0) ++ "\n"

  first <- readIORef first_hit
  let first' = first - startT
  putStrLn$"Start time in cycles: "++commaint startT      
  putStrLn $ "First hit in raw clock cycles was " ++ commaint first'
  let nanos = ((1000 * 1000 * 1000 * (fromIntegral first')) `quot` (fromIntegral freq :: Integer))
  putStrLn $ " In nanoseconds: "++commaint nanos
  putStrLn $ "FIRSTHIT " ++ show nanos

------------------------------------------------------------------------------------------

first_hit :: IORef Word64
first_hit = unsafePerformIO$ newIORef maxBound

-- Wait for a certain number of milleseconds.
wait_microsecs :: Word64 -> Node -> IO WorkRet
wait_microsecs clocks n = do
  myT <- rdtsc
  atomicModifyIORef first_hit (\ t -> (min t myT,()))
  let loop !n = do
        now <- rdtsc
        if now - myT >= clocks
        then return n   
        else loop (n+1)
  cnt <- loop 0
  return (fromIntegral cnt, n)


-- Measure clock frequency, spinning rather than sleeping to try to
-- stay on the same core.
measureFreq :: IO Word64 -- What units is this in? -- LK
measureFreq = do
  let millisecond = 1000 * 1000 * 1000 -- picoseconds are annoying
      -- Measure for how long to be sure?      
--      measure = 200 * millisecond
      measure = 1000 * millisecond      
      scale :: Integer
      scale = 1000 * millisecond `quot` measure
  t1 <- rdtsc 
  start <- getCPUTime
  let loop :: Word64 -> Word64 -> IO (Word64,Word64)
      loop !n !last = 
       do t2 <- rdtsc 
	  when (t2 < last) $
	       putStrLn$ "COUNTERS WRAPPED "++ show (last,t2) 
	  cput <- getCPUTime		
	  if (cput - start < measure)
	   then loop (n+1) t2
	   else return (n,t2)
  (n,t2) <- loop 0 t1
  putStrLn$ "  Approx getCPUTime calls per second: "++ commaint (scale * fromIntegral n)
  when (t2 < t1) $ 
    putStrLn$ "WARNING: rdtsc not monotonically increasing, first "++show t1++" then "++show t2++" on the same OS thread"

  return$ fromIntegral (fromIntegral scale * (t2 - t1))

wait_sins :: Word64 -> Node -> IO WorkRet
wait_sins num node = do
  myT <- rdtsc
  atomicModifyIORef first_hit (\ t -> (min t myT,()))
  res <- evaluate (sin_iter num (2.222 + fromIntegral node))
  return (res, node)

-- Measure the cost of N Sin operations.
measureSin :: Word64 -> IO Word64
measureSin n = do
  t0 <- rdtsc
  res <- evaluate (sin_iter n 38.38)
  t1 <- rdtsc  
  return$ t1-t0

-- Readable large integer printing:
commaint :: (Show a, Integral a) => a -> String
commaint n = 
   reverse $ concat $
   intersperse "," $ 
   chunksOf 3 $ 
   reverse (show n)
