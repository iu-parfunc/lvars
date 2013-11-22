{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables, CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


-- | Defines a representation of STATIC graphs as a dense vector containing neighbor
-- listings for each vertex.
--
-- The on-disk representation is simple and described here:
--    http://www.cs.cmu.edu/~pbbs/benchmarks/graphIO.html
--
-- This implies an equally simple in-memory representation.  In this library each
-- graph will allocate exactly two unboxed vectors.  (One for vertices, one for
-- edges.)  If you have graph data that will be live for multiple GC rounds, this is
-- a desirable format to keep in memory.

-- TODO:
--  * It is probably a better strategy to shift the slices around to match number
--    boundaries than it is to deal with this whole fragments business.  Try that.

module Data.Graph.Adjacency
       (
         -- * Graph types and accessors
         AdjacencyGraph(..), NodeID, nbrs,

         -- * Reading from disk or parsing in memory
         readAdjacencyGraph, parseAdjacencyGraph,
         
         -- * Generally useful utilities
         readNumFile, parReadNats,
                      
         -- * Testing
         t0,t1,t2,t3,t3B,t4,t5,
         unitTests,

         main -- TEMP
       ) where 

import Control.Monad   (foldM)
import Control.DeepSeq (NFData,rnf)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Control.Concurrent (getNumCapabilities)
import Control.LVish as LV
import Control.LVish.Internal (liftIO)
import qualified Data.LVar.IVar as I
import qualified Data.Par.Range as R

import Data.Word
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Unsafe (unsafeTail, unsafeHead)
import Data.Time.Clock

import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree (Gr)

import System.IO.Posix.MMap (unsafeMMapFile)
import Test.HUnit
import Prelude hiding (min,max,fst,last)

--------------------------------------------------------------------------------
-- PBBS specific:

-- | The adjacency-graph representation.
data AdjacencyGraph =
  AdjacencyGraph {
    vertOffsets :: U.Vector Int, 
    allEdges   :: U.Vector NodeID
  }
 deriving (Read,Show,Eq,Ord)

-- | Following the same conventions as many graph libaries (e.g. fgl), we require
-- that graph nodes (vertices) be identified by a scalar number.  Additionally, for
-- the `AdjacencyGraph` representation, nodes are expected to densely occupy a
-- continuous range of integers from 0..N.
type NodeID = Int

-- | Retrieve the neighbors of a given node.
--   This is /O(1)/ allocation and /O(1)/ time.
nbrs :: AdjacencyGraph -> NodeID -> U.Vector NodeID
nbrs AdjacencyGraph{vertOffsets, allEdges} nid = 
    let ind = vertOffsets U.! (fromIntegral nid)
        nxt = vertOffsets U.! (fromIntegral (nid+1))
        suff = U.drop (fromIntegral ind) allEdges in
    if fromIntegral nid == U.length vertOffsets - 1
    then suff 
    else U.take (fromIntegral$ nxt-ind) suff
{-# INLINE nbrs #-}

-- | Read an AdjacencyGraph file into memory from a file.
readAdjacencyGraph :: FilePath -> IO AdjacencyGraph
-- TODO! Once we have fine-grained effect tracking in this should STAY in Par, but
-- require IO effects.
readAdjacencyGraph path = do
  bs <- fmap (B.dropWhile isSpace) $
        unsafeMMapFile path
  ncap <- getNumCapabilities
  runParIO $ parseAdjacencyGraph (ncap * overPartition) bs

-- | Parse an AdjacencyGraph file already in memory (ByteString), in parallel.
--   The first parameter is a tuning parameter -- how many parallel chunks to parse.
parseAdjacencyGraph :: Int -> B.ByteString -> Par d s AdjacencyGraph
parseAdjacencyGraph chunks bs = 
  case B.splitAt (B.length tag) bs of
    (fst, rst) | fst /= tag -> error$ "readAdjacencyGraph: First word in file was not "++B.unpack tag
               | otherwise -> do                 
      ls <- parReadNats chunks rst
      let vec  = U.concat (sewEnds ls)
          vec' = U.drop 2 vec
      unless (U.length vec >= 2)$  error "readAdjacencyGraph: file ends prematurely."
      let verts   = fromIntegral$ vec U.! 0
          edges   = fromIntegral$ vec U.! 1
          (v1,v2) = U.splitAt verts vec'
      if U.length v1 == verts && U.length v2 == edges
        then return (AdjacencyGraph v1 v2)
        else error "readAdjacencyGraph: file doesn't contain as many entry as the header claims."
 where
   tag = "AdjacencyGraph"

--------------------------------------------------------------------------------



-- | How much should we partition a loop beyond what is necessary to have one task
-- per processor core.
overPartition :: Int
overPartition = 4
-- Overpartitioning definitely makes it faster... over 2X faster.
-- 8 doesn't gain anything over 4 however.. but it may reduce variance.
-- Hyperthreading shows some benefit!!

--------------------------------------------------------------------------------
#if 1
{-# INLINE readNumFile #-}
{-# INLINE parReadNats #-}
{-# INLINE readNatsPartial #-}
#else
{-# NOINLINE readNumFile #-}
{-# NOINLINE parReadNats #-}
{-# NOINLINE readNatsPartial #-}
#endif

-- | A simple front-end to 'parReadNats'.  This @mmap@s the file as a byte string and
-- parses it in parallel.  It returns a list of chunks of arbitrary size that may be
-- concattenated for a final result.
readNumFile :: forall nty . (U.Unbox nty, Integral nty, Eq nty, Show nty, Read nty) =>
               FilePath -> IO [U.Vector nty]
readNumFile path = do
  bs    <- unsafeMMapFile path
  ncpus <- getNumCapabilities 
  ls    <- runParIO $ parReadNats (ncpus * overPartition) bs
  return (sewEnds ls)

testReadNumFile :: forall nty . (U.Unbox nty, Integral nty, Eq nty, Show nty, Read nty) =>
                   FilePath -> IO [U.Vector nty]
testReadNumFile path = do
  bs    <- unsafeMMapFile path
  ncpus <- getNumCapabilities 
  ls    <- runParIO $ parReadNats (ncpus * overPartition) bs
  consume ls
  let ls' = sewEnds ls
  putStrLn $ "Number of chunks after sewing: "++show (length ls')
  putStrLn $ "Lengths: "++show (map U.length ls')++" sum "++ show(sum$ map U.length ls')
  let flat = U.concat ls'
  if (U.toList flat == map (read . B.unpack) (B.words bs)) 
   then putStrLn "Sewed version matched expected!!"
   else error "Did not match expected!"
  return ls'


-- | Read all the decimal numbers from a Bytestring.  They must be positive integers.
-- Be warned that this function is very permissive -- all non-digit characters are
-- treated as separators.
parReadNats :: forall nty d s . (U.Unbox nty, Num nty, Eq nty) =>
               Int -> S.ByteString -> Par d s [PartialNums nty]
parReadNats chunks bs = par
  where
    (each,left) = S.length bs `quotRem` chunks
    mapper ind = do
      let howmany = each + if ind==chunks-1 then left else 0
          mychunk = S.take howmany $ S.drop (ind * each) bs
 --              liftIO $ putStrLn$ "(monad-par/tree) Launching chunk of "++show howmany
      partial <- liftIO (readNatsPartial mychunk) -- TODO: move to ST.
      return [partial]
    reducer a b = return (a++b) -- Quadratic, but just at the chunk level.

    par :: LV.Par d s [PartialNums nty]
    par = do _ <- I.new
             -- parMapReduceRangeThresh 1 (InclusiveRange 0 (chunks - 1))
             --                      mapper reducer []              
             R.parMapReduceThresh 1 (R.range 0 chunks)
                mapper reducer [] 

--------------------------------------------------------------------------------                          
-- Partially parsed number fragments
--------------------------------------------------------------------------------

-- | A sequence of parsed numbers with ragged edges.
data PartialNums n = Compound !(Maybe (RightFrag n)) ![U.Vector n] !(Maybe (LeftFrag n))
                   | Single !(MiddleFrag n)
  deriving (Show,Eq,Ord,Read)

-- | This represents the rightmost portion of a decimal number that was interrupted
-- in the middle.
data RightFrag n = RightFrag {
                numDigits    :: {-# UNPACK #-} !Int,
                partialParse :: !n
                -- ^ The partialParse will need to be combined with the other half
                -- through addition (shifting first if it represents a left-half).
                }
  deriving (Show,Eq,Ord,Read)
           
data LeftFrag n = LeftFrag !n
  deriving (Show,Eq,Ord,Read)
           
-- | A fragment from the middle of a number, (potentially) missing pieces on both ends.
data MiddleFrag n = MiddleFrag {-# UNPACK #-} !Int !n
  deriving (Show,Eq,Ord,Read)

instance NFData (RightFrag n) where
  rnf (RightFrag _ _) = ()
instance NFData (LeftFrag n) where
  rnf (LeftFrag _) = ()
instance NFData (MiddleFrag n) where
  rnf (MiddleFrag _ _) = ()

instance NFData (PartialNums n) where
  rnf (Compound a b c) = a `seq` b `seq` c `seq` ()
  rnf (Single a)       = rnf a

{-# INLINE sewEnds #-}
-- Sew up a list of ragged-edged fragments into a list of normal vector chunks.
sewEnds :: forall nty . (U.Unbox nty, Integral nty, Eq nty) => [PartialNums nty] -> [U.Vector nty]
sewEnds [] = []
sewEnds origls = loop Nothing origls
 where
   loop _mleft []     = error "Internal error."
   loop mleft [last] = 
     case last of
       Single _                  -> error "sewEnds: Got a MiddleFrag at the END!"
       Compound _ _ (Just _)     -> error "sewEnds: Got a LeftFrag at the END!"
       Compound rf ls Nothing    -> sew mleft rf ls
     
   loop mleft (Compound rf ls lf : rst) = 
     sew mleft rf ls ++ loop lf rst

   -- TODO: Test this properly... doesn't occur in most files:
   loop mleft (Single (MiddleFrag nd m) : rst) =
     case mleft of
       Nothing           -> loop (Just (LeftFrag m)) rst
       Just (LeftFrag n) -> loop (Just (LeftFrag (shiftCombine n m nd))) rst
         
   sew mleft rf ls = 
     case (mleft, rf) of
       (Just (LeftFrag n), Just (RightFrag nd m)) -> let num = shiftCombine n m nd in
                                                     U.singleton num : ls 
       (Just (LeftFrag n), Nothing)               -> U.singleton n   : ls 
       (Nothing, Just (RightFrag _ m))            -> U.singleton m   : ls 
       (Nothing, Nothing)                         ->                   ls 

   shiftCombine n m nd = n * (10 ^ (fromIntegral nd :: nty)) + m


--------------------------------------------------------------------------------
-- Efficient sequential parsing
--------------------------------------------------------------------------------

-- {-# SPECIALIZE readNatsPartial :: S.ByteString -> IO [PartialNums Word] #-}
-- {-# SPECIALIZE readNatsPartial :: S.ByteString -> IO [PartialNums Word8] #-}  
-- {-# SPECIALIZE readNatsPartial :: S.ByteString -> IO [PartialNums Word16] #-}
-- {-# SPECIALIZE readNatsPartial :: S.ByteString -> IO [PartialNums Word32] #-}
-- {-# SPECIALIZE readNatsPartial :: S.ByteString -> IO [PartialNums Word64] #-}
-- {-# SPECIALIZE readNatsPartial :: S.ByteString -> IO [PartialNums Int] #-}
-- | Sequentially reads all the unsigned decimal (ASCII) numbers within a a
-- bytestring, which is typically a slice of a larger bytestring.  Extra complexity
-- is needed to deal with the cases where numbers are cut off at the boundaries.
-- readNatsPartial :: S.ByteString -> IO [PartialNums Word]
readNatsPartial :: forall nty . (U.Unbox nty, Num nty, Eq nty) => S.ByteString -> IO (PartialNums nty)
readNatsPartial bs
 | bs == S.empty = return (Single (MiddleFrag 0 0))
 | otherwise = do   
  let hd         = S.head bs
      charsTotal = S.length bs
  initV <- M.new (vecSize charsTotal)
  (vs,lfrg) <- scanfwd charsTotal 0 initV [] hd (S.tail bs)
  -- putStrLn$ " Got back "++show(length vs)++" partial reads"
  
  -- Once we are done looping we need some logic to figure out the corner cases:
  ----------------------------------------
  let total = sum $ map U.length vs
  if digit hd then
    (let first = U.head $ head vs -- The first (possibly partial) number parsed.
         rest  = U.tail (head vs) : tail vs
         -- If we start in the middle of a number, then the RightFrag goes till the first whitespace:
         rfrag = Just (RightFrag (fromJust$ S.findIndex (not . digit) bs) first) in
     if total == 0 
     then case lfrg of
           Nothing           -> return (Compound rfrag [] Nothing)
           Just (LeftFrag w) -> return (Single$ MiddleFrag charsTotal w)
     else return (Compound rfrag   rest lfrg)) -- Rfrag gobbles first.
   else   return (Compound Nothing vs   lfrg) -- May be completely empty (whitespace only).
  ---------------------------------------- 
 where
   -- Given the number of characters left, how big of a vector chunk shall we allocate?
   -- vecSize n = min chunkSize ((n `quot` 2) + 1) -- At minimum numbers must be one character.
   vecSize n = ((n `quot` 4) + 1) -- Assume at least 3 digit numbers... tunable parameter.
   
   -- loop :: Int -> Int -> nty -> M.IOVector nty -> Word8 -> S.ByteString ->
   --         IO (M.IOVector nty, Maybe (LeftFrag nty), Int)
   loop !lmt !ind !acc !vec !vecacc !nxt !rst
     -- Extend the currently accumulating number in 'acc':
     | digit nxt =
       let acc' = (10*acc + (fromIntegral nxt-48)) in 
       if lmt == 1 
       then closeOff vec vecacc ind (Just (LeftFrag acc'))
       else loop (lmt-1) ind acc' vec vecacc (unsafeHead rst) (unsafeTail rst)

     -- When we fill one chunk we move to the next:
     | ind >= M.length vec = do
         -- putStrLn$ " [!] Overflow at "++show ind++", next chunk!"
         -- putStr$ show ind ++ " "
         fresh <- M.new (vecSize$ S.length rst) :: IO (M.IOVector nty)
         vec'  <- U.unsafeFreeze vec
         loop lmt 0 acc fresh (vec':vecacc) nxt rst

     | otherwise =
       do M.write vec ind acc          
          if lmt == 1
            then closeOff vec vecacc (ind+1) Nothing
            else scanfwd (lmt-1) (ind+1) vec vecacc (unsafeHead rst) (unsafeTail rst)

   scanfwd !lmt !ind !vec !vecacc !nxt !rst
     | digit nxt = loop lmt ind 0 vec vecacc nxt rst -- We've started a number.
     | otherwise = if lmt == 1
                   then closeOff vec vecacc ind Nothing
                   else scanfwd (lmt-1) ind vec vecacc (unsafeHead rst) (unsafeTail rst)

   digit nxt = nxt >= 48 && nxt <= 57

   closeOff vec vecacc ind frag = 
     do vec' <- U.unsafeFreeze (M.take ind vec)
        return (reverse (vec':vecacc), frag)



--------------------------------------------------------------------------------
-- Unit Tests
--------------------------------------------------------------------------------

-- | HUnit unit tests.
unitTests :: [Test]
unitTests =
  [ TestCase$ assertEqual "t1" (Compound (Just (RightFrag 3 (123::Word))) [U.fromList []] Nothing) =<<
              readNatsPartial (S.take 4 "123 4")
  , TestCase$ assertEqual "t1" (Compound (Just (RightFrag 3 (123::Word))) [U.fromList []] (Just (LeftFrag 4))) =<<
              readNatsPartial (S.take 5 "123 4")
  , TestCase$ assertEqual "t3" (Single (MiddleFrag 3 (123::Word))) =<<
              readNatsPartial (S.take 3 "123")
  , TestCase$ assertEqual "t4" (Single (MiddleFrag 2 (12::Word))) =<<
              readNatsPartial (S.take 2 "123")
  , TestCase$ assertEqual "t5" (Compound Nothing [] (Just (LeftFrag (12::Word32)))) =<<
              readNatsPartial (S.take 3 " 123")

  , TestCase$ assertEqual "t6"
              (Compound (Just (RightFrag 3 23)) [U.fromList [456]] (Just (LeftFrag (78::Word64)))) =<<
              readNatsPartial (S.take 10 "023 456 789")
  ]

-- Simple graphs:

-- Two nodes, zero edges:
g1 :: AdjacencyGraph
g1 = runPar $ parseAdjacencyGraph 1 (B.pack "AdjacencyGraph\n 2\n 0\n 0\n 0\n")

-- Two nodes, one edge:
g2 :: AdjacencyGraph
g2 = runPar $ parseAdjacencyGraph 1 (B.pack "AdjacencyGraph\n 2\n 1\n 0\n 1\n 1\n")

fgl2 :: Gr () ()
fgl2 = toFGL g2 

---------------------------
-- Bigger, temporary tests:
---------------------------

t0 :: IO [U.Vector Word]
-- t0 = testReadNumFile "/tmp/grid_1000"
-- t0 = testReadNumFile "../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_1000"
t0 = testReadNumFile "1000_nums"

t1 :: IO [U.Vector Word]
-- t1 = testReadNumFile "/tmp/grid_125000"
t1 = testReadNumFile "../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_125000"

t2 :: IO ()
t2 = do t0_ <- getCurrentTime
        ls <- readNumFile "../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_10000000"
        t1_ <- getCurrentTime
        let v :: U.Vector Word
            v = U.concat ls
        putStrLn$ "Resulting vector has length: "++show (U.length v)
        t2_ <- getCurrentTime
        putStrLn$ "Time parsing/reading "++show (diffUTCTime t1_ t0_)++
                  " and coalescing "++show(diffUTCTime t2_ t1_)
        
-- This one is fast... but WHY?  It should be the same as the hacked 1-chunk parallel versions.
t3 :: IO [PartialNums Word]
t3 = do bs <- unsafeMMapFile "../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_10000000"
        pn <- readNatsPartial bs
        consume [pn]
        return [pn]

-- | Try it with readFile...
t3B :: IO [PartialNums Word]
t3B = do putStrLn "Sequential version + readFile"
         t0_ <- getCurrentTime
         bs <- S.readFile "../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_10000000"
         t1_ <- getCurrentTime
         putStrLn$ "Time to read file sequentially: "++show (diffUTCTime t1_ t0_)
         pn <- readNatsPartial bs
         consume [pn]
         return [pn]


t4 :: IO [PartialNums Word]
t4 = do putStrLn$ "Using parReadNats + readFile"
        t0_ <- getCurrentTime
        bs <- S.readFile "../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_10000000"
        t1_ <- getCurrentTime
        putStrLn$ "Time to read file sequentially: "++show (diffUTCTime t1_ t0_)
        pns <- runParIO $ parReadNats 4 bs
        consume pns
        return pns

t5 :: IO ()
t5 = do t0_ <- getCurrentTime
        AdjacencyGraph v1 v2 <- readAdjacencyGraph "../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_10000000"
        t1_ <- getCurrentTime
        putStrLn$ "Read adjacency graph in: "++show (diffUTCTime t1_ t0_)
        putStrLn$ " Edges and Verts: "++show (U.length v1, U.length v2)
        return ()

-- Make sure everything is forced
consume :: (Show n, M.Unbox n) => [PartialNums n] -> IO ()
consume ox = do
    evaluate (rnf ox)
    putStrLn$ "Result: "++show (length ox)++" segments of output"
    mapM_ fn ox
  where
  fn (Single (MiddleFrag c x)) = putStrLn$ " <middle frag "++ show (c,x)++">"
  fn (Compound r uvs l) = putStrLn$ " <segment, lengths "++show (map U.length uvs)++", ends "++show(r,l)++">"


--------------------------------------------------------------------------------
-- FGL instance:
--------------------------------------------------------------------------------
-- We COULD provide this, but it would be ludicrously inefficient, so it's better to
-- just provide a conversion function.

#if 0
-- | A labeled adjacency graph.
data LAdjacencyGraph vlab elab =
     LAdjacencyGraph {
       structure :: AdjacencyGraph,
       vlabs     :: V.Vector vlab,
       elabs     :: V.Vector elab
     }

--   TODO: promote LAdjacencyGraph to a family:
-- data family LAdjacencyGraph vertlab edgelab
--   TODO: need a closed type family so that we can optimize the unit case while
--   defaulting to the other case:
-- data instance LAdjacencyGraph () () = Unlabeled AdjacencyGraph

instance G.Graph LAdjacencyGraph where
  empty   = LAdjacencyGraph (AdjacencyGraph U.empty U.empty) V.empty V.empty
  isEmpty = U.null . vertOffsets . structure
-- match :: Node -> gr a b -> Decomp gr a b
-- mkGraph :: [LNode a] -> [LEdge b] -> gr a b
-- labNodes :: gr a b -> [LNode a]
#endif

#define USE_FGL
#ifdef USE_FGL
-- | Convert from an `AdjacencyGraph` to some FGL graph representation.
toFGL :: G.Graph g => AdjacencyGraph -> g () ()
toFGL origGr@AdjacencyGraph{vertOffsets, allEdges} =  
  -- A whole lot of dictionary elimination followed by inlining will need to happen
  -- for this to become deforested....
   G.mkGraph [ (v,()) | v <- allVerts ] edges
  where
    allVerts = [ 0 .. U.length vertOffsets - 1] 
    edges :: [G.LEdge ()]
    edges = [ (v1,v2,()) | v1 <- allVerts
                         , v2 <- U.toList (nbrs origGr v1) ]            
#endif

--------------------------------------------------------------------------------
-- DEVELOPMENT NOTES
--------------------------------------------------------------------------------
{-

[2013.07.01] {First Performance Notes}
--------------------------------------

Ran for the first time on the 557Mb file 3Dgrid_J_10000000.
On my laptop it took 26.4 seconds sequential and 9.6 seconds on four cores.
Productivity was 62% in the latter case.  CPU usage stayed pegged at 400%.

Aha, there is some screwup in the parallel stage, the sequential routine itself
(readNatsPartial, t3) only takes 4.5 seconds of realtime to read the whole file
(97.6% productivity).  Actually... that's better than the sequential time of the PBBS
C++, I believe.

NFData may be the culprit...


[2013.07.01] {Mysterious}
-------------------------

I'm trying to lock down where this huge perf difference comes from, but it's getting
stranger and stranger.  Even when I launch ONE parallel chunk it is still slow.  Even
when I dispense with the parallel libraries and SEQUENTIALLY launch a single
chunk... t2 is still very slow (and yet it should be doing the SAME thing as t3).

I rebuilt t3 to check again... still fast.  Compiling with -threaded ... still
fast. Ok, what about the obvious thing.  Maybe readNatsPartial is not as strict as I
think it is (though that should apply to BOTH the parallel and sequential tests, as
long as NFData isn't used...).  Ok, so I did the heavy handed thing and added a
deepseq to t3... it's STILL FAST.  What gives?

Ok, I'm worried that there are some weird effects with the mmap-based file reading.
I'm trying with simple, strict, readFile instead.  Here's the thing... It only takes
0.303837s to read the 500M file on my laptop.  The rest of the time is all parsing
and should be scalable.
  I introduced t3B to use the sequential routine + readFile and, yep, it's STILL FAST.
(and t4 is still slow).

Ok, let's take a look at the actual output sizes:

    $ time ./t3B_use_readFile.exe +RTS -N1 -s
    Sequential version + readFile
    Time to read file sequentially: 0.330334s
    Result: 1 segments of output
     <segment, length 69,568,627>

vs. 

    $ time ./t4_use_readFile.exe +RTS -N1 -s
    Using parReadNats + readFile
    Time to read file sequentially: 0.312601s
    Sequential debug version running on sizes: [557968893]
    (SEQUENTIAL) Launching chunk of 557968893
    Result: 1 segments of output
     <segment, length 69,568,627>

But the first takes 4.5 seconds and the second takes 25.4 seconds!!

Ok, well let's make them IDENTICAL... and then back down from there.  Eek, I added a
fourth case to the #if above, getting rid of "loop" and "splitAt" so that the
"parallel" version *literally* just calls getNumCapabilities and then the sequential.
It STILL takes 25 seconds.

Well, let's take away the last thing distinguishing them... getNumCapabilities.  THAT
WAS IT!  Taking that away makes them both drop to 4.5 seconds.  If I call
getNumCapabilities, even if I don't use the resulting value it criples the program to
take >5X longer.

This is on Mac OS GHC 7.6.2.  Let's try on linux and see if the same bug exists.

Whoa, wait, when I try to package this for reproduction, changing the module name and
not using -main-is .... that seems to make the bug vanish!!

With proper parallelism:
------------------------

If I simply avoid that call to the offending getNumCapabilities, hardcoding the
number of threads, I actually see quite nice parallel performance.

  * 1.7 seconds with readFile / monad-par, 4x overpartition (16 chunks)
  * 1.4 seconds with mmap / monad-par, 4x overpartition

-}

main = print (G.labNodes fgl2, G.labEdges fgl2)

