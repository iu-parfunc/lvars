{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell, ScopedTypeVariables #-}

-- | Utilities for reading PBBS data files, etc.

module Util.PBBS where 

import Control.DeepSeq
import Control.Monad.Par.IO
import Control.Monad.Par.Combinator
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (getNumCapabilities)
import Data.Word
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.ByteString as S
import Data.ByteString.Unsafe (unsafeTail, unsafeHead)

-- import qualified Data.ByteString.Word8      as S
-- import qualified Data.ByteString.Lazy.Word8 as L

import System.IO.Posix.MMap (unsafeMMapFile)

import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.TH (defaultMainGenerator)

--------------------------------------------------------------------------------

-- How many words shoud go in each continuously allocated vector?
chunkSize :: Int
chunkSize = 32768

-- | How much should we partition a loop beyond what is necessary to have one task
-- per processor core.
overPartition = 4

--------------------------------------------------------------------------------


readNumFile :: (U.Unbox nty, Num nty, Eq nty) =>
               FilePath -> IO [PartialNums nty]
readNumFile path = do
  bs <- unsafeMMapFile path
  parReadNats bs

-- | Read all the decimal numbers from a Bytestring.  This is very permissive -- all
-- non-digit characters are treated as separators.
parReadNats :: forall nty . (U.Unbox nty, Num nty, Eq nty) =>
--               S.ByteString -> IO [U.Vector nty]
               S.ByteString -> IO [PartialNums nty]
parReadNats bs = do
  ncap <- getNumCapabilities
  runParIO (par ncap)
--  return (error "FINISHME")
 where
   par :: Int -> ParIO [PartialNums nty]
   par ncap = 
        let chunks = ncap * overPartition
            (each,left) = S.length bs `quotRem` chunks

            mapper ind = do
              let howmany = each + if ind==chunks-1 then left else 0
                  mychunk = S.take howmany $ S.drop (ind * each) bs
              partial <- liftIO (readNatsPartial mychunk)
              return [partial]
            reducer a b = return (a++b)
        in
        parMapReduceRangeThresh 1 (InclusiveRange 0 (chunks - 1))
                                   mapper reducer []

-- Partially parsed number fragments
--------------------------------------------------------------------------------

-- | A sequence of parsed numbers with ragged edges.
data PartialNums n = Compound !(Maybe (RightFrag n)) !(U.Vector n) !(Maybe (LeftFrag n))
                   | Single (MiddleFrag n)
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

--------------------------------------------------------------------------------
-- Efficient sequential parsing
--------------------------------------------------------------------------------

{-# SPECIALIZE readNatsPartial :: S.ByteString -> IO (PartialNums Word) #-}
{-# SPECIALIZE readNatsPartial :: S.ByteString -> IO (PartialNums Word32) #-}
{-# SPECIALIZE readNatsPartial :: S.ByteString -> IO (PartialNums Word64) #-}

-- | Sequentially reads all the unsigned decimal (ASCII) numbers within a a
-- bytestring, which is typically a slice of a larger bytestring.  Extra complexity
-- is needed to deal with the cases where numbers are cut off at the boundaries.
readNatsPartial :: forall nty . (U.Unbox nty, Num nty, Eq nty) =>
               S.ByteString -> IO (PartialNums nty)
readNatsPartial bs
 | bs == S.empty = return$ Single (MiddleFrag 0 0)
 | otherwise = do   
  let hd        = S.head bs
      charLimit = S.length bs
--  initV <- M.new (min chunkSize ((charLimit `quot` 2) + 1))
  -- FIXME: NEED TO GROW IF ESTIMATE FAILS:
--  initV <- M.new ((charLimit `quot` 4) + 3)
  initV <- M.new ((charLimit `quot` 2) + 1)
  (v,w,ind) <- scanfwd charLimit 0 initV hd (S.tail bs)
  v'        <- U.unsafeFreeze v
  let pref  = U.take ind v'
      rfrag = Just (RightFrag (fromJust$ S.findIndex (not . digit) bs) (U.head pref))
  if digit hd then
    (if pref == U.empty
     then case w of
           Nothing  -> return$ Compound rfrag (U.tail pref) Nothing
           Just (LeftFrag w) -> return$ Single$ MiddleFrag charLimit w
     else return$ Compound rfrag (U.tail pref) w)
   else
    return$ Compound Nothing pref w
 where
   loop :: Int -> Int -> nty -> M.IOVector nty -> Word8 -> S.ByteString ->
           IO (M.IOVector nty, Maybe (LeftFrag nty), Int)
   loop !lmt !ind !acc !vec !nxt !rst
     -- Extend the currently accumulating number in 'acc':
     | digit nxt =
       let acc' = (10*acc + (fromIntegral nxt-48)) in 
       if lmt == 1
       then return (vec, Just (LeftFrag acc'), ind)
       else loop (lmt-1) ind acc' vec (unsafeHead rst) (unsafeTail rst)
     | otherwise =
       do M.write vec ind acc
          if lmt == 1
            then return (vec, Nothing, ind+1)
            else scanfwd (lmt-1) (ind+1) vec (unsafeHead rst) (unsafeTail rst)

   scanfwd !lmt !ind !vec !nxt !rst
     | digit nxt = loop lmt ind 0 vec nxt rst
     | otherwise = if lmt == 1
                   then return (vec, Nothing, ind)
                   else scanfwd (lmt-1) ind vec (unsafeHead rst) (unsafeTail rst)

   digit nxt = nxt >= 48 && nxt <= 57


--------------------------------------------------------------------------------
-- Unit Tests
--------------------------------------------------------------------------------

case_t1 :: IO ()
case_t1 = assertEqual "t1" (Compound (Just (RightFrag 3 (123::Word))) (U.fromList []) Nothing) =<<
          readNatsPartial (S.take 4 "123 4")
case_t2 = assertEqual "t1" (Compound (Just (RightFrag 3 (123::Word))) (U.fromList []) (Just (LeftFrag 4))) =<<
          readNatsPartial (S.take 5 "123 4")
case_t3 = assertEqual "t3" (Single (MiddleFrag 3 (123::Word))) =<<
          readNatsPartial (S.take 3 "123")
case_t4 = assertEqual "t4" (Single (MiddleFrag 2 (12::Word))) =<<
          readNatsPartial (S.take 2 "123")
case_t5 = assertEqual "t5" (Compound Nothing U.empty (Just (LeftFrag (12::Word64)))) =<<
          readNatsPartial (S.take 3 " 123")

case_t6 = assertEqual "t6"
          (Compound (Just (RightFrag 3 23)) (U.fromList [456]) (Just (LeftFrag (78::Word32)))) =<<
          readNatsPartial (S.take 10 "023 456 789")

runTests = $(defaultMainGenerator)


t0 :: IO [PartialNums Word]
t0 = readNumFile "/tmp/grid_1000"

t1 :: IO [PartialNums Word]
t1 = readNumFile "/tmp/grid_125000"

t2 :: IO [PartialNums Word]
t2 = readNumFile "../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_10000000"

t3 :: IO (PartialNums Word)
t3 = do bs <- unsafeMMapFile "../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_10000000"
        readNatsPartial bs

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

-}

--------------------------------------------------------------------------------

{-
test :: Int -> Int -> Int -> S.ByteString -> Int
test k i !n t
    | y == '\n' = -- done reading the line, process it:
        if n `divisibleBy` k then add k (i+1) ys
                             else add k i     ys
    | otherwise = test k i n' ys
  where (y,ys) = uncons t
        n'     = parse y + 10 * n
-}


{-
readInt :: ByteString -> Maybe (Int, ByteString)
readInt as
    | null as   = Nothing
    | otherwise =
        case unsafeHead as of
            '-' -> loop True  0 0 (B.unsafeTail as)
            '+' -> loop False 0 0 (B.unsafeTail as)
            _   -> loop False 0 0 as

    where loop :: Bool -> Int -> Int -> ByteString -> Maybe (Int, ByteString)
          STRICT4(loop)
          loop neg i n ps
              | null ps   = end neg i n ps
              | otherwise =
                  case B.unsafeHead ps of
                    w | w >= 0x30
                     && w <= 0x39 -> loop neg (i+1)
                                          (n * 10 + (fromIntegral w - 0x30))
                                          (B.unsafeTail ps)
                      | otherwise -> end neg i n ps

          end _    0 _ _  = Nothing
          end True _ n ps = Just (negate n, ps)
          end _    _ n ps = Just (n, ps)
-}
