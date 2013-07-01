{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}

-- | Utilities for reading PBBS data files, etc.

module Util.PBBS where 

import Control.Monad.Par.IO
import Data.Word
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.ByteString as S
import Data.ByteString.Unsafe (unsafeTail, unsafeHead)

-- import qualified Data.ByteString.Word8      as S
-- import qualified Data.ByteString.Lazy.Word8 as L

import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.TH (defaultMainGenerator)


-- How many words shoud go in each continuously allocated vector?
chunkSize :: Int
chunkSize = 2 * 32768

--------------------------------------------------------------------------------
-- Partially parsed number fragments
--------------------------------------------------------------------------------

-- | A sequence of parsed numbers with ragged edges.
data PartialNums = Compound !(Maybe RightFrag) !(U.Vector Word) !(Maybe LeftFrag)
                 | Single MiddleFrag
  deriving (Show,Eq,Ord,Read)

-- | This represents the rightmost portion of a decimal number that was interrupted
-- in the middle.
data RightFrag = RightFrag {
                numDigits    :: {-# UNPACK #-} !Int,
                partialParse :: {-# UNPACK #-} !Word
                -- ^ The partialParse will need to be combined with the other half
                -- through addition (shifting first if it represents a left-half).
                }
  deriving (Show,Eq,Ord,Read)
           
data LeftFrag = LeftFrag {-# UNPACK #-} !Word
  deriving (Show,Eq,Ord,Read)
           
-- | A fragment from the middle of a number, (potentially) missing pieces on both ends.
data MiddleFrag = MiddleFrag {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Word
  deriving (Show,Eq,Ord,Read)

--------------------------------------------------------------------------------
-- Efficient sequential parsing
--------------------------------------------------------------------------------

-- | Sequentially reads all the unsigned decimal (ASCII) numbers within a a
-- bytestring, which is typically a slice of a larger bytestring.  Extra complexity
-- is needed to deal with the cases where numbers are cut off at the boundaries.
readWord64s :: S.ByteString -> IO PartialNums
readWord64s bs
 | bs == S.empty = return$ Single (MiddleFrag 0 0)
 | otherwise = do   
  let hd        = S.head bs
      charLimit = S.length bs
  initV <- M.new chunkSize      
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
   loop :: Int -> Int -> Word -> M.IOVector Word -> Word8 -> S.ByteString ->
           IO (M.IOVector Word, Maybe LeftFrag, Int)
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
case_t1 = assertEqual "t1" (Compound (Just (RightFrag 3 123)) (U.fromList []) Nothing) =<<
          readWord64s (S.take 4 "123 4")
case_t2 = assertEqual "t1" (Compound (Just (RightFrag 3 123)) (U.fromList []) (Just (LeftFrag 4))) =<<
          readWord64s (S.take 5 "123 4")
case_t3 = assertEqual "t3" (Single (MiddleFrag 3 123)) =<<
          readWord64s (S.take 3 "123")
case_t4 = assertEqual "t4" (Single (MiddleFrag 2 12)) =<<
          readWord64s (S.take 2 "123")
case_t5 = assertEqual "t5" (Compound Nothing U.empty (Just (LeftFrag 12))) =<<
          readWord64s (S.take 3 " 123")

case_t6 = assertEqual "t6"
          (Compound (Just (RightFrag 3 23)) (U.fromList [456]) (Just (LeftFrag 78))) =<<
          readWord64s (S.take 10 "023 456 789")


main = $(defaultMainGenerator)

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
