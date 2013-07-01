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

-- | A number that was interrupted in the middle.  The same datatype is used for left
-- and right halves.
-- data Fragment = None                
--               | RightFrag {
--                 numDigits    :: {-# UNPACK #-} !Int,
--                 partialParse :: {-# UNPACK #-} !Word
--                 -- ^ The partialParse will need to be combined with the other half
--                 -- through addition (shifting first if it represents a left-half).
--                 }
--               | MiddleFrag {
--                 numDigits    :: {-# UNPACK #-} !Int,
--                 partialParse :: {-# UNPACK #-} !Word
--                 }
--               | LeftFrag {
--                 partialParse :: {-# UNPACK #-} !Word
--                 }


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


-- | A sequence of parsed numbers with ragged edges.
data PartialNums = Compound !(Maybe RightFrag) !(U.Vector Word) !(Maybe LeftFrag)
                 | Single MiddleFrag
  deriving (Show,Eq,Ord,Read)


-- | Sequentially reads all the unsigned 64bit decimal (ASCII) numbers within a
-- region of a bytestring.  As well as returning the main payload of numbers, this
-- also returns a possible fragment that was interrupted on the right end, as well as
-- a count of how many digits were present in the first number (e.g. including
-- leading zeros) if a number began on the very first character.  (Which makes it
-- also possibly a fragment.)
readWord64s :: Int -> S.ByteString -> IO PartialNums
-- readWords :: Int -> S.ByteString -> IO (M.IOVector Word, Word)
readWord64s charLimit bs | charLimit > S.length bs =
  error $ "readWords: asked for more characters ("++show charLimit++
          ") than are present in bytestring ("++ show (S.length bs)++")"
readWord64s 0 bs = return$ Single (MiddleFrag 0 0)
readWord64s charLimit bs = do
  initV <- M.new chunkSize
  let hd = S.head bs      
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

case_t1 :: IO ()
case_t1 = assertEqual "t1" (Compound (Just (RightFrag 3 123)) (U.fromList []) Nothing) =<<
          readWord64s 4 ("123 4")
case_t2 = assertEqual "t1" (Compound (Just (RightFrag 3 123)) (U.fromList []) (Just (LeftFrag 4))) =<<
          readWord64s 5 ("123 4")
case_t3 = assertEqual "t3" (Single (MiddleFrag 3 123)) =<<
          readWord64s 3 ("123")
case_t4 = assertEqual "t4" (Single (MiddleFrag 2 12)) =<<
          readWord64s 2 ("123")
case_t5 = assertEqual "t5" (Compound Nothing U.empty (Just (LeftFrag 12))) =<<
          readWord64s 3 (" 123")

case_t6 = assertEqual "t6"
          (Compound (Just (RightFrag 3 23)) (U.fromList [456]) (Just (LeftFrag 78))) =<<
          readWord64s 10 ("023 456 789")


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
