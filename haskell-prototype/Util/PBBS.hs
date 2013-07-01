{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | Utilities for reading PBBS data files, etc.

module Util.PBBS where 

import Control.Monad.Par.IO
import Data.Word
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.ByteString as S
import Data.ByteString.Unsafe (unsafeTail, unsafeHead)
-- import qualified Data.ByteString.Word8      as S
-- import qualified Data.ByteString.Lazy.Word8 as L


-- How many words shoud go in each continuously allocated vector?
chunkSize :: Int
chunkSize = 2 * 32768

-- | A number that was interrupted in the middle.  The same datatype is used for left
-- and right halves.
data Fragment = None                
              | Fragment {
                numDigits :: {-# UNPACK #-} !Int,
                partialParse :: {-# UNPACK #-} !Word
                -- ^ The partialParse will need to be combined with the other half
                -- through addition (shifting first if it represents a left-half).
                }


-- | Sequentially reads all the unsigned 64bit decimal (ASCII) numbers within a
-- region of a bytestring.  As well as returning the main payload of numbers, this
-- also returns a possible fragment that was interrupted on the right end, as well as
-- a count of how many digits were present in the first number (e.g. including
-- leading zeros) if a number began on the very first character.  (Which makes it
-- also possibly a fragment.)
readWord64s :: Int -> S.ByteString -> IO (Maybe Int, U.Vector Word, Maybe Word)
-- readWords :: Int -> S.ByteString -> IO (M.IOVector Word, Word)
readWord64s charLimit bs | charLimit > S.length bs =
  error $ "readWords: asked for more characters ("++show charLimit++
          ") than are present in bytestring ("++ show (S.length bs)++")"
readWord64s 0 bs = return (Nothing, U.empty, Nothing)
readWord64s charLimit bs = do
  initV <- M.new chunkSize
  -- let bs' = S.dropWhile (not . digit) bs 
  -- (v,w,ind) <- loop charLimit 0 0 initV (S.head bs') (S.tail bs')
  (v,w,ind) <- scanfwd charLimit 0 initV (S.head bs) (S.tail bs)
  v'        <- U.unsafeFreeze v
  return (Just 999, U.take ind v', Just w)
 where
   loop :: Int -> Int -> Word -> M.IOVector Word -> Word8 -> S.ByteString ->
           IO (M.IOVector Word, Word, Int)
   loop !lmt !ind !acc !vec !nxt !rst
     -- Extend the currently accumulating number in 'acc':
     | digit nxt =
       let acc' = (10*acc + (fromIntegral nxt-48)) in 
       if lmt == 1
       then return (vec, acc', ind)
       else loop (lmt-1) ind acc' vec (unsafeHead rst) (unsafeTail rst)
     | otherwise =
       do M.write vec ind acc
          if lmt == 1
            then return (vec, 0, ind+1)
            else scanfwd (lmt-1) (ind+1) vec (unsafeHead rst) (unsafeTail rst)

   scanfwd !lmt !ind !vec !nxt !rst
     | digit nxt = loop lmt ind 0 vec nxt rst
     | otherwise = if lmt == 1
                   then return (vec, 0, ind)
                   else scanfwd (lmt-1) ind vec (unsafeHead rst) (unsafeTail rst)

   digit nxt = nxt >= 48 && nxt <= 57

--   scanfwd !lmt !vec !nxt !rst =
--     | 
--     error "implement scanfwd"                                

example =
  readWord64s 10 ("123 456 789")
  
--  loop 

case_t1 = readWord64s 4 ("123 4")
case_t2 = readWord64s 5 ("123 4")
case_t3 = readWord64s 3 ("123")

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
