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

readWords :: Int -> S.ByteString -> IO (U.Vector Word, Word)
-- readWords :: Int -> S.ByteString -> IO (M.IOVector Word, Word)
readWords charLimit bs = do
  initV <- M.new chunkSize
  -- let bs' = S.dropWhile (not . digit) bs 
  -- (v,w,ind) <- loop charLimit 0 0 initV (S.head bs') (S.tail bs')
  (v,w,ind) <- scanfwd charLimit 0 initV (S.head bs) (S.tail bs)
  v'    <- U.unsafeFreeze v
  return (U.take ind v', w)
 where
   loop :: Int -> Int -> Word -> M.IOVector Word -> Word8 -> S.ByteString ->
           IO (M.IOVector Word, Word, Int)
   loop !lmt !ind !acc !vec !nxt !rst
     | lmt == 0       = return (vec, acc, ind)
--     | rst == S.empty = error "readWords: ran out of characters before hitting the limit."
     | digit nxt = loop (lmt-1) ind (10*acc + (fromIntegral nxt-48))
                                     vec (unsafeHead rst) (unsafeTail rst)
     | otherwise =
       do M.write vec ind acc
          scanfwd (lmt-1) (ind+1) vec (unsafeHead rst) (unsafeTail rst)

   scanfwd !lmt !ind !vec !nxt !rst
     | lmt == 0  = return (vec, 0, ind)
     | rst == S.empty = error "readWords: ran out of characters before hitting the limit."
     | digit nxt = loop lmt ind 0 vec nxt rst
     | otherwise = scanfwd (lmt-1) ind vec (unsafeHead rst) (unsafeTail rst)

   digit nxt = nxt >= 48 && nxt <= 57

--   scanfwd !lmt !vec !nxt !rst =
--     | 
--     error "implement scanfwd"                                

example =
  readWords 10 ("123 456 789")
  
--  loop 

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
