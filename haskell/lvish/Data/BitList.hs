{-# LANGUAGE CPP #-}

-- | A simple module that provides a more memory-efficient representation of `[Bool]`.

module Data.BitList 
  ( BitList
  , cons, head, tail, empty 
  , pack, unpack, length, drop
  )
where

import Data.Int
import Data.Bits
import Data.Word
import Prelude as P hiding (head,tail,drop,length)
import qualified Data.List as L

#ifdef TESTING
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Gen
#endif

-- | An immutable list of bits.
data BitList = One  {-# UNPACK #-} !Word8 {-# UNPACK #-} !Int64
             | More {-# UNPACK #-} !Word8 {-# UNPACK #-} !Int64 BitList
  -- ^ The Word8 stores how many bits are used within the current chunk.
  --   The Int64 is the chunk payload.
  deriving (Ord, Show)

{-# INLINABLE cons #-}
{-# INLINABLE head #-}
{-# INLINABLE tail #-}

-- instance Show BitList where 
--  show bl = "BitList " ++ show (map (\b -> case b of True -> '1'; False -> '0') (unpack bl))
-- -- show bl = "pack " ++ show (unpack bl)

-- TODO: Read instance.

empty :: BitList
empty = One 0 0

cons :: Bool -> BitList -> BitList
cons True  x@(One  64 _ )   = More 1 1 x
cons False x@(One  64 _ )   = More 1 0 x
cons True  x@(More 64 _ _)  = More 1 1 x
cons False x@(More 64 _ _)  = More 1 0 x
cons True    (One   i bv)   = One  (i+1) (bv `setBit` toI i)
cons False   (One   i bv)   = One  (i+1) (bv               )
cons True    (More  i bv r) = More (i+1) (bv `setBit` toI i) r
cons False   (More  i bv r) = More (i+1) (bv               ) r

toI :: Word8 -> Int
toI = fromIntegral

-- TODO: May consider (More 0 _ _) representation to reduce extra
-- allocation when size of the BitList is fluctuating back and forth.

head :: BitList -> Bool
head (One  0 _   ) = error "tried to take head of an empty BitList"
head (More 0 _  _) = error "BitList: data structure invariant failure!"
head (One  i bv  ) = bv `testBit` (toI i-1)
head (More i bv _) = bv `testBit` (toI i-1)

tail :: BitList -> BitList
tail (One  0 _   ) = error "tried to take the tail of an empty BitList"
tail (One  i bv  ) = One  (i-1) bv
tail (More 1 _  r) = r
tail (More i bv r) = More (i-1) bv r

pack :: [Bool] -> BitList
pack  []   = One 0 0
pack (h:t) = cons h (pack t)

unpack :: BitList -> [Bool] 
unpack (One 0 _)     = []
unpack (One i bv)    = (bv `testBit` (toI i-1)) : unpack (One (i-1) bv)
unpack (More 0 _ r)  = unpack r
unpack (More i bv r) = (bv `testBit` (toI i-1)) : unpack (More (i-1) bv r)

-- drop :: Int -> BitList -> BitList
-- drop 0 bl           = bl
-- drop n bl | n >= 64 = case bl of 
-- 		        One _ _    -> error "drop: not enough elements in BitList"
-- 			More i _ r -> drop (n-i) r
-- drop n bl = case bl of 
-- 	      One i  bv   -> One  (i-n) bv
-- 	      More i bv r -> More (i-n) bv r

drop :: Int -> BitList -> BitList
drop n (One i bv)
   | n >= toI i = empty
   | otherwise = One (i - fromIntegral n) bv
drop n (More i bv r)
   | n >= toI i = drop (n - toI i) r
   | otherwise = More (i - fromIntegral n) bv r

length :: BitList -> Int
length (One  i _)   = toI i
length (More i _ r) = toI i + length r


-- TODO: index, take, etc

-- TODO: functor instance, etc.

instance Eq BitList where
  -- Here we specifically ignore upper bits in the representation:
  One i1 bv1 == One i2 bv2 
     | i1 == i2 && mask i1 bv1 == mask i2 bv2  = True
     | otherwise = False

  More i1 bv1 tl1 == More i2 bv2 tl2 = 
   (One i1 bv1 == One i2 bv2) && 
   (tl1 == tl2)

  _ == _ = False
  
mask :: Bits a => Word8 -> a -> a
mask i x = shiftR (shiftL x n) n
  where n = 64 - toI i

--------------------------------------------------------------------------------
-- Testing:

t1 :: BitList
t1 = pack (L.concat$ L.replicate 10 [True,False,True])

t2 :: Int
t2 = L.length $ unpack $ pack $ replicate 1000 True

t3 :: BitList
t3 = pack $ replicate 1000 True

t4 :: BitList
t4 = drop 500 t3

p3 :: Bool
p3 = L.and (unpack t3)

p4 :: Bool
p4 = L.and (unpack t4)

t5 :: BitList
t5 = iterate tail t4 !! 250

t5a :: Int
t5a = length t5

t5b :: Int
t5b = L.length (unpack t5)

t6 :: BitList
t6 = drop 5 (More 1 0 (One 64 0)) 
-- More (-4) 0 (One 64 0)

t7 :: Bool
t7 = prop_droptail (pack [True])

prop_droptail :: BitList -> Bool
prop_droptail xs =
  (length xs == 0) ||
  (drop 1 xs == tail xs)

#ifdef TESTING
tests :: Test
tests = 
  TestList 
    [ 
      show t1 ~=? "BitList \"101101101101101101101101101101\""
    , t2  ~=? 1000
    , t5a ~=? 250
    , t5b ~=? 250
    , p3  ~=? True
    , p4  ~=? True
    , length t6 ~=? 60
    ]

-- TODO: QuickCheck

-- \s -> length (take 5 s) == 5

q1 = quickCheck (prop_droptail :: BitList -> Bool)

instance Arbitrary BitList where
  arbitrary = MkGen $ \ rng n -> 
	        let ls = (unGen arbitrary) rng n
		in pack ls
#endif


--------------------------------------------------------------------------------
-- Improvement suggestion from Ryan Ingram:
-- https://www.haskell.org/pipermail/haskell-cafe/2011-October/095978.html
--------------------------------------------------------------------------------
{-
I suggest

data BitTail = Zero | More {-# UNPACK #-} !Int64 BitTail
data BitList = Head {-# UNPACK #-} !Int {-# UNPACK #-} !Int64 BitTail
empty = Head 0 0 Zero

or else just
data BitList = Head {-# UNPACK #-} !Int {-# UNPACK #-} !Int64 [Int64]
empty = Head 0 0 []
length (Head n _ xs) = n + 64 * List.length xs

unpack :: BitList -> [Bool]
> unpack (One 0 _)     = []
> unpack (One i bv)    = (bv `testBit` (i-1)) : unpack (One (i-1) bv)
> unpack (More 0 _ r)  =  unpack r
> unpack (More i bv r) = (bv `testBit` (i-1)) : unpack (More (i-1) bv r)
>

I'd implement as

view :: BitList -> Maybe (Bool, BitList)
view (One 0 _) = Nothing
view bl = Just (head bl, tail bl)

unpack = unfoldr view

-}
