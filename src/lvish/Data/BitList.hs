{-# LANGUAGE CPP #-}

-- | A simple module that provides a more memory-efficient representation of `[Bool]`.

module Data.BitList
  ( BitList
  , cons, head, tail, empty, null
  , pack, unpack, length, drop, reverse
  , fromString, popCount

   -- * Debugging only:
  , showRep
  )
where

import           Data.Bits hiding (popCount)
import qualified Data.Bits as B
import           Data.Int
import qualified Data.List as L
import           Data.Word
import           Prelude   hiding (drop, head, length, null, reverse, tail,
                            (>>))

#ifdef TESTING
import Test.HUnit
import Test.QuickCheck     hiding ((.&.))
import Test.QuickCheck.Gen
#endif

-- | An immutable list of bits.
data BitList = One  {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word64
             | More {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word64 BitList
  -- ^ The Word8 stores how many bits are used within the current chunk.
  --   The Int64 is the chunk payload.
--  deriving (Ord, Show)

{-# INLINABLE cons #-}
{-# INLINABLE head #-}
{-# INLINABLE tail #-}
{-# INLINABLE null #-}

instance Show BitList where
 show bl = "BitList " ++ show (map (\b -> case b of True -> '1'; False -> '0') (unpack bl))
-- show bl = "pack " ++ show (unpack bl)

-- | Show the internal representation, for debugging purposes.
showRep :: BitList -> String
showRep (One i b)    = "One "++show i++" "++show b++""
showRep (More i b t) = "More "++show i++" "++show b++" ("++showRep t++")"

-- | Convert from a human-readable string of '0' and '1' characters,
-- i.e. "1101".
fromString :: String -> BitList
fromString []       = empty
fromString ('1':tl) = cons True  (fromString tl)
fromString ('0':tl) = cons False (fromString tl)
fromString str      = error $ "BitList.fromString: Not a string of zeros and ones: "++str

-- TODO: Read instance.

-- | An empty list containing no bits.
empty :: BitList
empty = One 0 0

-- | Is the list empty?
null :: BitList -> Bool
null (One 0 _) = True
null _         = False

-- | Add a single bit to the front of the list.
cons :: Bool -> BitList -> BitList
cons True  x@(One  64 _ )   = More 1 1 x
cons False x@(One  64 _ )   = More 1 0 x
cons True  x@(More 64 _ _)  = More 1 1 x -- We waste the Word8, but we don't reallocate the object.
cons False x@(More 64 _ _)  = More 1 0 x
cons True    (One   i bv)   = One  (i+1) (bv `setBit` toI i)
cons False   (One   i bv)   = One  (i+1) (bv               )
cons True    (More  i bv r) = More (i+1) (bv `setBit` toI i) r
cons False   (More  i bv r) = More (i+1) (bv               ) r

toI :: Word8 -> Int
toI = fromIntegral

-- TODO: May consider (More 0 _ _) representation to reduce extra
-- allocation when size of the BitList is fluctuating back and forth.

-- | Return the first bit, or an error if the list is null.
head :: BitList -> Bool
head (One  0 _   ) = error "tried to take head of an empty BitList"
head (More 0 _  _) = error "BitList: data structure invariant failure!"
head (One  i bv  ) = bv `testBit` (toI i-1)
head (More i bv _) = bv `testBit` (toI i-1)

-- | Drop the first bit.
tail :: BitList -> BitList
tail (One  0 _   ) = error "tried to take the tail of an empty BitList"
tail (One  i bv  ) = One  (i-1) bv
tail (More 1 _  r) = r
tail (More i bv r) = More (i-1) bv r

-- | Switch to the more dense (but isomprophic) memory representation.
pack :: [Bool] -> BitList
pack  []   = One 0 0
pack (h:t) = cons h (pack t)

-- | Switch to the more sparse (but isomprophic) memory representation.
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

-- | Drop the first `n` bits.
drop :: Int -> BitList -> BitList
drop n (One i bv)
   | n >= toI i = empty
   | otherwise = One (i - fromIntegral n) bv
drop n (More i bv r)
   | n >= toI i = drop (n - toI i) r
   | otherwise = More (i - fromIntegral n) bv r

-- | How many bits are in the BitList?
length :: BitList -> Int
length (One  i _)   = toI i
length (More i _ r) = toI i + length r

-- | How many `True`s are in the list?
popCount :: BitList -> Int
popCount (One ix bv)     = B.popCount (flp ix bv)
popCount (More ix bv tl) = B.popCount (flp ix bv) + popCount tl


-- TODO: index, take, etc

-- TODO: functor instance, etc.

-- TODO: Reverse.. reverse CAN be efficient if we allow chunks that are NOT full.

-- | Reverse a bitlist, O(N) time and space.
--
-- NOTE: This is somewhat more efficient than reversing a regular list
-- because bitwise operations can be used to reverse 64 bit chunks at
-- a time.  However, it is still costly because reversing the order of
-- bits is not a native operation.
reverse :: BitList -> BitList
reverse (One  ix0 bv0)     =           One ix0 (flp ix0 bv0)
reverse (More ix0 bv0 tl0) = loop tl0 (One ix0 (flp ix0 bv0))
 where
   loop (More ix bv tl) acc = loop tl (More ix (flp ix bv) acc)
   loop (One  ix bv   ) acc = More ix (flp ix bv) acc

{-# INLINE flp #-}
flp :: Word8 -> Word64 -> Word64
flp ix bv = rev64 (bv << (64 - toI ix))

instance Eq BitList where
  -- Here we specifically ignore upper bits in the representation:
  One i1 bv1 == One i2 bv2
     | i1 == i2 && mask i1 bv1 == mask i2 bv2  = True
     | otherwise = False

  More i1 bv1 tl1 == More i2 bv2 tl2 =
   (One i1 bv1 == One i2 bv2) &&
   (tl1 == tl2)

  _ == _ = False

-- Mask off the high order (unused) bits.
mask :: (Show a, Bits a, Num a) => Word8 -> a -> a
mask 0 _ = 0
mask i x = (x << n) >> n
-- mask i x =
-- trace ("SHIFT left "++show n++" yielding "++show (x `unsafeShiftL` n)++
--        " unsafe, "++show (x `shiftL` n)++" safe.") $
--   (x `unsafeShiftL` n) >> n
  where n = 64 - toI i


-- | This lexiographic Ord instance makes it suitable for using for pedigree:
instance Ord BitList where
  {-# INLINABLE compare #-}

  -- False < True
  compare a b =
    if null a
    then if null b
         then EQ
         else LT
    else if null b
         then GT
         else case compare (head a) (head b) of
                LT -> LT
                GT -> GT
                EQ -> compare (tail a) (tail b)

{-
  compare a b = cmp a b
    where
     cmp (One  i1 v1)    (One  i2 v2)    = go i1 v1 i2 v2
     cmp (More i1 v1 tl1) (More i2 v2 tl2) =
       case go i1 v1 i2 v2 of
         LT -> LT
         GT -> GT
         EQ -> compare tl1 tl2
     {-# INLINE go #-}
     go i1 v1 i2 v2 =
       case compare i1 i2 of
         LT -> LT
         GT -> GT
         EQ -> compare (mask i1 v1) (mask i2 v2)
-}


---------------------------------------------------------------------------
-- Bit reversal tricks from the wonderful site:
--   http://graphics.stanford.edu/~seander/bithacks.html#BitReverseObvious
---------------------------------------------------------------------------

-- | Reverse the low byte of a Word64
_rev8 :: Word64 -> Word64
_rev8 b = (((b * 0x80200802) .&. 0x0884422110) * 0x0101010101) >> 32;

_rev32 :: Word32 -> Word32
_rev32 x0 =  (x4 >> 16) .|. (x4 << 16)
  where
   x1 = (((x0 .&. 0xaaaaaaaa) >> 1) .|. ((x0 .&. 0x55555555) << 1))
   x2 = (((x1 .&. 0xcccccccc) >> 2) .|. ((x1 .&. 0x33333333) << 2))
   x3 = (((x2 .&. 0xf0f0f0f0) >> 4) .|. ((x2 .&. 0x0f0f0f0f) << 4))
   x4 = (((x3 .&. 0xff00ff00) >> 8) .|. ((x3 .&. 0x00ff00ff) << 8));

rev64 :: Word64 -> Word64
rev64 x0 = (x5 >> 32) .|. (x5 << 32)
  where
   x1 = (((x0 .&. 0xaaaaaaaaaaaaaaaa) >> 1)  .|. ((x0 .&. 0x5555555555555555) << 1))
   x2 = (((x1 .&. 0xcccccccccccccccc) >> 2)  .|. ((x1 .&. 0x3333333333333333) << 2))
   x3 = (((x2 .&. 0xf0f0f0f0f0f0f0f0) >> 4)  .|. ((x2 .&. 0x0f0f0f0f0f0f0f0f) << 4))
   x4 = (((x3 .&. 0xff00ff00ff00ff00) >> 8)  .|. ((x3 .&. 0x00ff00ff00ff00ff) << 8));
   x5 = (((x4 .&. 0xffff0000ffff0000) >> 16) .|. ((x4 .&. 0x0000ffff0000ffff) << 16));

(>>) :: Bits a => a -> Int -> a
(<<) :: Bits a => a -> Int -> a
-- (>>) = shiftR
-- (<<) = shiftL
(>>) = unsafeShiftR
(<<) = unsafeShiftL

--------------------------------------------------------------------------------
-- Testing:

_t1 :: BitList
_t1 = pack (L.concat$ L.replicate 10 [True,False,True])

_t2 :: Int
_t2 = L.length $ unpack $ pack $ replicate 1000 True

t3 :: BitList
t3 = pack $ replicate 1000 True

t4 :: BitList
t4 = drop 500 t3

_p3 :: Bool
_p3 = L.and (unpack t3)

_p4 :: Bool
_p4 = L.and (unpack t4)

t5 :: BitList
t5 = iterate tail t4 !! 250

_t5a :: Int
_t5a = length t5

_t5b :: Int
_t5b = L.length (unpack t5)

_t6 :: BitList
_t6 = drop 5 (More 1 0 (One 64 0))
-- More (-4) 0 (One 64 0)

_t7 :: Bool
_t7 = prop_droptail (pack [True])

_t8a :: Bool
_t8a = tail oo == drop 1 oo

_t8b :: Bool
_t8b = null (tail oo)

_t8c :: Bool
_t8c = null (drop 1 oo)

oo :: BitList
oo = One 1 1

prop_droptail :: BitList -> Bool
prop_droptail xs =
  (length xs == 0) ||
  (drop 1 xs == tail xs)

_prop_ord :: BitList -> BitList -> Bool
_prop_ord xs ys =
  (compare xs ys ==
   compare (unpack xs) (unpack ys))


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
    , t7  ~=? True
    , t8a ~=? True
    , t8b ~=? True
    , t8c ~=? True
    ]

main = do runTestTT tests ; all_props

-- TODO: QuickCheck

-- \s -> length (take 5 s) == 5

q1 = quickCheck prop_droptail

q2 = quickCheck prop_ord

q3 = quickCheck (\w -> rev64 (rev64 w) == w)
q4 = quickCheck (\w -> rev32 (rev32 w) == w)

q5 = quickCheck (\b -> reverse (reverse b) == b)

q6 = quickCheck (\b -> popCount b == P.length (filter id (unpack b)))

all_props = do q1; q2; q3; q4; q5; q6

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
