{-# LANGUAGE InstanceSigs #-}
module Codec.Arithmetic.Variety.BitVec
  ( BitVec

  -- * Construction
  , bitVec

  -- * Conversion
  , fromBits
  , toBits
  , fromBytes
  , toBytes
  , fromInteger
  , toInteger
  , fromString
  , toString

  -- * Methods
  , empty
  , null
  , length
  , singleton
  , append
  , take
  , drop
  , splitAt
  , replicate
  , countLeadingZeros
  , (!!)
  , (!?)

  -- * Extra
  , bitLen
  ) where

import Prelude hiding
  (null, length, take, drop, splitAt, replicate, (!!), fromInteger, toInteger)
import GHC.Num (integerLog2)
import Control.Exception (assert)

import Data.Bits ((.&.),(.|.))
import qualified Data.Bits as Bits
import Data.Word (Word8)
import qualified Data.List as L
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

err :: String -> a
err = error . ("Variety.BitVec: " ++)

-- | A vector of bits
data BitVec = BitVec !Int !Integer
  deriving (Show,Read,Ord,Eq)

instance Semigroup BitVec where
  (<>) :: BitVec -> BitVec -> BitVec
  (<>) = append

instance Monoid BitVec where
  mempty :: BitVec
  mempty = empty

-- | Construct a BitVec from a length and Integer.
bitVec :: Int -> Integer -> BitVec
bitVec = BitVec

-----------------
-- CONVERSIONS --
-----------------

-- | Construct from a list of bits. `True` is @1@ and `False` is @0@.
fromBits :: [Bool] -> BitVec
fromBits bs = BitVec len $ L.foldl' Bits.setBit 0 ones
  where
    len = L.length bs
    idxs = [len-1,len-2..0]
    ones = fmap snd $ L.filter fst $ zip bs idxs

-- | Return as a list of bits. `True` is @1@ and `False` is @0@.
toBits :: BitVec -> [Bool]
toBits (BitVec len int) = Bits.testBit int <$> [len-1,len-2..0]

-- | Construct from a lazy @ByteString@
fromBytes :: ByteString -> BitVec
fromBytes = fromBits . concatMap unpack8bits . BS.unpack

-- | Pack the bits into a lazy @ByteString@. Pads the left with @0@s if
-- the length is not a multiple of 8.
toBytes :: BitVec -> ByteString
toBytes v@(BitVec len _) = BS.pack $ fmap pack8bits $
                           chunksOf 8 $ pad ++ toBits v
  where
    padLen = (-len) `mod` 8
    pad = assert ((len + padLen) `mod` 8 == 0) $
          L.replicate padLen False

    chunksOf _ [] = []
    chunksOf i xs = a : chunksOf i b
      where (a,b) = L.splitAt i xs

-- | Read bits from the binary representation of an @Integer@. This
-- excludes the possibility of any leading zeros. Use `bitVec` for more
-- flexible construction.
fromInteger :: Integer -> BitVec
fromInteger int = BitVec sz int
  where sz = bitLen int

-- | Return the @Integer@ representation of the @BitVec@.
toInteger :: BitVec -> Integer
toInteger (BitVec _ int) = int

-- | Read the code from a list of @0@ and @1@ chars.
fromString :: String -> BitVec
fromString = fromBits . fmap f
  where f '0' = False
        f '1' = True
        f c = err $ "Non-binary char encountered: " ++ [c]

-- | Return the bits as a list of @0@ and @1@ chars.
toString :: BitVec -> String
toString = fmap f . toBits
  where f b = if b then '1' else '0'

-------------
-- METHODS --
-------------

-- | The empty bit vector.
empty :: BitVec
empty = BitVec 0 0

-- | Returns `True` iff the bit vector is empty.
null :: BitVec -> Bool
null (BitVec 0 0) = True
null _ = False

-- | Returns the number of bits in the vector.
length :: BitVec -> Int
length (BitVec len _) = len

-- | Concatenate two bit vectors.
append :: BitVec -> BitVec -> BitVec
append (BitVec len0 int0) (BitVec len1 int1) =
  BitVec (len0 + len1) (Bits.shiftL int0 len1 + int1)

-- | A vector of length 1 with the given bit.
singleton :: Bool -> BitVec
singleton False = BitVec 1 0
singleton True = BitVec 1 1

-- | @`take` n bv@ returns the bit vector consisting of the first @n@
-- bits of @bv@.
take :: Int -> BitVec -> BitVec
take n bv@(BitVec len int)
  | n <= 0    = empty
  | n >= len  = bv
  | otherwise = BitVec n $ int `Bits.shiftR` (len - n)

-- | @`drop` n bv@ returns @bv@ with the first @n@ bits removed.
drop :: Int -> BitVec -> BitVec
drop n bv@(BitVec len int)
  | n <= 0    = bv
  | n >= len  = empty
  | otherwise = BitVec (len - n) $
                int .&. ((1 `Bits.shiftL` (len - n)) - 1)

-- | @`splitAt` n bv@ is equivalent to @(`take` n bv, `drop` n bv)@
splitAt :: Int -> BitVec -> (BitVec, BitVec)
splitAt n bv = (take n bv, drop n bv)

-- | @`replicate` n b@ constructs a bit vector of length @n@ with @b@
-- the value of every bit.
replicate :: Int -> Bool -> BitVec
replicate n False = BitVec n 0
replicate n True = BitVec n (Bits.bit n - 1)

-- | Count the number of @0@ bits preceeding the first @1@ bit.
countLeadingZeros :: BitVec -> Int
countLeadingZeros (BitVec len int) = len - intLen
  where intLen = bitLen int

-- | Returns the value of a bit at a given index, with @0@ being the
-- index of the most significant (left-most) bit.
(!!) :: BitVec -> Int -> Bool
(BitVec len int) !! i = Bits.testBit int (len - i - 1)
infixl 9 !!

-- | Returns the value of a bit at a given index if within bounds, with
-- @0@ being the index of the most significant (left-most) bit.
(!?) :: BitVec -> Int -> Maybe Bool
(BitVec len int) !? i
  | i < 0 || i >= len = Nothing
  | otherwise = Just $ Bits.testBit int (len - i - 1)
infixl 9 !?

-------------
-- HELPERS --
-------------

-- | Pack exactly 8 bits into a byte
pack8bits :: [Bool] -> Word8
pack8bits = L.foldl' f 0
  where f acc b = (acc `Bits.shiftL` 1) .|. fromIntegral (fromEnum b)

-- | Return the 8 bits that make a byte
unpack8bits :: Word8 -> [Bool]
unpack8bits w = Bits.testBit w <$> [7,6,5,4,3,2,1,0]

-- | The number of bits in the binary expansion of a positive
-- integer. For consistency with inductive definitions, leading zeros
-- are not considered and so @`bitLen` 0 == 0@.
bitLen :: Integer -> Int
bitLen 0 = 0
bitLen n = fromIntegral (integerLog2 n) + 1
