{-# LANGUAGE InstanceSigs #-}
module Codec.Arithmetic.Variety.BitVec
  ( BitVec
  -- * Typical Methods
  , bitVec
  , empty
  , null
  , length
  , append
  , take
  , drop
  , splitAt
  , replicate
  , countLeadingZeros

  -- * Conversions
  , fromInteger
  , toInteger
  , fromBits
  , toBits
  , fromBytes
  , toBytes
  , fromString
  , toString

  -- * Helpers
  , bitLen
  , pack8bits
  , unpack8bits
  ) where

import Prelude hiding
  (null, length, take, drop, splitAt, replicate, fromInteger, toInteger)
import GHC.Num (integerLog2)
import Control.Exception (assert)

import Data.Bits ((.&.),(.|.))
import qualified Data.Bits as Bits
import Data.Word (Word8)
import qualified Data.List as L
import qualified Data.List.Extra as L
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

err :: String -> a
err = error . ("Variety.BitVec: " ++)

-- | A bit vector made of a size and an Integer as a store for bits
data BitVec = BitVec !Int !Integer
  deriving (Show,Read,Ord,Eq)

instance Semigroup BitVec where
  (<>) :: BitVec -> BitVec -> BitVec
  (<>) = append

instance Monoid BitVec where
  mempty :: BitVec
  mempty = empty

---------------------
-- TYPICAL METHODS --
---------------------

-- | Construct a BitVec from a length and Integer
bitVec :: Int -> Integer -> BitVec
bitVec = BitVec

-- | The empty bit vector
empty :: BitVec
empty = BitVec 0 0

null :: BitVec -> Bool
null (BitVec 0 0) = True
null _ = False

length :: BitVec -> Int
length (BitVec len _) = len

-- | Concatenate two bit vectors
append :: BitVec -> BitVec -> BitVec
append (BitVec len0 int0) (BitVec len1 int1) =
  BitVec (len0 + len1) (Bits.shiftL int0 len1 + int1)

take :: Int -> BitVec -> BitVec
take n bv@(BitVec len int)
  | n <= 0    = empty
  | n >= len  = bv
  | otherwise = BitVec n $ int `Bits.shiftR` (len - n)

drop :: Int -> BitVec -> BitVec
drop n bv@(BitVec len int)
  | n <= 0    = bv
  | n >= len  = empty
  | otherwise = BitVec (len - n) $
                int .&. ((1 `Bits.shiftL` (len - n)) - 1)

splitAt :: Int -> BitVec -> (BitVec, BitVec)
splitAt n bv = (take n bv, drop n bv)

replicate :: Int -> Bool -> BitVec
replicate n False = BitVec n 0
replicate n True = BitVec n (Bits.bit n - 1)

countLeadingZeros :: BitVec -> Int
countLeadingZeros (BitVec len int) = len - intLen
  where intLen = bitLen int

-----------------
-- CONVERSIONS --
-----------------

-- | Read bits from the binary representation of an Integer. This
-- excludes the possibility of any leading zeros. Use @bitVec@ for more
-- flexible construction.
fromInteger :: Integer -> BitVec
fromInteger int = BitVec sz int
  where sz = bitLen int

toInteger :: BitVec -> Integer
toInteger (BitVec _ int) = int

-- | Construct from a list of bits
fromBits :: [Bool] -> BitVec
fromBits bs = BitVec len $ L.foldl' Bits.setBit 0 ones
  where
    len = L.length bs
    idxs = [len-1,len-2..0]
    ones = fmap snd $ L.filter fst $ zip bs idxs

-- | Return each bit in the vector, left-to-right
toBits :: BitVec -> [Bool]
toBits (BitVec len int) = Bits.testBit int <$> [len-1,len-2..0]
-- TODO: make sure out of bounds result in 0s

-- | Convert a lazy ByteString into a bit vector
fromBytes :: ByteString -> BitVec
fromBytes = fromBits . concatMap unpack8bits . BS.unpack

-- | Pack the bits into a lazy ByteString
toBytes :: BitVec -> ByteString
toBytes v@(BitVec len _) = BS.pack $ fmap pack8bits $
                           L.chunksOf 8 $ pad ++ toBits v
  where
    padLen = (-len) `mod` 8
    pad = assert ((len + padLen) `mod` 8 == 0) $
          L.replicate padLen False

-- | Read the code from a list of '0' and '1' chars
fromString :: String -> BitVec
fromString = fromBits . fmap f
  where f '0' = False
        f '1' = True
        f c = err $ "Non-binary char encountered: " ++ [c]

-- | Return the bits as a list of '0' and '1' chars
toString :: BitVec -> String
toString = fmap f . toBits
  where f b = if b then '1' else '0'

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
-- are not considered and so @bitLen 0 == 0@.
bitLen :: Integer -> Int
bitLen 0 = 0
bitLen n = fromIntegral (integerLog2 n) + 1
