{-# LANGUAGE InstanceSigs #-}
module Codec.Arithmetic.Variety.BitVec
  ( BitVec
  , empty
  , append
  , bitVec
  , length
  , integer
  , fromBits
  , toBits
  , fromBytes
  , toBytes
  , fromString
  , toString
  , bitLen
  ) where

import Prelude hiding (length)
import GHC.Num (integerLog2)
import Control.Exception (assert)

import Data.Bits
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

-- | The empty bit vector
empty :: BitVec
empty = BitVec 0 0

-- | Concatenate two bit vectors
append :: BitVec -> BitVec -> BitVec
append (BitVec len0 int0) (BitVec len1 int1) =
  BitVec (len0 + len1) (shiftL int0 len1 + int1)

-- | Construct a BitVec from a length and Integer
bitVec :: Int -> Integer -> BitVec
bitVec = BitVec

length :: BitVec -> Int
length (BitVec len _) = len

integer :: BitVec -> Integer
integer (BitVec _ int) = int

-- | Construct from a list of bits
fromBits :: [Bool] -> BitVec
fromBits bs = BitVec len $ L.foldl' setBit 0 ones
  where
    len = L.length bs
    idxs = [len-1,len-2..0]
    ones = fmap snd $ filter fst $ zip bs idxs

-- | Return each bit in the vector, left-to-right
toBits :: BitVec -> [Bool]
toBits (BitVec len int) = testBit int <$> [len-1,len-2..0]
-- TODO: make sure out of bounds result in 0s

-- | Convert a lazy ByteString into a bit vector
fromBytes :: ByteString -> BitVec
fromBytes = fromBits . concatMap unpackBits . BS.unpack

-- | Pack the bits into a lazy ByteString
toBytes :: BitVec -> ByteString
toBytes v@(BitVec len _) = BS.pack $ fmap packBits $
                        L.chunksOf 8 $ pad ++ toBits v
  where
    padLen = (-len) `mod` 8
    pad = assert ((len + padLen) `mod` 8 == 0) $
          replicate padLen False

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

-- | Pack exactly 8 bits into a byte
packBits :: [Bool] -> Word8
packBits = L.foldl' f 0
  where f acc b = (acc `shiftL` 1) .|. fromIntegral (fromEnum b)

-- | Return the 8 bits that make a byte
unpackBits :: Word8 -> [Bool]
unpackBits w = testBit w <$> [7,6,5,4,3,2,1,0]

-- | The number of bits in the binary expansion of a positive
-- integer. For consistency with inductive definitions, leading zeros
-- are not considered and so @bitLen 0 == 0@.
bitLen :: Integer -> Int
bitLen 0 = 0
bitLen n = fromIntegral (integerLog2 n) + 1
