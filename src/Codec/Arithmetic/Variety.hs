-- | Produce optimal arithmetic encodings of values in uniform spaces
-- with arbitrary precision.
module Codec.Arithmetic.Variety (
  Precision(..),
  encode,
  decode,
  encode1,
  decode1,
  ) where

import Data.Word (Word8)
import Data.Bits ((.|.), shiftL, testBit)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Builder (toLazyByteString, word8, Builder)

import Codec.Arithmetic.Variety.Bits (Precision(..))
import qualified Codec.Arithmetic.Variety.Bits as Bits

-- | Convert [Bool] to ByteString
bitsToBytestring :: [Bool] -> ByteString
bitsToBytestring = toLazyByteString . go 0 0
  where
    go :: Word8 -> Int -> [Bool] -> Builder
    go byte 8 bits = word8 byte <> go 0 0 bits
    go byte _ [] = word8 byte
    go byte i (b:bits) = go byte' (i + 1) bits
      where byte' = byte .|. (fromIntegral (fromEnum b) `shiftL` i)

-- | Convert ByteString to [Bool]
bytestringToBits :: ByteString -> [Bool]
bytestringToBits = concatMap byteToBits . BS.unpack
  where
    byteToBits :: Word8 -> [Bool]
    byteToBits byte = map (testBit byte) [0..7]

-- | @'encode1' i n@ will encode a single value @i@ given its base @n@,
-- or the number of possible values @i@ can take. Note that @i@ is an
-- index and ranges from @[0..n-1]@, while @n@ is a length and is
-- greater or equal to 1.
encode1 :: Integer -> Integer -> ByteString
encode1 = bitsToBytestring .: Bits.encode1

-- | Given a precision policy, optimally encode a sequence of value-base
-- pairs @(i,n)@, where @i@ is the value and @n@ is the base, or the
-- number of values @i@ could take. Note that @i@ is an index and ranges
-- from @[0..n-1]@, while @n@ is a length and is greater or equal to 1.
encode :: Precision -> [(Integer, Integer)] -> ByteString
encode  = bitsToBytestring .: Bits.encode

-- | Given a base, recover a single value from a serialization.
decode1 :: Integer -> ByteString -> Integer
decode1 n = Bits.decode1 n . bytestringToBits

-- | Given the same precision policy and bases that were used to encode
-- a sequence of values, recover the values from the serialization.
decode :: Precision -> [Integer] -> ByteString -> [Integer]
decode prec bases = Bits.decode prec bases . bytestringToBits

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.).(.)
infixr 8 .:
