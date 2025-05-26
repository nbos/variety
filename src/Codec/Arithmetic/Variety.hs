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

err :: String -> a
err = error . ("Variety: " ++)

-- | Convert String to ByteString
bitsToBytestring :: String -> ByteString
bitsToBytestring = toLazyByteString . go 0 0
  where
    go :: Word8 -> Int -> String -> Builder
    go byte 8 bits = word8 byte <> go 0 0 bits
    go byte _ [] = word8 byte
    go byte i (b:bits) = go byte' (i + 1) bits
      where
        byte' = byte .|. (bit `shiftL` i)
        bit = case b of '0' -> 0; '1' -> 1
                        _ -> err $ "Non-binary char encountered: " ++ [b]
-- TODO: compare against using readBin on 8-bit chunks

-- | Convert ByteString to String
bytestringToBits :: ByteString -> String
bytestringToBits = concatMap byteToBits . BS.unpack
  where
    byteToBits :: Word8 -> String
    byteToBits byte = map (bool2char . testBit byte) [0..7]
    bool2char b = if b then '1' else '0'
-- TODO: compare against using writeBin

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
encode = bitsToBytestring .: Bits.encode

-- | Given a base, recover a single value from a serialization.
decode1 :: ByteString -> Integer
decode1 = Bits.decode1 . bytestringToBits

-- | Given the same precision policy and bases that were used to encode
-- a sequence of values, recover the values from the serialization.
decode :: Precision -> [Integer] -> ByteString -> [Integer]
decode prec bases = Bits.decode prec bases . bytestringToBits

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.).(.)
infixr 8 .:
