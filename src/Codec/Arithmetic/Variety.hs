-- | Produce optimal arithmetic encodings of values in uniform spaces
-- with arbitrary precision.
module Codec.Arithmetic.Variety (
  Code(..),
  getBytes,
  fromBits,
  fromBytes,
  Precision(..),
  encode,
  decode,
  encode1,
  decode1,
  ) where

import Prelude
import Numeric (readBin, showBin)
import Data.Word (Word8)
import Data.Bits ((.|.), shiftL, testBit, Bits(bit))

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Builder (toLazyByteString, word8, Builder)

-- | A binary code
newtype Code = Code { getBits :: String }

-- | Pack the code into a lazy ByteString
getBytes :: Code -> ByteString
getBytes (Code bits) = bits2Bytes bits

-- | Read the code from a list of '0' and '1' chars
fromBits :: String -> Code
fromBits = Code

-- | Read the code from a lazy ByteString
fromBytes :: ByteString -> Code
fromBytes = Code . bytes2Bits

err :: String -> a
err = error . ("Variety: " ++)

-- | Serialize a non-negative integer into a string of 0s and 1s. To
-- remain consistent with recursive definitions, there are no leading
-- zeros so @0@ maps to the empty list.
toBinary :: Integer -> Code
toBinary i
  | i == 0 = Code []
  | otherwise = Code $ showBin i ""

-- | De-serialize a string of 0s and 1s into an Integer
fromBinary :: Code -> Integer
fromBinary (Code str) | null str = 0
                      | (i,[]):_ <- readBin str = i
                      | otherwise = err "fromBinary: Parsing error"

-- | Precision policy for encoding/decoding sequences of values. The
-- same 'Precision' that was used for encoding must be given to 'decode'
-- for a successful de-serialization.
data Precision = InfinitePrecision -- ^ Always produce minimal codes
               -- with no regard to speed or memory
               | PrecisionBytes !Int -- ^ Limit the working 'Integer'
               -- base to 'Int' bytes of memory at the cost of a
               -- slightly longer code
  deriving (Show,Eq)

instance Ord Precision where
  compare InfinitePrecision InfinitePrecision = EQ
  compare InfinitePrecision (PrecisionBytes _) = GT
  compare (PrecisionBytes _) InfinitePrecision = LT
  compare (PrecisionBytes a) (PrecisionBytes b) = compare a b

boundsCheck :: Integer -> Integer -> a -> a
boundsCheck i n
  | 0 <= i && i < n = id
  | otherwise = err $ "Value to encode is out of bounds: " ++ show (i,n)

widthFromBase :: Integer -> Int
widthFromBase base = length $ getBits $ toBinary maxValue
  where maxValue = base - 1
-- TODO: compare against using iLog2Ceiling

-- | @'encode1' i n@ will encode a single value @i@ given its base @n@,
-- or the number of possible values @i@ can take (a.k.a. cardinality or
-- variety). Note that @i@ is an index and ranges from @[0..n-1]@, while
-- @n@ is a non-zero positive integer.
encode1 :: Integer -> Integer -> String
encode1 val base = boundsCheck val base $
                   padding ++ valBits
  where
    valBits = getBits $ toBinary val
    width = widthFromBase base
    padding = replicate (width - length valBits) '0'

-- | De-serialize an Integer. Does not depend on the base.
decode1 :: Code -> Integer
decode1 = fromBinary

withinPrecision :: Precision -> Integer -> Bool
withinPrecision InfinitePrecision = const True
withinPrecision (PrecisionBytes n)
  | n < 0 = err "Precision must be positive"
  -- | n > 16000000000 = err
  --   "Precision too high, use InfinitePrecision instead"
  | otherwise = (< bit (n*8))

-- | Given a precision, optimally encode a sequence of value-base pairs
-- @(i,n)@, where @i@ is the value and @n@ is the base, or the number of
-- values @i@ could take (a.k.a. cardinality or variety). Note that @i@
-- is an index and ranges from @[0..n-1]@, while @n@ is a non-zero
-- positive integer.
encode :: Precision -> [(Integer,Integer)] -> Code
encode _ [] = Code []
encode prec ((i0,n0):tl) = boundsCheck i0 n0 $
                           Code (goFresh i0 n0 tl)
  where
    tooBig = not . withinPrecision prec

    -- we don't multiply if a given base is already too big
    goFresh val _ [] = getBits $ toBinary val
    goFresh val base ((i,n):ins)
      | tooBig base = getBits (toBinary val) ++ goFresh i n ins
      | otherwise = goAcc val base ((i,n):ins)

    goAcc val _ [] = getBits $ toBinary val
    goAcc val base ((i,n):ins)
      | tooBig base' = getBits (toBinary val) ++ goFresh i n ins
      | otherwise = goAcc (val*n + i) base' ins
      where
        base' = base * n

-- | Given the same precision policy and bases that were used to encode
-- a sequence of values, recover the values from the serialization.
decode :: Precision -> [Integer] -> Code -> [Integer]
decode _ [] _ = []
decode prec (n0:tl) (Code str) = goFresh n0 tl str
  where
    tooBig = not . withinPrecision prec

    goFresh _ [] [] = []
    goFresh _ [] bits = [decode1 $ Code bits]
    goFresh base (n:ns) bits
      | tooBig base =
          let (valBits, bits') = splitAt (widthFromBase base) bits
          in decode1 (Code valBits) : goFresh n ns bits'
      | otherwise = goAcc base (n:ns) bits

    goAcc _ [] [] = []
    goAcc _ [] bits = [decode1 $ Code bits]
    goAcc base (n:ns) bits
      | tooBig base' =
          let (valBits, bits') = splitAt (widthFromBase base) bits
          in decode1 (Code valBits) : goFresh n ns bits'
      | otherwise = goAcc base' ns bits
      where
        base' = base * n

-- | Convert String to ByteString
bits2Bytes :: String -> ByteString
bits2Bytes = toLazyByteString . go 0 0
  where
    go :: Word8 -> Int -> String -> Builder
    go byte 8 bits = word8 byte <> go 0 0 bits
    go byte _ [] = word8 byte
    go byte i (c:bits) = go byte' (i + 1) bits
      where
        byte' = byte .|. (b `shiftL` i)
        b = case c of '0' -> 0; '1' -> 1
                      _ -> err $ "Non-binary char encountered: " ++ [c]
-- TODO: compare against using readBin on 8-bit chunks

-- | Convert ByteString to String
bytes2Bits :: ByteString -> String
bytes2Bits = concatMap byteToBits . BS.unpack
  where
    byteToBits :: Word8 -> String
    byteToBits byte = map (bool2char . testBit byte) [0..7]
    bool2char b = if b then '1' else '0'
-- TODO: compare against using writeBin
