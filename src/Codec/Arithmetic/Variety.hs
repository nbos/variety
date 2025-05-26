-- | Produce optimal arithmetic encodings of values in uniform spaces
-- with arbitrary precision.
module Codec.Arithmetic.Variety (
  Code(..),
  fromBits,
  toBits,
  fromBytes,
  toBytes,
  fromString,
  toString,
  fromNat,
  toNat,
  Precision(..),
  encode,
  decode,
  encode1,
  decode1,
  ) where

import GHC.Num (integerLog2)
import Data.Bits
import Data.Word (Word8)
import qualified Data.List as L
import qualified Data.List.Extra as L
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

err :: String -> a
err = error . ("Variety: " ++)

----------
-- BITS --
----------

packBits :: [Bool] -> Word8
packBits = L.foldl' (\acc b -> (acc `shiftL` 1) .|. fromIntegral (fromEnum b)) 0

unpackBits :: Word8 -> [Bool]
unpackBits w = testBit w <$> [7,6,5,4,3,2,1,0]

---------------
-- INTERFACE --
---------------

-- | A binary code
data Code = Code {
  codeLen :: !Int,
  natural :: !Integer
} deriving (Show,Read)

instance Semigroup Code where
  (<>) = append

instance Monoid Code where
  mempty = empty

append :: Code -> Code -> Code
append (Code len0 nat0) (Code len1 nat1) =
  Code (len0 + len1) (shiftL nat0 len1 + nat1)

empty :: Code
empty = Code 0 0

fromBits :: [Bool] -> Code
fromBits bs = Code len $ L.foldl' setBit 0 ones
  where
    len = length bs
    idxs = [len-1,len-2..0]
    ones = fmap snd $ filter fst $ zip bs idxs

toBits :: Code -> [Bool]
toBits (Code len nat) = testBit nat <$> [len-1,len-2..0]
-- TODO: make sure out of bounds result in 0s

-- | Read the code from a lazy ByteString
fromBytes :: ByteString -> Code
fromBytes = fromBits . concatMap unpackBits . BS.unpack

-- | Pack the code into a lazy ByteString
toBytes :: Code -> ByteString
toBytes c@(Code len _) = BS.pack $ fmap packBits $
                         L.chunksOf 8 $ pad ++ toBits c
  where
    padLen = len `mod` 8
    pad = replicate padLen False

-- | Read the code from a list of '0' and '1' chars
fromString :: String -> Code
fromString = fromBits . fmap f
  where f '0' = False
        f '1' = True
        f c = err $ "Non-binary char encountered: " ++ [c]

toString :: Code -> String
toString = fmap f . toBits
  where f b = if b then '1' else '0'

log2 :: Integer -> Int
log2 = fromIntegral . integerLog2

fromNat :: Integer -> Code
fromNat nat = Code (log2 nat) nat

-- | Numeric representation of the code
toNat :: Code -> Integer
toNat = natural

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

-- | @'encode1' i n@ will encode a single value @i@ given its base @n@,
-- or the number of possible values @i@ can take (a.k.a. cardinality or
-- variety). Note that @i@ is an index and ranges from @[0..n-1]@, while
-- @n@ is a non-zero positive integer.
encode1 :: Integer -> Integer -> Code
encode1 val base = boundsCheck val base $
                   Code len val
  where
    maxVal = base - 1
    len = log2 maxVal

-- | Return the code as an Integer, regardless of its base
decode1 :: Code -> Integer
decode1 = toNat

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
encode _ [] = empty
encode prec ((i0,n0):tl) = boundsCheck i0 n0 $
                           goFresh i0 n0 tl
  where
    tooBig = not . withinPrecision prec

    -- we don't multiply if a given base is already too big
    goFresh val base [] = encode1 val base
    goFresh val base ((i,n):ins)
      | tooBig base = encode1 val base <> goFresh i n ins
      | otherwise = goAcc val base ((i,n):ins)

    goAcc val base [] = encode1 val base
    goAcc val base ((i,n):ins)
      | tooBig base' = encode1 val base <> goFresh i n ins
      | otherwise = goAcc (val*n + i) base' ins
      where
        base' = base * n

-- | Given the same precision policy and bases that were used to encode
-- a sequence of values, recover the values from the serialization.
decode :: Precision -> [Integer] -> Code -> [Integer]
decode _ [] _ = []
decode prec (n0:tl) code = goFresh n0 tl $ toBits code
  where
    tooBig = not . withinPrecision prec

    -- we don't multiply if a given base is already too big
    goFresh _ [] [] = []
    goFresh _ [] bits = [decode1 $ fromBits bits]
    goFresh base (n:ns) bits
      | tooBig base =
          let len = log2 (base - 1)
              (valBits, bits') = splitAt len bits
              valCode = fromBits valBits -- codelen == len
          in decode1 valCode : goFresh n ns bits'
      | otherwise = goAcc base (n:ns) bits

    goAcc _ [] [] = []
    goAcc _ [] bits = [decode1 $ fromBits bits]
    goAcc base (n:ns) bits
      | tooBig base' =
          let len = log2 (base - 1)
              (valBits, bits') = splitAt len bits
              valCode = fromBits valBits -- codelen == len
          in decode1 valCode : goFresh n ns bits'
      | otherwise = goAcc base' ns bits
      where
        base' = base * n
