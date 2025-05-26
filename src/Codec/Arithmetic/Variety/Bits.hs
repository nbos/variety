-- | A 'Bool' interface to the bits of the serialization where 'True'
-- \(\mapsto\) @1@ and 'False' \(\mapsto\) @0@.
module Codec.Arithmetic.Variety.Bits (
  Precision(..),
  encode,
  decode,
  encode1,
  decode1,
  toBinary,
  fromBinary,
  ) where

import Prelude
import Data.Bits (Bits(bit))

import Numeric (readBin, showBin)

err :: String -> a
err = error . ("Variety.Bits: " ++)

-- | Serialize a non-negative integer into a string of 0s and 1s. To
-- remain consistent with recursive definitions, there are no leading
-- zeros so @0@ maps to the empty list.
toBinary :: Integer -> String
toBinary i
  | i == 0 = []
  | otherwise = showBin i ""

-- | De-serialize a string of 0s and 1s into an Integer
fromBinary :: String -> Integer
fromBinary str | null str = 0
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

boundsCheck :: Integer -> Integer -> a -> a
boundsCheck i n
  | 0 <= i && i < n = id
  | otherwise = err $ "Value to encode is out of bounds: " ++ show (i,n)

widthFromBase :: Integer -> Int
widthFromBase base = length $ toBinary maxValue
  where maxValue = base - 1

-- | @'encode1' i n@ will encode a single value @i@ given its base @n@,
-- or the number of possible values @i@ can take. Note that @i@ is an
-- index and ranges from @[0..n-1]@, while @n@ is a non-zero positive
-- integer.
encode1 :: Integer -> Integer -> String
encode1 val base = boundsCheck val base $
                   padding ++ binVal
  where
    binVal = toBinary val
    width = widthFromBase base
    padding = replicate (width - length binVal) '0'

-- | De-serialize an Integer. Does not depend on the base.
decode1 :: String -> Integer
decode1 = fromBinary

instance Ord Precision where
  compare InfinitePrecision InfinitePrecision = EQ
  compare InfinitePrecision (PrecisionBytes _) = GT
  compare (PrecisionBytes _) InfinitePrecision = LT
  compare (PrecisionBytes a) (PrecisionBytes b) = compare a b

withinPrecision :: Precision -> Integer -> Bool
withinPrecision InfinitePrecision = const True
withinPrecision (PrecisionBytes n)
  | n < 0 = err "Precision must be positive"
  -- | n > 16000000000 = err
  --   "Precision too high, use InfinitePrecision instead"
  | otherwise = (< bit (n*8))

-- | Given a precision policy, optimally encode a sequence of value-base
-- pairs @(i,n)@, where @i@ is the value and @n@ is the base, or the
-- number of values @i@ could take (a.k.a. cardinality or variety). Note
-- that @i@ is an index and ranges from @[0..n-1]@, while @n@ is a
-- length and is greater or equal to 1.
encode :: Precision -> [(Integer,Integer)] -> String
encode _ [] = []
encode prec ((i0,n0):tl) = boundsCheck i0 n0 $
                           goFresh i0 n0 tl
  where
    tooBig = not . withinPrecision prec

    -- guarantees we don't multiply if a given base is already too big
    goFresh val _ [] = toBinary val
    goFresh val base ((i,n):ins)
      | tooBig base = toBinary val ++ goFresh i n ins
      | otherwise = goAcc val base ((i,n):ins)

    goAcc val _ [] = toBinary val
    goAcc val base ((i,n):ins)
      | tooBig base' = toBinary val ++ goFresh i n ins
      | otherwise = goAcc (val*n + i) base' ins
      where
        base' = base * n


-- | Given the same precision policy and bases that were used to encode
-- a sequence of values, recover the values from the serialization.
decode :: Precision -> [Integer] -> String -> [Integer]
decode _ [] = const []
decode prec (n0:tl) = goFresh n0 tl
  where
    tooBig = not . withinPrecision prec

    goFresh _ [] [] = []
    goFresh _ [] bits = [decode1 bits]
    goFresh base (n:ns) bits
      | tooBig base =
          let (valBits, bits') = splitAt (widthFromBase base) bits
          in decode1 valBits : goFresh n ns bits'
      | otherwise = goAcc base (n:ns) bits

    goAcc _ [] [] = []
    goAcc _ [] bits = [decode1 bits]
    goAcc base (n:ns) bits
      | tooBig base' =
          let (valBits, bits') = splitAt (widthFromBase base) bits
          in decode1 valBits : goFresh n ns bits'
      | otherwise = goAcc base' ns bits
      where
        base' = base * n
