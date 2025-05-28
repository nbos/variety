-- | Produce optimal arithmetic encodings of values in uniform spaces
-- with arbitrary precision.
module Codec.Arithmetic.Variety.Bounded (
  module Codec.Arithmetic.Variety.Bounded) where

import Data.Bits

import Codec.Arithmetic.Variety.BitVec

err :: String -> a
err = error . ("Variety: " ++)

boundsCheck :: Integer -> Integer -> a -> a
boundsCheck i n
  | 0 <= i && i < n = id
  | otherwise = err $ "Value to encode is out of bounds: " ++ show (i,n)

-- | @'encode1' i n@ will encode a single value @i@ given its base @n@,
-- or the number of possible values @i@ can take (a.k.a. cardinality or
-- variety). Note that @i@ is an index and ranges from @[0..n-1]@, while
-- @n@ is a non-zero positive integer.
encode1 :: Integer -> Integer -> BitVec
encode1 val base = boundsCheck val base $
                   bitVec len val
  where
    maxVal = base - 1
    len = bitLen maxVal

-- | Return the code as an Integer, regardless of its base
decode1 :: BitVec -> Integer
decode1 = integer

-- | Given a precision, optimally encode a sequence of value-base pairs
-- @(i,n)@, where @i@ is the value and @n@ is the base, or the number of
-- values @i@ could take (a.k.a. cardinality or variety). Note that @i@
-- is an index and ranges from @[0..n-1]@, while @n@ is a non-zero
-- positive integer.
encode :: Int -> [(Integer,Integer)] -> BitVec
encode _ [] = empty
encode prec ((i0,n0):tl) | prec < 0 = err "Precision must be positive"
                         | otherwise = boundsCheck i0 n0 $
                           goFresh i0 n0 tl
  where
    tooBig = (>= bit (prec*8))

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
decode :: Int -> [Integer] -> BitVec -> [Integer]
decode _ [] _ = []
decode prec (n0:tl) code | prec < 0 = err "Precision must be positive"
                         | otherwise = goFresh n0 tl $ toBits code
  where
    tooBig = (>= bit (prec*8))

    -- we don't multiply if a given base is already too big
    goFresh _ [] [] = []
    goFresh _ [] bits = [decode1 $ fromBits bits]
    goFresh base (n:ns) bits
      | tooBig base =
          let len = bitLen (base - 1)
              (valBits, bits') = splitAt len bits
              valCode = fromBits valBits -- codelen == len
          in decode1 valCode : goFresh n ns bits'
      | otherwise = goAcc base (n:ns) bits

    goAcc _ [] [] = []
    goAcc _ [] bits = [decode1 $ fromBits bits]
    goAcc base (n:ns) bits
      | tooBig base' =
          let len = bitLen (base - 1)
              (valBits, bits') = splitAt len bits
              valCode = fromBits valBits -- codelen == len
          in decode1 valCode : goFresh n ns bits'
      | otherwise = goAcc base' ns bits
      where
        base' = base * n
