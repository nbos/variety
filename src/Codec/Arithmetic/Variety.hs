{-# LANGUAGE BangPatterns, InstanceSigs #-}
-- | The optimal (shortest) binary code of a value in a domain of
-- uniform probability is simply the binary expansion of the index of
-- the value in that space. The optimal code of two such values is the
-- index of the pair in the cartesian product of both domains, and so on
-- for any number of values. This package defines a type `Value` with a
-- `Monoid` instance that performs this sort of composition. The only
-- difference with typical [arithmetic
-- coding](https://en.wikipedia.org/wiki/Arithmetic_coding) on a
-- rational number code is that for each operation, we operate on the
-- whole code with infinite precision. For a codec with finite
-- precision, see the
-- [Variety.Bounded](https://hackage-content.haskell.org/package/variety/docs/Codec-Arithmetic-Variety-Bounded.html)
-- module.
module Codec.Arithmetic.Variety
  ( -- * Value-base Interface

    encode
  , codeLen
  , decode
  , encode1
  , codeLen1
  , decode1

  -- * Value Type

  , Value(..)
  , mkValue
  , toBitVec
  , compose
  , maxValue
  ) where

import Codec.Arithmetic.Variety.BitVec (BitVec, bitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

err :: String -> a
err = error . ("Variety." ++)

-- | Encode a series of value-base pairs into a single bit vector. A
-- base must be at least equal to @1@ and the associated value must
-- exist in the range @[0..base-1]@.
encode :: [(Integer,Integer)] -> BitVec
encode = toBitVec . mconcat . fmap (uncurry mkValue)

-- | Return the length of the code of a sequence of values in the given
-- list of bases in bits.
codeLen :: [Integer] -> Int
codeLen = codeLen1 . product

-- | Try to decode the head of a bit vector given the same series of
-- bases that was used to encode. If successful, returns the decoded
-- values and the remainder of the `BitVec` with the values' code
-- removed. Returns @Nothing@ if the bit vector doesn't contain enough
-- bits to specify values for each given base.
decode :: [Integer] -> BitVec -> Maybe ([Integer], BitVec)
decode bases bv = case init $ scanr (*) 1 bases of -- last is 1
  [] -> Just ([], bv)
  (base:ns) | BV.length bv0 < len -> Nothing
            | otherwise -> Just ( go (BV.toInteger bv0) ns
                                , bv1 )
    where
      len = codeLen1 base -- base == product bases
      (bv0,bv1) = BV.splitAt len bv
  where
    go i [] = [i]
    go i2 (n1:ns) = i0 : go i1 ns
      where (i0,i1) = quotRem i2 n1

-- | Consider a positive integer as a bit vector, given its base. The
-- base is only required to determine the number of leading 0s.
encode1 :: Integer -> Integer -> BitVec
encode1 = toBitVec .: mkValue

-- | Return the length of the code of a single value in the given base
-- in bits.
codeLen1 :: Integer -> Int
codeLen1 n | n < 1 = err "codeLen: base must be positive and non-zero"
           | otherwise = BV.bitLen $ n - 1

-- | Recover the value from a bit vector.
decode1 :: Integer -> BitVec -> Maybe (Integer, BitVec)
decode1 base bv | BV.length bv0 < len = Nothing
                | otherwise = Just (BV.toInteger bv0, bv1)
  where
    len = codeLen1 base
    (bv0,bv1) = BV.splitAt len bv

----------------
-- VALUE TYPE --
----------------

-- | A value with its base, or the number of possible values that could
-- be (i.e. radix, or
-- [variety](https://en.wikipedia.org/wiki/Variety_(cybernetics\))). The
-- value is like an index and ranges from [0..base-1] while the base is
-- a cardinality is always positive and non-zero.
newtype Value = Value {
  -- | Recover the value and the base as @Integer@s
  fromValue :: (Integer, Integer)
} deriving (Eq,Show,Read)

-- | Construct from a value and a base. Throws an error if either is
-- negative or if the value is not strictly less than the base.
mkValue :: Integer -> Integer -> Value
mkValue i n | 0 <= i && i < n = Value (i,n)
            | otherwise = err $ "mkValue: out of bounds: " ++ show (i,n)

instance Semigroup Value where
  (<>) :: Value -> Value -> Value
  (<>) = compose

instance Monoid Value where
  mempty :: Value
  mempty = Value (0,1)

-- | Compose two values into a value of a greater base. Associative, not
-- commutative.
compose :: Value -> Value -> Value
compose (Value (i0,n0)) (Value (i1,n1)) = Value (i2, n2)
  where
    !i2 = i0 * n1 + i1
    !n2 = n0 * n1

-- | Maximal possible value as an @Integer@ in the given base.
maxValue :: Value -> Integer
maxValue = (+(-1)) . snd . fromValue

-- | Drop the base and consider the value as a bit vector. Conceptually,
-- the base rounds to the next power of 2.
toBitVec :: Value -> BitVec
toBitVec (Value (i,n)) = bitVec (codeLen1 n) i

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 8 .:
{-# INLINE (.:) #-}
