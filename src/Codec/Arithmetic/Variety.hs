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
-- whole code with infinite precision. For an codec with finite
-- precision, see the @Variety.Bounded@ module.
module Codec.Arithmetic.Variety
  ( -- * Codec

    encode
  , decode
  , encode1
  , decode1

  -- * Value Interface

  , Value(..)
  , mkValue
  , toBitVec
  , compose
  , maxValue
  , codeLen
  ) where

import Codec.Arithmetic.Variety.BitVec (BitVec, bitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

err :: String -> a
err = error . ("Variety: " ++)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 8 .:

-- | Encode a series of value-base pairs into a single bit vector. Bases
-- must be at least equal to @1@ and the associated values must exist in
-- the range @[0..base-1]@.
encode :: [(Integer,Integer)] -> BitVec
encode = toBitVec . mconcat . fmap (uncurry mkValue)

-- | Decode a bit vector given the same series of bases that was used to
-- encode it. Throws an error if the given vector's size doesn't match
-- the given bases.
decode :: [Integer] -> BitVec -> [Integer]
decode bases bv = case init $ scanr (*) 1 bases of -- last is 1
  [] -> []
  (base:ns) -- base == product bases
    | len == expectedLen -> go (BV.toInteger bv) ns
    | otherwise ->
        err $ "Bit vector incompatible with provided list of bases: "
        ++ show (bv, bases) ++ "\nVector has " ++ show len
        ++ " bits while the bases make for a " ++ show expectedLen
        ++ " bit message."
    where
      len = BV.length bv
      expectedLen = BV.bitLen (base - 1)

  where
    go i [] = [i]
    go i2 (n1:ns) = i0 : go i1 ns
      where (i0,i1) = quotRem i2 n1

-- | Consider a positive integer as a bit vector, given its base. The
-- base is only required to determine the number of leading 0s.
encode1 :: Integer -> Integer -> BitVec
encode1 = toBitVec .: mkValue

-- | Recover the value from a bit vector.
decode1 :: BitVec -> Integer
decode1 = BV.toInteger

---------------------
-- VALUE INTERFACE --
---------------------

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
            | otherwise = err $ "Value is out of bounds: " ++ show (i,n)

instance Semigroup Value where
  (<>) :: Value -> Value -> Value
  (<>) = compose

instance Monoid Value where
  mempty :: Value
  mempty = Value (0,1)

-- | Compose two values into a value of a greater base. This is
-- associative, but not commutative.
compose :: Value -> Value -> Value
compose (Value (i0,n0)) (Value (i1,n1)) = Value (i2, n2)
  where
    !i2 = i0 * n1 + i1
    !n2 = n0 * n1

-- | Maximal possible value as an @Integer@ in the given base.
maxValue :: Value -> Integer
maxValue = (+(-1)) . snd . fromValue

-- | Length of the binary expansion of any value with this base.
codeLen :: Value -> Int
codeLen = BV.bitLen . maxValue

-- | Drop the base and consider the value as a bit vector. The base
-- conceptually rounds to the next power of 2.
toBitVec :: Value -> BitVec
toBitVec v@(Value (i,_)) = bitVec (codeLen v) i
