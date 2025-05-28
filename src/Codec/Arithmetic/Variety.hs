{-# LANGUAGE BangPatterns, InstanceSigs #-}
-- | Produce optimal arithmetic encodings of values in uniform spaces
-- with infinite precision.
module Codec.Arithmetic.Variety
  ( encode
  , decode
  , encode1
  , decode1
  -- * Value Interface
  , Value
  , mkValue
  , fromValue
  , toBitVec
  , compose
  , maxDigit
  , codeLen
  ) where

import Codec.Arithmetic.Variety.BitVec (BitVec, bitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

err :: String -> a
err = error . ("Variety: " ++)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 8 .:

-- | Encode a series of digit-base pairs into a single bit vector.
encode :: [(Integer,Integer)] -> BitVec
encode = toBitVec . mconcat . fmap (uncurry mkValue)

-- | Decode a bit vector given the same series of bases that was used to
-- encode it. Throws an error if the vector is too large or too small
-- for the given bases.
decode :: BitVec -> [Integer] -> [Integer]
decode bv bases = case init $ scanr (*) 1 bases of -- last is 1
  [] -> error "impossible"
  (base:ns) -- base == product bases
    | len == expectedLen -> go (BV.integer bv) ns
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
    go i2 (n1:ns) = let (i0,i1) = quotRem i2 n1
                    in i0 : go i1 ns

-- | Consider a positive integer as a bit vector, given its base. The
-- base is only required to determine the number of leading 0s.
encode1 :: Integer -> Integer -> BitVec
encode1 = toBitVec .: mkValue

-- | Recover the digit from a bit vector.
decode1 :: BitVec -> Integer
decode1 = BV.integer

---------------------
-- VALUE INTERFACE --
---------------------

-- | A digit with its base, or the number of values the digit can take
-- (a.k.a. radix). The digit (fst) is like an index and ranges from
-- [0..base-1] while the base (snd) is a cardinality is always positive
-- and non-zero.
newtype Value = Value {
  -- | Recover the digit (fst) and the base (snd) from the value
  fromValue :: (Integer, Integer)
}

-- | Construct a value from a digit and a base (radix). Throws an error
-- if either is negative or if the digit is not strictly less than the
-- base.
mkValue :: Integer -> Integer -> Value
mkValue i n | 0 <= i && i < n = Value (i,n)
            | otherwise = err $ "Digit is out of bounds: " ++ show (i,n)

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

-- | Maximal possible digit in a given base.
maxDigit :: Value -> Integer
maxDigit = (+(-1)) . snd . fromValue

-- | Length of the binary expansion of any value with this base.
codeLen :: Value -> Int
codeLen = BV.bitLen . maxDigit

-- | Drop the base and consider the digit as a bit vector. The base
-- effectively rounds to the next power of 2.
toBitVec :: Value -> BitVec
toBitVec v@(Value (i,_)) = bitVec len i
  where len = codeLen v
