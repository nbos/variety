-- | Since the arithmetic operations of composition might get
-- computationally expensive on very large codes, a similar interface is
-- provided here which produces and consumes bits in chunks whenever
-- spaces are about to grow beyond a certain size given in bytes, at the
-- cost of at most one bit per chunk.
--
-- While the Haskell language standard defines `Integer` as having no
-- upper bound, GHC most commonly uses the GNU Multiple Precision
-- Arithmetic Library (GMP) as a backend for it, which incurs a limit of
-- 16GiB (or a little over 17GB) on the size of `Integer` values.
module Codec.Arithmetic.Variety.Bounded
  ( encode
  , decode
  ) where

import Data.Bits (Bits(bit))

import Codec.Arithmetic.Variety (Value(fromValue), mkValue, toBitVec, decode1)
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

err :: String -> a
err = error . ("Variety.Bounded: " ++)

-- | Given a max precision in bytes, encode a series of value-base pairs
-- into a single bit vector. Bases must be at least equal to @1@ and the
-- associated values must exist in the range @[0..base-1]@.
encode :: Int -> [(Integer,Integer)] -> BitVec
encode prec ins | prec < 0 = err "Precision must be positive"
                | otherwise = case vals of
                                [] -> BV.empty
                                (hd:tl) -> goFresh hd tl
  where
    vals = uncurry mkValue <$> ins
    tooBig v | (_,base) <- fromValue v = base >= bit (prec*8)

    -- we don't multiply if a given base is already too big
    goFresh acc [] = toBitVec acc
    goFresh acc (v:vs)
      | tooBig acc = toBitVec acc <> goFresh v vs
      | otherwise = goAcc acc (v:vs)

    goAcc acc [] = toBitVec acc
    goAcc acc (v:vs)
      | tooBig acc' = toBitVec acc <> goFresh v vs
      | otherwise = goAcc acc' vs
      where
        acc' = acc <> v

-- | Decode a bit vector given the same precision and series of bases
-- that was used to encode it. Throws an error if the given vector's
-- size doesn't match the given parameters.
decode :: Int -> [Integer] -> BitVec -> [Integer]
decode prec | prec < 0 = err "Precision must be positive"
            | otherwise = goFresh 1
  where
    tooBig = (>= bit (prec*8))

    -- we don't multiply if a given base is already too big
    goFresh _ [] bv | BV.null bv = []
                    | otherwise = err $ "Bits left over after decoding: "
                                  ++ show bv
    goFresh acc (n:ns) bv
      | tooBig acc =
          let len = BV.bitLen (acc - 1)
              (valBits, bv') = BV.splitAt len bv
          in decode1 valBits : goFresh n ns bv'
      | otherwise = goAcc acc (n:ns) bv

    goAcc _ [] bv | BV.null bv = []
                  | otherwise = err $ "Bits left over after decoding: "
                                ++ show bv
    goAcc acc (n:ns) bv
      | tooBig acc' =
          let len = BV.bitLen (acc - 1)
              (valBits, bv') = BV.splitAt len bv
          in decode1 valBits : goFresh n ns bv'
      | otherwise = goAcc acc' ns bv
      where
        acc' = acc * n
