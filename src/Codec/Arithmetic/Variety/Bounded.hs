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
  , codeLen
  , decode
  ) where

import Data.Bits (Bits(bit))

import qualified Codec.Arithmetic.Variety as V
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

err :: String -> a
err = error . ("Variety.Bounded: " ++)

groupWithinPrec :: (a -> Integer) -> Int -> [a] -> [(Integer,[a])]
groupWithinPrec getBase prec
  | prec < 0 = err "negative precision"
  | otherwise = ffmap reverse . go 1 []
  where
    maxBase = bit (prec*8 + 1) - 1 -- max val with `prec` bytes
    go base group [] = filter (not . null . snd) [(base,group)]
    go 1 group (a:as) = go (getBase a) (a:group) as
    go base group (a:as)
      | base' > maxBase = (base,group) : go b [a] as
      | otherwise = go base' (a:group) as
      where
        b = getBase a
        base' = base * b
{-# INLINE groupWithinPrec #-}

-- | Given a max precision in bytes, encode a series of value-base pairs
-- into a single bit vector. Bases must be at least equal to @1@ and the
-- associated values must exist in the range @[0..base-1]@.
encode :: Int -> [(Integer,Integer)] -> BitVec
encode = mconcat
         . fmap (V.encode . snd)
         .: groupWithinPrec snd

-- | Return the length of the code of a sequence of values in the given
-- precision and list of bases in bits.
codeLen :: Int -> [Integer] -> Int
codeLen = fromIntegral
          . sum
          . fmap (V.codeLen1 . fst)
          .: groupWithinPrec id

-- | Try to decode a sequence of values at the head of a bit vector
-- given the same precision and list of bases that was used to encode
-- it. If successful, returns the decoded values and the remainder of
-- the `BitVec`, with the sequence's code removed. Throws an error if
-- the given vector's size doesn't match the given bases.
decode :: Int -> [Integer] -> BitVec -> [Integer]
decode = go .: groupWithinPrec id
  where
    go [] bv | not (BV.null bv) = err "decode: too many bits"
             | otherwise = []
    go ((base,bases):rest) bv
      | BV.length hd /= len = err "decode: not enough bits"
      | otherwise = V.decode bases hd ++ go rest tl
      where
        len = BV.bitLen (base - 1)
        (hd,tl) = BV.splitAt len bv

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.).(.)
infixr 8 .:
{-# INLINE (.:) #-}

ffmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
ffmap = fmap . fmap
{-# INLINE ffmap #-}
