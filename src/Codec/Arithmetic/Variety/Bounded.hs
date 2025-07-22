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
import Data.Bifunctor (Bifunctor(first))

import qualified Codec.Arithmetic.Variety as Var
import Codec.Arithmetic.Variety.BitVec (BitVec)

err :: String -> a
err = error . ("Variety.Bounded: " ++)

-- | Given a base getter function and precision, chunk the values into
-- lists that do not exceed the given precision or are singleton lists,
-- annotated with the total base for each chunk.
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
         . fmap (Var.encode . snd)
         .: groupWithinPrec snd

-- | Return the length of the code of a sequence of values in the given
-- precision and list of bases in bits.
codeLen :: Int -> [Integer] -> Int
codeLen = fromIntegral
          . sum
          . fmap (Var.codeLen1 . fst)
          .: groupWithinPrec id

-- | Try to decode the head of a bit vector given the same precision and
-- series of bases that was used to encode. If successful, returns the
-- decoded values and the remainder of the `BitVec` with the values'
-- code removed. Returns @Nothing@ if the bit vector doesn't contain
-- enough bits to specify values for each given base.
decode :: Int -> [Integer] -> BitVec -> Maybe ([Integer], BitVec)
decode = go .: groupWithinPrec id
  where
    go [] bv = Just ([], bv)
    go ((_,bases):rest) bv = do
      (vals, bv') <- Var.decode bases bv
      first (vals ++) <$> go rest bv'

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.).(.)
infixr 8 .:
{-# INLINE (.:) #-}

ffmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
ffmap = fmap . fmap
{-# INLINE ffmap #-}
