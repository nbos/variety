module Codec.Arithmetic.Combinatorics
  ( rankPermutation
  , unrankPermutation
  ) where

import Control.Exception (assert)
import qualified Data.Set as S
import Math.Combinatorics.Exact.Factorial (factorial)

import Codec.Arithmetic.Variety (Value(fromValue), mkValue, toBitVec, decode)

err :: String -> a
err = error . ("Combinatorics: " ++)

-- | Given a permutation of `n` elements, return its encoding/index
-- (fst) in the lexicographic enumeration of all permutations of those
-- elements as well as the total number of possible permutations (snd)
-- `n!`.
rankPermutation :: Ord a => [a] -> (Integer, Integer)
rankPermutation as | length as /= n0 = err' "Not a list of unique elements"
                   | otherwise = fromValue val
  where
    err' = err . ("rankPermutation: " ++)
    s0 = S.fromList as
    n0 = S.size s0
    ns = fromIntegral <$> [n0,n0-1..1]
    is = fromIntegral <$> go s0 as
    val = assert (length is == length ns)
          mconcat $
          zipWith mkValue is ns

    -- | Lookup element index in the set of remaining elements
    go s [] = assert (S.null s) []
    go s (a:rest) = i : go s' rest
      where i = S.findIndex a s
            s' = S.delete a s

-- | Given a list of `n` unique elements and an index `i < n!`, produce
-- the permutation at index `i` of the lexicographic enumeration of all
-- permutations of those `n` elements.
unrankPermutation :: Ord a => [a] -> Integer -> [a]
unrankPermutation as i0 | length as /= n0 = err' "Not a list of unique elements"
                        | otherwise = go s0 is
  where
    err' = err . ("unrankPermutation: " ++)
    s0 = S.fromList as
    n0 = S.size s0
    ns = fromIntegral <$> [n0,n0-1..1]
    bv = toBitVec $ mkValue i0 $ factorial $ fromIntegral n0
    is = fromIntegral <$> decode bv ns

    -- | Iteratively extract elements at given indexes from a set
    go s [] = assert (S.null s) []
    go s (i:rest) = S.elemAt i s : go (S.deleteAt i s)  rest
