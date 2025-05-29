{-# LANGUAGE ScopedTypeVariables #-}
module Codec.Arithmetic.Combinatorics
  ( rankPermutation
  , unrankPermutation
  , rankMultisetPermutation
  , multinomial
  ) where

import Control.Exception (assert)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Math.Combinatorics.Exact.Factorial (factorial)

import Codec.Arithmetic.Variety (Value(fromValue), mkValue, toBitVec, decode)

err :: String -> a
err = error . ("Combinatorics: " ++)

-- | Given a multiset permutation in the form of a list of indexes,
-- return the number of times each index appears for @[0..maxIndex]@
-- (fst) (the multiset) as well as the index of the given permutation in
-- the lexicographic enumeration of all permutations of that multiset
-- (fst . snd) with the total number of permutations in that enumeration
-- (snd . snd) i.e. the multinomial coefficient with a composition equal
-- to the multiset.
rankMultisetPermutation :: forall a. Ord a => [a] ->
                           ([(a,Int)], (Integer, Integer))
rankMultisetPermutation msp = ( M.toList counts
                              , (index, coef0) )
  where
    counts = L.foldl' (\m k -> M.insertWith (+) k 1 m) M.empty msp
    total0 = sum counts
    coef0 = factorial total0
            `div` product (factorial <$> counts)
    index = sum $ go (fromIntegral total0) coef0 counts msp

    go :: Integer -> Integer -> Map a Int -> [a] -> [Integer]
    go _ _ _ [] = []
    go total coef m (a:as) = sum lowerSubCoefs :
                             go total' coef'' m' as
      where
        (lt,eq,_) = M.splitLookup a m
        total' = total - 1 -- decrement `total` by 1
        coef' = coef `div` total -- rm `total` factor from num
        lowerSubCoefs = (coef' *) . fromIntegral <$> lt
        n = fromJust eq
        n' = n - 1 -- decrement `a`'s count by 1
        coef'' = coef' * fromIntegral n -- rm `n` factor from denom
        m' = M.update (\_ -> if n' == 0 then Nothing else Just n')
             a m

multinomial :: [Int] -> Integer
multinomial ns = factorial (sum ns)
                 `div` product (factorial <$> ns)

-- | Given a permutation of `n` elements, return its encoding/index
-- (fst) in the lexicographic enumeration of all permutations of those
-- elements as well as the total number of possible permutations (snd)
-- `n!`. This is the same as ranking the multiset permutation where each
-- element only appears once.
rankPermutation :: Ord a => [a] -> (Integer, Integer)
rankPermutation p | length p /= n0 = err' "Not a list of unique elements"
                  | otherwise = fromValue val
  where
    err' = err . ("rankPermutation: " ++)
    s0 = S.fromList p
    n0 = S.size s0
    ns = fromIntegral <$> [n0,n0-1..1]
    is = fromIntegral <$> go s0 p
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
-- permutations of those `n` elements. This is the same as unranking the
-- multiset permutation where each element only appears once.
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
