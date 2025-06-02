-- | Optimal codes for combinatorial objects.
--
-- The integer on which a combinatorial objects is mapped is typically
-- called its rank. Below are implementations of ranking and unranking
-- algorithms for the indexes of common combinatorial objects in the
-- lexicographic enumeration of objects of the same parameters.
module Codec.Arithmetic.Combinatorics
  ( -- * Multiset Permutations

    -- | [Multiset permutations]
    -- (https://en.wikipedia.org/wiki/Permutation#Permutations_of_multisets)
    -- are ways to order the elements of a set where elements may appear
    -- more than once. The number of such permutations is equal to the
    -- multinomial coefficient with the same parameters: \[ {n \choose
    -- k_{1}, k_{2}, \ldots, k_{m}} = \frac{n!}{k_{1}! k_{2}! \cdots
    -- k_{m}!} \] This is the most general definition in this module,
    -- of which all following objects are special cases.

    rankMultisetPermutation
  , unrankMultisetPermutation
  , multinomial

  -- * Permutations

  -- | A [permutation](https://en.wikipedia.org/wiki/Permutation) is an
  -- ordering of all the objects of a set. The number of permutations of
  -- a set of \(n\) elements is \(n!\).

  , rankPermutation
  , unrankPermutation

  -- * Combinations

  -- | A [combination](https://en.wikipedia.org/wiki/Combination) is a
  -- selection of \(k\) elements from a set of size \(n\). The number of
  -- combinations for parameters \(n\) and \(k\) is given by the
  -- binomial coefficient: \[ {n \choose k} = \frac{n!}{k! (n-k)!}  \]

  , rankCombination
  , unrankCombination
  , choose

  -- * Distributions

  -- | A distribution (usually discussed under the name [stars and
  -- bars](https://en.wikipedia.org/wiki/Stars_and_bars_(combinatorics\))
  -- is a way to distribute \(n\) equal elements (stars) among \(k\)
  -- bins (i.e. \(k-1\) bars ).

  , rankDistribution
  , unrankDistribution

  -- * Non-Empty Distributions

  -- | The class of distributions that have at least one element per
  -- bin.

  , rankDistribution1
  , unrankDistribution1
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

-- | Rank a multiset permutation. Returns the count of each element in
-- the set, the rank and the total number of permutations with those
-- counts (the multinomial coefficient).
rankMultisetPermutation :: Ord a => [a] -> ([(a,Int)], (Integer, Integer))
rankMultisetPermutation msp = ( M.toList counts
                              , (index, coef0) )
  where
    counts = L.foldl' (\m k -> M.insertWith (+) k 1 m) M.empty msp
    total0 = sum counts
    coef0 = factorial total0
            `div` product (factorial <$> counts)
    index = sum $ go (fromIntegral total0) coef0 counts msp

    go :: Ord a => Integer -> Integer -> Map a Int -> [a] -> [Integer]
    go _ _ _ [] = []
    go total coef m (a:as) = sum lowerSubCoefs :
                             go total' coef' m' as
      where
        (lt,eq,_) = M.splitLookup a m
        total' = total - 1 -- decrement `total` by 1
        lowerSubCoefs = (`div` total) . (coef *) . fromIntegral <$> lt
        n = fromJust eq
        n' = n - 1 -- decrement `a`'s count by 1
        coef' = (coef * fromIntegral n) `div` total -- rm `n` factor from denom
        m' = M.update (\_ -> if n' == 0 then Nothing else Just n')
             a m

-- | Reconstruct a multiset permutation, given the count of each element
-- in the set and a rank.
unrankMultisetPermutation :: (Show a, Ord a) => [(a,Int)] -> Integer -> [a]
unrankMultisetPermutation l = go (fromIntegral total0) coef0 counts
  where
    counts = M.fromList $ filter ((> 0) . snd) l
    total0 = sum counts
    coef0 = factorial total0
            `div` product (factorial <$> counts)

    go total coef m i | M.null m = []
                      | otherwise = a : go total' coef' m' i'
      where
        total' = total - 1 -- decrement `total` by 1
        subCoefs = (`div` total) . (coef *) . fromIntegral <$> m
        (a, lowerSubCoefsSum, coef') = findBin 0 $ M.toList subCoefs
        i' = i - lowerSubCoefsSum -- update index to local bin
        m' = M.update (\n -> if n == 1 then Nothing else Just $ n - 1)
             a m

        findBin _ [] = error "impossible"
        findBin acc ((el,subCoef):ascs) | null ascs || acc' > i = (el, acc, subCoef)
                                        | otherwise = findBin acc' ascs
          where acc' = acc + subCoef

-- | Computes the multinomial coefficient.
multinomial :: [Int] -> Integer
multinomial ns = factorial (sum ns)
                 `div` product (factorial <$> ns)

-- | Rank a permutation. Returns the rank and the total number of
-- permutations of sets with that size ( \(n!\) ).
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

-- | Reconstruct a permutation given a set of elements and a rank. The
-- order in which the elements of the set is given does not matter.
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

-- | Rank a combination in the form of a list of booleans. Returns the
-- \((n,k)\) parameters (where \(k\) is the number of `True` values and
-- \(n\) is the total), the rank and the total number of combinations
-- with those parameters (the binomial coefficient).
rankCombination :: [Bool] -> ((Int, Int), (Integer, Integer))
rankCombination c = ( (n0, k0)
                    , (res, n0Ck0) )
  where
    n0 = length c
    k0 = sum $ fromEnum <$> c
    n0Ck0 = n0 `choose` k0
    res = sum $ go (fromIntegral n0) (fromIntegral k0) n0Ck0 c

    go :: Integer -> Integer -> Integer -> [Bool] -> [Integer]
    go _ _ _ [] = []
    go n k nCk (b:bs) = if b then go (n-1) k nCk0 bs
                        else nCk0 : go (n-1) (k-1) nCk1 bs
      where
        nCk0 = nCk - nCk1 -- sub coef if 0/False
        nCk1 = (nCk * k) `div` n -- sub coef if 1/True

-- | Reconstruct a combination given parameters \((n,k)\) and a rank.
unrankCombination :: (Int, Int) -> Integer -> [Bool]
unrankCombination (n0,k0) = go (fromIntegral n0) (fromIntegral k0) $
                            n0 `choose` k0
  where
    go n k nCk i | i < nCk0 = False : go (n-1) k nCk0 i
                 | otherwise = True : go (n-1) (k-1) nCk1 (i-nCk0)
      where
        nCk0 = nCk - nCk1 -- sub coef if 0/False
        nCk1 = (nCk * k) `div` n -- sub coef if 1/True

-- | Computes the binomial coefficent given parameters \(n\) and \(k\).
choose :: Int -> Int -> Integer
choose n k | k > n = 0
           | otherwise = num `div` denom
  where num = factorial n
        denom = factorial k * factorial (n-k)

-- | Rank a distribution in the form of a list bin counts. Returns the
-- \((n,k)\) parameters (where \(n\) is the total number of elements and
-- \(k\) is the number of bins), the rank and the total number of
-- distributions with those parameters.
rankDistribution :: [Int] -> ((Int, Int), (Integer, Integer))
rankDistribution [] = ((0,0),(0,1))
rankDistribution (n0:ns)
  | n0 >= 0 && all (>= 0) ns =
    rankCombination $ replicate n0 False -- 0s are stars, 1s are bars
    ++ concatMap ((True:) . flip replicate False) ns
  | otherwise = err "rankDistrubtion1: negative bin count"

-- | Reconstruct a distribution given parameters \((n,k)\) and a rank.
unrankDistribution :: (Int, Int) -> Integer -> [Int]
unrankDistribution (total,bins) i = go bs
  where
    n = total + bins - 1 -- 0s and 1s
    k = bins - 1 -- number of 1s
    bs = unrankCombination (n,k) i

    go l | null l = []
         | otherwise = length bin : go l'
      where (bin,l') = break id l -- break on 1

-- | Rank a non-empty distribution in the form of a list bin
-- counts. Returns the \((n,k)\) parameters (where \(n\) is the total
-- number of elements and \(k\) is the number of bins), the rank and the
-- total number of distributions with those parameters.
rankDistribution1 :: [Int] -> ((Int, Int), (Integer, Integer))
rankDistribution1 ns | all (>= 0) ns' = ((total+bins,bins),res)
                     | otherwise = invalid
  where
    ns' = (+(-1)) <$> ns
    ((total, bins), res) = rankDistribution ns'
    invalid = err $ "rankDistrubtion1: "
      ++ "all bins must have at least one element"

-- | Reconstruct a distribution given parameters \((n,k)\) and a rank.
unrankDistribution1 :: (Int, Int) -> Integer -> [Int]
unrankDistribution1 (total,bins) =
  fmap (+1) . unrankDistribution (total-bins,bins)
