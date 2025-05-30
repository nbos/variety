{-# LANGUAGE ScopedTypeVariables #-}
module Codec.Arithmetic.Combinatorics
  ( rankMultisetPermutation
  , unrankMultisetPermutation
  , multinomial

  , rankPermutation
  , unrankPermutation

  , rankCombination
  , unrankCombination
  , choose

  , rankDistribution
  , unrankDistribution

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

-- | Given a multiset permutation in the form of a list, return the
-- number of times each element appears in the list (fst) (the multiset)
-- as well as the index of the given permutation in the lexicographic
-- enumeration of all permutations of that multiset (fst . snd) as well
-- as the total number of permutations in that enumeration (snd . snd)
-- i.e. the multinomial coefficient with a composition equal to the
-- multiset.
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

-- | Given a list of elements with corresponding counts and the index of
-- a permutation in the lexicographic enumeration of all permutations of
-- such a multiset, return the permutation as a list of elements.
unrankMultisetPermutation :: Ord a => [(a,Int)] -> Integer -> [a]
unrankMultisetPermutation l = go (fromIntegral total0) coef0 counts
  where
    counts = M.fromList $ filter ((> 0) . snd) l
    total0 = sum counts
    coef0 = factorial total0
            `div` product (factorial <$> counts)

    go total coef m i | M.null m = []
                      | otherwise = go total' coef'' m' i'
      where
        total' = total - 1 -- decrement `total` by 1
        coef' = coef `div` total -- rm `total` factor from num
        subCoefs = (coef' *) . fromIntegral <$> m
        (a, lowerSubCoefsSum, coef'') = findBin 0 $ M.toList subCoefs
        i' = i - lowerSubCoefsSum -- update index to local bin
        m' = M.update (\n -> if n == 1 then Nothing else Just $ n - 1)
             a m

        findBin _ [] = error "impossible"
        findBin acc ((el,subCoef):ascs) | acc' > i = (el, acc, subCoef)
                                        | otherwise = findBin acc' ascs
          where acc' = acc + subCoef

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

unrankCombination :: (Int, Int) -> Integer -> [Bool]
unrankCombination (n0,k0) = go (fromIntegral n0) (fromIntegral k0) $
                            n0 `choose` k0
  where
    go n k nCk i | i < nCk0 = False : go (n-1) k nCk0 i
                 | otherwise = True : go (n-1) (k-1) nCk1 (i-nCk0)
      where
        nCk0 = nCk - nCk1 -- sub coef if 0/False
        nCk1 = (nCk * k) `div` n -- sub coef if 1/True

choose :: Int -> Int -> Integer
choose n k = num `div` denom
  where num = factorial n
        denom = factorial k * factorial (n-k)

rankDistribution :: [Int] -> ((Int, Int), (Integer, Integer))
rankDistribution [] = ((0,0),(0,1))
rankDistribution (n0:ns) = rankCombination $
  replicate n0 False -- 0s are stars, 1s are bars
  ++ concatMap ((True:) . flip replicate False) ns

unrankDistribution :: (Int, Int) -> Integer -> [Int]
unrankDistribution (total,bins) i = go bs
  where
    n = total + bins - 1 -- 0s and 1s
    k = bins - 1 -- number of 1s
    bs = unrankCombination (n,k) i

    go l | null l = []
         | otherwise = length bin : go l'
      where (bin,l') = break id l -- break on 1

rankDistribution1 :: [Int] -> ((Int, Int), (Integer, Integer))
rankDistribution1 ns | all (>= 0) ns' = ((total+bins,bins),res)
                     | otherwise = invalid
  where
    ns' = (+(-1)) <$> ns
    ((total, bins), res) = rankDistribution ns'
    invalid = err $ "rankDistrubtion1: "
      ++ "all bins must have at least one element"

unrankDistribution1 :: (Int, Int) -> Integer -> [Int]
unrankDistribution1 (total,bins) =
  fmap (+1) . unrankDistribution (total-bins,bins)
