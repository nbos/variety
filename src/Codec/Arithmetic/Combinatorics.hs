{-# LANGUAGE BangPatterns #-}
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
    -- k_{m}!} ~~~~~\mathrm{where}~~~~~ n = \sum_i k_i \]

    encodeMultisetPermutation
  , decodeMultisetPermutation
  , rankMultisetPermutation
  , unrankMultisetPermutation
  , multinomial

  -- * Permutations

  -- | A [permutation](https://en.wikipedia.org/wiki/Permutation) is an
  -- ordering of the objects of a set of distinct elements. The number
  -- of permutations of a set of \(n\) elements is \(n!\).

  , encodePermutation
  , decodePermutation
  , rankPermutation
  , unrankPermutation
  , factorial

  -- * Combinations

  -- | A [combination](https://en.wikipedia.org/wiki/Combination) is a
  -- selection of \(k\) elements from a set of size \(n\). The number of
  -- combinations for parameters \(n\) and \(k\) is given by the
  -- binomial coefficient: \[ {n \choose k} = \frac{n!}{k! (n-k)!}  \]

  , encodeCombination
  , decodeCombination
  , rankCombination
  , unrankCombination
  , choose

  -- * Distributions

  -- | A distribution (usually discussed under the name [stars and
  -- bars](https://en.wikipedia.org/wiki/Stars_and_bars_(combinatorics\)))
  -- is a way to distribute \(n\) equal elements (stars) among \(k\)
  -- bins (i.e. \(k-1\) bars ).

  , encodeDistribution
  , decodeDistribution
  , rankDistribution
  , unrankDistribution
  , countDistributions

  -- * Non-Empty Distributions

  -- | The class of distributions that have at least one element per
  -- bin.

  , encodeDistribution1
  , decodeDistribution1
  , rankDistribution1
  , unrankDistribution1
  , countDistributions1
  ) where

import Control.Exception (assert)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Math.Combinatorics.Exact.Factorial as E (factorial)

import qualified Codec.Arithmetic.Variety as Var
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

err :: String -> a
err = error . ("Combinatorics." ++)

--------------------------
-- MULTISET PERMUTATION --
--------------------------

encodeMultisetPermutation :: Ord a => [a] -> ([(a,Int)], BitVec)
encodeMultisetPermutation = fmap (uncurry Var.encode1)
                            . rankMultisetPermutation

decodeMultisetPermutation :: Ord a => [(a,Int)] -> BitVec -> Maybe ([a], BitVec)
decodeMultisetPermutation aks bv | BV.length bv0 < len = Nothing
                                 | otherwise = Just (msp, bv1)
  where
    base = multinomial $ snd <$> aks
    len = Var.codeLen1 base
    (bv0,bv1) = BV.splitAt len bv
    msp = unrankMultisetPermutation aks $ BV.toInteger bv0

-- | Rank a multiset permutation. Returns the count of each element in
-- the set, the rank and the total number of permutations with those
-- counts (the multinomial coefficient).
rankMultisetPermutation :: Ord a => [a] -> ([(a,Int)], (Integer, Integer))
rankMultisetPermutation msp = ( M.toList counts
                              , (index, coef0) )
  where
    counts = L.foldl' (\m k -> M.insertWith (+) k 1 m) M.empty msp
    total0 = sum counts
    coef0 = E.factorial total0
            `div` product (E.factorial <$> counts)
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
unrankMultisetPermutation :: Ord a => [(a,Int)] -> Integer -> [a]
unrankMultisetPermutation l i0
  | any ((< 0) . snd) l = err' "negative count"
  | i0 < 0 || i0 >= coef0 = err' $ "out of bounds: " ++ show (i0,coef0)
  | otherwise = go (fromIntegral total0) coef0 counts i0
  where
    err' = err . ("unrankMultisetPermutation: " ++)
    counts = M.fromList $ filter ((> 0) . snd) l
    total0 = sum counts
    coef0 = E.factorial total0
            `div` product (E.factorial <$> counts)

    go total coef m i | M.null m = []
                      | otherwise = a : go total' coef' m' i'
      where
        total' = total - 1 -- decrement `total` by 1
        subCoefs = (`div` total) . (coef *) . fromIntegral <$> m
        (a, lowerSubCoefsSum, coef') = findBin 0 $ M.toList subCoefs
        i' = i - lowerSubCoefsSum -- update index to local bin
        m' = M.update (\n -> if n == 1 then Nothing else Just $ n - 1)
             a m

        findBin _ [] = err "impossible"
        findBin acc ((el,subCoef):ascs)
          | null ascs || acc' > i = (el, acc, subCoef)
          | otherwise = findBin acc' ascs
          where acc' = acc + subCoef

-- | Computes the multinomial coefficient given a list of counts \(k_i\).
multinomial :: [Int] -> Integer
multinomial ns | any (< 0) ns = 0
               | otherwise = E.factorial (sum ns)
                             `div` product (E.factorial <$> ns)

-----------------
-- PERMUTATION --
-----------------

encodePermutation :: Ord a => [a] -> BitVec
encodePermutation = uncurry Var.encode1 . rankPermutation

decodePermutation :: Ord a => [a] -> BitVec -> Maybe ([a], BitVec)
decodePermutation as bv | BV.length bv0 < len = Nothing
                        | otherwise = Just (p, bv1)
  where
    base = E.factorial $ length as
    len = Var.codeLen1 base
    (bv0,bv1) = BV.splitAt len bv
    p = unrankPermutation as $ BV.toInteger bv0

-- | Rank a permutation. Returns the rank (`fst`) and the total number
-- of permutations of sets with that size ( \(n!\) ) (`snd`).
rankPermutation :: Ord a => [a] -> (Integer, Integer)
rankPermutation p | length p /= n0 = err' "not unique elements"
                  | otherwise = Var.fromValue val
  where
    err' = err . ("rankPermutation: " ++)
    s0 = S.fromList p
    n0 = S.size s0
    ns = fromIntegral <$> [n0,n0-1..1]
    is = fromIntegral <$> go s0 p
    val = assert (length is == length ns)
          mconcat $
          zipWith Var.mkValue is ns

    -- | Lookup element index in the set of remaining elements
    go s [] = assert (S.null s) []
    go s (a:rest) = i : go s' rest
      where i = S.findIndex a s
            s' = S.delete a s

-- | Reconstruct a permutation given a set of elements and a rank. The
-- order in which the elements of the set is given does not matter.
unrankPermutation :: Ord a => [a] -> Integer -> [a]
unrankPermutation as index
  | length as /= n = err' "not unique elements"
  | index < 0 || index >= base = err' $ "out of bounds" ++ show (index,base)
  | otherwise = go set is
  where
    err' = err . ("unrankPermutation: " ++)
    set = S.fromList as
    n = S.size set
    ns = fromIntegral <$> [n,n-1..1]
    base = E.factorial $ fromIntegral n
    bv = Var.toBitVec $ Var.mkValue index base
    is = fromIntegral <$> fst (fromJust $ Var.decode ns bv)

    -- | Successively delete elements at given indexes from a set
    go s [] = assert (S.null s) []
    go s (i:rest) = S.elemAt i s : go (S.deleteAt i s)  rest

factorial :: Int -> Integer
factorial = E.factorial

-----------------
-- COMBINATION --
-----------------

encodeCombination :: [Bool] -> ((Int, Int), BitVec)
encodeCombination = fmap (uncurry Var.encode1) . rankCombination

decodeCombination :: (Int, Int) -> BitVec -> Maybe ([Bool], BitVec)
decodeCombination (n,k) bv | BV.length bv0 < len = Nothing
                           | otherwise = Just (p, bv1)
  where
    base = choose n k
    len = Var.codeLen1 base
    (bv0,bv1) = BV.splitAt len bv
    p = unrankCombination (n,k) $ BV.toInteger bv0

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
    go n k nCk (b:bs) = if b then nCk0 : go (n-1) (k-1) nCk1 bs
                        else go (n-1) k nCk0 bs
      where
        nCk0 = nCk - nCk1 -- sub coef if 0/False
        nCk1 = (nCk * k) `div` n -- sub coef if 1/True

-- | Reconstruct a combination given parameters \((n,k)\) and a rank.
unrankCombination :: (Int, Int) -> Integer -> [Bool]
unrankCombination nk@(n0,k0) i0
  | k0 > n0 || k0 < 0 || n0 < 0 = err' $ "invalid parameters: " ++ show nk
  | i0 < 0 || i0 > n0Ck0 = err' $ "out of range: " ++ show (i0,n0Ck0)
  | otherwise = go (fromIntegral n0) (fromIntegral k0) n0Ck0 i0

  where
    err' = err . ("unrankPermutation: " ++)
    n0Ck0 = n0 `choose` k0
    go n k nCk i | n == 0 = []
                 | i < nCk0 = False : go (n-1) k nCk0 i
                 | otherwise = True : go (n-1) (k-1) nCk1 (i-nCk0)
      where
        nCk0 = nCk - nCk1 -- sub coef if 0/False
        nCk1 = (nCk * k) `div` n -- sub coef if 1/True

-- | Computes the binomial coefficent given parameters \(n\) and \(k\).
choose :: Int -> Int -> Integer
choose n k | denom == 0 = 0
           | otherwise = num `div` denom
  where num = E.factorial n
        denom = E.factorial k * E.factorial (n-k)

------------------
-- DISTRIBUTION --
------------------

encodeDistribution :: [Int] -> ((Int, Int), BitVec)
encodeDistribution = fmap (uncurry Var.encode1) . rankDistribution

decodeDistribution :: (Int, Int) -> BitVec -> Maybe ([Int], BitVec)
decodeDistribution (balls,bins) bv | BV.length bv0 < len = Nothing
                                   | otherwise = Just (d, bv1)
  where
    base = countDistributions balls bins
    len = Var.codeLen1 base
    (bv0,bv1) = BV.splitAt len bv
    d = unrankDistribution (balls,bins) $ BV.toInteger bv0

-- | Rank a distribution in the form of a list bin counts. Returns the
-- \((n,k)\) parameters (where \(n\) is the total number of elements and
-- \(k\) is the number of bins), the rank and the total number of
-- distributions with those parameters.
rankDistribution :: [Int] -> ((Int, Int), (Integer, Integer))
rankDistribution [] = ((0,0),(0,1))
rankDistribution (n0:ns)
  | n0 < 0 || any (< 0) ns = err' "negative count"
  | otherwise = ((bins,balls),(i,base))
  where
    err' = err . ("rankDistribution: " ++)
    comb = replicate n0 False -- 0s are stars, 1s are bars
           ++ concatMap ((True:) . flip replicate False) ns
    ((n,k),(i,base)) = rankCombination comb
    bins = k + 1
    balls = n - bins + 1

-- | Reconstruct a distribution given parameters \((n,k)\) and a rank.
unrankDistribution :: (Int, Int) -> Integer -> [Int]
unrankDistribution (balls,bins) i
  | balls < 0 || bins < 0 = err' $ "invalid parameters: " ++ show (balls,bins)
  | i < 0 || i >= base = err' $ "out of range: " ++ show (i,base)
  | bins == 0 = []
  | otherwise = countGaps 0 bs
  where
    err' = err . ("unrankDistribution: " ++)
    n = balls + bins - 1 -- stars and bars
    k = bins - 1 -- number of bars
    base = if bins == 0 then 1 else n `choose` k
    bs = unrankCombination (n,k) i

    countGaps !acc [] = [acc]
    countGaps !acc (False:rest) = countGaps (acc + 1) rest
    countGaps !acc (True:rest) = acc : countGaps 0 rest

countDistributions :: Int -> Int -> Integer
countDistributions balls bins = base
  where
    n = balls + bins - 1 -- stars and bars
    k = bins - 1 -- number of bars
    base = if bins == 0 then 1 else n `choose` k

----------------------------
-- NON-EMPTY DISTRIBUTION --
----------------------------

encodeDistribution1 :: [Int] -> ((Int, Int), BitVec)
encodeDistribution1 = fmap (uncurry Var.encode1) . rankDistribution1

decodeDistribution1 :: (Int, Int) -> BitVec -> Maybe ([Int], BitVec)
decodeDistribution1 (bins,balls) bv | BV.length bv0 < len = Nothing
                                    | otherwise = Just (d1, bv1)
  where
    base = countDistributions1 bins balls
    len = Var.codeLen1 base
    (bv0,bv1) = BV.splitAt len bv
    d1 = unrankDistribution1 (balls,bins) $ BV.toInteger bv0

-- | Rank a non-empty distribution in the form of a list bin
-- counts. Returns the \((n,k)\) parameters (where \(n\) is the total
-- number of elements and \(k\) is the number of bins), the rank and the
-- total number of distributions with those parameters.
rankDistribution1 :: [Int] -> ((Int, Int), (Integer, Integer))
rankDistribution1 ns
  | any (< 0) ns = err' "negative count"
  | any (< 1) ns = err' "empty count"
  | otherwise = ((balls,bins),(i,base))
  where
    err' = err . ("rankDistribution1: " ++)
    ((balls',bins),(i,base)) = rankDistribution $ (+(-1)) <$> ns
    balls = balls' + bins

-- | Reconstruct a distribution given parameters \((n,k)\) and a rank.
unrankDistribution1 :: (Int, Int) -> Integer -> [Int]
unrankDistribution1 (balls,bins) i
  | balls < bins || bins < 0 =
      err' $ "invalid parameters: " ++ show (balls,bins)
  | otherwise = (+1) <$> unrankDistribution (balls',bins) i
  where
    err' = err . ("unrankDistribution1: " ++)
    balls' = balls - bins

countDistributions1 :: Int -> Int -> Integer
countDistributions1 balls bins
  | balls < bins || bins < 0 =
      err' $ "invalid parameters: " ++ show (balls,bins)
  | otherwise = countDistributions balls' bins
  where
    err' = err . ("unrankDistribution1: " ++)
    balls' = balls - bins
