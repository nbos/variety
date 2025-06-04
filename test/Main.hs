{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.HUnit
import Test.QuickCheck
import Control.Monad

import qualified Codec.Elias as E
import qualified Codec.Arithmetic.Variety as V
import qualified Codec.Arithmetic.Variety.BitVec as BV
import qualified Codec.Arithmetic.Variety.Bounded as VB
import qualified Codec.Arithmetic.Combinatorics as Comb

main :: IO ()
main = do
  -- HUnit
  void $ runTestTT $ TestLabel "Combinatorics:msp" multisetPermutation
  void $ runTestTT $ TestLabel "Combinatorics:permutation" permutation
  void $ runTestTT $ TestLabel "Combinatorics:combination" combination
  void $ runTestTT $ TestLabel "Combinatorics:distribution" distribution
  void $ runTestTT $ TestLabel "Combinatorics:distribution1" distribution1
  void $ runTestTT $ TestLabel "Elias:reference" eliasReference
  void $ runTestTT $ TestLabel "Elias:comprehensive" eliasComprehensive

  -- QuickTest
  quickCheck (forAll genValueBasePairs prop_encode_decode_roundtrip)
  quickCheck (forAll genValueBasePair (uncurry prop_encode1_decode1_roundtrip))
  quickCheck (forAll ((,) <$> choose (1, 10) <*> genValueBasePairs)
               (uncurry prop_bounded_encode_decode_roundtrip))

multisetPermutation :: Test
multisetPermutation = "Multiset Permutation " ~: TestList $ (<$> samples) $ \s ->
  let (params,(_,base)) = Comb.rankMultisetPermutation s
      f = fst . snd . Comb.rankMultisetPermutation
          . Comb.unrankMultisetPermutation params
  in TestList $ (<$> [0..base-1]) $ \i -> show (params, i) ~: f i @?= i
  where samples = [ [ ]
                  , [1]
                  , [1,1,2]
                  , [1,1,2,2]
                  , [1,1,2,3,3,3] ] :: [[Int]]

-----------------
-- HUNIT TESTS --
-----------------

permutation :: Test
permutation = "Permutation " ~: TestList $ (<$> samples) $ \s ->
  let (_,base) = Comb.rankPermutation s
      f = fst . Comb.rankPermutation . Comb.unrankPermutation s
  in TestList $ (<$> [0..base-1]) $ \i -> show (s, i) ~: f i @?= i
  where samples = [ [ ]
                  , [0]
                  , [0,1]
                  , [0,1,2]
                  , [0,1,2,3,4] ] :: [[Int]]

combination :: Test
combination = "Combination " ~: TestList $ (<$> samples) $ \(n,k)->
  let base = n `Comb.choose` k
      f = fst . snd . Comb.rankCombination . Comb.unrankCombination (n,k)
  in TestList $ (<$> [0..base-1]) $ \i -> show ((n,k), i) ~: f i @?= i
  where samples = [ (0, 0)
                  , (5, 0)
                  , (5, 3)
                  , (5, 5) ]

distribution :: Test
distribution = "Distribution " ~: TestList $ (<$> samples) $ \params ->
  let (_,(_,base)) = Comb.rankDistribution $ Comb.unrankDistribution params 0
      f = fst . snd . Comb.rankDistribution . Comb.unrankDistribution params
  in TestList $ (<$> [0..base-1]) $ \i -> show (params, i) ~: f i @?= i
  where samples = [ (0, 0)
                  , (5, 0)
                  , (5, 3)
                  , (5, 5)
                  , (3, 5) ]

distribution1 :: Test
distribution1 = "Non-empty Distribution " ~: TestList $ (<$> samples) $ \params ->
  let (_,(_,base)) = Comb.rankDistribution1 $ Comb.unrankDistribution1 params 0
      f = fst . snd . Comb.rankDistribution1 . Comb.unrankDistribution1 params
  in TestList $ (<$> [0..base-1]) $ \i -> show (params, i) ~: f i @?= i
  where samples = [ (0, 0)
                  , (5, 0)
                  , (5, 3)
                  , (5, 5) ]

eliasReference :: Test
eliasReference = TestList [ TestList gamma
                          , TestList delta
                          , TestList omega ]
  where
    gamma = [ "g(1)"  ~: E.encodeGamma 1  @?= BV.fromString "1"
            , "g(2)"  ~: E.encodeGamma 2  @?= BV.fromString "010"
            , "g(3)"  ~: E.encodeGamma 3  @?= BV.fromString "011"
            , "g(4)"  ~: E.encodeGamma 4  @?= BV.fromString "00100"
            , "g(5)"  ~: E.encodeGamma 5  @?= BV.fromString "00101"
            , "g(6)"  ~: E.encodeGamma 6  @?= BV.fromString "00110"
            , "g(7)"  ~: E.encodeGamma 7  @?= BV.fromString "00111"
            , "g(8)"  ~: E.encodeGamma 8  @?= BV.fromString "0001000"
            , "g(9)"  ~: E.encodeGamma 9  @?= BV.fromString "0001001"
            , "g(10)" ~: E.encodeGamma 10 @?= BV.fromString "0001010"
            , "g(15)" ~: E.encodeGamma 15 @?= BV.fromString "0001111"
            , "g(16)" ~: E.encodeGamma 16 @?= BV.fromString "000010000"
            , "g(17)" ~: E.encodeGamma 17 @?= BV.fromString "000010001" ]

    delta = [ "d(1)"  ~: E.encodeDelta 1  @?= BV.fromString "1"
            , "d(2)"  ~: E.encodeDelta 2  @?= BV.fromString "0100"
            , "d(3)"  ~: E.encodeDelta 3  @?= BV.fromString "0101"
            , "d(4)"  ~: E.encodeDelta 4  @?= BV.fromString "01100"
            , "d(5)"  ~: E.encodeDelta 5  @?= BV.fromString "01101"
            , "d(6)"  ~: E.encodeDelta 6  @?= BV.fromString "01110"
            , "d(7)"  ~: E.encodeDelta 7  @?= BV.fromString "01111"
            , "d(8)"  ~: E.encodeDelta 8  @?= BV.fromString "00100000"
            , "d(9)"  ~: E.encodeDelta 9  @?= BV.fromString "00100001"
            , "d(10)" ~: E.encodeDelta 10 @?= BV.fromString "00100010"
            , "d(15)" ~: E.encodeDelta 15 @?= BV.fromString "00100111"
            , "d(16)" ~: E.encodeDelta 16 @?= BV.fromString "001010000"
            , "d(17)" ~: E.encodeDelta 17 @?= BV.fromString "001010001" ]

    omega = [ "o(1)"  ~: E.encodeOmega 1  @?= BV.fromString "0"
            , "o(2)"  ~: E.encodeOmega 2  @?= BV.fromString "100"
            , "o(3)"  ~: E.encodeOmega 3  @?= BV.fromString "110"
            , "o(4)"  ~: E.encodeOmega 4  @?= BV.fromString "101000"
            , "o(5)"  ~: E.encodeOmega 5  @?= BV.fromString "101010"
            , "o(6)"  ~: E.encodeOmega 6  @?= BV.fromString "101100"
            , "o(7)"  ~: E.encodeOmega 7  @?= BV.fromString "101110"
            , "o(8)"  ~: E.encodeOmega 8  @?= BV.fromString "1110000"
            , "o(9)"  ~: E.encodeOmega 9  @?= BV.fromString "1110010"
            , "o(10)" ~: E.encodeOmega 10 @?= BV.fromString "1110100"
            , "o(15)" ~: E.encodeOmega 15 @?= BV.fromString "1111110"
            , "o(16)" ~: E.encodeOmega 16 @?= BV.fromString "10100100000"
            , "o(17)" ~: E.encodeOmega 17 @?= BV.fromString "10100100010" ]

eliasComprehensive :: Test
eliasComprehensive = TestList [ TestList gamma
                              , TestList delta
                              , TestList omega ]
  where
    gamma = (<$> [10..1000]) $ \i ->
      "o(" ++ show i ++ ")" ~: E.decodeGamma (E.encodeGamma i) @?= Just (i, BV.empty)
    delta = (<$> [10..1000]) $ \i ->
      "o(" ++ show i ++ ")" ~: E.decodeDelta (E.encodeDelta i) @?= Just (i, BV.empty)
    omega = (<$> [10..1000]) $ \i ->
      "o(" ++ show i ++ ")" ~: E.decodeOmega (E.encodeOmega i) @?= Just (i, BV.empty)

----------------
-- QUICKCHECK --
----------------

genValueBasePair :: Gen (Integer, Integer)
genValueBasePair = do
  base <- choose (1, 100)
  value <- choose (0, base - 1)
  return (value, base)

genValueBasePairs :: Gen [(Integer, Integer)]
genValueBasePairs = do
  n <- choose (0, 100)
  vectorOf n genValueBasePair

prop_encode_decode_roundtrip :: [(Integer, Integer)] -> Property
prop_encode_decode_roundtrip pairs =
  all (\(v, b) -> v >= 0 && v < b && b >= 1) pairs ==>
    let encoded = V.encode pairs
        bases = map snd pairs
        expected = map fst pairs
        decoded = V.decode bases encoded
    in decoded === expected

prop_encode1_decode1_roundtrip :: Integer -> Integer -> Property
prop_encode1_decode1_roundtrip value base =
  value >= 0 && value < base && base >= 1 ==>
    let encoded = V.encode1 value base
        decoded = V.decode1 encoded
    in decoded === value

prop_bounded_encode_decode_roundtrip :: Int -> [(Integer, Integer)] -> Property
prop_bounded_encode_decode_roundtrip prec pairs =
  prec > 0 && all (\(v, b) -> v >= 0 && v < b && b >= 1) pairs ==>
    let encoded = VB.encode prec pairs
        bases = map snd pairs
        expected = map fst pairs
        decoded = VB.decode prec bases encoded
    in decoded === expected
