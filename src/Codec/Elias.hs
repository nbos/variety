module Codec.Elias
    ( -- * Gamma coding
      encodeGamma
    , decodeGamma

    -- * Delta coding
    , encodeDelta
    , decodeDelta

    -- * Omega coding
    , encodeOmega
    , decodeOmega
    ) where

import Data.Bifunctor (Bifunctor(first))
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

err :: String -> a
err = error . ("Elias: " ++)

-- | Encode a positive, non-zero, number in a Elias gamma code.
encodeGamma :: Integer -> BitVec
encodeGamma n | n > 0 = zeros <> bv
              | otherwise =
                  err $ "encodeGamma: "
                  ++ "Number must be positive and non-zero"
  where
    len = BV.bitLen n
    zeros = BV.replicate (len-1) False
    bv = BV.bitVec len n

-- | Try to decode a Elias gamma code at the head of the given bit
-- vector. Upon success, returns the value and remainder of the bitvec,
-- with the encoding prefix removed. Returns @Nothing@ if the bit vector
-- doesn't contain enough bits to define a number.
decodeGamma :: BitVec -> Maybe (Integer, BitVec)
decodeGamma bv | BV.length valBits /= len = Nothing
               | otherwise = Just (val, bv'')
  where
    len = BV.countLeadingZeros bv + 1
    bv' = BV.bitVec (BV.length bv - len) $ BV.toInteger bv
    (valBits, bv'') = BV.splitAt len bv'
    val = BV.toInteger valBits

encodeDelta :: Integer -> BitVec
encodeDelta n | n > 0 = encodeGamma (fromIntegral len) <> bv
              | otherwise =
                  err $ "encodeDelta: "
                  ++ "Number must be positive and non-zero"
  where
    len = BV.bitLen n
    bv = BV.bitVec len n

decodeDelta :: BitVec -> Maybe (Integer, BitVec)
decodeDelta bv = do
  (len, bv') <- first ((+1) . fromIntegral) <$> decodeGamma bv
  let (valBits, bv'') = BV.splitAt len bv'
  if BV.length valBits /= len then Nothing
    else Just (BV.toInteger valBits, bv'')

encodeOmega :: Integer -> BitVec
encodeOmega n0 | n0 > 0 = go n0 eom
               | otherwise =
                   err $ "encodeOmega: "
                   ++ "Number must be positive and non-zero"
  where
    eom = BV.bitVec 1 0 -- "0"

    go 1 = id -- end
    go n = go (len-1) . (bv <>)
      where
        bv = BV.fromInteger n
        len = fromIntegral $ BV.length bv

decodeOmega :: BitVec -> Maybe (Integer, BitVec)
decodeOmega = go 1
  where
    go n bv = do
      b <- bv BV.!? 0 -- head
      case b of
        True | BV.length valBits /= len -> Nothing
             | otherwise -> go n' bv'
          where
            len = fromIntegral n
            (valBits, bv') = BV.splitAt len bv
            n' = BV.toInteger valBits

        False -> Just (n, BV.drop 1 bv) -- eom
