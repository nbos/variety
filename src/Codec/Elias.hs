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

import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

err :: String -> a
err = error . ("Elias: " ++)

-- | Encode a positive, non-zero, number in a Elias gamma code.
encodeGamma :: Integer -> BitVec
encodeGamma int | int > 0 = zeros <> bv
                | otherwise =
                    err $ "encodeGamma: "
                    ++ "Number must be positive and non-zero"
  where
    len = BV.bitLen int
    zeros = BV.replicate (len-1) False
    bv = BV.bitVec len int

-- | Try to decode a Elias gamma code at the head of the given bit
-- vector. Upon success, returns the value and remainder of the bitvec,
-- with the encoding prefix removed. Returns @Nothing@ if the bit vector
-- doesn't contain enough bits to define a number.
decodeGamma :: BitVec -> Maybe (Integer, BitVec)
decodeGamma bv | BV.length valBits /= len = Nothing
               | otherwise = Just (val, rest)
  where
    n = BV.countLeadingZeros bv
    len = BV.length bv
    int = BV.toInteger bv
    bv' = BV.bitVec (len - n) int
    (valBits, rest) = BV.splitAt len bv'
    val = BV.toInteger valBits

encodeDelta :: Integer -> BitVec
encodeDelta = undefined

decodeDelta :: BitVec -> Maybe (BitVec, Integer, BitVec)
decodeDelta = undefined

encodeOmega :: Integer -> BitVec
encodeOmega = undefined

decodeOmega :: BitVec -> Maybe Integer
decodeOmega = undefined
