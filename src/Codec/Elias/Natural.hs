module Codec.Elias.Natural
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
import qualified Codec.Elias as E

err :: String -> a
err = error . ("Elias.Natural: " ++)

-- | Encode a positive, non-zero, number in a Elias gamma code.
encodeGamma :: Integer -> BitVec
encodeGamma n | n >= 0 = E.encodeGamma (n+1)
              | otherwise = err "encodeGamma: negative number"

-- | Try to decode a Elias gamma code at the head of the given bit
-- vector. Upon success, returns the value and remainder of the bitvec,
-- with the encoding prefix removed. Returns @Nothing@ if the bit vector
-- doesn't contain enough bits to define a number.
decodeGamma :: BitVec -> Maybe (Integer, BitVec)
decodeGamma = fmap (first (+(-1))) . E.decodeGamma

encodeDelta :: Integer -> BitVec
encodeDelta n | n >= 0 = E.encodeDelta (n+1)
              | otherwise = err "encodeDelta: negative number"

decodeDelta :: BitVec -> Maybe (Integer, BitVec)
decodeDelta = fmap (first (+(-1))) . E.decodeDelta

encodeOmega :: Integer -> BitVec
encodeOmega n | n >= 0 = E.encodeOmega n
              | otherwise = err "encodeOmega: negative number"

decodeOmega :: BitVec -> Maybe (Integer, BitVec)
decodeOmega = fmap (first (+(-1))) . E.decodeOmega
