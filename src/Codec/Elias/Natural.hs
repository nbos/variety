-- | [Elias codes](https://en.wikipedia.org/wiki/Elias_coding) are
-- prefix codes for positive, non-zero integers with no assumption or
-- limit to their size.
--
-- Functions of this module add @1@ at encoding time and subtract @1@ at
-- decoding time to support any natural number, including zero.
module Codec.Elias.Natural
    ( -- * Gamma coding

      -- | An Elias gamma code consists of the binary expansion of an
      -- integer, preceded by the unary encoding of the length of that
      -- expansion in zeros.

      encodeGamma
    , decodeGamma

    -- * Delta coding

    -- | An Elias delta code is like an Elias gamma code except that the
    -- length is itself coded like a gamma code instead of simply a
    -- unary encoding.

    , encodeDelta
    , decodeDelta

    -- * Omega coding

    -- | An Elias omega code is the result of recursively encoding the
    -- length of binary expansions in the prefix until a length of @1@
    -- is reached. Since binary expansions are written without any
    -- leading zeros, a single @0@ bit marks the end of the code.

    , encodeOmega
    , decodeOmega
    ) where

import Data.Bifunctor (Bifunctor(first))
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Elias as E

boundsError :: a
boundsError = error "Elias.Natural: negative number"

-- | Encode a number in a Elias gamma code. Throws an error if the input
-- is negative.
encodeGamma :: Integer -> BitVec
encodeGamma n | n >= 0 = E.encodeGamma (n+1)
              | otherwise = boundsError

-- | Try to decode an Elias gamma code at the head of the given bit
-- vector. If successful, returns the decoded value and the remainder of
-- the `BitVec` with the value's code removed. Returns @Nothing@ if the
-- bit vector doesn't contain enough bits to specify a number.
decodeGamma :: BitVec -> Maybe (Integer, BitVec)
decodeGamma = fmap (first (+(-1))) . E.decodeGamma

-- | Encode a number in a Elias delta code. Throws an error if the input
-- is negative.
encodeDelta :: Integer -> BitVec
encodeDelta n | n >= 0 = E.encodeDelta (n+1)
              | otherwise = boundsError

-- | Try to decode an Elias delta code at the head of the given bit
-- vector. If successful, returns the decoded value and the remainder of
-- the `BitVec` with the value's code removed. Returns @Nothing@ if the
-- bit vector doesn't contain enough bits to specify a number.
decodeDelta :: BitVec -> Maybe (Integer, BitVec)
decodeDelta = fmap (first (+(-1))) . E.decodeDelta

-- | Encode a number in a Elias omega code. Throws an error if the input
-- is negative.
encodeOmega :: Integer -> BitVec
encodeOmega n | n >= 0 = E.encodeOmega n
              | otherwise = boundsError

-- | Try to decode an Elias omega code at the head of the given bit
-- vector. If successful, returns the decoded value and the remainder of
-- the `BitVec` with the value's code removed. Returns @Nothing@ if the
-- bit vector doesn't contain enough bits to specify a number.
decodeOmega :: BitVec -> Maybe (Integer, BitVec)
decodeOmega = fmap (first (+(-1))) . E.decodeOmega
