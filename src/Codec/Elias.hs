-- | Elias codes are prefix codes for positive, non-zero integers with
-- no prior assumption to their size.
module Codec.Elias
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
    -- length of binary expansions until a length of @1@ is
    -- reached. Since binary expansions are written without any leading
    -- zeros, a single @0@ bit marks the end of the code.

    , encodeOmega
    , decodeOmega
    ) where

import qualified Data.Bits as Bits
import Data.Bifunctor (Bifunctor(first))
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

boundsError :: a
boundsError = error "Elias: Number must be positive and non-zero"

-- | Encode a number in a Elias gamma code. Throws an error if the input
-- is not positive and non-zero.
encodeGamma :: Integer -> BitVec
encodeGamma x | x > 0 = BV.replicate n False <> xBits
              | otherwise = boundsError
  where
    xBits = BV.fromInteger x
    n = BV.length xBits - 1

-- | Try to decode an Elias gamma code at the head of the given bit
-- vector. If successful, returns the decoded value and the remainder of
-- the `BitVec`, with the value code removed. Returns @Nothing@ if the
-- bit vector doesn't contain enough bits to define a number.
decodeGamma :: BitVec -> Maybe (Integer, BitVec)
decodeGamma bv | BV.length xBits /= xLen = Nothing
               | otherwise = Just (x, bv'')
  where
    n = BV.countLeadingZeros bv
    xLen = n + 1
    bv' = BV.bitVec (BV.length bv - n) $ BV.toInteger bv -- truncate
    (xBits, bv'') = BV.splitAt xLen bv'
    x = BV.toInteger xBits

-- | Encode a number in a Elias delta code. Throws an error if the input
-- is not positive and non-zero.
encodeDelta :: Integer -> BitVec
encodeDelta x | x > 0 = encodeGamma (fromIntegral xLen) <> tailBits
              | otherwise = boundsError
  where
    xBits = BV.fromInteger x
    xLen = BV.length xBits
    n = xLen - 1
    tailBits = BV.bitVec n $ Bits.clearBit x n -- without leading bit

-- | Try to decode an Elias delta code at the head of the given bit
-- vector. If successful, returns the decoded value and the remainder of
-- the `BitVec`, with the value code removed. Returns @Nothing@ if the
-- bit vector doesn't contain enough bits to define a number.
decodeDelta :: BitVec -> Maybe (Integer, BitVec)
decodeDelta bv = do
  (xLen, bv') <- first fromIntegral <$> decodeGamma bv
  let n = xLen - 1
      (xTail, bv'') = BV.splitAt n bv'
      xBits = BV.singleton True <> xTail
  if BV.length xBits /= xLen then Nothing
    else Just (BV.toInteger xBits, bv'')

-- | Encode a number in a Elias omega code. Throws an error if the input
-- is not positive and non-zero.
encodeOmega :: Integer -> BitVec
encodeOmega x0 | x0 > 0 = go x0 eom
               | otherwise = boundsError
  where
    eom = BV.bitVec 1 0 -- "0"

    go 1 = id -- end
    go n = go (len-1) . (bv <>)
      where
        bv = BV.fromInteger n
        len = fromIntegral $ BV.length bv

-- | Try to decode an Elias omega code at the head of the given bit
-- vector. If successful, returns the decoded value and the remainder of
-- the `BitVec`, with the value code removed. Returns @Nothing@ if the
-- bit vector doesn't contain enough bits to define a number.
decodeOmega :: BitVec -> Maybe (Integer, BitVec)
decodeOmega = go 1
  where
    go n bv = do
      b <- bv BV.!? 0 -- head
      case b of
        True | BV.length valBits /= len -> Nothing
             | otherwise -> go n' bv'
          where
            len = fromIntegral n + 1
            (valBits, bv') = BV.splitAt len bv
            n' = BV.toInteger valBits

        False -> Just (n, BV.drop 1 bv) -- eom
