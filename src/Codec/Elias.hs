-- | Elias codes are prefix codes for positive, non-zero integers with
-- no prior assumption to their size.
module Codec.Elias
    ( -- * Gamma coding

      -- | An Elias gamma code consists of the binary expansion of an
      -- integer, preceded by the unary encoding of the length of that
      -- expansion as a string of zeros.

      encodeGamma
    , decodeGamma

    -- * Delta coding

    -- | An Elias delta code is like an Elias gamma code except that the
    -- length is itself coded in a gamma code instead of its unary
    -- encoding.

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

import Data.Bifunctor (Bifunctor(first))
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

err :: a
err = error "Elias: Number must be positive and non-zero"

-- | Encode a number in a Elias gamma code. Throws an error if the input
-- is not positive and non-zero.
encodeGamma :: Integer -> BitVec
encodeGamma n | n > 0 = zeros <> bv
              | otherwise = err
  where
    len = BV.bitLen n
    zeros = BV.replicate (len-1) False
    bv = BV.bitVec len n

-- | Try to decode an Elias gamma code at the head of the given bit
-- vector. If successful, returns the decoded value and the remainder of
-- the `BitVec`, with the value code removed. Returns @Nothing@ if the
-- bit vector doesn't contain enough bits to define a number.
decodeGamma :: BitVec -> Maybe (Integer, BitVec)
decodeGamma bv | BV.length valBits /= len = Nothing
               | otherwise = Just (val, bv'')
  where
    len = BV.countLeadingZeros bv + 1
    bv' = BV.bitVec (BV.length bv - len) $ BV.toInteger bv
    (valBits, bv'') = BV.splitAt len bv'
    val = BV.toInteger valBits

-- | Encode a number in a Elias delta code. Throws an error if the input
-- is not positive and non-zero.
encodeDelta :: Integer -> BitVec
encodeDelta n | n > 0 = encodeGamma (fromIntegral len) <> bv
              | otherwise = err
  where
    len = BV.bitLen n
    bv = BV.bitVec len n

-- | Try to decode an Elias delta code at the head of the given bit
-- vector. If successful, returns the decoded value and the remainder of
-- the `BitVec`, with the value code removed. Returns @Nothing@ if the
-- bit vector doesn't contain enough bits to define a number.
decodeDelta :: BitVec -> Maybe (Integer, BitVec)
decodeDelta bv = do
  (len, bv') <- first ((+1) . fromIntegral) <$> decodeGamma bv
  let (valBits, bv'') = BV.splitAt len bv'
  if BV.length valBits /= len then Nothing
    else Just (BV.toInteger valBits, bv'')

-- | Encode a number in a Elias omega code. Throws an error if the input
-- is not positive and non-zero.
encodeOmega :: Integer -> BitVec
encodeOmega n0 | n0 > 0 = go n0 eom
               | otherwise = err
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
            len = fromIntegral n
            (valBits, bv') = BV.splitAt len bv
            n' = BV.toInteger valBits

        False -> Just (n, BV.drop 1 bv) -- eom
