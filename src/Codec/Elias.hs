-- | [Elias codes](https://en.wikipedia.org/wiki/Elias_coding) are
-- prefix codes for positive, non-zero integers with no assumption or
-- limit to their size.
--
-- For codes that include the value @0@, see
-- [Elias.Natural](https://hackage-content.haskell.org/package/variety/docs/Codec-Elias-Natural.html).
module Codec.Elias
    ( -- * Gamma coding

      -- | An Elias gamma code consists of the binary expansion of an
      -- integer, preceded by the unary encoding of the length of that
      -- expansion in zeros.
      --
      -- For example, while the binary expansion of @21@ is:
      --
      -- >>> import qualified Codec.Arithmetic.Variety.BitVec as BV
      -- >>> BV.toString $ BV.fromInteger 21
      -- "10101"
      --
      -- its Elias code is:
      --
      -- >>> BV.toString $ enodeGamma 21
      -- "000010101"
      --
      -- where an expansion of \(i\) is always preceeded by \(i-1\)
      -- zeros.

      encodeGamma
    , decodeGamma

    -- * Delta coding

    -- | An Elias delta code is like an Elias gamma code except that the
    -- length is itself coded like a gamma code instead of simply a
    -- unary encoding.
    --
    -- For example, the binary expansion of \(1\,000\,000\)
    --
    -- >>> BV.toString $ BV.fromInteger (10^6)
    -- "11110100001001000000"
    -- >>> length "11110100001001000000"
    -- 20
    --
    -- is prefixed with the gamma encoding of @20@ and loses its leading
    -- bit (which begins every binary expansion):
    --
    -- >>> BV.toString <$> [encodeGamma 20, BV.fromInteger (10^6)]
    -- ["000010100","11110100001001000000"]
    -- >>> BV.toString $ encodeDelta 1000000
    -- "0000101001110100001001000000"
    -- >>> length "0000101001110100001001000000"
    -- 28

    , encodeDelta
    , decodeDelta

    -- * Omega coding

    -- | An Elias omega code is the result of recursively encoding the
    -- length of binary expansions in the prefix until a length of @1@
    -- is reached. Since binary expansions are written without any
    -- leading zeros, a single @0@ bit marks the end of the code.
    --
    -- For example:
    --
    -- >>> BV.toString . BV.fromInteger <$> [2,4,19,10^6]
    -- ["10","100","10011","11110100001001000000"]
    -- >>> length <$> ["10","100","10011","11110100001001000000"]
    -- [2,3,5,20]
    -- >>> BV.toString $ encodeOmega (10^6)
    -- "1010010011111101000010010000000"
    -- >>> length $ "1010010011111101000010010000000"
    -- 31
    --
    -- Notice that, while /asymptotically/ more efficient, omega codes
    -- are longer than delta codes until around 1 googol, or \(10^{100}\).

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
