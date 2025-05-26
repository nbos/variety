-- | A 'Bool' interface to the bits of the serialization where 'True'
-- \(\mapsto\) @1@ and 'False' \(\mapsto\) @0@.
module Codec.Arithmetic.Variety.Bits (
  Precision(..),
  encode,
  decode,
  encode1,
  decode1,
  ) where

import Prelude hiding (rem,null)
import Control.Arrow (Arrow(first))
import Control.Exception (assert)
import Data.Ratio (Ratio, (%))
import Data.Bits (Bits(bit))

boundsCheck :: Integer -> Integer -> a -> a
boundsCheck i n
  | 0 <= i && i < n = id
  | otherwise = error $ "Value to encode is out of bounds: " ++ show (i,n)

-- | @'encode1' i n@ will encode a single value @i@ given its base @n@,
-- or the number of possible values @i@ can take. Note that @i@ is an
-- index and ranges from @[0..n-1]@, while @n@ is a length and is
-- greater or equal to 1.
encode1 :: Integer -> Integer -> [Bool]
encode1 i n = boundsCheck i n $ go 0 (n % 1)
  where
    iLo  = i % 1         -- i
    iMid = (i*2 + 1) % 2 -- i.5
    iHi  = (i + 1) % 1   -- i + 1

    go nLo nHi | nLo >= iLo && nHi <= iHi = []
               | iMid > nMid = True  : go nMid nHi
               | otherwise   = False : go nLo nMid
      where nMid = nLo + (nHi - nLo) / 2

-- | Given a base, recover a single value from a serialization.
decode1 :: Integer -> [Bool] -> Integer
decode1 = go 0 . fromInteger
  where
    go :: Ratio Integer -> Ratio Integer -> [Bool] -> Integer
    go lo hi [] | hi <= fromInteger floorLo + 1 = floorLo
                | otherwise = error "Not enough bits to decode value"
      where floorLo = floor lo

    go lo hi (bool:rest) | bool      = go mid hi rest
                         | otherwise = go lo mid rest
      where mid = lo + (hi - lo) / 2

-- | Precision policy for encoding/decoding sequences of values. The
-- same 'Precision' that was used for encoding must be given to 'decode'
-- for a successful de-serialization.
data Precision = InfinitePrecision -- ^ Always produce minimal codes
               -- with no regard to speed or memory
               | PrecisionBytes !Int -- ^ Limit the working base to an
               -- 'Integer' taking 'Int' bytes of memory at the cost of
               -- a slightly longer code. This is not only a question of
               -- memory, as working with smaller numbers will
               -- accelerate serialization/de-serialization. (TODO:
               -- include reasonable assignments)
  deriving (Show,Eq)

instance Ord Precision where
  compare InfinitePrecision InfinitePrecision = EQ
  compare InfinitePrecision (PrecisionBytes _) = GT
  compare (PrecisionBytes _) InfinitePrecision = LT
  compare (PrecisionBytes a) (PrecisionBytes b) = compare a b

withinPrecision :: Precision -> Integer -> Bool
withinPrecision InfinitePrecision = const True
withinPrecision (PrecisionBytes n)
  | n < 1 = error "Precision must be positive"
  | n > maxBound `div` 8 = error
    "Precision too high, use InfinitePrecision instead"
  | otherwise = (< bit (n*8)) -- allocs O(n) bytes

minCodeLength :: [Integer] -> Int
minCodeLength = undefined

-- | Given a precision policy, optimally encode a sequence of value-base
-- pairs @(i,n)@, where @i@ is the value and @n@ is the base, or the
-- number of values @i@ could take. Note that @i@ is an index and ranges
-- from @[0..n-1]@, while @n@ is a length and is greater or equal to 1.
encode :: Precision -> [(Integer,Integer)] -> [Bool]
encode _ [] = []
encode prec ((i0,n0):ins0) = boundsCheck i0 n0 $ go i0 n0 ins0
  where
    tooBig = not . withinPrecision prec

    go i n ins
      | even n = case n `div` 2 of
          hn | i >= hn   -> True  : go (i-hn) hn ins
             | otherwise -> False : go i hn ins
      | tooBig n = assert (even $ n+1) go i (n+1) ins
      | otherwise = case ins of
          [] -> encode1 i n
          ((a,b):rest) -> boundsCheck a b $ go (i*b+a) (n*b) rest

data Queue a = Q ![a] ![a]
-- deriving (Functor,Traversable)

singleton :: a -> Queue a
singleton a = Q [a] []

pushBack :: a -> Queue a -> Queue a
pushBack a (Q hd rtl) = Q hd (a:rtl)

pushFront :: a -> Queue a -> Queue a
pushFront a (Q hd rtl) = Q (a:hd) rtl

popFront :: Queue a -> Maybe (a, Queue a)
popFront (Q (a:hd) rtl) = Just (a, Q hd rtl)
popFront (Q [] rtl) = case reverse rtl of
  [] -> Nothing
  (a:hd) -> Just (a, Q hd [])

toList :: Queue a -> [a]
toList (Q hd rtl) = hd ++ reverse rtl

-- | Decoder state
data Decoder = Decoder {
  _lo    :: !Integer, -- ^ value i=0 is in
  _rem   :: !Integer, -- ^ how far into lo i=0 is (0 <=, < scale)
  _scale :: !Integer, -- ^ how many i's per value
  _hi    :: !Integer, -- ^ value i=n-1 is in
  _queue :: !(Queue Integer) -- ^ queue of spaces to resolve
}

-- | Given the same precision policy and bases that were used to encode
-- a sequence of values, recover the values from the serialization.
decode :: Precision -> [Integer] -> [Bool] -> [Integer]
decode prec = g0
  where
    tooBig :: Integer -> Bool
    tooBig = not . withinPrecision prec

    g0 :: [Integer] -> [Bool] -> [Integer]
    g0 [] _ = []
    g0 (n:ns) bits = g1 (Decoder 0 0 1 hi q) n ns bits
      where q = singleton n
            hi = n - 1

    g1 :: Decoder -> Integer -> [Integer] -> [Bool] -> [Integer]
    g1 dr@(Decoder lo rem scale hi q) n ns bits
      | lo == hi = case popFront q of
          -- produce value if there's a base in the queue
          Just (_, q') -> (lo:) $ case popFront q' of
            -- rescale if there's a second base in the queue
            Just (next, q'') ->
              let scale' = scale `div` next
                  (lo', rem') = rem `divMod` scale'
                  hi' = rem + n `div` scale'
                  q''' = pushFront next q'' -- save the reverse
              in g1 (Decoder lo' rem' scale' hi' q''') n ns bits

            Nothing -> assert (scale == 1) assert (n == 1)
                       g0 ns bits -- start fresh

          Nothing -> assert (scale == 1) assert (n == 1)
                     g0 ns bits -- start fresh

      | even n = -- split space in half:
          let hn = n `div` 2
          in case bits of
            (True:bits') ->
              let (lo', rem') = first (lo+) $ (hn + rem) `divMod` scale
              in g1 (Decoder lo' rem' scale hi q) hn ns bits'
            (False:bits') ->
              let hi' = (hn + rem - 1) `div` scale
              in g1 (Decoder lo rem scale hi' q) hn ns bits'
            [] -> error "Ran out of bits while decoding"

      | tooBig n = -- force the space to split by going to n+1
          assert (even (n+1)) g1 dr (n+1) ns bits

      | otherwise = case ns of
          [] -> let i = decode1 n bits + rem
                in g2 lo scale i $ toList q -- end

          next:ns' -> -- upscale
                let rem'   = next * rem
                    scale' = next * scale
                    q'     = pushBack next q
                    n'     = next * n
                in g1 (Decoder lo rem' scale' hi q') n' ns' bits

    g2 :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
    g2 lo scale i (_:q) =
      let (inc,i') = i `divMod` scale
      in lo + inc : case q of
        [] -> []
        next:_ -> g2 0 (scale `div` next) i' q

    g2 lo scale i [] = assert (lo == 0)
                       assert (scale == 1)
                       assert (i == 0) []

_bitString :: [Bool] -> String
_bitString = fmap $ \bool -> if bool then '1' else '0'
