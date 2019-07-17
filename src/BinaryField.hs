module BinaryField
  ( BinaryField
  ) where

import Protolude

import Control.Monad.Random (Random(..), getRandom)
import Test.Tasty.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (GaloisField(..))

-------------------------------------------------------------------------------
-- Binary field type
-------------------------------------------------------------------------------

-- | Binary fields @GF(2^q)[X]/\<f(X)\>@ for @q@ positive and
-- @f(X)@ irreducible monic in @GF(2^q)[X]@ encoded as an integer.
newtype BinaryField (ib :: Nat) = BF Integer
  deriving (Eq, Generic, NFData, Read, Show)

-- Binary fields are arbitrary.
instance KnownNat ib => Arbitrary (BinaryField ib) where
  arbitrary = BF <$> choose (0, 2 ^ natVal (witness :: BinaryField ib) - 1)

-- Binary fields are fields.
instance KnownNat ib => Fractional (BinaryField ib) where
  recip y@(BF x)      = case inv (natVal y) x of
    Just z -> BF z
    _      -> panic "no multiplicative inverse."
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Binary fields are Galois fields.
instance KnownNat ib => GaloisField (BinaryField ib) where
  char = const 2
  {-# INLINE char #-}
  deg  = bin . natVal
  {-# INLINE deg #-}
  frob = flip pow 2
  {-# INLINE frob #-}
  pow  = (^)
  {-# INLINE pow #-}
  rnd  = getRandom
  {-# INLINE rnd #-}

-- Binary fields are fields.
instance KnownNat ib => Num (BinaryField ib) where
  BF x + BF y = BF (xor x y)
  {-# INLINE (+) #-}
  BF x * BF y = fromInteger (mul x y)
  {-# INLINE (*) #-}
  BF x - BF y = BF (xor x y)
  {-# INLINE (-) #-}
  negate      = identity
  {-# INLINE negate #-}
  fromInteger = BF . red (natVal (witness :: BinaryField ib))
  {-# INLINABLE fromInteger #-}
  abs         = panic "not implemented."
  signum      = panic "not implemented."

-- Binary fields are pretty.
instance KnownNat ib => Pretty (BinaryField ib) where
  pretty (BF x) = pretty x

-- Binary fields are random.
instance KnownNat ib => Random (BinaryField ib) where
  random  = first BF . randomR (0, 2 ^ natVal (witness :: BinaryField ib) - 1)
  {-# INLINE random #-}
  randomR = panic "not implemented."

-- Binary logarithm.
bin :: Integer -> Int
bin = logP 2
  where
    logP :: Integer -> Integer -> Int
    logP p x = let l = 2 * logP (p * p) x
               in if x < p then 0 else log' l (quot x (p ^ l))
      where
        log' :: Int -> Integer -> Int
        log' q y = if y < p then q else log' (q + 1) (quot y p)
{-# INLINE bin #-}

-- Binary multiplication.
mul :: Integer -> Integer -> Integer
mul x y = mul' (bin y) (if testBit y 0 then x else 0)
  where
    mul' :: Int -> Integer -> Integer
    mul' 0 n = n
    mul' l n = mul' (l - 1) (if testBit y l then xor n (shift x l) else n)
{-# INLINE mul #-}

-- Binary reduction.
red :: Integer -> Integer -> Integer
red f = red'
  where
    red' :: Integer -> Integer
    red' x = let n = bin x - bin f
             in if n < 0 then x else red' (xor x (shift f n))
{-# INLINE red #-}

-- Binary inversion.
inv :: Integer -> Integer -> Maybe Integer
inv f x = case inv' 1 x 0 f of
  (y, 1) -> Just y
  _      -> Nothing
  where
    inv' :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
    inv' t r _  0  = (t, r)
    inv' t r t' r' = let q = max 0 (bin r - bin r')
                     in inv' t' r' (xor t (shift t' q)) (xor r (shift r' q))
{-# INLINE inv #-}
