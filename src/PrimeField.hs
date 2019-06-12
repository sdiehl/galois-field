{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module PrimeField
  ( PrimeField(..)
  ) where

import Protolude

import GaloisField (GaloisField(..))

-- | Prime number @p@
class Prime p where
  {-# MINIMAL p #-}
  p :: p -> Integer

-- | Prime fields @GF(p)@ for @p@ prime
newtype PrimeField p = PF Integer
  deriving (Show, Generic, NFData)

-- | Prime fields are Galois fields
instance Prime p => GaloisField (PrimeField p) where
  ch = const $ p (undefined :: p)

-- | Prime fields have endpoints
instance Prime p => Bounded (PrimeField p) where
  minBound = fromInteger 0
  maxBound = fromInteger $ -1

-- | Prime fields are linearly ordered
instance Prime p => Enum (PrimeField p) where
  toEnum   = fromIntegral
  fromEnum = fromInteger . toInteger

-- | Prime fields are normal models
instance Prime p => Eq (PrimeField p) where
  (==) = (. toInteger) . (==) . toInteger

-- | Prime fields are division rings
instance Prime p => Fractional (PrimeField p) where
  fromRational (a :% b) = fromInteger a / fromInteger b
  {-# INLINE recip #-}
  recip (PF n)          = case modInv n $ p (undefined :: p) of
    Right n' -> fromIntegral n'
    Left n'  -> panic "no multiplicative inverse."

-- | Prime fields are integral domains
instance Prime p => Integral (PrimeField p) where
  {-# INLINE quotRem #-}
  quotRem q r      = (fromInteger q', fromInteger r')
    where
      (q', r') = quotRem (toInteger q) (toInteger r)
  toInteger (PF n) = mod n $ p (undefined :: p)

-- | Prime fields are additive monoids
instance Prime p => Monoid (PrimeField p) where
  mempty = fromInteger 0

-- | Prime fields are rings
instance Prime p => Num (PrimeField p) where
  PF n + PF n'  = fromInteger $ n + n'
  PF n * PF n'  = fromInteger $ n * n'
  abs           = notImplemented
  signum        = notImplemented
  fromInteger n = PF . mod n $ p (undefined :: p)
  negate (PF n) = fromInteger $ negate n

-- | Prime fields are ordered fields
instance Prime p => Ord (PrimeField p) where
  (<=) = (. toInteger) . (<=) . toInteger

-- | Prime fields inject to the reals
instance Prime p => Real (PrimeField p) where
  toRational = fromInteger . toInteger

-- | Prime fields are additive semigroups
instance Prime p => Semigroup (PrimeField p) where
  (<>) = (+)

-- | Inverse in field
{-# INLINABLE modInv #-}
modInv :: Integral a => a -> a -> Either a a
modInv x p = let (g, (y, _)) = extGCD p x in if g == 1 then Right y else Left g
  where
    extGCD :: Integral a => a -> a -> (a, (a, a))
    extGCD y 0 = (y, (0, 1))
    extGCD y x = (g, (t - s * q, s))
      where
        (q, r)      = quotRem y x
        (g, (s, t)) = extGCD x r
