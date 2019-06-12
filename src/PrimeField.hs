{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module PrimeField
  ( PrimeField(..)
  ) where

import Protolude

import GaloisField (GaloisField(..))

-- | Prime fields @GF(p)@ for @p@ prime
newtype PrimeField (p :: Nat) = PF Integer
  deriving (Show, Generic, NFData)

-- | Prime fields are Galois fields
instance KnownNat p => GaloisField (PrimeField p) where
  ch = natVal

-- | Prime fields have endpoints
instance KnownNat p => Bounded (PrimeField p) where
  minBound = fromInteger 0
  maxBound = fromInteger $ -1

-- | Prime fields are linearly ordered
instance KnownNat p => Enum (PrimeField p) where
  toEnum   = fromIntegral
  fromEnum = fromInteger . toInteger

-- | Prime fields are normal models
instance KnownNat p => Eq (PrimeField p) where
  (==) = (. toInteger) . (==) . toInteger

-- | Prime fields are division rings
instance KnownNat p => Fractional (PrimeField p) where
  fromRational (a :% b) = fromInteger a / fromInteger b
  {-# INLINE recip #-}
  recip f@(PF n)        = case modInv n $ natVal f of
    Right n' -> fromIntegral n'
    Left n'  -> panic "no multiplicative inverse."

-- | Prime fields are integral domains
instance KnownNat p => Integral (PrimeField p) where
  {-# INLINE quotRem #-}
  quotRem q r        = (fromInteger q', fromInteger r')
    where
      (q', r') = quotRem (toInteger q) (toInteger r)
  toInteger f@(PF n) = mod n $ natVal f

-- | Prime fields are additive monoids
instance KnownNat p => Monoid (PrimeField p) where
  mempty = fromInteger 0

-- | Prime fields are rings
instance KnownNat p => Num (PrimeField p) where
  PF n + PF n'  = fromInteger $ n + n'
  PF n * PF n'  = fromInteger $ n * n'
  abs f         = f
  signum f      = if f == 0 then 0 else 1
  fromInteger n = let f = PF . mod n $ natVal f in f
  negate (PF n) = fromInteger $ negate n

-- | Prime fields are ordered fields
instance KnownNat p => Ord (PrimeField p) where
  (<=) = (. toInteger) . (<=) . toInteger

-- | Prime fields inject to the reals
instance KnownNat p => Real (PrimeField p) where
  toRational = fromInteger . toInteger

-- | Prime fields are additive semigroups
instance KnownNat p => Semigroup (PrimeField p) where
  (<>) = (+)

-- | Modular inverse
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
