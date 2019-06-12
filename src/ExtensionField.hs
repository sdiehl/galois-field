{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module ExtensionField
  () where

import Protolude

import GaloisField (GaloisField(..))
import PrimeField (PrimeField(..))

-- | Extension fields @GF(p^q)[X]/<f(X)>@ for @p@ prime, @q@ non-negative, and
-- @f(X)@ monic irreducible
data ExtensionField k = EF [k]
  deriving (Show, Generic)
instance NFData (ExtensionField k) where
  rnf (EF ks) = foldr seq () ks

-- | Extension fields are Galois fields
instance GaloisField k => GaloisField (ExtensionField k) where
  ch = notImplemented

-- | Extension fields are bounded
instance GaloisField k => Bounded (ExtensionField k) where
  minBound = notImplemented
  maxBound = notImplemented

-- | Extension fields are equatable
instance GaloisField k => Eq (ExtensionField k) where
  (==) = notImplemented

-- | Extension fields are fields
instance GaloisField k => Fractional (ExtensionField k) where
  fromRational = notImplemented
  recip        = notImplemented
instance GaloisField k => Num (ExtensionField k) where
  (+)         = notImplemented
  (*)         = notImplemented
  abs         = notImplemented
  signum      = notImplemented
  fromInteger = notImplemented
  negate      = notImplemented

-- | Extension fields are integral domains
instance GaloisField k => Integral (ExtensionField k) where
  quotRem   = notImplemented
  toInteger = notImplemented
instance GaloisField k => Enum (ExtensionField k) where
  toEnum   = notImplemented
  fromEnum = notImplemented
instance GaloisField k => Real (ExtensionField k) where
  toRational = notImplemented
instance GaloisField k => Ord (ExtensionField k) where
  (<=) = notImplemented
