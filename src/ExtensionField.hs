{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module ExtensionField
  ( ExtensionField(..)
  ) where

import Protolude

import GaloisField (GaloisField(..))
import PrimeField (PrimeField(..))
import PolynomialRing (R(..), division, euclid)

-- | Analogue of KnownNat and natVal
class KnownNats (ns :: [Nat]) where natsSing :: SNats ns
newtype SNats (ns :: [Nat]) = SNats [Integer]
natsVal :: forall ns proxy . KnownNats ns => proxy ns -> [Integer]
natsVal _ = case natsSing :: SNats ns of SNats xs -> xs

-- | Extension fields @GF(p^q)[X]/<f(X)>@ for @p@ prime, @q@ non-negative, and
-- @f(X)@ monic irreducible in @GF(p)[X]@
newtype ExtensionField k (ps :: [Nat]) = EF (R k)
  deriving (Show, Generic, NFData)

-- | Extension fields are equatable
instance (KnownNats ps, GaloisField k) => Eq (ExtensionField k ps) where
  (==) = (. toPoly) . (==) . toPoly

-- | Extension fields are fields
instance (KnownNats ps, GaloisField k) => Fractional (ExtensionField k ps) where
  fromRational (a :% b) = fromInteger a / fromInteger b
  {-# INLINE recip #-}
  recip a               = case euclid (toPoly a) (getPoly a) of
    (R [_], (f, _)) -> fromPoly f
    _               -> panic "no multiplicative inverse."

-- | Extension fields are Galois fields
instance (KnownNats ps, GaloisField k) => GaloisField (ExtensionField k ps) where
  char = const $ char (undefined :: k) -- TODO

-- | Extension fields are rings
instance (KnownNats ps, GaloisField k) => Num (ExtensionField k ps) where
  a + b       = fromPoly $ toPoly a + toPoly b
  a * b       = fromPoly $ toPoly a * toPoly b
  abs a       = a
  signum a    = if a == 0 then 0 else 1
  fromInteger = fromPoly . R . return . fromInteger
  negate      = fromPoly . negate . toPoly

-- | Monic irreducible polynomial
getPoly :: (KnownNats ps, GaloisField k) => ExtensionField k ps -> R k
getPoly = R . map fromInteger . natsVal

-- | Conversion from polynomial
fromPoly :: (KnownNats ps, GaloisField k) => R k -> ExtensionField k ps
fromPoly f = fix $ EF . snd . division f . getPoly

-- | Conversion to polynomial
toPoly :: (KnownNats ps, GaloisField k) => ExtensionField k ps -> R k
toPoly a@(EF f) = snd . division f $ getPoly a
