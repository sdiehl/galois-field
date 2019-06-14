{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module ExtensionField
  ( ExtensionField(..)
  , IrreducibleMonic(..)
  , Poly(..)
  ) where

import Protolude

import GaloisField (GaloisField(..))
import PrimeField (PrimeField(..))
import PolynomialRing (Polynomial(..), getPoly, polyDiv, polyInv)

-- | Extension fields @GF(p^q)[X]/<f(X)>@ for @p@ prime, @q@ non-negative, and
-- @f(X)@ irreducible monic in @GF(p)[X]@
newtype ExtensionField k (ps :: [Nat]) = EF (Polynomial k)
  deriving (Show, Generic, NFData)

-- | Irreducible monic splitting polynomial of extension fields
newtype Poly (ns :: [Nat]) = Poly {poly :: [Integer]}
class IrreducibleMonic (ns :: [Nat])
  where splitting :: Poly ns

-- | Extension fields are equatable
instance (GaloisField k, IrreducibleMonic ps)
  => Eq (ExtensionField k ps) where
  (==) = (. toPoly) . (==) . toPoly

-- | Extension fields are fields
instance (GaloisField k, IrreducibleMonic ps)
  => Fractional (ExtensionField k ps) where
  fromRational (a :% b) = fromInteger a / fromInteger b
  {-# INLINE recip #-}
  recip a               = case polyInv (toPoly a) (polyVal a) of
    Just f -> fromPoly f
    _      -> panic "no multiplicative inverse."

-- | Extension fields are Galois fields
instance (GaloisField k, IrreducibleMonic ps)
  => GaloisField (ExtensionField k ps) where
  char = const $ char (undefined :: k) -- TODO

-- | Extension fields are rings
instance (GaloisField k, IrreducibleMonic ps)
  => Num (ExtensionField k ps) where
  a + b       = fromPoly $ toPoly a + toPoly b
  a * b       = fromPoly $ toPoly a * toPoly b
  abs a       = a
  signum a    = if a == 0 then 0 else 1
  fromInteger = fromPoly . getPoly . return . fromInteger
  negate      = fromPoly . negate . toPoly

-- | Get splitting polynomial
polyVal :: forall k ps . (GaloisField k, IrreducibleMonic ps)
  => ExtensionField k ps -> Polynomial k
polyVal = X . (++ [1]) . map fromInteger . const (poly (splitting :: Poly ps))

-- | Conversion from polynomial
fromPoly :: (GaloisField k, IrreducibleMonic ps)
  => Polynomial k -> ExtensionField k ps
fromPoly f = fix $ EF . snd . polyDiv f . polyVal

-- | Conversion to polynomial
toPoly :: (GaloisField k, IrreducibleMonic ps)
  => ExtensionField k ps -> Polynomial k
toPoly a@(EF f) = snd . polyDiv f $ polyVal a
