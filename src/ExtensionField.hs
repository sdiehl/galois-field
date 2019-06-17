module ExtensionField
  ( ExtensionField(..)
  , IrreducibleMonic(..)
  , d
  , fromPoly
  , x
  ) where

import Protolude

import GaloisField (GaloisField(..))
import PolynomialRing (Polynomial(..), getPoly, polyDiv, polyInv)

-- | Extension fields @GF(p^q)[X]/<f(X)>@ for @p@ prime, @q@ non-negative, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@
newtype ExtensionField k im = EF (Polynomial k)
  deriving (Generic, NFData, Show)

-- | Irreducible monic splitting polynomial of extension field
class IrreducibleMonic k im where
  split :: (k, im) -> Polynomial k

-- | Extension fields are equatable
instance (GaloisField k, IrreducibleMonic k im)
  => Eq (ExtensionField k im) where
  (==) = (. toPoly) . (==) . toPoly

-- | Extension fields are fields
instance (GaloisField k, IrreducibleMonic k im)
  => Fractional (ExtensionField k im) where
  recip x             = case polyInv (toPoly x) (split (witness :: (k, im))) of
    Just y -> fromPoly y
    _      -> panic "no multiplicative inverse."
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y

-- | Extension fields are Galois fields
instance (GaloisField k, IrreducibleMonic k im)
  => GaloisField (ExtensionField k im) where
  char = const (char (witness :: k))

-- | Extension fields are rings
instance (GaloisField k, IrreducibleMonic k im)
  => Num (ExtensionField k im) where
  x + y       = fromPoly (toPoly x + toPoly y)
  x * y       = fromPoly (toPoly x * toPoly y)
  x - y       = fromPoly (toPoly x - toPoly y)
  negate      = fromPoly . negate . toPoly
  fromInteger = fromPoly . getPoly . return . fromInteger
  abs         = panic "not implemented."
  signum      = panic "not implemented."

-- | Conversion from polynomial
fromPoly :: forall k im . (GaloisField k, IrreducibleMonic k im)
  => Polynomial k -> ExtensionField k im
fromPoly x = EF (snd (polyDiv x (split (witness :: (k, im)))))
{-# INLINE fromPoly #-}

-- | Conversion to polynomial
toPoly :: forall k im . (GaloisField k, IrreducibleMonic k im)
  => ExtensionField k im -> Polynomial k
toPoly (EF x) = snd (polyDiv x (split (witness :: (k, im))))
{-# INLINE toPoly #-}

-- | Indeterminate variable
x :: GaloisField k => Polynomial k
x = X [0, 1]
{-# INLINE x #-}

-- | Descend variables
d :: (GaloisField k, IrreducibleMonic k im)
  => Polynomial k -> Polynomial (ExtensionField k im)
d = X . return . fromPoly
{-# INLINE d #-}
