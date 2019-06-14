module ExtensionField
  ( ExtensionField(..)
  , IrreducibleMonic(..)
  , x
  ) where

import Protolude

import GaloisField (GaloisField(..))
import PrimeField (PrimeField(..))
import PolynomialRing (Polynomial(..), getPoly, polyDiv, polyInv)

-- | Extension fields @GF(p^q)[X]/<f(X)>@ for @p@ prime, @q@ non-negative, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@
newtype ExtensionField k im = EF (Polynomial k)
  deriving (Show, Generic, NFData)

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
  fromRational (a :% b) = fromInteger a / fromInteger b
  {-# INLINE recip #-}
  recip a               = case polyInv (toPoly a) (split (fix(\x->x)::(k,im))) of
    Just f -> fromPoly f
    _      -> panic "no multiplicative inverse."

-- | Extension fields are Galois fields
instance (GaloisField k, IrreducibleMonic k im)
  => GaloisField (ExtensionField k im) where
  char = const $ char (fix(\x->x)::k)

-- | Extension fields are rings
instance (GaloisField k, IrreducibleMonic k im)
  => Num (ExtensionField k im) where
  a + b       = fromPoly $ toPoly a + toPoly b
  a * b       = fromPoly $ toPoly a * toPoly b
  abs a       = a
  signum a    = if a == 0 then 0 else 1
  fromInteger = fromPoly . getPoly . return . fromInteger
  negate      = fromPoly . negate . toPoly

-- | Conversion from polynomial
fromPoly :: forall k im . (GaloisField k, IrreducibleMonic k im)
  => Polynomial k -> ExtensionField k im
fromPoly f = fix $ EF . snd . polyDiv f . const (split (fix(\x->x)::(k,im)))

-- | Conversion to polynomial
toPoly :: forall k im . (GaloisField k, IrreducibleMonic k im)
  => ExtensionField k im -> Polynomial k
toPoly (EF f) = snd . polyDiv f $ split (fix(\x->x)::(k,im))

-- | Indeterminate variable X
x :: GaloisField k => Polynomial k
x = X [0, 1]
