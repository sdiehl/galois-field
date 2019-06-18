module ExtensionField
  ( ExtensionField
  , IrreducibleMonic(..)
  , extend
  , extract
  , t
  , x
  ) where

import Protolude

import GaloisField (GaloisField(..))
import PolynomialRing (Polynomial(..), polyDiv, polyInv)

-- | Extension fields @GF(p^q)[X]/<f(X)>@ for @p@ prime, @q@ non-negative, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@
newtype ExtensionField k im = EF (Polynomial k)
  deriving (Eq, Generic, NFData, Show)

-- | Irreducible monic splitting polynomial of extension field
class IrreducibleMonic k im where
  split :: (k, im) -> Polynomial k

-- | Extension fields are fields
instance (GaloisField k, IrreducibleMonic k im)
  => Fractional (ExtensionField k im) where
  recip (EF x)        = case polyInv x (split (witness :: (k, im))) of
    Just y -> extend y
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
  EF x + EF y   = extend (x + y)
  EF x * EF y   = extend (snd (polyDiv (x * y) (split (witness :: (k, im)))))
  EF x - EF y   = extend (x - y)
  negate (EF x) = extend (-x)
  fromInteger   = extend . fromInteger
  abs           = panic "not implemented."
  signum        = panic "not implemented."

-- | Construct field extension
extend :: Polynomial k -> ExtensionField k im
extend = EF
{-# INLINE extend #-}

-- | Extract field coefficients
extract :: ExtensionField k im -> [k]
extract (EF (X ks)) = ks
{-# INLINE extract #-}

-- | Current indeterminate variable
x :: GaloisField k => Polynomial k
x = X [0, 1]
{-# INLINE x #-}

-- | Descend variable tower
t :: Polynomial k -> Polynomial (ExtensionField k im)
t = X . return . EF
{-# INLINE t #-}
