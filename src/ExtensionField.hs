module ExtensionField
  ( ExtensionField
  , IrreducibleMonic(..)
  , fromField
  , fromList
  , t
  , x
  ) where

import Protolude

import GaloisField (GaloisField(..))
import PolynomialRing (Polynomial(..), polyDiv, polyInv, toPoly)

-- | Extension fields @GF(p^q)[X]/<f(X)>@ for @p@ prime, @q@ positive, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@
newtype ExtensionField k im = EF (Polynomial k)
  deriving (Eq, Generic, NFData, Show)

-- | Irreducible monic splitting polynomial of extension field
class IrreducibleMonic k im where
  split :: (k, im) -> Polynomial k

-- | Extension fields are fields
instance (GaloisField k, IrreducibleMonic k im)
  => Fractional (ExtensionField k im) where
  recip (EF y)        = case polyInv y (split (witness :: (k, im))) of
    Just z -> EF z
    _      -> panic "no multiplicative inverse."
  {-# INLINE recip #-}
  fromRational (y:%z) = fromInteger y / fromInteger z

-- | Extension fields are Galois fields
instance (GaloisField k, IrreducibleMonic k im)
  => GaloisField (ExtensionField k im) where
  char = const (char (witness :: k))

-- | Extension fields are rings
instance (GaloisField k, IrreducibleMonic k im)
  => Num (ExtensionField k im) where
  EF y + EF z   = EF (y + z)
  EF y * EF z   = EF (snd (polyDiv (y * z) (split (witness :: (k, im)))))
  EF y - EF z   = EF (y - z)
  negate (EF y) = EF (-y)
  fromInteger   = EF . fromInteger
  abs           = panic "not implemented."
  signum        = panic "not implemented."

-- | List from field
fromField :: ExtensionField k im -> [k]
fromField (EF (X ks)) = ks
{-# INLINE fromField #-}

-- | Field from list
fromList :: forall k im . (GaloisField k, IrreducibleMonic k im)
  => [k] -> ExtensionField k im
fromList = EF . snd . flip polyDiv (split (witness :: (k, im))) . toPoly
{-# INLINE fromList #-}

-- | Current indeterminate variable
x :: GaloisField k => Polynomial k
x = X [0, 1]
{-# INLINE x #-}

-- | Descend variable tower
t :: Polynomial k -> Polynomial (ExtensionField k im)
t = X . return . EF
{-# INLINE t #-}
