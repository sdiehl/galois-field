module ExtensionField
  ( ExtensionField
  , IrreducibleMonic(..)
  , fromField
  , fromList
  , t
  , x
  ) where

import Protolude

import Test.Tasty.QuickCheck (Arbitrary(..), choose, sized)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (GaloisField(..))
import PolynomialRing (Polynomial(..), cut, polyInv, polyMul, polyQR)

-- | Extension fields @GF(p^q)[X]/<f(X)>@ for @p@ prime, @q@ positive, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@
newtype ExtensionField k im = EF (Polynomial k)
  deriving (Eq, Generic, NFData, Show)

-- | Irreducible monic splitting polynomial of extension field
class IrreducibleMonic k im where
  split :: (k, im) -> Polynomial k

-- | Extension fields are arbitrary
instance (Arbitrary k, GaloisField k, IrreducibleMonic k im)
  => Arbitrary (ExtensionField k im) where
  arbitrary = fromList <$> sized (const poly)
    where
      poly = choose (1, length xs - 1) >>= mapM (const arbitrary) . enumFromTo 1
        where
          X xs = split (witness :: (k, im))

-- | Extension fields are fields
instance (GaloisField k, IrreducibleMonic k im)
  => Fractional (ExtensionField k im) where
  recip (EF (X ys))   = case polyInv ys xs of
    Just zs -> EF (X zs)
    _       -> panic "no multiplicative inverse."
    where
      X xs = split (witness :: (k, im))
  {-# INLINE recip #-}
  fromRational (y:%z) = fromInteger y / fromInteger z
  {-# INLINABLE fromRational #-}

-- | Extension fields are Galois fields
instance (GaloisField k, IrreducibleMonic k im)
  => GaloisField (ExtensionField k im) where
  char = const (char (witness :: k))

-- | Extension fields are rings
instance (GaloisField k, IrreducibleMonic k im)
  => Num (ExtensionField k im) where
  EF y + EF z           = EF (y + z)
  {-# INLINE (+) #-}
  EF (X ys) * EF (X zs) = EF (X (snd (polyQR (polyMul ys zs) xs)))
    where
      X xs = split (witness :: (k, im))
  {-# INLINE (*) #-}
  EF y - EF z           = EF (y - z)
  {-# INLINE (-) #-}
  negate (EF y)         = EF (-y)
  {-# INLINE negate #-}
  fromInteger           = EF . fromInteger
  {-# INLINABLE fromInteger #-}
  abs                   = panic "not implemented."
  signum                = panic "not implemented."

-- | Extension fields are pretty
instance (GaloisField k, IrreducibleMonic k im)
  => Pretty (ExtensionField k im) where
  pretty (EF y) = pretty y

-- | List from field
fromField :: ExtensionField k im -> [k]
fromField (EF (X ks)) = ks
{-# INLINABLE fromField #-}

-- | Field from list
fromList :: forall k im . (GaloisField k, IrreducibleMonic k im)
  => [k] -> ExtensionField k im
fromList = EF . X . snd . flip polyQR xs . cut
  where
    X xs = split (witness :: (k, im))
{-# INLINABLE fromList #-}

-- | Current indeterminate variable
x :: GaloisField k => Polynomial k
x = X [0, 1]
{-# INLINE x #-}

-- | Descend variable tower
t :: Polynomial k -> Polynomial (ExtensionField k im)
t = X . return . EF
{-# INLINE t #-}
