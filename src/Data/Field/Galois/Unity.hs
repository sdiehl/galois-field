module Data.Field.Galois.Unity
  ( RootsOfUnity
  , rootOfUnity
  , primitiveRootOfUnity
  ) where

import Protolude

import Data.Group as G (Group(..))

import Data.Field.Galois.Base as F (GaloisField(..))
import Data.Field.Galois.Prime (Prime)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | @n@-th roots of unity of Galois fields.
newtype RootsOfUnity (n :: Nat) k = U k
  deriving (Bits, Eq, Functor, Generic, NFData, Ord, Show)

-- Roots of unity are groups.
instance (KnownNat n, GaloisField k) => Group (RootsOfUnity n k) where
  invert (U x) = U $ recip x
  {-# INLINABLE invert #-}
  pow (U x) n = U $ F.pow x n
  {-# INLINABLE pow #-}

-- Roots of unity are monoids.
instance (KnownNat n, GaloisField k) => Monoid (RootsOfUnity n k) where
  mempty = U 1
  {-# INLINABLE mempty #-}

-- Roots of unity are semigroups.
instance (KnownNat n, GaloisField k) => Semigroup (RootsOfUnity n k) where
  U x <> U y = U $ x * y
  {-# INLINABLE (<>) #-}

-- Roots of unity are pretty.
instance (KnownNat n, GaloisField k) => Pretty (RootsOfUnity n k) where
  pretty (U x) = pretty x

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- Check power of unity.
powerOfUnity :: (Integral n, GaloisField k) => k -> n -> Bool
powerOfUnity = ((==) 1 .) . F.pow
{-# INLINABLE powerOfUnity #-}

-- | Check root of unity.
rootOfUnity :: forall n k . (KnownNat n, GaloisField k) => RootsOfUnity n k -> Bool
rootOfUnity (U x) = powerOfUnity x $ natVal (witness :: Prime n)
{-# INLINABLE rootOfUnity #-}

-- | Check primitive root of unity.
primitiveRootOfUnity :: forall n k . (KnownNat n, GaloisField k) => RootsOfUnity n k -> Bool
primitiveRootOfUnity (U x) = powerOfUnity x n && not (any (powerOfUnity x) [1 .. n - 1])
  where
    n = natVal (witness :: Prime n)
{-# INLINABLE primitiveRootOfUnity #-}
