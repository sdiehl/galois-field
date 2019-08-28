module Data.Field.Galois.Unity
  ( RootsOfUnity
  , Subgroup(..)
  ) where

import Protolude

import Data.Group as G (Group(..))

import Data.Field.Galois.Base as F (GaloisField(..))
import Data.Field.Galois.Prime (Prime)

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Subgroups of groups.
class Group g => Subgroup g where
  {-# MINIMAL def #-}
  -- | Check if element is well-defined in subgroup.
  def :: g -> Bool

-- | @n@-th roots of unity of Galois fields.
newtype RootsOfUnity (n :: Nat) k = U k
  deriving (Bits, Eq, Functor, Generic, NFData, Ord, Show)

-------------------------------------------------------------------------------
-- Group instances
-------------------------------------------------------------------------------

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

-- Roots of unity are subgroups.
instance (KnownNat n, GaloisField k) => Subgroup (RootsOfUnity n k) where

  def = (==) mempty . flip G.pow (natVal (witness :: Prime n))
  {-# INLINABLE def #-}
