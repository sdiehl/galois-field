{-# LANGUAGE UndecidableInstances #-}

module Data.Field.Galois.Unity
  ( Cyclic(..)
  , RootsOfUnity
  , Subgroup(..)
  ) where

import Protolude hiding (natVal)

import Control.Monad.Random (Random(..))
import Data.Group as G (Group(..))
import GHC.Natural (Natural, naturalToInteger)
import GHC.TypeNats (natVal)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Field.Galois.Base as F (GaloisField(..))
import Data.Field.Galois.Prime (Prime)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Subgroups of finite groups.
class Group g => Subgroup g where
  {-# MINIMAL card, cof, def, prim #-}
  -- | Cardinality of subgroup.
  card :: g -> Natural
  -- | Cofactor of subgroup in group.
  cof :: g -> Natural
  -- | Check group element in subgroup.
  def :: g -> Bool
  -- | Check primitive generator of subgroup.
  prim :: g -> Bool

-- | Finite cyclic groups.
class Group g => Cyclic g where
  {-# MINIMAL gen #-}
  -- | Generator of subgroup.
  gen :: g

-- | @n@-th roots of unity of Galois fields.
newtype RootsOfUnity (n :: Nat) k = U k
  deriving (Bits, Eq, Functor, Generic, NFData, Ord, Show)

-------------------------------------------------------------------------------
-- Instances
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

-- Roots of unity are pretty.
instance (KnownNat n, GaloisField k) => Pretty (RootsOfUnity n k) where
  pretty (U x) = pretty x

-- Roots of unity are semigroups.
instance (KnownNat n, GaloisField k) => Semigroup (RootsOfUnity n k) where
  U x <> U y = U $ x * y
  {-# INLINABLE (<>) #-}

-- Roots of unity are subgroups.
instance (KnownNat n, GaloisField k) => Subgroup (RootsOfUnity n k) where
  cof          = quot (order (witness :: k)) . card
  {-# INLINABLE cof #-}
  def u@(U x)  = powerOfUnity x $ card u
  {-# INLINABLE def #-}
  card         = const $ natVal (witness :: Prime n)
  {-# INLINABLE card #-}
  prim u@(U x) = (&&) (def u) $ not $ any (powerOfUnity x) [1 .. card u - 1]
  {-# INLINABLE prim #-}

-- Roots of unity cyclic subgroups are random.
instance (KnownNat n, GaloisField k, Cyclic (RootsOfUnity n k),
          Group (RootsOfUnity n k), Subgroup (RootsOfUnity n k)) => Random (RootsOfUnity n k) where
  random  = first (G.pow gen) . randomR (0, naturalToInteger $ order (witness :: k) - 1)
  {-# INLINABLE random #-}
  randomR = panic "Unity.randomR: not implemented."

-- Check power of unity.
powerOfUnity :: (Integral n, GaloisField k) => k -> n -> Bool
powerOfUnity = ((==) 1 .) . F.pow
{-# INLINABLE powerOfUnity #-}
