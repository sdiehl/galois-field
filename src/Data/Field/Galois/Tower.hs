{-# LANGUAGE UndecidableInstances #-}

module Data.Field.Galois.Tower
  ( TowerOfFields(..)
  , (*^)
  ) where

import Protolude

import Data.Field.Galois.Base (GaloisField)
import Data.Field.Galois.Prime (Prime, fromP)
import Data.Field.Galois.Extension (Extension, IrreducibleMonic, pattern V)
import Data.Field.Galois.Binary (Binary, toB')

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Tower of fields @L@ over @K@ strict partial ordering.
class (GaloisField k, GaloisField l) => TowerOfFields k l where
  {-# MINIMAL embed #-}
  -- | Embed @K@ into @L@ naturally.
  embed :: k -> l

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Prime field towers are reflexive.
instance KnownNat p => TowerOfFields (Prime p) (Prime p) where
  embed = identity
  {-# INLINABLE embed #-}

-- Extension field towers are reflexive.
instance IrreducibleMonic k p => TowerOfFields (Extension k p) (Extension k p) where
  embed = identity
  {-# INLINABLE embed #-}

-- Extension fields are towers of fields.
instance {-# OVERLAPPING #-} IrreducibleMonic k p => TowerOfFields k (Extension k p) where
  embed = V
  {-# INLINABLE embed #-}

-- Extension field towers are transitive.
instance {-# OVERLAPPABLE #-} (TowerOfFields k l, IrreducibleMonic l p, TowerOfFields l (Extension l p))
  => TowerOfFields k (Extension l p) where
  embed = embed . (embed :: k -> l)
  {-# INLINABLE embed #-}

-- Binary field towers are reflexive.
instance KnownNat p => TowerOfFields (Binary p) (Binary p) where
  embed = identity
  {-# INLINABLE embed #-}

-- Binary fields are towers of fields.
instance KnownNat p => TowerOfFields (Prime 2) (Binary p) where
  embed = toB' . fromP
  {-# INLINABLE embed #-}

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Scalar multiplication.
infixl 7 *^
(*^) :: TowerOfFields k l => k -> l -> l
(*^) = (*) . embed
{-# INLINE (*^) #-}
