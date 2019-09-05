{-# LANGUAGE UndecidableInstances #-}

module Data.Field.Galois.Tower
  ( TowerOfFields(..)
  , (*^)
  ) where

import Protolude

import Data.Field.Galois.Base (GaloisField)
import Data.Field.Galois.Prime (Prime, fromP)
import Data.Field.Galois.Extension (Extension, IrreducibleMonic, toE')
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
instance IrreducibleMonic k im => TowerOfFields (Extension k im) (Extension k im) where
  embed = identity
  {-# INLINABLE embed #-}

-- Extension fields are towers of fields.
instance {-# OVERLAPPING #-} IrreducibleMonic k im => TowerOfFields k (Extension k im) where
  embed = toE' . return
  {-# INLINABLE embed #-}

-- Extension field towers are transitive.
instance {-# OVERLAPPABLE #-} (TowerOfFields k l, IrreducibleMonic l im, TowerOfFields l (Extension l im))
  => TowerOfFields k (Extension l im) where
  embed = embed . (embed :: k -> l)
  {-# INLINABLE embed #-}

-- Binary field towers are reflexive.
instance KnownNat im => TowerOfFields (Binary im) (Binary im) where
  embed = identity
  {-# INLINABLE embed #-}

-- Binary fields are towers of fields.
instance KnownNat im => TowerOfFields (Prime 2) (Binary im) where
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
