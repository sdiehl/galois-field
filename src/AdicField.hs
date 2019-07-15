module AdicField
  ( AdicField
  ) where

import Protolude

import Control.Monad.Random (Random(..), getRandom)
import Test.Tasty.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (GaloisField(..))

-------------------------------------------------------------------------------
-- Adic field type
-------------------------------------------------------------------------------

-- | Adic fields @GF(p^q)[X]/\<f(X)\>@ for @p@ prime, @q@ positive, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@.
newtype AdicField k (im :: Nat) = EF Integer
  deriving (Eq, Generic, Integral, NFData, Ord, Read, Real, Show)

-- Adic fields are Galois fields.
instance (GaloisField k, KnownNat im) => GaloisField (AdicField k im) where

  char       = const (char (witness :: k))
  {-# INLINE char #-}

  deg        = adicDeg
  {-# INLINE deg #-}

  frob       = pow <*> char
  {-# INLINE frob #-}

  fromZ      = adicMod
  {-# INLINE fromZ #-}

  toZ (EF x) = x
  {-# INLINE toZ #-}

  (.+.)      = adicAdd
  {-# INLINE (.+.) #-}

  (.-.)      = adicSub
  {-# INLINE (.-.) #-}

  (.*.)      = adicMul
  {-# INLINE (.*.) #-}

  neg        = adicNeg
  {-# INLINE neg #-}

  inv        = adicInv
  {-# INLINE inv #-}

  pow        = (^)
  {-# INLINE pow #-}

  rnd        = getRandom
  {-# INLINE rnd #-}

-------------------------------------------------------------------------------
-- Adic field instances
-------------------------------------------------------------------------------

-- Adic fields are arbitrary.
instance (GaloisField k, KnownNat im) => Arbitrary (AdicField k im) where
  arbitrary = EF <$> choose (0, order (witness :: AdicField k im) - 1)

-- Adic fields are bounded.
instance (GaloisField k, KnownNat im) => Bounded (AdicField k im) where
  minBound = 0
  maxBound = -1

-- Adic fields are enumerable.
instance (GaloisField k, KnownNat im) => Enum (AdicField k im) where
  fromEnum = fromIntegral . toZ
  {-# INLINABLE fromEnum #-}
  toEnum   = fromZ . fromIntegral
  {-# INLINABLE toEnum #-}

-- Adic fields are fields.
instance (GaloisField k, KnownNat im) => Fractional (AdicField k im) where
  recip               = inv
  {-# INLINE recip #-}
  fromRational (x:%y) = fromZ x / fromZ y
  {-# INLINABLE fromRational #-}

-- Adic fields are rings.
instance (GaloisField k, KnownNat im) => Num (AdicField k im) where
  (+)         = (.+.)
  {-# INLINE (+) #-}
  (*)         = (.*.)
  {-# INLINE (*) #-}
  (-)         = (.-.)
  {-# INLINE (-) #-}
  negate      = neg
  {-# INLINE negate #-}
  fromInteger = fromZ
  abs         = panic "not implemented."
  signum      = panic "not implemented."

-- Adic fields are pretty.
instance (GaloisField k, KnownNat im) => Pretty (AdicField k im) where
  pretty (EF x) = pretty x

-- Adic fields are random.
instance (GaloisField k, KnownNat im) => Random (AdicField k im) where
  random  = first EF . randomR (0, order (witness :: AdicField k im) - 1)
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Adic field arithmetic
-------------------------------------------------------------------------------

-- | Adic field degree.
adicDeg :: (GaloisField k, KnownNat im)
  => AdicField k im -> Int
adicDeg = notImplemented
{-# INLINE adicDeg #-}

-- | Adic field addition.
adicAdd :: (GaloisField k, KnownNat im)
  => AdicField k im -> AdicField k im -> AdicField k im
adicAdd = notImplemented
{-# INLINE adicAdd #-}

-- | Adic field subtraction.
adicSub :: (GaloisField k, KnownNat im)
  => AdicField k im -> AdicField k im -> AdicField k im
adicSub = notImplemented
{-# INLINE adicSub #-}

-- | Adic field multiplication.
adicMul :: (GaloisField k, KnownNat im)
  => AdicField k im -> AdicField k im -> AdicField k im
adicMul = notImplemented
{-# INLINE adicMul #-}

-- | Adic field negation.
adicNeg :: (GaloisField k, KnownNat im)
  => AdicField k im -> AdicField k im
adicNeg = notImplemented
{-# INLINE adicNeg #-}

-- | Adic field modulus.
adicMod :: (GaloisField k, KnownNat im)
  => Integer -> AdicField k im
adicMod = notImplemented
{-# INLINE adicMod #-}

-- | Adic field inversion.
adicInv :: (GaloisField k, KnownNat im)
  => AdicField k im -> AdicField k im
adicInv = notImplemented
{-# INLINE adicInv #-}
