module PrimeField
  ( PrimeField
  ) where

import Protolude

import Control.Monad.Random (Random(..), getRandom)
import GHC.Integer.GMP.Internals (powModInteger, recipModInteger)
import Test.Tasty.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (GaloisField(..))

-------------------------------------------------------------------------------
-- Prime field type
-------------------------------------------------------------------------------

-- | Prime fields @GF(p)@ for @p@ prime.
newtype PrimeField (p :: Nat) = PF Integer
  deriving (Bits, Eq, Generic, Integral, NFData, Ord, Read, Real, Show)

-- Prime fields are Galois fields.
instance KnownNat p => GaloisField (PrimeField p) where

  char              = natVal
  {-# INLINE char #-}

  deg               = const 1
  {-# INLINE deg #-}

  frob              = identity
  {-# INLINE frob #-}

  fromZ x           = PF (if y >= 0 then y else y + p)
    where
      y = rem x p
      p = natVal (witness :: PrimeField p)
  {-# INLINE fromZ #-}

  toZ (PF x)        = x
  {-# INLINE toZ #-}

  PF x .+. w@(PF y) = PF (if xyp >= 0 then xyp else xy)
    where
      xy  = x + y
      xyp = xy - natVal w
  {-# INLINE (.+.) #-}

  PF x .-. w@(PF y) = PF (if xy >= 0 then xy else xy + natVal w)
    where
      xy = x - y
  {-# INLINE (.-.) #-}

  PF x .*. w@(PF y) = PF (rem (x * y) (natVal w))
  {-# INLINE (.*.) #-}

  neg   (PF 0)      = PF 0
  neg w@(PF x)      = PF (natVal w - x)
  {-# INLINE neg #-}

  inv   (PF 0)      = panic "no multiplicative inverse."
  inv w@(PF x)      = PF (recipModInteger x (natVal w))
  {-# INLINE inv #-}

  pow w@(PF x) n    = PF (powModInteger x n (natVal w))
  {-# INLINE pow #-} 

  rnd               = getRandom
  {-# INLINE rnd #-}

-------------------------------------------------------------------------------
-- Prime field instances
-------------------------------------------------------------------------------

-- Prime fields are arbitrary.
instance KnownNat p => Arbitrary (PrimeField p) where
  arbitrary = fromInteger <$> arbitrary

-- Prime fields are bounded.
instance KnownNat p => Bounded (PrimeField p) where
  minBound = 0
  maxBound = -1

-- Prime fields are enumerable.
instance KnownNat p => Enum (PrimeField p) where
  fromEnum = fromIntegral . toZ
  {-# INLINABLE fromEnum #-}
  toEnum   = fromZ . fromIntegral
  {-# INLINABLE toEnum #-}

-- Prime fields are fields.
instance KnownNat p => Fractional (PrimeField p) where
  recip               = inv
  {-# INLINE recip #-}
  fromRational (x:%y) = fromZ x / fromZ y
  {-# INLINABLE fromRational #-}

-- Prime fields are rings.
instance KnownNat p => Num (PrimeField p) where
  (+)         = (.+.)
  {-# INLINE (+) #-}
  (*)         = (.*.)
  {-# INLINE (*) #-}
  (-)         = (.-.)
  {-# INLINE (-) #-}
  negate      = neg
  {-# INLINE negate #-}
  fromInteger = fromZ
  {-# INLINABLE fromInteger #-}
  abs         = panic "not implemented."
  signum      = panic "not implemented."

-- Prime fields are pretty.
instance KnownNat p => Pretty (PrimeField p) where
  pretty (PF x) = pretty x

-- Prime fields are random.
instance KnownNat p => Random (PrimeField p) where
  random  = first PF . randomR (0, natVal (witness :: PrimeField p) - 1)
  {-# INLINE random #-}
  randomR = panic "not implemented."
