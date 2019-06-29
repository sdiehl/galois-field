module PrimeField
  ( PrimeField
  , toInt
  ) where

import Protolude

import GHC.Integer.GMP.Internals (recipModInteger)
import Test.Tasty.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (GaloisField(..))

-- | Prime fields @GF(p)@ for @p@ prime
newtype PrimeField (p :: Nat) = PF Integer
  deriving (Bits, Eq, Generic, NFData, Show)

-- | Prime fields are arbitrary
instance KnownNat p => Arbitrary (PrimeField p) where
  arbitrary = fromInteger <$> arbitrary

-- | Prime fields are fields
instance KnownNat p => Fractional (PrimeField p) where
  recip y@(PF x)      = PF (recipModInteger x (natVal y))
  fromRational (x:%y) = fromInteger x / fromInteger y

-- | Prime fields are Galois fields
instance KnownNat p => GaloisField (PrimeField p) where
  char = natVal

-- | Prime fields are rings
instance KnownNat p => Num (PrimeField p) where
  z@(PF x) + PF y = PF (if xy >= p then xy - p else xy)
    where
      xy = x + y
      p  = natVal z
  {-# INLINE (+) #-}
  z@(PF x) * PF y = PF (rem (x * y) (natVal z))
  z@(PF x) - PF y = PF (if xy >= 0 then xy else xy + natVal z)
    where
      xy = x - y
  {-# INLINE (-) #-}
  negate y@(PF x) = PF (if x == 0 then 0 else -x + natVal y)
  fromInteger x   = PF (if y >= 0 then y else y + p)
    where
      y = rem x p
      p = natVal (witness :: PrimeField p)
  {-# INLINE fromInteger #-}
  abs             = panic "not implemented."
  signum          = panic "not implemented."

-- | Prime fields are pretty
instance KnownNat p => Pretty (PrimeField p) where
  pretty (PF x) = pretty [x]

-- | Embed to integers
toInt :: PrimeField p -> Integer
toInt (PF x) = x
{-# INLINE toInt #-}
