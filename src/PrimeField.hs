{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module PrimeField
  ( PrimeField(..)
  ) where

import Protolude

import GaloisField (GaloisField(..))

-- | Prime fields @GF(p)@ for @p@ prime
newtype PrimeField (p :: Nat) = PF Integer
  deriving (Show, Generic, NFData)

-- | Prime fields are Galois fields
instance KnownNat p => GaloisField (PrimeField p) where
  char = natVal

-- | Prime fields are equatable
instance KnownNat p => Eq (PrimeField p) where
  (==) = (. toInt) . (==) . toInt

-- | Prime fields are fields
instance KnownNat p => Fractional (PrimeField p) where
  fromRational (a :% b) = fromInteger a / fromInteger b
  {-# INLINE recip #-}
  recip f               = case modInv (toInt f) (natVal f) of
    Right n' -> fromIntegral n'
    Left n'  -> panic "no multiplicative inverse."
instance KnownNat p => Num (PrimeField p) where
  PF n + PF n'  = fromInteger $ n + n'
  PF n * PF n'  = fromInteger $ n * n'
  abs f         = f
  signum f      = if f == 0 then 0 else 1
  fromInteger n = fix $ PF . mod n . natVal
  negate (PF n) = fromInteger $ negate n

-- | Conversion to integer
toInt :: KnownNat p => PrimeField p -> Integer
toInt f@(PF n) = mod n $ natVal f

{-# INLINABLE modInv #-}
-- | Modular inverse
modInv :: Integral a => a -> a -> Either a a
modInv x p = let (g, (y, _)) = extGCD p x in if g == 1 then Right y else Left g
  where
    extGCD :: Integral a => a -> a -> (a, (a, a))
    extGCD y 0 = (y, (0, 1))
    extGCD y x = (g, (t - s * q, s))
      where
        (q, r)      = quotRem y x
        (g, (s, t)) = extGCD x r
