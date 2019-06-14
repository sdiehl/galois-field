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

-- | Prime fields are equatable
instance KnownNat p => Eq (PrimeField p) where
  (==) = (. toInt) . (==) . toInt

-- | Prime fields are fields
instance KnownNat p => Fractional (PrimeField p) where
  fromRational (a :% b) = fromInt a / fromInt b
  {-# INLINE recip #-}
  recip a               = case modInv (toInt a) (natVal a) of
    Just n -> fromInt n
    _      -> panic "no multiplicative inverse."

-- | Prime fields are Galois fields
instance KnownNat p => GaloisField (PrimeField p) where
  char = natVal

-- | Prime fields are rings
instance KnownNat p => Num (PrimeField p) where
  a + b       = fromInt $ toInt a + toInt b
  a * b       = fromInt $ toInt a * toInt b
  abs a       = a
  signum a    = if a == 0 then 0 else 1
  fromInteger = fromInt
  negate      = fromInt . negate . toInt

-- | Conversion from integer
fromInt :: KnownNat p => Integer -> PrimeField p
fromInt n = fix $ PF . mod n . natVal

-- | Conversion to integer
toInt :: KnownNat p => PrimeField p -> Integer
toInt a@(PF n) = mod n $ natVal a

{-# INLINABLE modInv #-}
-- | Modular inverse
modInv :: Integral a => a -> a -> Maybe a
modInv x p = case extGCD p x of
  (1, (y, _)) -> Just y
  _           -> Nothing
  where
    extGCD :: Integral a => a -> a -> (a, (a, a))
    extGCD y 0 = (y, (0, 1))
    extGCD y x = (g, (t - s * q, s))
      where
        (q, r)      = quotRem y x
        (g, (s, t)) = extGCD x r
