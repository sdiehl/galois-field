module PrimeField
  ( PrimeField(..)
  ) where

import Protolude hiding (toInteger)

import GaloisField (GaloisField(..))

-- | Prime fields @GF(p)@ for @p@ prime
newtype PrimeField (p :: Nat) = PF Integer
  deriving (Show, Generic, NFData)

-- | Prime fields are equatable
instance KnownNat p => Eq (PrimeField p) where
  (==) = (. toInteger) . (==) . toInteger

-- | Prime fields are fields
instance KnownNat p => Fractional (PrimeField p) where
  fromRational (a :% b) = fromInteger a / fromInteger b
  {-# INLINE recip #-}
  recip a               = case modInv (toInteger a) (natVal a) of
    Just n -> fromInteger n
    _      -> panic "no multiplicative inverse."

-- | Prime fields are Galois fields
instance KnownNat p => GaloisField (PrimeField p) where
  char = natVal

-- | Prime fields are rings
instance KnownNat p => Num (PrimeField p) where
  a + b         = fromInteger $ toInteger a + toInteger b
  a * b         = fromInteger $ toInteger a * toInteger b
  abs a         = a
  signum a      = if a == 0 then 0 else 1
  fromInteger n = fix $ PF . mod n . natVal
  negate        = fromInteger . negate . toInteger

-- | Conversion to integer
toInteger :: KnownNat p => PrimeField p -> Integer
toInteger a@(PF n) = mod n $ natVal a

{-# INLINE modInv #-}
-- | Modular inverse
modInv :: forall a . Integral a => a -> a -> Maybe a
modInv x p = case extGCD p x of
  (1, (y, _)) -> Just y
  _           -> Nothing
  where
    extGCD :: a -> a -> (a, (a, a))
    extGCD y 0 = (y, (0, 1))
    extGCD y x = (g, (t - s * q, s))
      where
        (q, r)      = quotRem y x
        (g, (s, t)) = extGCD x r
