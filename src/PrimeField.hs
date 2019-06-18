module PrimeField
  ( PrimeField
  , embed
  ) where

import Protolude

import GaloisField (GaloisField(..))

-- | Prime fields @GF(p)@ for @p@ prime
newtype PrimeField (p :: Nat) = PF Integer
  deriving (Eq, Generic, NFData, Show)

-- | Prime fields are fields
instance KnownNat p => Fractional (PrimeField p) where
  recip (PF x)        = case modInv x (natVal (witness :: PrimeField p)) of
    Just y -> fromInteger y
    _      -> panic "no multiplicative inverse."
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y

-- | Prime fields are Galois fields
instance KnownNat p => GaloisField (PrimeField p) where
  char = natVal

-- | Prime fields are rings
instance KnownNat p => Num (PrimeField p) where
  PF x + PF y   = fromInteger (x + y)
  PF x * PF y   = fromInteger (x * y)
  PF x - PF y   = fromInteger (x - y)
  negate (PF x) = fromInteger (-x)
  fromInteger x = PF (mod x (natVal (witness :: PrimeField p)))
  abs           = panic "not implemented."
  signum        = panic "not implemented."

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
{-# INLINE modInv #-}

-- | Embed to integers
embed :: PrimeField p -> Integer
embed (PF x) = x
{-# INLINE embed #-}
