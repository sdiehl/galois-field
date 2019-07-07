module BinaryField
  ( BinaryField
  ) where

import Protolude

import Control.Monad.Random (Random(..), getRandom)
import Test.Tasty.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (GaloisField(..))

-- | Binary fields @GF(2^q)[X]/<f(X)>@ for @q@ positive and
-- @f(X)@ irreducible monic in @GF(2^q)[X]@
newtype BinaryField (ib :: Nat) = BF Integer
  deriving (Eq, Generic, NFData, Show)

-- | Binary fields are arbitrary
instance KnownNat ib => Arbitrary (BinaryField ib) where
  arbitrary = BF <$> choose (0, 2 ^ natVal (witness :: BinaryField ib) - 1)

-- | Binary fields are fields
instance KnownNat ib => Fractional (BinaryField ib) where
  recip y@(BF x)      = case polyInv (natVal y) x of
    Just z -> BF z
    _      -> panic "no multiplicative inverse."
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- | Binary fields are Galois fields
instance KnownNat ib => GaloisField (BinaryField ib) where
  char = const 2
  {-# INLINE char #-}
  deg  = logPrime 2 . natVal
  {-# INLINE deg #-}
  pow  = (^)
  {-# INLINE pow #-}
  rnd  = getRandom
  {-# INLINE rnd #-}

-- | Binary fields are fields
instance KnownNat ib => Num (BinaryField ib) where
  BF x + BF y = BF (xor x y)
  {-# INLINE (+) #-}
  BF x * BF y = fromInteger (polyMul x y)
  {-# INLINE (*) #-}
  BF x - BF y = BF (xor x y)
  {-# INLINE (-) #-}
  negate      = identity
  {-# INLINE negate #-}
  fromInteger = BF . polyMod (natVal (witness :: BinaryField ib))
  {-# INLINABLE fromInteger #-}
  abs         = panic "not implemented."
  signum      = panic "not implemented."

-- | Binary fields are pretty
instance KnownNat ib => Pretty (BinaryField ib) where
  pretty (BF x) = pretty x

-- | Binary fields are random
instance KnownNat ib => Random (BinaryField ib) where
  random  = first BF . randomR (0, 2 ^ natVal (witness :: BinaryField ib) - 1)
  randomR = panic "not implemented."

-- | Prime base logarithm
logPrime :: Integer -> Integer -> Int
logPrime p x = if x < p then 0 else logQuot l (quot x (p ^ l))
  where
    l           = 2 * logPrime (p * p) x
    logQuot q y = if y < p then q else logQuot (q + 1) (quot y p)

-- | Polynomial modulus
polyMod :: Integer -> Integer -> Integer
polyMod = notImplemented

-- | Polynomial multiplication
polyMul :: Integer -> Integer -> Integer
polyMul = notImplemented

-- | Polynomial inversion
polyInv :: Integer -> Integer -> Maybe Integer
polyInv = notImplemented
