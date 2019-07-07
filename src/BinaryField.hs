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
  recip               = notImplemented
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- | Binary fields are Galois fields
instance KnownNat ib => GaloisField (BinaryField ib) where
  char = const 2
  {-# INLINE char #-}
  deg  = notImplemented
  {-# INLINE deg #-}
  pow  = notImplemented
  {-# INLINE pow #-}
  rnd  = getRandom
  {-# INLINE rnd #-}

-- | Binary fields are fields
instance KnownNat ib => Num (BinaryField ib) where
  BF x + BF y   = BF (xor x y)
  {-# INLINE (+) #-}
  (*)           = notImplemented
  {-# INLINE (*) #-}
  BF x - BF y   = BF (xor x y)
  {-# INLINE (-) #-}
  negate        = identity
  {-# INLINE negate #-}
  fromInteger x = let y = rem x 2 in BF (if y >= 0 then y else y + 2)
  {-# INLINABLE fromInteger #-}
  abs           = panic "not implemented."
  signum        = panic "not implemented."

-- | Binary fields are pretty
instance KnownNat ib => Pretty (BinaryField ib) where
  pretty (BF x) = pretty x

-- | Binary fields are random
instance KnownNat ib => Random (BinaryField ib) where
  random  = first BF . randomR (0, 2 ^ natVal (witness :: BinaryField ib) - 1)
  randomR = panic "not implemented."
