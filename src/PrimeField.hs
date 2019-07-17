module PrimeField
  ( PrimeField
  , toInt
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
  deriving (Bits, Eq, Generic, NFData, Read, Show)

-- Prime fields are Galois fields.
instance KnownNat p => GaloisField (PrimeField p) where
  char           = natVal
  {-# INLINE char #-}
  deg            = const 1
  {-# INLINE deg #-}
  frob           = identity
  {-# INLINE frob #-}
  pow y@(PF x) n = PF (powModInteger x n (natVal y))
  {-# INLINE pow #-}
  rnd            = getRandom
  {-# INLINE rnd #-}

-------------------------------------------------------------------------------
-- Prime field instances
-------------------------------------------------------------------------------

-- Prime fields are arbitrary.
instance KnownNat p => Arbitrary (PrimeField p) where
  arbitrary = fromInteger <$> arbitrary

-- Prime fields are fields.
instance KnownNat p => Fractional (PrimeField p) where
  recip y@(PF x)      = PF (if x == 0 then panic "no multiplicative inverse."
                            else recipModInteger x (natVal y))
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Prime fields are rings.
instance KnownNat p => Num (PrimeField p) where
  z@(PF x) + PF y = PF (if xyp >= 0 then xyp else xy)
    where
      xy  = x + y
      xyp = xy - natVal z
  {-# INLINE (+) #-}
  z@(PF x) * PF y = PF (rem (x * y) (natVal z))
  {-# INLINE (*) #-}
  z@(PF x) - PF y = PF (if xy >= 0 then xy else xy + natVal z)
    where
      xy = x - y
  {-# INLINE (-) #-}
  negate y@(PF x) = PF (if x == 0 then 0 else -x + natVal y)
  {-# INLINE negate #-}
  fromInteger x   = PF (if y >= 0 then y else y + p)
    where
      y = rem x p
      p = natVal (witness :: PrimeField p)
  {-# INLINABLE fromInteger #-}
  abs             = panic "not implemented."
  signum          = panic "not implemented."

-- Prime fields are pretty.
instance KnownNat p => Pretty (PrimeField p) where
  pretty (PF x) = pretty x

-- Prime fields are random.
instance KnownNat p => Random (PrimeField p) where
  random  = first PF . randomR (0, natVal (witness :: PrimeField p) - 1)
  {-# INLINE random #-}
  randomR = panic "not implemented."

-- | Embed field element to integers.
toInt :: PrimeField p -> Integer
toInt (PF x) = x
{-# INLINABLE toInt #-}
