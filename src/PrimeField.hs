module PrimeField
  ( PrimeField
  , toInt
  ) where

import Protolude as P hiding (Semiring)

import Control.Monad.Random (Random(..))
import Data.Euclidean (Euclidean(..), GcdDomain(..))
import Data.Semiring (Ring(..), Semiring(..))
import GHC.Integer.GMP.Internals (powModInteger, recipModInteger)
import Test.Tasty.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (Field(..), GaloisField(..))

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Prime fields @GF(p)@ for @p@ prime.
newtype PrimeField (p :: Nat) = PF Integer
  deriving (Bits, Eq, Generic, Ord, Show)

-- Prime fields are Galois fields.
instance KnownNat p => GaloisField (PrimeField p) where
  char         = natVal
  {-# INLINE char #-}
  deg          = const 1
  {-# INLINE deg #-}
  frob         = identity
  {-# INLINE frob #-}
  pow (PF x) n = PF (powModInteger x n (natVal (witness :: PrimeField p)))
  {-# INLINE pow #-}

{-# RULES "PrimeField/pow"
  forall (k :: KnownNat p => PrimeField p) (n :: Integer) . (^) k n = pow k n
  #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Prime fields are fractional.
instance KnownNat p => Fractional (PrimeField p) where
  recip (PF 0)        = panic "no multiplicative inverse."
  recip (PF x)        = PF (recipModInteger x (natVal (witness :: PrimeField p)))
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Prime fields are numeric.
instance KnownNat p => Num (PrimeField p) where
  PF x + PF y   = PF (if xyp >= 0 then xyp else xy)
    where
      xy  = x + y
      xyp = xy - natVal (witness :: PrimeField p)
  {-# INLINE (+) #-}
  PF x * PF y   = PF (P.rem (x * y) (natVal (witness :: PrimeField p)))
  {-# INLINE (*) #-}
  PF x - PF y   = PF (if xy >= 0 then xy else xy + natVal (witness :: PrimeField p))
    where
      xy = x - y
  {-# INLINE (-) #-}
  negate (PF 0) = PF 0
  negate (PF x) = PF (natVal (witness :: PrimeField p) - x)
  {-# INLINE negate #-}
  fromInteger x = PF (if y >= 0 then y else y + p)
    where
      y = P.rem x p
      p = natVal (witness :: PrimeField p)
  {-# INLINABLE fromInteger #-}
  abs           = panic "not implemented."
  signum        = panic "not implemented."

-------------------------------------------------------------------------------
-- Semiring instances
-------------------------------------------------------------------------------

-- Prime fields are Euclidean domains.
instance KnownNat p => Euclidean (PrimeField p) where
  quotRem = (flip (,) 0 .) . (/)
  {-# INLINE quotRem #-}
  degree  = panic "not implemented."
  {-# INLINE degree #-}

-- Prime fields are fields.
instance KnownNat p => Field (PrimeField p) where
  invert = recip
  {-# INLINE invert #-}
  minus  = (-)
  {-# INLINE minus #-}

-- Prime fields are GCD domains.
instance KnownNat p => GcdDomain (PrimeField p)

-- Prime fields are rings.
instance KnownNat p => Ring (PrimeField p) where
  negate = P.negate
  {-# INLINE negate #-}

-- Prime fields are semirings.
instance KnownNat p => Semiring (PrimeField p) where
  zero        = 0
  {-# INLINE zero #-}
  plus        = (+)
  {-# INLINE plus #-}
  one         = 1
  {-# INLINE one #-}
  times       = (*)
  {-# INLINE times #-}
  fromNatural = fromIntegral
  {-# INLINE fromNatural #-}

-------------------------------------------------------------------------------
-- Other instances
-------------------------------------------------------------------------------

-- Prime fields are arbitrary.
instance KnownNat p => Arbitrary (PrimeField p) where
  arbitrary = PF <$> choose (0, natVal (witness :: PrimeField p) - 1)

-- Prime fields are pretty.
instance KnownNat p => Pretty (PrimeField p) where
  pretty (PF x) = pretty x

-- Prime fields are random.
instance KnownNat p => Random (PrimeField p) where
  random  = first PF . randomR (0, natVal (witness :: PrimeField p) - 1)
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Type conversions
-------------------------------------------------------------------------------

-- | Embed field element to integers.
toInt :: PrimeField p -> Integer
toInt (PF x) = x
{-# INLINABLE toInt #-}
