module Data.Field.Galois.Prime
  ( Prime
  , PrimeField(..)
  , toP
  , toP'
  ) where

import Protolude as P hiding (Semiring)

import Control.Monad.Random (Random(..))
import Data.Euclidean (Euclidean(..), GcdDomain(..))
import Data.Semiring (Ring(..), Semiring(..))
import GHC.Integer.GMP.Internals (powModInteger, recipModInteger)
import Test.Tasty.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Field.Galois.Base (Field(..), GaloisField(..))

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Prime fields @GF(p) = Z/pZ@ for @p@ prime.
class (Bits k, GaloisField k) => PrimeField k where
  {-# MINIMAL fromP #-}
  -- | Convert from @GF(p)@ to @Z@.
  fromP :: k -> Integer

-- | Prime field elements.
newtype Prime (p :: Nat) = P Integer
  deriving (Bits, Eq, Generic, NFData, Ord, Show)

-- Prime fields are convertible.
instance KnownNat p => PrimeField (Prime p) where
  fromP (P x) = x
  {-# INLINABLE fromP #-}

-- Prime fields are Galois fields.
instance KnownNat p => GaloisField (Prime p) where
  char        = natVal
  {-# INLINABLE char #-}
  deg         = const 1
  {-# INLINABLE deg #-}
  frob        = identity
  {-# INLINABLE frob #-}
  order       = natVal
  {-# INLINABLE order #-}
  pow (P x) n = P (powModInteger x n (natVal (witness :: Prime p)))
  {-# INLINE pow #-}

{-# RULES "Prime.pow"
  forall (k :: KnownNat p => Prime p) (n :: Integer) . (^) k n = pow k n
  #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Prime fields are fractional.
instance KnownNat p => Fractional (Prime p) where
  recip (P 0)         = divZeroError
  recip (P x)         = P (recipModInteger x (natVal (witness :: Prime p)))
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Prime fields are numeric.
instance KnownNat p => Num (Prime p) where
  P x + P y     = P (if xyp >= 0 then xyp else xy)
    where
      xy  = x + y
      xyp = xy - natVal (witness :: Prime p)
  {-# INLINE (+) #-}
  P x * P y     = P (P.rem (x * y) (natVal (witness :: Prime p)))
  {-# INLINE (*) #-}
  P x - P y     = P (if xy >= 0 then xy else xy + natVal (witness :: Prime p))
    where
      xy = x - y
  {-# INLINE (-) #-}
  negate (P 0)  = P 0
  negate (P x)  = P (natVal (witness :: Prime p) - x)
  {-# INLINE negate #-}
  fromInteger x = P (if y >= 0 then y else y + p)
    where
      y = P.rem x p
      p = natVal (witness :: Prime p)
  {-# INLINABLE fromInteger #-}
  abs           = panic "Prime.abs: not implemented."
  signum        = panic "Prime.signum: not implemented."

-------------------------------------------------------------------------------
-- Semiring instances
-------------------------------------------------------------------------------

-- Prime fields are Euclidean domains.
instance KnownNat p => Euclidean (Prime p) where
  quotRem = (flip (,) 0 .) . (/)
  {-# INLINE quotRem #-}
  degree  = panic "Prime.degree: not implemented."

-- Prime fields are fields.
instance KnownNat p => Field (Prime p) where
  invert = recip
  {-# INLINE invert #-}
  minus  = (-)
  {-# INLINE minus #-}

-- Prime fields are GCD domains.
instance KnownNat p => GcdDomain (Prime p)

-- Prime fields are rings.
instance KnownNat p => Ring (Prime p) where
  negate = P.negate
  {-# INLINE negate #-}

-- Prime fields are semirings.
instance KnownNat p => Semiring (Prime p) where
  zero        = 0
  {-# INLINE zero #-}
  plus        = (+)
  {-# INLINE plus #-}
  one         = 1
  {-# INLINE one #-}
  times       = (*)
  {-# INLINE times #-}
  fromNatural = fromIntegral
  {-# INLINABLE fromNatural #-}

-------------------------------------------------------------------------------
-- Other instances
-------------------------------------------------------------------------------

-- Prime fields are arbitrary.
instance KnownNat p => Arbitrary (Prime p) where
  arbitrary = P <$> choose (0, natVal (witness :: Prime p) - 1)
  {-# INLINABLE arbitrary #-}

-- Prime fields are pretty.
instance KnownNat p => Pretty (Prime p) where
  pretty (P x) = pretty x

-- Prime fields are random.
instance KnownNat p => Random (Prime p) where
  random  = first P . randomR (0, natVal (witness :: Prime p) - 1)
  {-# INLINABLE random #-}
  randomR = panic "Prime.randomR: not implemented."

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Safe convert from @Z@ to @GF(p)@.
toP :: KnownNat p => Integer -> Prime p
toP = fromInteger
{-# INLINABLE toP #-}

-- | Unsafe convert from @Z@ to @GF(p)@.
toP' :: KnownNat p => Integer -> Prime p
toP' = P
{-# INLINABLE toP' #-}
