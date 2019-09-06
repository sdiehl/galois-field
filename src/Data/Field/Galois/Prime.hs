module Data.Field.Galois.Prime
  ( Prime
  , PrimeField
  , fromP
  , toP
  , toP'
  ) where

import Protolude as P hiding (Semiring, natVal)

import Control.Monad.Random (Random(..))
import Data.Euclidean (Euclidean(..), GcdDomain(..))
import Data.Field (Field)
import Data.Semiring (Ring(..), Semiring(..))
import GHC.Integer.GMP.Internals (recipModInteger)
import GHC.Natural (Natural, naturalFromInteger, naturalToInteger, powModNatural)
import GHC.TypeNats (natVal)
import Test.Tasty.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Field.Galois.Base (GaloisField(..))

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Prime fields @GF(p) = Z/pZ@ for @p@ prime.
type PrimeField = PrimeGaloisField
class (Bits k, GaloisField k) => PrimeGaloisField k where
  {-# MINIMAL fromP #-}
  -- | Convert from @GF(p)@ to @Z@.
  fromP :: k -> Integer

-- | Prime field elements.
newtype Prime (p :: Nat) = P Natural
  deriving (Bits, Eq, Generic, NFData, Ord, Show)

-- Prime fields are convertible.
instance KnownNat p => PrimeGaloisField (Prime p) where
  fromP (P x) = naturalToInteger x
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
  pow (P x) n = P $ powModNatural x (fromIntegral n) $ natVal (witness :: Prime p)
  {-# INLINE pow #-}

{-# RULES "Prime.pow"
  forall (k :: KnownNat p => Prime p) n . (^) k n = pow k n
  #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Prime fields are fractional.
instance KnownNat p => Fractional (Prime p) where
  recip (P 0)         = divZeroError
  recip (P x)         = P $ recipModNatural x $ natVal (witness :: Prime p)
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Prime fields are numeric.
instance KnownNat p => Num (Prime p) where
  P x + P y     = P $ if xy >= p then xy - p else xy
    where
      xy = x + y
      p  = natVal (witness :: Prime p)
  {-# INLINE (+) #-}
  P x * P y     = P $ P.rem (x * y) $ natVal (witness :: Prime p)
  {-# INLINE (*) #-}
  P x - P y     = P $ if x >= y then x - y else natVal (witness :: Prime p) + x - y
  {-# INLINE (-) #-}
  negate (P 0)  = P 0
  negate (P x)  = P $ natVal (witness :: Prime p) - x
  {-# INLINE negate #-}
  fromInteger x = P $ naturalFromInteger $ P.mod x $ naturalToInteger $ natVal (witness :: Prime p)
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
instance KnownNat p => Field (Prime p)

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
  arbitrary = P . naturalFromInteger <$>
    choose (0, naturalToInteger $ natVal (witness :: Prime p) - 1)
  {-# INLINABLE arbitrary #-}

-- Prime fields are pretty.
instance KnownNat p => Pretty (Prime p) where
  pretty (P x) = pretty $ naturalToInteger x

-- Prime fields are random.
instance KnownNat p => Random (Prime p) where
  random  = first (P . naturalFromInteger) .
    randomR (0, naturalToInteger $ natVal (witness :: Prime p) - 1)
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
toP' = P . naturalFromInteger
{-# INLINABLE toP' #-}

-------------------------------------------------------------------------------
-- Prime arithmetic
-------------------------------------------------------------------------------

-- Reciprocals modulo naturals.
recipModNatural :: Natural -> Natural -> Natural
recipModNatural x p = naturalFromInteger $
  recipModInteger (naturalToInteger x) (naturalToInteger p)
{-# INLINE recipModNatural #-}
