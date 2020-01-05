module Data.Field.Galois.Prime
  ( Prime
  , PrimeField
  , fromP
  , toP
  , toP'
  ) where

import Protolude as P hiding (Semiring, natVal, rem)

import Control.Monad.Random (Random(..))
import Data.Euclidean as S (Euclidean(..), GcdDomain)
import Data.Field (Field)
import Data.Group (Group(..))
import Data.Mod (Mod, unMod, (^%))
import Data.Semiring (Ring(..), Semiring(..))
import GHC.Natural (Natural, naturalFromInteger, naturalToInteger)
import GHC.TypeNats (natVal)
import Test.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))
import Unsafe.Coerce (unsafeCoerce)

import Data.Field.Galois.Base (GaloisField(..))

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Prime fields @GF(p) = Z/pZ@ for @p@ prime.
class GaloisField k => PrimeField k where
  {-# MINIMAL fromP #-}
  -- | Convert from @GF(p)@ to @Z@.
  fromP :: k -> Integer

-- | Prime field elements.
newtype Prime (p :: Nat) = P Natural
  deriving (Bits, Eq, Generic, Hashable, NFData, Ord, Show)

-- Prime fields are convertible.
instance KnownNat p => PrimeField (Prime p) where
  fromP (P x) = naturalToInteger x
  {-# INLINABLE fromP #-}

-- Prime fields are Galois fields.
instance KnownNat p => GaloisField (Prime p) where
  char = natVal
  {-# INLINABLE char #-}
  deg  = const 1
  {-# INLINABLE deg #-}
  frob = identity
  {-# INLINABLE frob #-}

{-# RULES "Prime.pow"
  forall (k :: KnownNat p => Prime p) n . (^) k n = pow k n
  #-}

-------------------------------------------------------------------------------
-- Group instances
-------------------------------------------------------------------------------

-- Prime fields are multiplicative groups.
instance KnownNat p => Group (Prime p) where
  invert = recip
  {-# INLINE invert #-}
  pow x  = P . unMod . (^%) (unsafeCoerce x :: Mod p)
  {-# INLINE pow #-}

-- Prime fields are multiplicative monoids.
instance KnownNat p => Monoid (Prime p) where
  mempty = P 1
  {-# INLINE mempty #-}

-- Prime fields are multiplicative semigroups.
instance KnownNat p => Semigroup (Prime p) where
  (<>)   = (*)
  {-# INLINE (<>) #-}
  stimes = flip pow
  {-# INLINE stimes #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Prime fields are fractional.
instance KnownNat p => Fractional (Prime p) where
  recip x             = P $ unMod $ recip $ (unsafeCoerce x :: Mod p)
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Prime fields are numeric.
instance KnownNat p => Num (Prime p) where
  x + y         = P $ unMod $ (unsafeCoerce x + unsafeCoerce y :: Mod p)
  {-# INLINE (+) #-}
  x * y         = P $ unMod $ (unsafeCoerce x * unsafeCoerce y :: Mod p)
  {-# INLINE (*) #-}
  x - y         = P $ unMod $ (unsafeCoerce x - unsafeCoerce y :: Mod p)
  {-# INLINE (-) #-}
  negate x      = P $ unMod $ P.negate $ (unsafeCoerce x :: Mod p)
  {-# INLINE negate #-}
  fromInteger x = P $ unMod $ (fromIntegral x :: Mod p)
  {-# INLINABLE fromInteger #-}
  abs           = panic "Prime.abs: not implemented."
  signum        = panic "Prime.signum: not implemented."

-------------------------------------------------------------------------------
-- Semiring instances
-------------------------------------------------------------------------------

-- Prime fields are Euclidean domains.
instance KnownNat p => Euclidean (Prime p) where
  degree  = panic "Prime.degree: not implemented."
  quotRem = (flip (,) 0 .) . (/)
  {-# INLINE quotRem #-}

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
  fromNatural = fromIntegral
  {-# INLINABLE fromNatural #-}
  one         = P 1
  {-# INLINE one #-}
  plus        = (+)
  {-# INLINE plus #-}
  times       = (*)
  {-# INLINE times #-}
  zero        = P 0
  {-# INLINE zero #-}

-------------------------------------------------------------------------------
-- Other instances
-------------------------------------------------------------------------------

-- Prime fields are arbitrary.
instance KnownNat p => Arbitrary (Prime p) where
  arbitrary = P . naturalFromInteger <$>
    choose (0, naturalToInteger $ natVal (witness :: Prime p) - 1)
  {-# INLINABLE arbitrary #-}

-- Prime fields are bounded.
instance KnownNat p => Bounded (Prime p) where
  maxBound = P $ natVal (witness :: Prime p) - 1
  {-# INLINE maxBound #-}
  minBound = P 0
  {-# INLINE minBound #-}

-- Prime fields are enumerable.
instance KnownNat p => Enum (Prime p) where
  fromEnum = fromIntegral
  {-# INLINABLE fromEnum #-}
  toEnum   = fromIntegral
  {-# INLINABLE toEnum #-}

-- Prime fields are integral.
instance KnownNat p => Integral (Prime p) where
  quotRem   = S.quotRem
  {-# INLINE quotRem #-}
  toInteger = fromP
  {-# INLINABLE toInteger #-}

-- Prime fields are pretty.
instance KnownNat p => Pretty (Prime p) where
  pretty (P x) = pretty $ naturalToInteger x

-- Prime fields are random.
instance KnownNat p => Random (Prime p) where
  random         = randomR (P 0, P $ natVal (witness :: Prime p) - 1)
  {-# INLINABLE random #-}
  randomR (a, b) = first (P . naturalFromInteger) . randomR (fromP a, fromP b)
  {-# INLINABLE randomR #-}

-- Prime fields are real.
instance KnownNat p => Real (Prime p) where
  toRational = fromIntegral
  {-# INLINABLE toRational #-}

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
