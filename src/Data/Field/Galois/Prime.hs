module Data.Field.Galois.Prime
  ( Prime
  , PrimeField
  , fromP
  , toP
  ) where

import Protolude as P hiding (Semiring, natVal, rem)

import Control.Monad.Random (Random(..))
import Data.Euclidean as S (Euclidean(..), GcdDomain)
import Data.Field (Field)
import Data.Group (Group(..))
import Data.Mod (Mod, unMod, (^%))
import Data.Semiring (Ring(..), Semiring(..))
import GHC.Natural (naturalToInteger)
import GHC.TypeNats (natVal)
import Test.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

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
newtype Prime (p :: Nat) = P (Mod p)
  deriving (Eq, Ord, Show, Generic, Num, Fractional, Euclidean, Field, GcdDomain, Ring, Semiring, Bounded, Enum, NFData)

instance Hashable (Prime p) where
  hashWithSalt s (P x) = hashWithSalt s (unMod x)

-- Prime fields are convertible.
instance KnownNat p => PrimeField (Prime p) where
  fromP (P x) = naturalToInteger (unMod x)
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
  pow (P x) k = P (x ^% k)
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
-- Other instances
-------------------------------------------------------------------------------

-- Prime fields are arbitrary.
instance KnownNat p => Arbitrary (Prime p) where
  arbitrary = choose (minBound, maxBound)
  {-# INLINABLE arbitrary #-}

-- Prime fields are integral.
instance KnownNat p => Integral (Prime p) where
  quotRem   = S.quotRem
  {-# INLINE quotRem #-}
  toInteger = fromP
  {-# INLINABLE toInteger #-}

-- Prime fields are pretty.
instance KnownNat p => Pretty (Prime p) where
  pretty (P x) = pretty $ naturalToInteger $ unMod x

-- Prime fields are random.
instance KnownNat p => Random (Prime p) where
  random         = randomR (minBound, maxBound)
  {-# INLINABLE random #-}
  randomR (a, b) = first fromInteger . randomR (fromP a, fromP b)
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
