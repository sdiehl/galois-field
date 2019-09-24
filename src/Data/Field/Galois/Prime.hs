module Data.Field.Galois.Prime
  ( Prime
  , PrimeField
  , fromP
  , toP
  , toP'
  ) where

import Protolude as P hiding (Semiring, natVal, one)

import Control.Monad.Random (Random(..))
import Data.Euclidean as S (Euclidean(..), GcdDomain(..))
import Data.Field as S (Field, recip, (/))
import Data.Group (Group(..))
import Data.Semiring as S (Ring(..), Semiring(..))
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
class (Bits k, GaloisField k) => PrimeField k where
  {-# MINIMAL fromP #-}
  -- | Convert from @GF(p)@ to @Z@.
  fromP :: k -> Integer

-- | Prime field elements.
newtype Prime (p :: Nat) = P Natural
  deriving (Bits, Eq, Generic, NFData, Ord, Show)

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
  invert        = S.recip
  {-# INLINE invert #-}
  pow y@(P x) n
    | n >= 0    = P $ powModNatural x (fromIntegral n) $ natVal (witness :: Prime p)
    | otherwise = pow (S.recip y) $ P.negate n
  {-# INLINE pow #-}

-- Prime fields are multiplicative monoids.
instance KnownNat p => Monoid (Prime p) where
  mempty = one
  {-# INLINE mempty #-}

-- Prime fields are multiplicative semigroups.
instance KnownNat p => Semigroup (Prime p) where
  (<>)   = times
  {-# INLINE (<>) #-}
  stimes = flip pow
  {-# INLINE stimes #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Prime fields are fractional.
instance KnownNat p => Fractional (Prime p) where
  (/)                 = S.quot
  {-# INLINE (/) #-}
  fromRational (x:%y) = fromInteger x S./ fromInteger y
  {-# INLINABLE fromRational #-}

-- Prime fields are numeric.
instance KnownNat p => Num (Prime p) where
  (+)         = plus
  {-# INLINE (+) #-}
  (*)         = times
  {-# INLINE (*) #-}
  P x - P y   = P $ if x >= y then x - y else natVal (witness :: Prime p) + x - y
  {-# INLINE (-) #-}
  negate      = S.negate
  {-# INLINE negate #-}
  fromInteger = P . naturalFromInteger . flip P.mod (naturalToInteger $ natVal (witness :: Prime p))
  {-# INLINABLE fromInteger #-}
  abs         = panic "Prime.abs: not implemented."
  signum      = panic "Prime.signum: not implemented."

-------------------------------------------------------------------------------
-- Semiring instances
-------------------------------------------------------------------------------

-- Prime fields are Euclidean domains.
instance KnownNat p => Euclidean (Prime p) where
  degree              = panic "Prime.degree: not implemented."
  quotRem _     (P 0) = divZeroError
  quotRem (P x) (P y) = (P $ flip P.rem (natVal (witness :: Prime p)) $ (*) x $
                         recipModNatural y $ natVal (witness :: Prime p), 0)
  {-# INLINE quotRem #-}

-- Prime fields are fields.
instance KnownNat p => Field (Prime p)

-- Prime fields are GCD domains.
instance KnownNat p => GcdDomain (Prime p)

-- Prime fields are rings.
instance KnownNat p => Ring (Prime p) where
  negate (P 0) = P 0
  negate (P x) = P $ natVal (witness :: Prime p) - x
  {-# INLINE negate #-}

-- Prime fields are semirings.
instance KnownNat p => Semiring (Prime p) where
  fromNatural       = P . flip P.rem (natVal (witness :: Prime p))
  {-# INLINABLE fromNatural #-}
  one               = P 1
  {-# INLINE one #-}
  plus (P x) (P y)  = P $ if xy >= p then xy - p else xy
    where
      xy = x + y
      p  = natVal (witness :: Prime p)
  {-# INLINE plus #-}
  times (P x) (P y) = P $ P.rem (x * y) $ natVal (witness :: Prime p)
  {-# INLINE times #-}
  zero              = P 0
  {-# INLINE zero #-}

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
  random         = first (P . naturalFromInteger) .
    randomR (0, naturalToInteger $ natVal (witness :: Prime p) - 1)
  {-# INLINABLE random #-}
  randomR (a, b) = first (P . naturalFromInteger) . randomR (fromP a, fromP b)
  {-# INLINABLE randomR #-}

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
