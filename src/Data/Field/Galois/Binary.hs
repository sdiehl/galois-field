module Data.Field.Galois.Binary
  ( Binary
  , BinaryField
  , fromB
  , toB
  , toB'
  ) where

import Protolude as P hiding (Semiring, natVal)

import Control.Monad.Random (Random(..))
import Data.Bit (Bit, F2Poly, gcdExt, toF2Poly, unF2Poly)
import Data.Euclidean as S (Euclidean(..), GcdDomain)
import Data.Field (Field)
import Data.Group (Group(..))
import Data.Semiring (Ring(..), Semiring(..))
import Data.Vector.Unboxed as V (fromList, length, toList)
import GHC.Exts (IsList(..))
import GHC.Natural (Natural)
import GHC.TypeNats (natVal)
import Test.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Field.Galois.Base (GaloisField(..))

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Binary fields @GF(2^q)[X]/\<f(X)\>@ for @q@ positive and
-- @f(X)@ irreducible monic in @GF(2^q)[X]@ encoded as an integer.
class GaloisField k => BinaryField k where
  {-# MINIMAL fromB #-}
  -- | Convert from @GF(2^q)[X]/\<f(X)\>@ to @Z@.
  fromB :: k -> Integer

-- | Binary field elements.
newtype Binary (p :: Nat) = B F2Poly
  deriving (Eq, Generic, NFData, Ord, Show)

-- Binary fields are convertible.
instance KnownNat p => BinaryField (Binary p) where
  fromB (B x) = toInteger x
  {-# INLINABLE fromB #-}

-- Binary fields are Galois fields.
instance KnownNat p => GaloisField (Binary p) where
  char = const 2
  {-# INLINABLE char #-}
  deg  = pred . fromIntegral . V.length . unF2Poly . toPoly . natVal
  {-# INLINABLE deg #-}
  frob = join (*)
  {-# INLINABLE frob #-}

{-# RULES "Binary.pow"
  forall (k :: KnownNat p => Binary p) n . (^) k n = pow k n
  #-}

-------------------------------------------------------------------------------
-- Group instances
-------------------------------------------------------------------------------

-- Binary fields are multiplicative groups.
instance KnownNat p => Group (Binary p) where
  invert        = recip
  {-# INLINE invert #-}
  pow x n
    | n >= 0    = x ^ n
    | otherwise = recip x ^ P.negate n
  {-# INLINE pow #-}

-- Binary fields are multiplicative monoids.
instance KnownNat p => Monoid (Binary p) where
  mempty = B 1
  {-# INLINE mempty #-}

-- Binary fields are multiplicative semigroups.
instance KnownNat p => Semigroup (Binary p) where
  (<>)   = (*)
  {-# INLINE (<>) #-}
  stimes = flip pow
  {-# INLINE stimes #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Binary fields are fractional.
instance KnownNat p => Fractional (Binary p) where
  recip (B x)         = case gcdExt x $ toPoly $ natVal (witness :: Binary p) of
    (1, y) -> B y
    _      -> divZeroError
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Binary fields are numeric.
instance KnownNat p => Num (Binary p) where
  B x + B y   = B $ x + y
  {-# INLINE (+) #-}
  B x * B y   = B $ P.rem (x * y) $ toPoly $ natVal (witness :: Binary p)
  {-# INLINE (*) #-}
  B x - B y   = B $ x + y
  {-# INLINE (-) #-}
  negate      = identity
  {-# INLINE negate #-}
  fromInteger = B . flip P.rem (toPoly $ natVal (witness :: Binary p)) . toPoly
  {-# INLINABLE fromInteger #-}
  abs         = panic "Binary.abs: not implemented."
  signum      = panic "Binary.signum: not implemented."

-------------------------------------------------------------------------------
-- Semiring instances
-------------------------------------------------------------------------------

-- Binary fields are Euclidean domains.
instance KnownNat p => Euclidean (Binary p) where
  degree  = panic "Binary.degree: not implemented."
  quotRem = (flip (,) 0 .) . (/)
  {-# INLINE quotRem #-}

-- Binary fields are fields.
instance KnownNat p => Field (Binary p) where

-- Binary fields are GCD domains.
instance KnownNat p => GcdDomain (Binary p)

-- Binary fields are rings.
instance KnownNat p => Ring (Binary p) where
  negate = P.negate
  {-# INLINE negate #-}

-- Binary fields are semirings.
instance KnownNat p => Semiring (Binary p) where
  fromNatural = fromIntegral
  {-# INLINABLE fromNatural #-}
  one         = B 1
  {-# INLINE one #-}
  plus        = (+)
  {-# INLINE plus #-}
  times       = (*)
  {-# INLINE times #-}
  zero        = B 0
  {-# INLINE zero #-}

-------------------------------------------------------------------------------
-- Other instances
-------------------------------------------------------------------------------

-- Binary fields are arbitrary.
instance KnownNat p => Arbitrary (Binary p) where
  arbitrary = toB' <$>
    choose (0, toInteger $ order (witness :: Binary p) - 1)
  {-# INLINABLE arbitrary #-}

-- Binary fields are lists.
instance KnownNat p => IsList (Binary p) where
  type instance Item (Binary p) = Bit
  fromList     = B . toF2Poly . V.fromList
  {-# INLINABLE fromList #-}
  toList (B x) = V.toList $ unF2Poly x
  {-# INLINABLE toList #-}

-- Binary fields are bounded.
instance KnownNat p => Bounded (Binary p) where
  maxBound = B $ toPoly $ order (witness :: Binary p) - 1
  {-# INLINE maxBound #-}
  minBound = B 0
  {-# INLINE minBound #-}

-- Binary fields are enumerable.
instance KnownNat p => Enum (Binary p) where
  fromEnum = fromIntegral
  {-# INLINABLE fromEnum #-}
  toEnum   = fromIntegral
  {-# INLINABLE toEnum #-}

-- Binary fields are integral.
instance KnownNat p => Integral (Binary p) where
  quotRem   = S.quotRem
  {-# INLINE quotRem #-}
  toInteger = fromB
  {-# INLINABLE toInteger #-}

-- Binary fields are pretty.
instance KnownNat p => Pretty (Binary p) where
  pretty (B x) = pretty $ toInteger x

-- Binary fields are random.
instance KnownNat p => Random (Binary p) where
  random         = randomR (B 0, B $ toPoly $ order (witness :: Binary p) - 1)
  {-# INLINABLE random #-}
  randomR (a, b) = first toB' . randomR (fromB a, fromB b)
  {-# INLINABLE randomR #-}

-- Binary fields are real.
instance KnownNat p => Real (Binary p) where
  toRational = fromIntegral
  {-# INLINABLE toRational #-}

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Safe convert from @Z@ to @GF(2^q)[X]/\<f(X)\>@.
toB :: KnownNat p => Integer -> Binary p
toB = fromInteger
{-# INLINABLE toB #-}

-- | Unsafe convert from @Z@ to @GF(2^q)[X]/\<f(X)\>@.
toB' :: KnownNat p => Integer -> Binary p
toB' = B . toPoly
{-# INLINABLE toB' #-}

-- Specialisation convert from integer to polynomial.
toPoly :: Integral a => a -> F2Poly
toPoly = fromIntegral
{-# INLINABLE toPoly #-}

{-# SPECIALISE toPoly ::
  Integer -> F2Poly,
  Natural -> F2Poly
  #-}
