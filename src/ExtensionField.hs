module ExtensionField
  ( ExtensionField
  , IrreducibleMonic(split)
  , fromField
  , fromList
  , t
  , pattern X
  ) where

import Protolude as P hiding (Semiring, quot, quotRem, rem)

import Control.Monad.Random (Random(..), getRandom)
import Data.Euclidean (Euclidean(..), GcdDomain(..))
import Data.Semiring (Ring(..), Semiring(..))
import Data.Poly.Semiring (VPoly, leading, monomial, scale, toPoly, unPoly, pattern X)
import qualified Data.Vector as V
import Test.Tasty.QuickCheck (Arbitrary(..), vector)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (Field(..), GaloisField(..))

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Extension fields @GF(p^q)[X]/\<f(X)\>@ for @p@ prime, @q@ positive, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@.
newtype ExtensionField k im = EF (VPoly k)
  deriving (Eq, Generic, Ord, Show)

-- | Irreducible monic splitting polynomial @f(X)@ of extension field.
class GaloisField k => IrreducibleMonic k im where
  {-# MINIMAL split #-}
  -- | Splitting polynomial @f(X)@.
  split :: ExtensionField k im -> VPoly k
  -- | Splitting polynomial degree.
  deg' :: ExtensionField k im -> Int
  deg' = pred . fromIntegral . degree . split

-- Extension fields are Galois fields.
instance IrreducibleMonic k im => GaloisField (ExtensionField k im) where
  char  = const (char (witness :: k))
  {-# INLINE char #-}
  deg w = deg (witness :: k) * deg' w
  {-# INLINE deg #-}
  frob  = pow <*> char
  {-# INLINE frob #-}
  pow   = (^)
  {-# INLINE pow #-}
  quad  = panic "not implemented."
  {-# INLINE quad #-}
  rnd   = getRandom
  {-# INLINE rnd #-}
  sr    = panic "not implemented."
  {-# INLINE sr #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Extension fields are fractional.
instance IrreducibleMonic k im => Fractional (ExtensionField k im) where
  recip w@(EF x)      = EF (polyInv x (split w))
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Extension fields are numeric.
instance IrreducibleMonic k im => Num (ExtensionField k im) where
  EF x + EF y     = EF (x + y)
  {-# INLINE (+) #-}
  w@(EF x) * EF y = EF (rem (x * y) (split w))
  {-# INLINE (*) #-}
  EF x - EF y     = EF (x - y)
  {-# INLINE (-) #-}
  negate (EF x)   = EF (P.negate x)
  {-# INLINE negate #-}
  fromInteger     = EF . fromInteger
  {-# INLINABLE fromInteger #-}
  abs             = panic "not implemented."
  signum          = panic "not implemented."

-------------------------------------------------------------------------------
-- Semiring instances
-------------------------------------------------------------------------------

-- Extension fields are Euclidean domains.
instance IrreducibleMonic k im => Euclidean (ExtensionField k im) where
  quotRem = (flip (,) 0 .) . (/)
  {-# INLINE quotRem #-}
  degree  = panic "not implemented."
  {-# INLINE degree #-}

-- Extension fields are fields.
instance IrreducibleMonic k im => Field (ExtensionField k im) where
  invert = recip
  {-# INLINE invert #-}
  minus  = (-)
  {-# INLINE minus #-}

-- Extension fields are GCD domains.
instance IrreducibleMonic k im => GcdDomain (ExtensionField k im)

-- Extension fields are rings.
instance IrreducibleMonic k im => Ring (ExtensionField k im) where
  negate = P.negate
  {-# INLINE negate #-}

-- Extension fields are semirings.
instance IrreducibleMonic k im => Semiring (ExtensionField k im) where
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

-- Extension fields are arbitrary.
instance IrreducibleMonic k im => Arbitrary (ExtensionField k im) where
  arbitrary = fromList <$> vector (deg' (witness :: ExtensionField k im))

-- Extension fields are pretty.
instance IrreducibleMonic k im => Pretty (ExtensionField k im) where
  pretty (EF x) = pretty (show x :: Text)

-- Extension fields are random.
instance IrreducibleMonic k im => Random (ExtensionField k im) where
  random  = first fromList . unfold (deg' (witness :: ExtensionField k im)) []
    where
      unfold n xs g
        | n <= 0    = (xs, g)
        | otherwise = case random g of
        (x, g') -> unfold (n - 1) (x : xs) g'
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Type conversions
-------------------------------------------------------------------------------

-- | Convert from field element to list representation.
fromField :: ExtensionField k im -> [k]
fromField (EF x) = toList (unPoly x)
{-# INLINABLE fromField #-}

-- | Convert from list representation to field element.
fromList :: forall k im . IrreducibleMonic k im
  => [k] -> ExtensionField k im
fromList = EF . flip rem (split (witness :: ExtensionField k im)) . toPoly . V.fromList
{-# INLINABLE fromList #-}

-- | Descend tower of indeterminate variables.
t :: IrreducibleMonic k im => VPoly k -> VPoly (ExtensionField k im)
t = monomial 0 . EF
{-# INLINE t #-}

-------------------------------------------------------------------------------
-- Polynomial arithmetic
-------------------------------------------------------------------------------

-- Polynomial inversion algorithm.
polyInv ::  GaloisField k => VPoly k -> VPoly k -> VPoly k
polyInv xs ps = case first leading (polyGCD xs ps) of
  (Just (0, x), ys) -> scale 0 (recip x) ys
  _                 -> panic "no multiplicative inverse."
{-# INLINE polyInv #-}

-- Polynomial extended greatest common divisor algorithm.
polyGCD :: forall k . GaloisField k => VPoly k -> VPoly k -> (VPoly k, VPoly k)
polyGCD = flip (polyGCD' 0 1)
  where
    polyGCD' :: VPoly k -> VPoly k -> VPoly k -> VPoly k -> (VPoly k, VPoly k)
    polyGCD' s s' r r'
      | r' == 0   = (r, s)
      | otherwise = polyGCD' s' (s - q * s') r' (r - q * r')
      where
        q = quot r r'
{-# INLINE polyGCD #-}
