module ExtensionField
  ( ExtensionField
  , PolynomialRing
  , IrreducibleMonic(split)
  , fromField
  , toField
  , pattern X
  , pattern X2
  , pattern X3
  , pattern Y
  ) where

import Protolude as P hiding (Semiring, quot, quotRem, rem)

import Control.Monad.Random (Random(..))
import Data.Euclidean (Euclidean(..), GcdDomain(..))
import Data.Poly.Semiring (VPoly, leading, monomial, scale, toPoly, unPoly, pattern X)
import Data.Semiring as S (Ring(..), Semiring(..))
import Data.Vector (fromList)
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

-- | Polynomial rings.
type PolynomialRing = VPoly

-- | Irreducible monic splitting polynomial @f(X)@ of extension field.
class GaloisField k => IrreducibleMonic k im where
  {-# MINIMAL split #-}
  -- | Splitting polynomial @f(X)@.
  split :: ExtensionField k im -> VPoly k
  -- Splitting polynomial degree.
  deg' :: ExtensionField k im -> Int
  deg' = pred . fromIntegral . degree . split
  {-# INLINABLE deg' #-}

-- Extension fields are Galois fields.
instance IrreducibleMonic k im => GaloisField (ExtensionField k im) where
  char = const (char (witness :: k))
  {-# INLINABLE char #-}
  deg  = (deg (witness :: k) *) . deg'
  {-# INLINABLE deg #-}
  frob = pow <*> char
  {-# INLINABLE frob #-}

{-# RULES "ExtensionField/pow"
  forall (k :: IrreducibleMonic k im => ExtensionField k im) n . (^) k n = pow k n
  #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Extension fields are fractional.
instance IrreducibleMonic k im => Fractional (ExtensionField k im) where
  recip (EF x)        = EF (polyInv x (split (witness :: ExtensionField k im)))
  {-# INLINABLE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Extension fields are numeric.
instance IrreducibleMonic k im => Num (ExtensionField k im) where
  EF x + EF y   = EF (plus x y)
  {-# INLINE (+) #-}
  EF x * EF y   = EF (rem (times x y) (split (witness :: ExtensionField k im)))
  {-# INLINABLE (*) #-}
  EF x - EF y   = EF (x - y)
  {-# INLINE (-) #-}
  negate (EF x) = EF (S.negate x)
  {-# INLINE negate #-}
  fromInteger   = EF . fromInteger
  {-# INLINABLE fromInteger #-}
  abs           = panic "not implemented."
  signum        = panic "not implemented."

-------------------------------------------------------------------------------
-- Semiring instances
-------------------------------------------------------------------------------

-- Extension fields are Euclidean domains.
instance IrreducibleMonic k im => Euclidean (ExtensionField k im) where
  quotRem = (flip (,) 0 .) . (/)
  {-# INLINE quotRem #-}
  degree  = panic "not implemented."

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
  {-# INLINABLE fromNatural #-}

-------------------------------------------------------------------------------
-- Other instances
-------------------------------------------------------------------------------

-- Extension fields are arbitrary.
instance IrreducibleMonic k im => Arbitrary (ExtensionField k im) where
  arbitrary = toField <$> vector (deg' (witness :: ExtensionField k im))
  {-# INLINABLE arbitrary #-}

-- Extension fields are pretty.
instance IrreducibleMonic k im => Pretty (ExtensionField k im) where
  pretty (EF x) = pretty (toList (unPoly x))

-- Extension fields are random.
instance IrreducibleMonic k im => Random (ExtensionField k im) where
  random  = first toField . unfold (deg' (witness :: ExtensionField k im)) []
    where
      unfold n xs g
        | n <= 0    = (xs, g)
        | otherwise = case random g of
        (x, g') -> unfold (n - 1) (x : xs) g'
  {-# INLINABLE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Type conversions
-------------------------------------------------------------------------------

-- | Convert from field element to list representation.
fromField :: ExtensionField k im -> [k]
fromField (EF x) = toList (unPoly x)
{-# INLINABLE fromField #-}

-- | Convert from list representation to field element.
toField :: forall k im . IrreducibleMonic k im => [k] -> ExtensionField k im
toField = EF . flip rem (split (witness :: ExtensionField k im)) . toPoly . fromList
{-# INLINABLE toField #-}

-- | Pattern for @X^2@.
pattern X2 :: GaloisField k => VPoly k
pattern X2 <- _ where X2 = toPoly (fromList [0, 0, 1])

-- | Pattern for @X^3@.
pattern X3 :: GaloisField k => VPoly k
pattern X3 <- _ where X3 = toPoly (fromList [0, 0, 0, 1])

-- | Pattern for descending tower of indeterminate variables.
pattern Y :: IrreducibleMonic k im => VPoly k -> VPoly (ExtensionField k im)
pattern Y <- _ where Y = monomial 0 . EF

-------------------------------------------------------------------------------
-- Polynomial arithmetic
-------------------------------------------------------------------------------

-- Polynomial inversion algorithm.
polyInv :: GaloisField k => VPoly k -> VPoly k -> VPoly k
polyInv xs ps = case first leading (polyGCD xs ps) of
  (Just (0, x), ys) -> scale 0 (recip x) ys
  _                 -> panic "no multiplicative inverse."
{-# INLINABLE polyInv #-}

-- Polynomial extended greatest common divisor algorithm.
polyGCD :: forall k . GaloisField k => VPoly k -> VPoly k -> (VPoly k, VPoly k)
polyGCD x y = polyGCD' 0 1 y x
  where
    polyGCD' :: VPoly k -> VPoly k -> VPoly k -> VPoly k -> (VPoly k, VPoly k)
    polyGCD' s _  r 0  = (r, s)
    polyGCD' s s' r r' = case quot r r' of
      q -> polyGCD' s' (s - times q s') r' (r - times q r')
{-# INLINABLE polyGCD #-}
