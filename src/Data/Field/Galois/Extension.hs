module Data.Field.Galois.Extension
  ( Extension
  , ExtensionField
  , IrreducibleMonic(..)
  , fromE
  , toE
  , toE'
  , pattern X
  , pattern X2
  , pattern X3
  , pattern Y
  ) where

import Protolude as P hiding (Semiring, quot, quotRem, rem)

import Control.Monad.Random (Random(..))
import Data.Euclidean (Euclidean(..), GcdDomain(..))
import Data.Field (Field)
import Data.Poly.Semiring (VPoly, gcdExt, monomial, toPoly, unPoly)
import Data.Semiring as S (Ring(..), Semiring(..))
import Data.Vector (fromList)
import Test.Tasty.QuickCheck (Arbitrary(..), vector)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Field.Galois.Base (GaloisField(..))

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Irreducible monic polynomial @f(X)@ of extension field.
class GaloisField k => IrreducibleMonic k p where
  {-# MINIMAL poly #-}
  -- | Polynomial @f(X)@.
  poly :: Extension k p -> VPoly k

-- | Extension fields @GF(p^q)[X]/\<f(X)\>@ for @p@ prime, @q@ positive, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@.
type ExtensionField = ExtensionGaloisField
class GaloisField k => ExtensionGaloisField k where
  {-# MINIMAL fromE #-}
  -- | Convert from @GF(p^q)[X]/\<f(X)\>@ to @GF(p^q)[X]@.
  fromE :: (GaloisField l, IrreducibleMonic l p, k ~ Extension l p) => k -> [l]

-- | Extension field elements.
newtype Extension k p = E (VPoly k)
  deriving (Eq, Generic, NFData, Ord, Show)

-- Extension fields are convertible.
instance IrreducibleMonic k p => ExtensionGaloisField (Extension k p) where
  fromE (E x) = toList $ unPoly x
  {-# INLINABLE fromE #-}

-- Extension fields are Galois fields.
instance IrreducibleMonic k p => GaloisField (Extension k p) where
  char = const $ char (witness :: k)
  {-# INLINABLE char #-}
  deg  = (deg (witness :: k) *) . deg'
  {-# INLINABLE deg #-}

{-# RULES "Extension.pow"
  forall (k :: IrreducibleMonic k p => Extension k p) n . (^) k n = pow k n
  #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Extension fields are fractional.
instance IrreducibleMonic k p => Fractional (Extension k p) where
  recip (E x)         = case gcdExt x $ poly (witness :: Extension k p) of
    (1, y) -> E y
    _      -> divZeroError
  {-# INLINABLE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Extension fields are numeric.
instance IrreducibleMonic k p => Num (Extension k p) where
  E x + E y    = E $ plus x y
  {-# INLINE (+) #-}
  E x * E y    = E $ rem (times x y) $ poly (witness :: Extension k p)
  {-# INLINABLE (*) #-}
  E x - E y    = E $ x - y
  {-# INLINE (-) #-}
  negate (E x) = E $ S.negate x
  {-# INLINE negate #-}
  fromInteger  = E . fromInteger
  {-# INLINABLE fromInteger #-}
  abs          = panic "Extension.abs: not implemented."
  signum       = panic "Extension.signum: not implemented."

-------------------------------------------------------------------------------
-- Semiring instances
-------------------------------------------------------------------------------

-- Extension fields are Euclidean domains.
instance IrreducibleMonic k p => Euclidean (Extension k p) where
  quotRem = (flip (,) 0 .) . (/)
  {-# INLINE quotRem #-}
  degree  = panic "Extension.degree: not implemented."

-- Extension fields are fields.
instance IrreducibleMonic k p => Field (Extension k p)

-- Extension fields are GCD domains.
instance IrreducibleMonic k p => GcdDomain (Extension k p)

-- Extension fields are rings.
instance IrreducibleMonic k p => Ring (Extension k p) where
  negate = P.negate
  {-# INLINE negate #-}

-- Extension fields are semirings.
instance IrreducibleMonic k p => Semiring (Extension k p) where
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
instance IrreducibleMonic k p => Arbitrary (Extension k p) where
  arbitrary = toE' <$> vector (fromIntegral $ deg' (witness :: Extension k p))
  {-# INLINABLE arbitrary #-}

-- Extension fields are pretty.
instance IrreducibleMonic k p => Pretty (Extension k p) where
  pretty (E x) = pretty $ toList $ unPoly x

-- Extension fields are random.
instance IrreducibleMonic k p => Random (Extension k p) where
  random  = first toE' . unfold (deg' (witness :: Extension k p)) []
    where
      unfold n xs g
        | n <= 0    = (xs, g)
        | otherwise = case random g of
        (x, g') -> unfold (n - 1) (x : xs) g'
  {-# INLINABLE random #-}
  randomR = panic "Extension.randomR: not implemented."

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- Polynomial degree.
deg' :: IrreducibleMonic k p => Extension k p -> Word
deg' = pred . fromIntegral . degree . poly
{-# INLINABLE deg' #-}

-- | Safe convert from @GF(p^q)[X]@ to @GF(p^q)[X]/\<f(X)\>@.
toE :: forall k p . IrreducibleMonic k p => [k] -> Extension k p
toE = E . flip rem (poly (witness :: Extension k p)) . toPoly . fromList
{-# INLINABLE toE #-}

-- | Unsafe convert from @GF(p^q)[X]@ to @GF(p^q)[X]/\<f(X)\>@.
toE' :: forall k p . IrreducibleMonic k p => [k] -> Extension k p
toE' = E . toPoly . fromList
{-# INLINABLE toE' #-}

-- | Pattern for @X@.
pattern X :: GaloisField k => VPoly k
pattern X <- _ where X = toPoly $ fromList [0, 1]

-- | Pattern for @X^2@.
pattern X2 :: GaloisField k => VPoly k
pattern X2 <- _ where X2 = toPoly $ fromList [0, 0, 1]

-- | Pattern for @X^3@.
pattern X3 :: GaloisField k => VPoly k
pattern X3 <- _ where X3 = toPoly $ fromList [0, 0, 0, 1]

-- | Pattern for descending tower of indeterminate variables.
pattern Y :: IrreducibleMonic k p => VPoly k -> VPoly (Extension k p)
pattern Y <- _ where Y = monomial 0 . E
