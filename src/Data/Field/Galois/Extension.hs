module Data.Field.Galois.Extension
  ( Extension
  , ExtensionField(..)
  , IrreducibleMonic(split)
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

-- | Irreducible monic splitting polynomial @f(X)@ of extension field.
class GaloisField k => IrreducibleMonic k im where
  {-# MINIMAL split #-}
  -- | Splitting polynomial @f(X)@.
  split :: Extension k im -> VPoly k
  -- Splitting polynomial degree.
  deg' :: Extension k im -> Word
  deg' = pred . fromIntegral . degree . split
  {-# INLINABLE deg' #-}

-- | Extension fields @GF(p^q)[X]/\<f(X)\>@ for @p@ prime, @q@ positive, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@.
class GaloisField k => ExtensionField k where
  {-# MINIMAL fromE #-}
  -- | Convert from @GF(p^q)[X]/\<f(X)\>@ to @GF(p^q)[X]@.
  fromE :: (GaloisField l, IrreducibleMonic l im, k ~ Extension l im) => k -> [l]

-- | Extension field elements.
newtype Extension k im = E (VPoly k)
  deriving (Eq, Generic, NFData, Ord, Show)

-- Extension fields are convertible.
instance IrreducibleMonic k im => ExtensionField (Extension k im) where
  fromE (E x) = toList $ unPoly x
  {-# INLINABLE fromE #-}

-- Extension fields are Galois fields.
instance IrreducibleMonic k im => GaloisField (Extension k im) where
  char = const $ char (witness :: k)
  {-# INLINABLE char #-}
  deg  = (deg (witness :: k) *) . deg'
  {-# INLINABLE deg #-}

{-# RULES "Extension.pow"
  forall (k :: IrreducibleMonic k im => Extension k im) n . (^) k n = pow k n
  #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Extension fields are fractional.
instance IrreducibleMonic k im => Fractional (Extension k im) where
  recip (E x)         = case gcdExt x $ split (witness :: Extension k im) of
    (1, y) -> E y
    _      -> divZeroError
  {-# INLINABLE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Extension fields are numeric.
instance IrreducibleMonic k im => Num (Extension k im) where
  E x + E y    = E $ plus x y
  {-# INLINE (+) #-}
  E x * E y    = E $ rem (times x y) $ split (witness :: Extension k im)
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
instance IrreducibleMonic k im => Euclidean (Extension k im) where
  quotRem = (flip (,) 0 .) . (/)
  {-# INLINE quotRem #-}
  degree  = panic "Extension.degree: not implemented."

-- Extension fields are fields.
instance IrreducibleMonic k im => Field (Extension k im)

-- Extension fields are GCD domains.
instance IrreducibleMonic k im => GcdDomain (Extension k im)

-- Extension fields are rings.
instance IrreducibleMonic k im => Ring (Extension k im) where
  negate = P.negate
  {-# INLINE negate #-}

-- Extension fields are semirings.
instance IrreducibleMonic k im => Semiring (Extension k im) where
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
instance IrreducibleMonic k im => Arbitrary (Extension k im) where
  arbitrary = toE' <$> vector (fromIntegral $ deg' (witness :: Extension k im))
  {-# INLINABLE arbitrary #-}

-- Extension fields are pretty.
instance IrreducibleMonic k im => Pretty (Extension k im) where
  pretty (E x) = pretty $ toList $ unPoly x

-- Extension fields are random.
instance IrreducibleMonic k im => Random (Extension k im) where
  random  = first toE' . unfold (deg' (witness :: Extension k im)) []
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

-- | Safe convert from @GF(p^q)[X]@ to @GF(p^q)[X]/\<f(X)\>@.
toE :: forall k im . IrreducibleMonic k im => [k] -> Extension k im
toE = E . flip rem (split (witness :: Extension k im)) . toPoly . fromList
{-# INLINABLE toE #-}

-- | Unsafe convert from @GF(p^q)[X]@ to @GF(p^q)[X]/\<f(X)\>@.
toE' :: forall k im . IrreducibleMonic k im => [k] -> Extension k im
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
pattern Y :: IrreducibleMonic k im => VPoly k -> VPoly (Extension k im)
pattern Y <- _ where Y = monomial 0 . E
