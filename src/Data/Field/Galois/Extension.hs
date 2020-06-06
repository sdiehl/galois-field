module Data.Field.Galois.Extension
  ( Extension
  , ExtensionField
  , IrreducibleMonic(..)
  , fromE
  , conj
  , toE
  , toE'
  , pattern U
  , pattern U2
  , pattern U3
  , pattern V
  , pattern X
  , pattern X2
  , pattern X3
  , pattern Y
  ) where

import Protolude as P hiding (Semiring, rem, toList)

import Control.Monad.Random (Random(..))
import Data.Euclidean (Euclidean(..), GcdDomain, gcdExt)
import Data.Field (Field)
import Data.Group (Group(..))
import Data.Poly (VPoly, monomial, toPoly, unPoly, scale, leading)
import Data.Semiring (Ring(..), Semiring(..))
import GHC.Exts (IsList(..))
import Test.QuickCheck (Arbitrary(..), vector)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Field.Galois.Base (GaloisField(..))
import Data.Field.Galois.Frobenius (frobenius)

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Irreducible monic polynomial @f(X)@ of extension field.
class GaloisField k => IrreducibleMonic p k where
  {-# MINIMAL poly #-}
  -- | Polynomial @f(X)@.
  poly :: Extension p k -> VPoly k

-- | Extension fields @GF(p^q)[X]/\<f(X)\>@ for @p@ prime, @q@ positive, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@.
class GaloisField k => ExtensionField k where
  {-# MINIMAL fromE #-}
  -- | Convert from @GF(p^q)[X]/\<f(X)\>@ to @GF(p^q)[X]@.
  fromE :: (GaloisField l, IrreducibleMonic p l, k ~ Extension p l) => k -> [l]

-- | Extension field elements.
newtype Extension p k = E (VPoly k)
  deriving (Eq, Generic, NFData, Ord, Show)

-- Extension fields are convertible.
instance IrreducibleMonic p k => ExtensionField (Extension p k) where
  fromE = toList
  {-# INLINABLE fromE #-}

-- Extension fields are Galois fields.
instance IrreducibleMonic p k => GaloisField (Extension p k) where
  char         = const $ char (witness :: k)
  {-# INLINABLE char #-}
  deg          = (deg (witness :: k) *) . deg'
  {-# INLINABLE deg #-}
  frob y@(E x) = case frobenius (unPoly x) (unPoly $ poly y) of
    Just z  -> E $ toPoly z
    Nothing -> pow y $ char y
  {-# INLINABLE frob #-}

{-# RULES "Extension.pow"
  forall (k :: IrreducibleMonic p k => Extension p k) n . (^) k n = pow k n
  #-}

-------------------------------------------------------------------------------
-- Group instances
-------------------------------------------------------------------------------

-- Extension fields are multiplicative groups.
instance IrreducibleMonic p k => Group (Extension p k) where
  invert        = recip
  {-# INLINE invert #-}
  pow x n
    | n >= 0    = x ^ n
    | otherwise = recip x ^ P.negate n
  {-# INLINE pow #-}

-- Extension fields are multiplicative monoids.
instance IrreducibleMonic p k => Monoid (Extension p k) where
  mempty = E 1
  {-# INLINE mempty #-}

-- Extension fields are multiplicative semigroups.
instance IrreducibleMonic p k => Semigroup (Extension p k) where
  (<>)   = (*)
  {-# INLINE (<>) #-}
  stimes = flip pow
  {-# INLINE stimes #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Extension fields are fractional.
instance IrreducibleMonic p k => Fractional (Extension p k) where
  recip (E x) = case leading g of
    Just (0, c) -> E $ scale 0 (recip c) y
    _           -> divZeroError
    where
      (g, y) = gcdExt x $ poly (witness :: Extension p k)
  {-# INLINABLE recip #-}
  fromRational rat = fromInteger (numerator rat) / fromInteger (denominator rat)
  {-# INLINABLE fromRational #-}

-- Extension fields are numeric.
instance IrreducibleMonic p k => Num (Extension p k) where
  E x + E y    = E $ x + y
  {-# INLINE (+) #-}
  E x * E y    = E $ rem (x * y) $ poly (witness :: Extension p k)
  {-# INLINABLE (*) #-}
  E x - E y    = E $ x - y
  {-# INLINE (-) #-}
  negate (E x) = E $ P.negate x
  {-# INLINE negate #-}
  fromInteger  = E . fromInteger
  {-# INLINABLE fromInteger #-}
  abs          = panic "Extension.abs: not implemented."
  signum       = panic "Extension.signum: not implemented."

-------------------------------------------------------------------------------
-- Semiring instances
-------------------------------------------------------------------------------

-- Extension fields are Euclidean domains.
instance IrreducibleMonic p k => Euclidean (Extension p k) where
  degree  = panic "Extension.degree: not implemented."
  quotRem = (flip (,) 0 .) . (/)
  {-# INLINE quotRem #-}

-- Extension fields are fields.
instance IrreducibleMonic p k => Field (Extension p k)

-- Extension fields are GCD domains.
instance IrreducibleMonic p k => GcdDomain (Extension p k)

-- Extension fields are rings.
instance IrreducibleMonic p k => Ring (Extension p k) where
  negate = P.negate
  {-# INLINE negate #-}

-- Extension fields are semirings.
instance IrreducibleMonic p k => Semiring (Extension p k) where
  fromNatural = fromIntegral
  {-# INLINABLE fromNatural #-}
  one         = E 1
  {-# INLINE one #-}
  plus        = (+)
  {-# INLINE plus #-}
  times       = (*)
  {-# INLINE times #-}
  zero        = E 0
  {-# INLINE zero #-}

-------------------------------------------------------------------------------
-- Other instances
-------------------------------------------------------------------------------

-- Extension fields are arbitrary.
instance IrreducibleMonic p k => Arbitrary (Extension p k) where
  arbitrary = fromList <$> vector (fromIntegral $ deg' (witness :: Extension p k))
  {-# INLINABLE arbitrary #-}

-- Extension fields are lists.
instance IrreducibleMonic p k => IsList (Extension p k) where
  type instance Item (Extension p k) = k
  fromList     = E . fromList
  {-# INLINABLE fromList #-}
  toList (E x) = toList $ unPoly x
  {-# INLINABLE toList #-}

-- Extension fields are pretty.
instance IrreducibleMonic p k => Pretty (Extension p k) where
  pretty (E x) = pretty $ toList x

-- Extension fields are random.
instance IrreducibleMonic p k => Random (Extension p k) where
  random  = first fromList . unfold (deg' (witness :: Extension p k)) []
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
deg' :: IrreducibleMonic p k => Extension p k -> Word
deg' = pred . fromIntegral . degree . poly
{-# INLINABLE deg' #-}

-- | Complex conjugation @a+bi -> a-bi@ of quadratic extension field.
conj :: IrreducibleMonic p k => Extension p k -> Extension p k
conj y@(E x) = case unPoly $ poly y of
  [_, 0, 1] -> case x of
    [a, b] -> [a, P.negate b]
    [a]    -> [a]
    _      -> []
  _         -> panic "Extension.conj: extension degree is not two."
{-# INLINABLE conj #-}

-- | Safe convert from @GF(p^q)[X]@ to @GF(p^q)[X]/\<f(X)\>@.
toE :: forall k p . IrreducibleMonic p k => [k] -> Extension p k
toE = E . flip rem (poly (witness :: Extension p k)) . fromList
{-# INLINABLE toE #-}

-- | Unsafe convert from @GF(p^q)[X]@ to @GF(p^q)[X]/\<f(X)\>@.
toE' :: forall k p . IrreducibleMonic p k => [k] -> Extension p k
toE' = fromList
{-# INLINABLE toE' #-}

-------------------------------------------------------------------------------
-- Pattern synonyms
-------------------------------------------------------------------------------

-- | Pattern for field element @U@.
pattern U :: IrreducibleMonic p k => Extension p k
pattern U <- _ where U = [0, 1]

-- | Pattern for field element @U^2@.
pattern U2 :: IrreducibleMonic p k => Extension p k
pattern U2 <- _ where U2 = toE [0, 0, 1]

-- | Pattern for field element @U^3@.
pattern U3 :: IrreducibleMonic p k => Extension p k
pattern U3 <- _ where U3 = toE [0, 0, 0, 1]

-- | Pattern for descending tower of indeterminate variables for field elements.
pattern V :: IrreducibleMonic p k => k -> Extension p k
pattern V <- _ where V = E . monomial 0

-- | Pattern for monic monomial @X@.
pattern X :: GaloisField k => VPoly k
pattern X <- _ where X = [0, 1]

-- | Pattern for monic monomial @X^2@.
pattern X2 :: GaloisField k => VPoly k
pattern X2 <- _ where X2 = [0, 0, 1]

-- | Pattern for monic monomial @X^3@.
pattern X3 :: GaloisField k => VPoly k
pattern X3 <- _ where X3 = [0, 0, 0, 1]

-- | Pattern for descending tower of indeterminate variables for monic monomials.
pattern Y :: IrreducibleMonic p k => VPoly k -> VPoly (Extension p k)
pattern Y <- _ where Y = monomial 0 . E
