module Data.Field.Galois.Binary
  ( Binary
  , BinaryField
  , fromB
  , toB
  , toB'
  ) where

import Protolude as P hiding (Semiring, natVal)

import Control.Monad.Random (Random(..))
import Data.Euclidean (Euclidean(..), GcdDomain)
import Data.Field (Field)
import Data.Group (Group(..))
import Data.Semiring (Ring(..), Semiring(..))
import GHC.Exts (IsList(..))
import GHC.Natural (Natural, naturalFromInteger, naturalToInteger)
import GHC.TypeNats (natVal)
import Test.Tasty.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Field.Galois.Base (GaloisField(..))

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Binary fields @GF(2^q)[X]/\<f(X)\>@ for @q@ positive and
-- @f(X)@ irreducible monic in @GF(2^q)[X]@ encoded as an integer.
class (Bits k, GaloisField k) => BinaryField k where
  {-# MINIMAL fromB #-}
  -- | Convert from @GF(2^q)[X]/\<f(X)\>@ to @Z@.
  fromB :: k -> Integer

-- | Binary field elements.
newtype Binary (p :: Nat) = B Natural
  deriving (Bits, Eq, Generic, NFData, Ord, Show)

-- Binary fields are convertible.
instance KnownNat p => BinaryField (Binary p) where
  fromB (B x) = naturalToInteger x
  {-# INLINABLE fromB #-}

-- Binary fields are Galois fields.
instance KnownNat p => GaloisField (Binary p) where
  char = const 2
  {-# INLINABLE char #-}
  deg  = binLog . natVal
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
  recip (B x)         = B $ binInv x $ natVal (witness :: Binary p)
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Binary fields are numeric.
instance KnownNat p => Num (Binary p) where
  B x + B y   = B $ xor x y
  {-# INLINE (+) #-}
  B x * B y   = B $ binMul (natVal (witness :: Binary p)) x y
  {-# INLINE (*) #-}
  B x - B y   = B $ xor x y
  {-# INLINE (-) #-}
  negate      = identity
  {-# INLINE negate #-}
  fromInteger = B . binMod (natVal (witness :: Binary p)) . naturalFromInteger
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
  arbitrary = B . naturalFromInteger <$>
    choose (0, naturalToInteger $ order (witness :: Binary p) - 1)
  {-# INLINABLE arbitrary #-}

-- Binary fields are lists.
instance KnownNat p => IsList (Binary p) where
  type instance Item (Binary p) = Natural
  fromList     = fromIntegral . foldr' ((. flip shiftL 1) . (+)) 0
  {-# INLINABLE fromList #-}
  toList (B x) = unfoldr unfold x
    where
      unfold y = if y == 0 then Nothing else Just (y .&. 1, shiftR y 1)
  {-# INLINABLE toList #-}

-- Binary fields are pretty.
instance KnownNat p => Pretty (Binary p) where
  pretty (B x) = pretty $ naturalToInteger x

-- Binary fields are random.
instance KnownNat p => Random (Binary p) where
  random  = first (B . naturalFromInteger) .
    randomR (0, naturalToInteger $ order (witness :: Binary p) - 1)
  {-# INLINABLE random #-}
  randomR = panic "Binary.randomR: not implemented."

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Safe convert from @Z@ to @GF(2^q)[X]/\<f(X)\>@.
toB :: KnownNat p => Integer -> Binary p
toB = fromInteger
{-# INLINABLE toB #-}

-- | Unsafe convert from @Z@ to @GF(2^q)[X]/\<f(X)\>@.
toB' :: KnownNat p => Integer -> Binary p
toB' = B . naturalFromInteger
{-# INLINABLE toB' #-}

-------------------------------------------------------------------------------
-- Binary arithmetic
-------------------------------------------------------------------------------

-- Binary logarithm.
binLog :: Natural -> Word
binLog = binLog' 2
  where
    binLog' :: Natural -> Natural -> Word
    binLog' p x
      | x < p     = 0
      | otherwise = case binLog' (p * p) x of
        l -> let l' = 2 * l in binLog'' (P.quot x $ p ^ l') l'
      where
        binLog'' :: Natural -> Word -> Word
        binLog'' y n
          | y < p     = n
          | otherwise = binLog'' (P.quot y p) (n + 1)
{-# INLINE binLog #-}

-- Binary multiplication.
binMul :: Natural -> Natural -> Natural -> Natural
binMul = (. binMul' 0) . (.) . binMod
  where
    binMul' :: Natural -> Natural -> Natural -> Natural
    binMul' n x y
      | y == 0      = n
      | testBit y 0 = binMul' (xor n x) x' y'
      | otherwise   = binMul' n x' y'
      where
        x' = shiftL x 1 :: Natural
        y' = shiftR y 1 :: Natural
{-# INLINE binMul #-}

-- Binary modulus.
binMod :: Natural -> Natural -> Natural
binMod f = binMod'
  where
    m = fromIntegral $ binLog f :: Int
    binMod' :: Natural -> Natural
    binMod' x
      | n < 0     = x
      | otherwise = binMod' (xor x $ shiftL f n)
      where
        n = fromIntegral (binLog x) - m :: Int
{-# INLINE binMod #-}

-- Binary inversion.
binInv :: Natural -> Natural -> Natural
binInv f x = case binInv' 0 1 x f of
  (y, 1) -> y
  _      -> divZeroError
  where
    binInv' :: Natural -> Natural -> Natural -> Natural -> (Natural, Natural)
    binInv' s s' r r'
      | r' == 0   = (s, r)
      | otherwise = binInv' s' (xor s $ shift s' q) r' (xor r $ shift r' q)
      where
        q = max 0 $ fromIntegral (binLog r) - fromIntegral (binLog r') :: Int
{-# INLINE binInv #-}
