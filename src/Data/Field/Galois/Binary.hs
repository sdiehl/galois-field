module Data.Field.Galois.Binary
  ( Binary
  , BinaryField(..)
  , toB
  , toB'
  ) where

import Protolude as P hiding (Semiring)

import Control.Monad.Random (Random(..))
import Data.Euclidean (Euclidean(..), GcdDomain(..))
import Data.Field (Field)
import Data.Semiring (Ring(..), Semiring(..))
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
newtype Binary (im :: Nat) = B Integer
  deriving (Bits, Eq, Generic, NFData, Ord, Show)

-- Binary fields are convertible.
instance KnownNat im => BinaryField (Binary im) where
  fromB (B x) = x
  {-# INLINABLE fromB #-}

-- Binary fields are Galois fields.
instance KnownNat im => GaloisField (Binary im) where
  char = const 2
  {-# INLINABLE char #-}
  deg  = binLog . natVal
  {-# INLINABLE deg #-}
  frob = flip pow (2 :: Int)
  {-# INLINABLE frob #-}

{-# RULES "Binary.pow"
  forall (k :: KnownNat im => Binary im) n . (^) k n = pow k n
  #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Binary fields are fractional.
instance KnownNat im => Fractional (Binary im) where
  recip (B x)         = B (binInv x (natVal (witness :: Binary im)))
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Binary fields are numeric.
instance KnownNat im => Num (Binary im) where
  B x + B y   = B (xor x y)
  {-# INLINE (+) #-}
  B x * B y   = B (binMul (natVal (witness :: Binary im)) x y)
  {-# INLINE (*) #-}
  B x - B y   = B (xor x y)
  {-# INLINE (-) #-}
  negate      = identity
  {-# INLINE negate #-}
  fromInteger = B . binMod (natVal (witness :: Binary im))
  {-# INLINABLE fromInteger #-}
  abs         = panic "Binary.abs: not implemented."
  signum      = panic "Binary.signum: not implemented."

-------------------------------------------------------------------------------
-- Semiring instances
-------------------------------------------------------------------------------

-- Binary fields are Euclidean domains.
instance KnownNat im => Euclidean (Binary im) where
  quotRem = (flip (,) 0 .) . (/)
  {-# INLINE quotRem #-}
  degree  = panic "Binary.degree: not implemented."

-- Binary fields are fields.
instance KnownNat im => Field (Binary im) where

-- Binary fields are GCD domains.
instance KnownNat im => GcdDomain (Binary im)

-- Binary fields are rings.
instance KnownNat im => Ring (Binary im) where
  negate = P.negate
  {-# INLINE negate #-}

-- Binary fields are semirings.
instance KnownNat im => Semiring (Binary im) where
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

-- Binary fields are arbitrary.
instance KnownNat im => Arbitrary (Binary im) where
  arbitrary = B <$> choose (0, order (witness :: Binary im) - 1)
  {-# INLINABLE arbitrary #-}

-- Binary fields are pretty.
instance KnownNat im => Pretty (Binary im) where
  pretty (B x) = pretty x

-- Binary fields are random.
instance KnownNat im => Random (Binary im) where
  random  = first B . randomR (0, order (witness :: Binary im) - 1)
  {-# INLINABLE random #-}
  randomR = panic "Binary.randomR: not implemented."

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Safe convert from @Z@ to @GF(2^q)[X]/\<f(X)\>@.
toB :: KnownNat im => Integer -> Binary im
toB = fromInteger
{-# INLINABLE toB #-}

-- | Unsafe convert from @Z@ to @GF(2^q)[X]/\<f(X)\>@.
toB' :: KnownNat im => Integer -> Binary im
toB' = B
{-# INLINABLE toB' #-}

-------------------------------------------------------------------------------
-- Binary arithmetic
-------------------------------------------------------------------------------

-- Binary logarithm.
binLog :: Integer -> Int
binLog = binLog' 2
  where
    binLog' :: Integer -> Integer -> Int
    binLog' p x
      | x < p     = 0
      | otherwise = case binLog' (p * p) x of
        l -> let l' = 2 * l in binLog'' (P.quot x (p ^ l')) l'
      where
        binLog'' :: Integer -> Int -> Int
        binLog'' y n
          | y < p     = n
          | otherwise = binLog'' (P.quot y p) (n + 1)
{-# INLINE binLog #-}

-- Binary multiplication.
binMul :: Integer -> Integer -> Integer -> Integer
binMul = (. binMul' 0) . (.) . binMod
  where
    binMul' :: Integer -> Integer -> Integer -> Integer
    binMul' n x y
      | y == 0      = n
      | testBit y 0 = binMul' (xor n x) x' y'
      | otherwise   = binMul' n x' y'
      where
        x' = shiftL x 1 :: Integer
        y' = shiftR y 1 :: Integer
{-# INLINE binMul #-}

-- Binary modulus.
binMod :: Integer -> Integer -> Integer
binMod f = binMod'
  where
    m = binLog f :: Int
    binMod' :: Integer -> Integer
    binMod' x
      | n < 0     = x
      | otherwise = binMod' (xor x (shiftL f n))
      where
        n = binLog x - m :: Int
{-# INLINE binMod #-}

-- Binary inversion.
binInv :: Integer -> Integer -> Integer
binInv f x = case binInv' 0 1 x f of
  (y, 1) -> y
  _      -> divZeroError
  where
    binInv' :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
    binInv' s s' r r'
      | r' == 0   = (s, r)
      | otherwise = binInv' s' (xor s (shift s' q)) r' (xor r (shift r' q))
      where
        q = max 0 (binLog r - binLog r') :: Int
{-# INLINE binInv #-}
