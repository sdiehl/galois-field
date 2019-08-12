module BinaryField
  ( BinaryField
  ) where

import Protolude as P hiding (Semiring)

import Control.Monad.Random (Random(..))
import Data.Euclidean (Euclidean(..), GcdDomain(..))
import Data.Semiring (Ring(..), Semiring(..))
import Test.Tasty.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (Field(..), GaloisField(..))

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Binary fields @GF(2^q)[X]/\<f(X)\>@ for @q@ positive and
-- @f(X)@ irreducible monic in @GF(2^q)[X]@ encoded as an integer.
newtype BinaryField (im :: Nat) = BF Integer
  deriving (Eq, Generic, Ord, Show)

-- Binary fields are Galois fields.
instance KnownNat im => GaloisField (BinaryField im) where
  char = const 2
  {-# INLINABLE char #-}
  deg  = binLog . natVal
  {-# INLINABLE deg #-}
  frob = flip pow 2
  {-# INLINABLE frob #-}

{-# RULES "BinaryField/pow"
  forall (k :: KnownNat im => BinaryField im) n . (^) k n = pow k n
  #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Binary fields are fractional.
instance KnownNat im => Fractional (BinaryField im) where
  recip (BF x)        = BF (binInv x (natVal (witness :: BinaryField im)))
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Binary fields are numeric.
instance KnownNat im => Num (BinaryField im) where
  BF x + BF y = BF (xor x y)
  {-# INLINE (+) #-}
  BF x * BF y = BF (binMul (natVal (witness :: BinaryField im)) x y)
  {-# INLINE (*) #-}
  BF x - BF y = BF (xor x y)
  {-# INLINE (-) #-}
  negate      = identity
  {-# INLINE negate #-}
  fromInteger = BF . binMod (natVal (witness :: BinaryField im))
  {-# INLINABLE fromInteger #-}
  abs         = panic "not implemented."
  signum      = panic "not implemented."

-------------------------------------------------------------------------------
-- Semiring instances
-------------------------------------------------------------------------------

-- Binary fields are Euclidean domains.
instance KnownNat im => Euclidean (BinaryField im) where
  quotRem = (flip (,) 0 .) . (/)
  {-# INLINE quotRem #-}
  degree  = panic "not implemented."

-- Binary fields are fields.
instance KnownNat im => Field (BinaryField im) where
  invert = recip
  {-# INLINE invert #-}
  minus  = (-)
  {-# INLINE minus #-}

-- Binary fields are GCD domains.
instance KnownNat im => GcdDomain (BinaryField im)

-- Binary fields are rings.
instance KnownNat im => Ring (BinaryField im) where
  negate = P.negate
  {-# INLINE negate #-}

-- Binary fields are semirings.
instance KnownNat im => Semiring (BinaryField im) where
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
instance KnownNat im => Arbitrary (BinaryField im) where
  arbitrary = BF <$> choose (0, order (witness :: BinaryField im) - 1)
  {-# INLINABLE arbitrary #-}

-- Binary fields are pretty.
instance KnownNat im => Pretty (BinaryField im) where
  pretty (BF x) = pretty x

-- Binary fields are random.
instance KnownNat im => Random (BinaryField im) where
  random  = first BF . randomR (0, order (witness :: BinaryField im) - 1)
  {-# INLINABLE random #-}
  randomR = panic "not implemented."

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
  _      -> panic "no multiplicative inverse."
  where
    binInv' :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
    binInv' s s' r r'
      | r' == 0   = (s, r)
      | otherwise = binInv' s' (xor s (shift s' q)) r' (xor r (shift r' q))
      where
        q = max 0 (binLog r - binLog r') :: Int
{-# INLINE binInv #-}
