module PrimeField
  ( PrimeField
  , toInt
  ) where

import Protolude as P hiding (Semiring)

import Control.Monad.Random (Random(..), getRandom)
import Data.Euclidean (Euclidean(..), GcdDomain(..))
import Data.Semiring (Ring(..), Semiring(..))
import GHC.Integer.GMP.Internals (powModInteger, recipModInteger)
import Test.Tasty.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (Field(..), GaloisField(..))

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Prime fields @GF(p)@ for @p@ prime.
newtype PrimeField (p :: Nat) = PF Integer
  deriving (Bits, Eq, Generic, Ord, Show)

-- Prime fields are Galois fields.
instance KnownNat p => GaloisField (PrimeField p) where
  char          = natVal
  {-# INLINE char #-}
  deg           = const 1
  {-# INLINE deg #-}
  frob          = identity
  {-# INLINE frob #-}
  pow (PF x) n  = PF (powModInteger x n (natVal (witness :: PrimeField p)))
  {-# INLINE pow #-}
  quad          = primeQuad
  {-# INLINE quad #-}
  rnd           = getRandom
  {-# INLINE rnd #-}
  sr 0          = Just 0
  sr (PF x)     = PF <$> primeSqrt (natVal (witness :: PrimeField p)) x
  {-# INLINE sr #-}

-------------------------------------------------------------------------------
-- Numeric instances
-------------------------------------------------------------------------------

-- Prime fields are fractional.
instance KnownNat p => Fractional (PrimeField p) where
  recip (PF 0)        = panic "no multiplicative inverse."
  recip (PF x)        = PF (recipModInteger x (natVal (witness :: PrimeField p)))
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Prime fields are numeric.
instance KnownNat p => Num (PrimeField p) where
  PF x + PF y   = PF (if xyp >= 0 then xyp else xy)
    where
      xy  = x + y
      xyp = xy - natVal (witness :: PrimeField p)
  {-# INLINE (+) #-}
  PF x * PF y   = PF (P.rem (x * y) (natVal (witness :: PrimeField p)))
  {-# INLINE (*) #-}
  PF x - PF y   = PF (if xy >= 0 then xy else xy + natVal (witness :: PrimeField p))
    where
      xy = x - y
  {-# INLINE (-) #-}
  negate (PF 0) = PF 0
  negate (PF x) = PF (natVal (witness :: PrimeField p) - x)
  {-# INLINE negate #-}
  fromInteger x = PF (if y >= 0 then y else y + p)
    where
      y = P.rem x p
      p = natVal (witness :: PrimeField p)
  {-# INLINABLE fromInteger #-}
  abs           = panic "not implemented."
  signum        = panic "not implemented."

-------------------------------------------------------------------------------
-- Semiring instances
-------------------------------------------------------------------------------

-- Prime fields are Euclidean domains.
instance KnownNat p => Euclidean (PrimeField p) where
  quotRem = (flip (,) 0 .) . (/)
  {-# INLINE quotRem #-}
  degree  = panic "not implemented."
  {-# INLINE degree #-}

-- Prime fields are fields.
instance KnownNat p => Field (PrimeField p) where
  invert = recip
  {-# INLINE invert #-}
  minus  = (-)
  {-# INLINE minus #-}

-- Prime fields are GCD domains.
instance KnownNat p => GcdDomain (PrimeField p)

-- Prime fields are rings.
instance KnownNat p => Ring (PrimeField p) where
  negate = P.negate
  {-# INLINE negate #-}

-- Prime fields are semirings.
instance KnownNat p => Semiring (PrimeField p) where
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

-- Prime fields are arbitrary.
instance KnownNat p => Arbitrary (PrimeField p) where
  arbitrary = fromInteger <$> arbitrary

-- Prime fields are pretty.
instance KnownNat p => Pretty (PrimeField p) where
  pretty (PF x) = pretty x

-- Prime fields are random.
instance KnownNat p => Random (PrimeField p) where
  random  = first PF . randomR (0, natVal (witness :: PrimeField p) - 1)
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Type conversions
-------------------------------------------------------------------------------

-- | Embed field element to integers.
toInt :: PrimeField p -> Integer
toInt (PF x) = x
{-# INLINABLE toInt #-}

-------------------------------------------------------------------------------
-- Quadratic equations
-------------------------------------------------------------------------------

-- Check quadratic nonresidue.
isQNR :: Integer -> Integer -> Bool
isQNR p n = powModInteger n (shiftR p 1) p /= 1
{-# INLINE isQNR #-}

-- Factor binary powers.
factor2 :: Integer -> (Integer, Int)
factor2 p = factor (p - 1, 0)
  where
    factor :: (Integer, Int) -> (Integer, Int)
    factor qs@(q, s)
      | testBit q 0 = qs
      | otherwise   = factor (shiftR q 1, s + 1)
{-# INLINE factor2 #-}

-- Get quadratic nonresidue.
getQNR :: Integer -> Integer
getQNR p
  | p7 == 3   = 2
  | p7 == 5   = 2
  | otherwise = case find (isQNR p) ps of
    Just q -> q
    _      -> panic "no quadratic nonresidue."
  where
    p7 = p .&. 7
    ps = 3 : 5 : 7 : 11 : 13 : concatMap (\x -> [x - 1, x + 1]) [18, 24 ..]
{-# INLINE getQNR #-}

-- Prime square root.
primeSqrt :: Integer -> Integer -> Maybe Integer
primeSqrt p n
  | isQNR p n = Nothing
  | otherwise = case (factor2 p, getQNR p) of
    ((q, s), z) -> let zq  = powModInteger z q p
                       nq  = powModInteger n (shiftR q 1) p
                       nnq = P.rem (n * nq) p
                   in loop s zq (P.rem (nq * nnq) p) nnq
      where
        loop :: Int -> Integer -> Integer -> Integer -> Maybe Integer
        loop _ _ 0 _ = Just 0
        loop _ _ 1 r = Just r
        loop m c t r = let i  = least t 0
                           b  = powModInteger c (bit (m - i - 1)) p
                           b2 = P.rem (b * b) p
                       in loop i b2 (P.rem (t * b2) p) (P.rem (r * b) p)
          where
            least :: Integer -> Int -> Int
            least 1  j = j
            least ti j = least (P.rem (ti * ti) p) (j + 1)
{-# INLINE primeSqrt #-}

-- Prime quadratic @ax^2+bx+c=0@.
primeQuad :: KnownNat p
  => PrimeField p -> PrimeField p -> PrimeField p -> Maybe (PrimeField p)
primeQuad 0 _ _ = Nothing
primeQuad a b c = (/ (2 * a)) . subtract b <$> sr (b * b - 4 * a * c)
{-# INLINE primeQuad #-}
