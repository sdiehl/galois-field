module PrimeField
  ( PrimeField
  , toInt
  ) where

import Protolude

import Control.Monad.Random (Random(..), getRandom)
import GHC.Integer.GMP.Internals (powModInteger, recipModInteger)
import Test.Tasty.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (GaloisField(..))

-------------------------------------------------------------------------------
-- Prime field type
-------------------------------------------------------------------------------

-- | Prime fields @GF(p)@ for @p@ prime.
newtype PrimeField (p :: Nat) = PF Integer
  deriving (Bits, Eq, Generic, NFData, Read, Show)

-- Prime fields are Galois fields.
instance KnownNat p => GaloisField (PrimeField p) where
  char           = natVal
  {-# INLINE char #-}
  deg            = const 1
  {-# INLINE deg #-}
  frob           = identity
  {-# INLINE frob #-}
  pow w@(PF x) n = PF (powModInteger x n (natVal w))
  {-# INLINE pow #-}
  rnd            = getRandom
  {-# INLINE rnd #-}
  sr w@(PF x)    = let p = natVal w
                   in if p == 2 || x == 0 then Just w else PF <$> sqrtP p x
  {-# INLINE sr #-}

-------------------------------------------------------------------------------
-- Prime field conversions
-------------------------------------------------------------------------------

-- | Embed field element to integers.
toInt :: PrimeField p -> Integer
toInt (PF x) = x
{-# INLINABLE toInt #-}

-------------------------------------------------------------------------------
-- Prime field instances
-------------------------------------------------------------------------------

-- Prime fields are arbitrary.
instance KnownNat p => Arbitrary (PrimeField p) where
  arbitrary = fromInteger <$> arbitrary

-- Prime fields are fields.
instance KnownNat p => Fractional (PrimeField p) where
  recip w@(PF x)      = PF (if x == 0 then panic "no multiplicative inverse."
                            else recipModInteger x (natVal w))
  {-# INLINE recip #-}
  fromRational (x:%y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-- Prime fields are rings.
instance KnownNat p => Num (PrimeField p) where
  w@(PF x) + PF y = PF (if xyp >= 0 then xyp else xy)
    where
      xy  = x + y
      xyp = xy - natVal w
  {-# INLINE (+) #-}
  w@(PF x) * PF y = PF (rem (x * y) (natVal w))
  {-# INLINE (*) #-}
  w@(PF x) - PF y = PF (if xy >= 0 then xy else xy + natVal w)
    where
      xy = x - y
  {-# INLINE (-) #-}
  negate w@(PF x) = PF (if x == 0 then 0 else -x + natVal w)
  {-# INLINE negate #-}
  fromInteger x   = PF (if y >= 0 then y else y + p)
    where
      y = rem x p
      p = natVal (witness :: PrimeField p)
  {-# INLINABLE fromInteger #-}
  abs             = panic "not implemented."
  signum          = panic "not implemented."

-- Prime fields are pretty.
instance KnownNat p => Pretty (PrimeField p) where
  pretty (PF x) = pretty x

-- Prime fields are random.
instance KnownNat p => Random (PrimeField p) where
  random  = first PF . randomR (0, natVal (witness :: PrimeField p) - 1)
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Prime field arithmetic
-------------------------------------------------------------------------------

-- Check quadratic nonresidue.
isQNR :: Integer -> Integer -> Bool
isQNR p n = powModInteger n (shiftR (p - 1) 1) p /= 1
{-# INLINE isQNR #-}

-- Factor binary powers.
factor2 :: Integer -> (Integer, Int)
factor2 p = factor 0 (p - 1)
  where
    factor :: Int -> Integer -> (Integer, Int)
    factor s q
      | testBit q 0 = (q, s)
      | otherwise   = factor (s + 1) (shiftR q 1)
{-# INLINE factor2 #-}

-- Get quadratic nonresidue.
getQNR :: Integer -> Integer
getQNR p
  | p7 == 3 || p7 == 5 = 2
  | otherwise          = case find (isQNR p) ps of
    Just q -> q
    _      -> panic "no quadratic nonresidue."
  where
    p7 = p .&. 7
    ps = 3 : 5 : 7 : 11 : 13 : concatMap (\x -> [x - 1, x + 1]) [18, 24 ..]
{-# INLINE getQNR #-}

-- Return minimal element.
returnMin :: Integer -> Integer -> Integer
returnMin p x = min x (p - x)

-- Prime square root.
sqrtP :: Integer -> Integer -> Maybe Integer
sqrtP p n
  | isQNR p n = Nothing
  | otherwise = returnMin p <$> case (factor2 p, getQNR p) of
    ((q, s), z) -> let zq  = powModInteger z q p
                       nq  = powModInteger n (quot q 2) p
                       nnq = rem (n * nq) p
                   in loop s zq (rem (nq * nnq) p) nnq
      where
        loop :: Int -> Integer -> Integer -> Integer -> Maybe Integer
        loop m c t r
          | t == 0    = Just 0
          | t == 1    = Just r
          | otherwise = let i  = least t 0
                            b  = powModInteger c (bit (m - i - 1)) p
                            b2 = rem (b * b) p
                        in loop i b2 (rem (t * b2) p) (rem (r * b) p)
          where
            least :: Integer -> Int -> Int
            least 1  j = j
            least ti j = least (rem (ti * ti) p) (j + 1)
{-# INLINE sqrtP #-}
