module AdicField
  ( AdicField
  ) where

import Protolude

import Control.Monad.Random (Random(..), getRandom)
import Test.Tasty.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (GaloisField(..))

-------------------------------------------------------------------------------
-- Adic field type
-------------------------------------------------------------------------------

-- | Adic fields @GF(p^q)[X]/\<f(X)\>@ for @p@ prime, @q@ positive, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@.
newtype AdicField k (im :: Nat) = EF Integer
  deriving (Eq, Generic, Integral, NFData, Ord, Read, Real, Show)

-- Adic fields are Galois fields.
instance (GaloisField k, KnownNat im) => GaloisField (AdicField k im) where

  char       = const (char (witness :: k))
  {-# INLINE char #-}

  deg        = notImplemented
  {-# INLINE deg #-}

  frob       = pow <*> char
  {-# INLINE frob #-}

  fromZ      = notImplemented
  {-# INLINE fromZ #-}

  toZ (EF x) = x
  {-# INLINE toZ #-}

  (.+.)      = adicZip (.+.)
  {-# INLINE (.+.) #-}

  (.-.)      = adicZip (.-.)
  {-# INLINE (.-.) #-}

  (.*.)      = notImplemented
  {-# INLINE (.*.) #-}

  neg        = adicMap neg
  {-# INLINE neg #-}

  inv        = notImplemented
  {-# INLINE inv #-}

  pow        = (^)
  {-# INLINE pow #-}

  rnd        = getRandom
  {-# INLINE rnd #-}

-------------------------------------------------------------------------------
-- Adic field instances
-------------------------------------------------------------------------------

-- Adic fields are arbitrary.
instance (GaloisField k, KnownNat im) => Arbitrary (AdicField k im) where
  arbitrary = EF <$> choose (0, order (witness :: AdicField k im) - 1)

-- Adic fields are bounded.
instance (GaloisField k, KnownNat im) => Bounded (AdicField k im) where
  minBound = 0
  maxBound = -1

-- Adic fields are enumerable.
instance (GaloisField k, KnownNat im) => Enum (AdicField k im) where
  fromEnum = fromIntegral . toZ
  {-# INLINABLE fromEnum #-}
  toEnum   = fromZ . fromIntegral
  {-# INLINABLE toEnum #-}

-- Adic fields are fields.
instance (GaloisField k, KnownNat im) => Fractional (AdicField k im) where
  recip               = inv
  {-# INLINE recip #-}
  fromRational (x:%y) = fromZ x / fromZ y
  {-# INLINABLE fromRational #-}

-- Adic fields are rings.
instance (GaloisField k, KnownNat im) => Num (AdicField k im) where
  (+)         = (.+.)
  {-# INLINE (+) #-}
  (*)         = (.*.)
  {-# INLINE (*) #-}
  (-)         = (.-.)
  {-# INLINE (-) #-}
  negate      = neg
  {-# INLINE negate #-}
  fromInteger = fromZ
  abs         = panic "not implemented."
  signum      = panic "not implemented."

-- Adic fields are pretty.
instance (GaloisField k, KnownNat im) => Pretty (AdicField k im) where
  pretty (EF x) = pretty x

-- Adic fields are random.
instance (GaloisField k, KnownNat im) => Random (AdicField k im) where
  random  = first EF . randomR (0, order (witness :: AdicField k im) - 1)
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Adic field arithmetic
-------------------------------------------------------------------------------

-- | Adic cons function.
adicCons :: (GaloisField k, KnownNat im)
  => k -> AdicField k im -> AdicField k im
adicCons x (EF xs) = EF (toZ x + order x * xs)
{-# INLINE adicCons #-}

-- | Adic uncons function.
adicUncons :: forall k im . (GaloisField k, KnownNat im)
  => AdicField k im -> (k, AdicField k im)
adicUncons (EF xs) = case quotRem xs (order (witness :: k)) of
  (q, r) -> (fromZ r, EF q)
{-# INLINE adicUncons #-}

-- | Adic map function.
adicMap :: forall k im . (GaloisField k, KnownNat im)
  => (k -> k) -> AdicField k im -> AdicField k im
adicMap f = adicMap'
  where
    adicMap' :: AdicField k im -> AdicField k im
    adicMap' x = case adicUncons x of
      (x',  0) -> EF (toZ (f x'))
      (x', xs) -> adicCons (f x') (adicMap' xs)
{-# INLINE adicMap #-}

-- | Adic zip function.
adicZip :: forall k im . (GaloisField k, KnownNat im)
  => (k -> k -> k) -> AdicField k im -> AdicField k im -> AdicField k im
adicZip f = adicZip'
  where
    adicZip' :: AdicField k im -> AdicField k im -> AdicField k im
    adicZip' x y = case (adicUncons x, adicUncons y) of
      ((x',  0), (y',  0)) -> EF (toZ (f x' y'))
      ((x', xs), (y', ys)) -> adicCons (f x' y') (adicZip' xs ys)
{-# INLINE adicZip #-}

-- | Adic last and length function.
adicLead :: forall k im . (GaloisField k, KnownNat im)
  => AdicField k im -> (k, Int)
adicLead = first fromZ . adicLead' (order (witness :: k)) . toZ
  where
    adicLead' :: Integer -> Integer -> (Integer, Int)
    adicLead' p x
      | x < p     = (x, 0)
      | otherwise = case adicLead' (p * p) x of
        (_, l) -> let l' = 2 * l in adicLead'' (quot x (p ^ l'), l')
      where
        adicLead'' :: (Integer, Int) -> (Integer, Int)
        adicLead'' yn@(y, n)
          | y < p     = yn
          | otherwise = adicLead'' (quot y p, n + 1)
{-# INLINE adicLead #-}
