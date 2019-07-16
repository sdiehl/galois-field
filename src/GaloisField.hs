module GaloisField
  ( GaloisField(..)
  ) where

import Protolude

import Control.Monad.Random (MonadRandom, Random)
import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Galois field class
-------------------------------------------------------------------------------

-- | Galois fields @GF(p^q)@ for @p@ prime and @q@ non-negative.
class (Arbitrary k, Bounded k, Fractional k, Integral k,
       Pretty k, Random k, Read k, Show k) => GaloisField k where
  {-# MINIMAL add, char, deg, (dvd | inv), frob,
    fromZ, mul, (neg | sub), pow, rnd, toZ #-}

  -- Characteristics

  -- | Characteristic @p@ of field and order of prime subfield.
  char :: k -> Integer

  -- | Degree @q@ of field as extension field over prime subfield.
  deg :: k -> Int

  -- | Order @p^q@ of field.
  order :: k -> Integer
  order = (^) <$> char <*> deg
  {-# INLINE order #-}

  -- | Frobenius endomorphism @x->x^p@ of prime subfield.
  frob :: k -> k

  -- Functions

  -- | Convert an integer to a field element.
  fromZ :: Integer -> k

  -- | Convert a field element to an integer.
  toZ :: k -> Integer

  -- | Addition of field elements as integers.
  add :: k -> Integer -> Integer -> Integer

  -- | Subtraction of field elements as integers.
  sub :: k -> Integer -> Integer -> Integer
  sub w x y = add w x (neg w y)
  {-# INLINE sub #-}

  -- | Multiplication of field elements as integers.
  mul :: k -> Integer -> Integer -> Integer

  -- | Division of field elements as integers.
  dvd :: k -> Integer -> Integer -> Integer
  dvd w x y = mul w x (inv w y)
  {-# INLINE dvd #-}

  -- | Negation of a field element as an integer.
  neg :: k -> Integer -> Integer
  neg = flip sub 0
  {-# INLINE neg #-}

  -- | Inversion of a field element as an integer.
  inv :: k -> Integer -> Integer
  inv = flip dvd 1
  {-# INLINE inv #-}

  -- | Exponentiation of a field element to an integer.
  pow :: k -> Integer -> k

  -- | Randomised field element.
  rnd :: MonadRandom m => m k
