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
class (Arbitrary k, Eq k, Fractional k, Pretty k, Random k, Show k)
  => GaloisField k where
  {-# MINIMAL (.+.), (.*.), ((.-.) | neg), ((./.) | inv),
    char, deg, frob, fromZ, pow, rnd, toZ #-}

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

  -- | Addition of field elements.
  infixl 6 .+.
  (.+.) :: k -> k -> k

  -- | Multiplication of field elements.
  infixl 7 .*.
  (.*.) :: k -> k -> k

  -- | Negation of a field element.
  neg :: k -> k
  neg = (.-.) 0
  {-# INLINE neg #-}

  -- | Subtraction of field elements.
  infixl 6 .-.
  (.-.) :: k -> k -> k
  (.-.) = (. neg) . (.+.)
  {-# INLINE (.-.) #-}

  -- | Inversion of a field element.
  inv :: k -> k
  inv = (./.) 1
  {-# INLINE inv #-}

  -- | Division of field elements.
  infixl 7 ./.
  (./.) :: k -> k -> k
  (./.) = (. inv) . (.*.)
  {-# INLINE (./.) #-}

  -- | Exponentiation of a field element to an integer.
  pow :: k -> Integer -> k

  -- | Randomised field element.
  rnd :: MonadRandom m => m k
