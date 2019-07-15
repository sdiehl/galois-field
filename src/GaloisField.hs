module GaloisField
  ( GaloisField(..)
  ) where

import Protolude

import Control.Monad.Random (MonadRandom, Random)
import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-- | Galois fields @GF(p^q)@ for @p@ prime and @q@ non-negative.
class (Arbitrary k, Eq k, Fractional k, Pretty k, Random k, Show k)
  => GaloisField k where
  {-# MINIMAL (.+.), (.*.), ((.-.) | neg), ((./.) | inv),
    char, deg, frob, pow, rnd #-}

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

  -- | Addition of @x@ with @y@.
  infixl 6 .+.
  (.+.) :: k -> k -> k

  -- | Multiplication of @x@ with @y@.
  infixl 7 .*.
  (.*.) :: k -> k -> k

  -- | Negation of @x@.
  neg :: k -> k
  neg = (.-.) 0
  {-# INLINE neg #-}

  -- | Subtraction of @x@ by @y@.
  infixl 6 .-.
  (.-.) :: k -> k -> k
  (.-.) = (. neg) . (.+.)
  {-# INLINE (.-.) #-}

  -- | Inversion of @x@.
  inv :: k -> k
  inv = (./.) 1
  {-# INLINE inv #-}

  -- | Division of @x@ by @y@.
  infixl 7 ./.
  (./.) :: k -> k -> k
  (./.) = (. inv) . (.*.)
  {-# INLINE (./.) #-}

  -- | Exponentiation @x@ to the power of @y@.
  pow :: k -> Integer -> k

  -- | Randomised element @x@ of field.
  rnd :: MonadRandom m => m k
