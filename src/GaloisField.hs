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
  {-# MINIMAL char, deg, frob, pow, rnd #-}

  -- Characteristics

  -- | Characteristic @p@ of field and order of prime subfield.
  char :: k -> Integer

  -- | Degree @q@ of field as extension field over prime subfield.
  deg :: k -> Int

  -- | Frobenius endomorphism @x->x^p@ of prime subfield.
  frob :: k -> k

  -- | Order @p^q@ of field.
  order :: k -> Integer
  order = (^) <$> char <*> deg
  {-# INLINE order #-}

  -- Functions

  -- | Exponentiation @x@ to the power of @y@.
  pow :: k -> Integer -> k

  -- | Randomised element @x@ of field.
  rnd :: MonadRandom m => m k
