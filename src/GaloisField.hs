module GaloisField
  ( GaloisField(..)
  ) where

import Protolude

import Control.Monad.Random (MonadRandom, Random)
import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-- | Galois fields @GF(p^q)@ for @p@ prime and @q@ non-negative
class (Arbitrary k, Eq k, Fractional k, Pretty k, Random k, Show k)
  => GaloisField k where
  {-# MINIMAL char, deg, pow, rnd #-}

  -- Characteristics
  char :: k -> Integer  -- ^ Characteristic @q@ of field

  deg :: k -> Int       -- ^ Degree @q@ of field

  order :: k -> Integer -- ^ Order @p^q@ of field
  order = (^) <$> char <*> deg
  {-# INLINE order #-}

  -- Functions
  pow :: k -> Integer -> k -- @x@ to the power of @y@

  -- Randomisation
  rnd :: MonadRandom m => m k -- ^ Random element of field
