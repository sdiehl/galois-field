module GaloisField
  ( GaloisField(..)
  ) where

import Protolude

import Test.QuickCheck (Arbitrary)

-- | Galois fields @GF(p^q)@ for @p@ prime and @q@ non-negative
class (Arbitrary k, Eq k, Fractional k, Show k) => GaloisField k where
  {-# MINIMAL char #-}
  -- | Characteristic of field
  char :: k -> Integer
