module GaloisField
  ( GaloisField(..)
  ) where

import Protolude

import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-- | Galois fields @GF(p^q)@ for @p@ prime and @q@ non-negative
class (Arbitrary k, Eq k, Fractional k, Pretty k, Show k) => GaloisField k where
  {-# MINIMAL char, degree #-}
  -- | Characteristic @q@ of field
  char   :: k -> Integer
  -- | Degree @q@ of field
  degree :: k -> Int
  -- | Order @p^q@ of field
  order  :: k -> Integer
  order k = char k ^ degree k
