module GaloisField
  ( GaloisField(..)
  ) where

import Protolude

-- | Galois fields @GF(p^q)@ for @p@ prime and @q@ non-negative
class (Bounded k, Eq k, Fractional k, Integral k) => GaloisField k where
  {-# MINIMAL ch #-}
  -- | Characteristic @p@ of field @GF(p^q)@
  ch :: k -> Integer
