module Data.Field.Galois.Base
  ( module Data.Field.Galois.Base
  ) where

import Protolude hiding ((-), one, quot)

import Control.Monad.Random (Random)
import Data.Field (Field)
import GHC.Natural (Natural)
import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Galois fields @GF(p^q)@ for @p@ prime and @q@ non-negative.
class (Arbitrary k, Field k, Fractional k, Generic k,
       NFData k, Ord k, Pretty k, Random k, Show k) => GaloisField k where
  {-# MINIMAL char, deg #-}

  -- | Characteristic @p@ of field and order of prime subfield.
  char :: k -> Natural

  -- | Degree @q@ of field as extension field over prime subfield.
  deg :: k -> Word

  -- | Frobenius endomorphism @x -> x^p@ of prime subfield.
  frob :: k -> k
  frob = (^) <*> char
  {-# INLINABLE frob #-}

  -- | Order @p^q@ of field.
  order :: k -> Natural
  order = (^) <$> char <*> deg
  {-# INLINABLE order #-}

  -- | Exponentiation of field element to integer.
  pow :: Integral n => k -> n -> k
  pow = (^)
  {-# INLINABLE pow #-}

{-# SPECIALISE pow ::
  GaloisField k => k -> Int -> k
  GaloisField k => k -> Integer -> k
  GaloisField k => k -> Natural -> k
  GaloisField k => k -> Word -> k
  #-}
