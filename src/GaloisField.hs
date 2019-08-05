module GaloisField
  ( Field(..)
  , GaloisField(..)
  ) where

import Protolude hiding ((-), one)

import Control.Monad.Random (MonadRandom, Random)
import Data.Euclidean (Euclidean)
import Data.Semiring (Ring, (-), one, times)
import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Fields.
class (Euclidean k, Ring k) => Field k where
  {-# MINIMAL (divide | invert) #-}

  -- Operations

  -- | Division.
  divide :: k -> k -> k
  divide = (. invert) . times
  {-# INLINE divide #-}

  -- | Inversion.
  invert :: k -> k
  invert = divide one
  {-# INLINE invert #-}

  -- | Subtraction.
  minus :: k -> k -> k
  minus = (-)
  {-# INLINE minus #-}

-- | Galois fields @GF(p^q)@ for @p@ prime and @q@ non-negative.
class (Arbitrary k, Eq k, Field k, Fractional k,
       Generic k, Ord k, Pretty k, Random k, Show k) => GaloisField k where
  {-# MINIMAL char, deg, frob, pow, quad, rnd, sr #-}

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

  -- | Exponentiation of a field element to an integer.
  pow :: k -> Integer -> k

  -- | Solve quadratic @ax^2+bx+c=0@ over field.
  quad :: k -> k -> k -> Maybe k

  -- | Randomised field element.
  rnd :: MonadRandom m => m k

  -- | Square root of a field element.
  sr :: k -> Maybe k
