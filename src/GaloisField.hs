{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module GaloisField
  ( Field(..)
  , GaloisField(..)
  ) where

import Protolude hiding (Semiring, one, zero)

import Control.Monad.Random (MonadRandom, Random)
import Data.Semiring (Semiring(..))
import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Field class
-------------------------------------------------------------------------------

-- | Fields.
class Semiring k => Field k where
  {-# MINIMAL (divide | inv), (minus | neg) #-}

  -- | Negation.
  neg :: k -> k
  neg = minus zero
  {-# INLINE neg #-}

  -- | Subtraction.
  minus :: k -> k -> k
  minus = (. neg) . plus
  {-# INLINE minus #-}

  -- | Inversion.
  inv :: k -> k
  inv = divide one
  {-# INLINE inv #-}

  -- | Division.
  divide :: k -> k -> k
  divide = (. inv) . times
  {-# INLINE divide #-}

-- Fractionals are semirings.
instance GaloisField k => Semiring k where
  zero  = 0
  {-# INLINE zero #-}
  plus  = (+)
  {-# INLINE plus #-}
  one   = 1
  {-# INLINE one #-}
  times = (*)
  {-# INLINE times #-}

-- Fields are fractionals.
instance GaloisField k => Field k where
  neg    = negate
  {-# INLINE neg #-}
  minus  = (-)
  {-# INLINE minus #-}
  inv    = recip
  {-# INLINE inv #-}
  divide = (/)
  {-# INLINE divide #-}

-------------------------------------------------------------------------------
-- Galois field class
-------------------------------------------------------------------------------

-- | Galois fields @GF(p^q)@ for @p@ prime and @q@ non-negative.
class (Arbitrary k, Eq k, Fractional k, Generic k,
       NFData k, Pretty k, Random k, Read k, Show k) => GaloisField k where
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
