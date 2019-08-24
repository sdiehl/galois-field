module Data.Field.Galois.Base
  ( module Data.Field.Galois.Base
  ) where

import Protolude hiding ((-), one, quot)

import Control.Monad.Random (Random)
import Data.Euclidean (Euclidean(..))
import Data.Semiring (Ring, (-), one)
import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Fields.
class (Euclidean k, Ring k) => Field k where

  -- | Division.
  divide :: k -> k -> k
  divide = quot
  {-# INLINABLE divide #-}

  -- | Inversion.
  invert :: k -> k
  invert = quot one
  {-# INLINABLE invert #-}

  -- | Subtraction.
  minus :: k -> k -> k
  minus = (-)
  {-# INLINABLE minus #-}

-- | Galois fields @GF(p^q)@ for @p@ prime and @q@ non-negative.
class (Arbitrary k, Field k, Fractional k, Generic k,
       NFData k, Ord k, Pretty k, Random k, Show k) => GaloisField k where
  {-# MINIMAL char, deg, frob #-}

  -- | Characteristic @p@ of field and order of prime subfield.
  char :: k -> Integer

  -- | Degree @q@ of field as extension field over prime subfield.
  deg :: k -> Int

  -- | Frobenius endomorphism @x -> x^p@ of prime subfield.
  frob :: k -> k

  -- | Order @p^q@ of field.
  order :: k -> Integer
  order = (^) <$> char <*> deg
  {-# INLINABLE order #-}

  -- | Exponentiation of field element to integer.
  pow :: k -> Integer -> k
  pow x n
    | n < 0     = pow (recip x) (negate n)
    | otherwise = pow' 1 x n
    where
      pow' z y m
        | m == 0    = z
        | m == 1    = z'
        | even m    = pow' z  y' m'
        | otherwise = pow' z' y' m'
        where
          z' = z * y
          y' = y * y
          m' = div m 2
  {-# INLINABLE pow #-}
