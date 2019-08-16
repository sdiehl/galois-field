module GaloisField
  ( Field(..)
  , GaloisField(..)
  ) where

import Protolude hiding ((-), one, quot)

import Control.Monad.Random (MonadRandom, Random, StdGen,
                             getRandom, mkStdGen, runRand)
import Data.Euclidean (Euclidean(..))
import Data.Semiring (Ring, (-), one)
import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Fields.
class (Euclidean k, Ring k) => Field k where

  -- Operations

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

  -- Characteristics

  -- | Characteristic @p@ of field and order of prime subfield.
  char :: k -> Integer

  -- | Degree @q@ of field as extension field over prime subfield.
  deg :: k -> Int

  -- | Order @p^q@ of field.
  order :: k -> Integer
  order = (^) <$> char <*> deg
  {-# INLINABLE order #-}

  -- | Frobenius endomorphism @x -> x^p@ of prime subfield.
  frob :: k -> k

  -- Functions

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

  -- | Get randomised quadratic nonresidue.
  qnr :: k
  qnr = getQNR
  {-# INLINABLE qnr #-}

  -- | Check if quadratic residue.
  qr :: k -> Bool
  qr = not . isQNR
  {-# INLINABLE qr #-}

  -- | Solve quadratic @ax^2 + bx + c = 0@ over field.
  quad :: k -> k -> k -> Maybe k
  quad = solveQuadratic
  {-# INLINABLE quad #-}

  -- | Randomised field element.
  rnd :: MonadRandom m => m k
  rnd = getRandom
  {-# INLINABLE rnd #-}

  -- | Square root of field element.
  sr :: k -> Maybe k
  sr = squareRoot
  {-# INLINABLE sr #-}

-------------------------------------------------------------------------------
-- Square roots
-------------------------------------------------------------------------------

-- Check if an element is a quadratic nonresidue.
isQNR :: GaloisField k => k -> Bool
isQNR n = pow n (shiftR (order n) 1) /= 1
{-# INLINABLE isQNR #-}

-- Factor the order @p - 1@ to get @q@ and @s@ such that @p - 1 = q2^s@.
factorOrder :: GaloisField k => k -> (Integer, Int)
factorOrder w = factorOrder' (order w - 1, 0)
  where
    factorOrder' :: (Integer, Int) -> (Integer, Int)
    factorOrder' qs@(q, s)
      | testBit q 0 = qs
      | otherwise   = factorOrder' (shiftR q 1, s + 1)
{-# INLINABLE factorOrder #-}

-- Get a random quadratic nonresidue.
getQNR :: forall k . GaloisField k => k
getQNR = getQNR' (runRand rnd (mkStdGen 0))
  where
    getQNR' :: (k, StdGen) -> k
    getQNR' (x, g)
      | x /= 0 && isQNR x = x
      | otherwise         = getQNR' (runRand rnd g)
{-# INLINABLE getQNR #-}

-- Get a square root of @n@ with the Tonelli-Shanks algorithm.
squareRoot :: forall k . GaloisField k => k -> Maybe k
squareRoot 0    = Just 0
squareRoot n
  | char n == 2 = Just (power n)
  | isQNR n     = Nothing
  | otherwise   = case (factorOrder n, getQNR) of
  ((q, s), z) -> let zq  = pow z q
                     nq  = pow n (shiftR q 1)
                     nnq = n * nq
                 in loop s zq (nq * nnq) nnq
  where
    power :: k -> k
    power = next (deg n)
      where
       next :: Int -> k -> k
       next 1 m = m
       next i m = next (i - 1) (m * m)
    loop :: Int -> k -> k -> k -> Maybe k
    loop _ _ 0 _ = Just 0
    loop _ _ 1 r = Just r
    loop m c t r = let i  = least t 0
                       b  = pow c (bit (m - i - 1))
                       b2 = b * b
                   in loop i b2 (t * b2) (r * b)
      where
        least :: k -> Int -> Int
        least 1  j = j
        least ti j = least (ti * ti) (j + 1)
{-# INLINABLE squareRoot #-}

-- Solve a quadratic equation @ax^2 + bx + c = 0@.
solveQuadratic :: forall k . GaloisField k => k -> k -> k -> Maybe k
solveQuadratic 0 _ _ = Nothing
solveQuadratic _ _ 0 = Just 0
solveQuadratic a 0 c = squareRoot (-c / a)
solveQuadratic a b c
  | char a == 2      = (* (b / a)) <$> solveQuadratic' (ac / bb)
  | otherwise        = (/ (2 * a)) . subtract b <$> squareRoot (bb - 4 * ac)
  where
    ac = a * c
    bb = b * b
    solveQuadratic' :: k -> Maybe k
    solveQuadratic' x
      | sum xs /= 0 = Nothing
      | odd m       = Just (sum h)
      | otherwise   = panic "not implemented."
      where
        m  = deg x
        xs = take m (iterate (join (*)) x)
        h  = zipWith ($) (cycle [identity, const 0]) xs
{-# INLINABLE solveQuadratic #-}
