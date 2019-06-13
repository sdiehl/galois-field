{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module ExtensionField
  ( ExtensionField(..)
  ) where

import Protolude
import Data.Singletons (SingI)

import GaloisField (GaloisField(..))
import PrimeField (PrimeField(..))

-- | Extension fields @GF(p^q)[X]/<f(X)>@ for @p@ prime, @q@ non-negative, and
-- @f(X)@ monic irreducible in @GF(p)[X]@
newtype ExtensionField k (ps :: [Nat]) = EF [k]
  deriving (Show, Generic, NFData)

-- | Extension fields are Galois fields
instance (SingI ps, GaloisField k) => GaloisField (ExtensionField k ps) where
  char = const $ char (undefined :: k) -- TODO

-- | Extension fields are equatable
instance (SingI ps, GaloisField k) => Eq (ExtensionField k ps) where
  (==) = (. toPoly) . (==) . toPoly

-- | Extension fields are fields
instance (SingI ps, GaloisField k) => Fractional (ExtensionField k ps) where
  fromRational (a :% b) = fromInteger a / fromInteger b
  {-# INLINE recip #-}
  recip a               = case polyEEA (toPoly a) (getPoly a) of
    ([], (f, _)) -> fromPoly f
    _            -> panic "no multiplicative inverse."
instance (SingI ps, GaloisField k) => Num (ExtensionField k ps) where
  a + b         = fromPoly $ polyAdd (toPoly a) (toPoly b)
  a * b         = fromPoly $ polyMul (toPoly a) (toPoly b)
  abs a         = a
  signum a      = if a == 0 then 0 else 1
  fromInteger n = fromPoly $ let m = fromInteger n in if m == 0 then [] else [m]
  negate        = fromPoly . map negate . toPoly

-- | Monic irreducible polynomial
getPoly :: (SingI ps, GaloisField k) => ExtensionField k ps -> [k]
getPoly = notImplemented -- TODO map fromInteger . fromSing

-- | Conversion from polynomial
fromPoly :: (SingI ps, GaloisField k) => [k] -> ExtensionField k ps
fromPoly f = fix $ EF . fst . polyEEA f . getPoly

-- | Conversion to polynomial
toPoly :: (SingI ps, GaloisField k) => ExtensionField k ps -> [k]
toPoly a@(EF f) = fst . polyEEA f $ getPoly a

{-# INLINABLE polyAdd #-}
-- | Polynomial addition
polyAdd :: (Eq a, Num a) => [a] -> [a] -> [a]
polyAdd [] xs         = xs
polyAdd xs []         = xs
polyAdd (x:xs) (y:ys) = case (x + y, polyAdd xs ys) of
  (0, []) -> []
  (z, zs) -> z : zs

{-# INLINABLE polyMul #-}
-- | Polynomial multiplication
polyMul :: (Eq a, Num a) => [a] -> [a] -> [a]
polyMul [] _      = []
polyMul _ []      = []
polyMul (x:xs) ys = case (xs, map (* x) ys) of
  ([], zs) -> zs
  (_ , zs) -> polyAdd zs $ 0 : polyMul xs ys

{-# INLINABLE polyEEA #-}
-- | Polynomial extended euclidean algorithm
polyEEA :: [a] -> [a] -> ([a], ([a], [a]))
polyEEA xs ps = notImplemented -- TODO
