module Data.Field.Galois.Frobenius
  ( frobenius
  ) where

import Protolude

import Data.Vector (Vector)

import Data.Field.Galois.Base (GaloisField(..))

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Frobenius endomorphism precomputation.
frobenius :: GaloisField k => Vector k -> Vector k -> Maybe (Vector k)
frobenius [ ] _ = Just []
frobenius [a] _ = Just [frob a]
frobenius [a, b] [x, 0, 1]
  | deg x == 2  = Just [a, negate b]
  | char x == 2 = Just [frob a - frob b * x]
  | otherwise   = Just [frob a, frob b * nxq]
  where
    nxq = negate x ^ shiftR (char x) 1
frobenius [a, b] [x, 0, 0, 1]
  | char x == 3 = Just [frob a - frob b * x]
  | r == 1      = Just [frob a, frob b * nxq]
  | otherwise   = Just [frob a, 0, frob b * nxq]
  where
    (q, r) = quotRem (char x) 3
    nxq    = negate x ^ q
frobenius [a, b, c] [x, 0, 0, 1]
  | char x == 3 = Just [frob a - (frob b - frob c * x) * x]
  | r == 1      = Just [frob a, frob b * nxq, frob c * nxq * nxq]
  | otherwise   = Just [frob a, frob c * nx * nxq * nxq, frob b * nxq]
  where
    (q, r) = quotRem (char x) 3
    nx     = negate x
    nxq    = nx ^ q
frobenius _ _   = Nothing
{-# INLINABLE frobenius #-}
