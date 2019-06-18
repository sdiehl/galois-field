module PolynomialRing
  ( Polynomial(..)
  , degree
  , last
  , polyDiv
  , polyInv
  , toPoly
  ) where

import Protolude

import GaloisField (GaloisField(..))

-- | Polynomial rings
newtype Polynomial k = X [k]
  deriving (Eq, Generic, NFData, Show)

-- | Polynomial rings are rings
instance GaloisField k => Num (Polynomial k) where
  X xs + X ys   = X (polyAdd xs ys)
  X xs * X ys   = X (polyMul xs ys)
  negate (X xs) = X (map negate xs)
  fromInteger   = fromInt
  abs           = panic "not implemented."
  signum        = panic "not implemented."

-- | Polynomial addition
polyAdd :: GaloisField k => [k] -> [k] -> [k]
polyAdd []     xs     = xs
polyAdd xs     []     = xs
polyAdd (x:xs) (y:ys)
  | z == 0 && null zs = []
  | otherwise         = z : zs
  where
    z  = x + y
    zs = polyAdd xs ys
{-# INLINE polyAdd #-}

-- | Polynomial multiplication
polyMul :: GaloisField k => [k] -> [k] -> [k]
polyMul []     _  = []
polyMul _      [] = []
polyMul (x:xs) ys
    | null xs     = ws
    | otherwise   = polyAdd ws (0 : zs)
    where
      ws = map (* x) ys
      zs = polyMul xs ys
{-# INLINE polyMul #-}

-- | Polynomial from integer
fromInt :: GaloisField k => Integer -> Polynomial k
fromInt n = X (let m = fromInteger n in if m == 0 then [] else [m])
{-# INLINE fromInt #-}

-- | Polynomial degree
degree :: Polynomial k -> Int
degree (X xs) = length xs
{-# INLINE degree #-}

-- | Polynomial leading coefficient
last :: GaloisField k => Polynomial k -> k
last (X [])     = 0
last (X [x])    = x
last (X (x:xs)) = last (X xs)
{-# INLINE last #-}

-- | Polynomial division
polyDiv :: forall k . GaloisField k
  => Polynomial k -> Polynomial k -> (Polynomial k, Polynomial k)
polyDiv ns ds = polyQR (0, ns)
  where
    polyQR :: (Polynomial k, Polynomial k) -> (Polynomial k, Polynomial k)
    polyQR qr@(qs, rs)
      | degree rs < degree ds = qr
      | otherwise             = polyQR (qs + ts, rs - ts * ds)
      where
        ts = X (replicate (degree rs - degree ds) 0 ++ [last rs / last ds])
{-# INLINE polyDiv #-}

-- | Polynomial inversion
polyInv :: forall k . GaloisField k
  => Polynomial k -> Polynomial k -> Maybe (Polynomial k)
polyInv (X [x]) _ = Just (X [recip x])
polyInv xs ps     = case extGCD ps xs of
  (X [y], (X ys, _)) -> Just (X (map (/ y) ys))
  _                  -> Nothing
  where
    extGCD :: Polynomial k -> Polynomial k
      -> (Polynomial k, (Polynomial k, Polynomial k))
    extGCD y 0 = (y, (0, 1))
    extGCD y x = (g, (t - s * q, s))
      where
        (q, r)      = polyDiv y x
        (g, (s, t)) = extGCD x r
{-# INLINE polyInv #-}

-- | List to polynomial
toPoly :: GaloisField k => [k] -> Polynomial k
toPoly = X . reverse . dropWhile (== 0) . reverse
{-# INLINE toPoly #-}
