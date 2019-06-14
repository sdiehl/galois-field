{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module PolynomialRing
  ( Polynomial(..)
  , getPoly
  , polyDiv
  , polyInv
  ) where

import Protolude

import GaloisField (GaloisField(..))

-- | Polynomial rings
newtype Polynomial k = X [k]
  deriving (Eq, Show, Generic, NFData)

-- | Polynomial ring is a ring
instance GaloisField k => Num (Polynomial k) where
  {-# INLINE (+) #-}
  X []     + X xs       = X xs
  X xs     + X []       = X xs
  X (x:xs) + X (y:ys)
    | z == 0 && null zs = 0
    | otherwise         = X $ z : zs
    where
      z    = x + y
      X zs = X xs + X ys
  {-# INLINE (*) #-}
  X []     * _          = 0
  _        * X []       = 0
  X (x:xs) * X ys
    | null xs           = ws
    | otherwise         = ws + X (0 : zs)
    where
      ws   = X $ map (* x) ys
      X zs = X xs * X ys
  abs xs                = xs
  signum xs             = xs
  {-# INLINE fromInteger #-}
  fromInteger n
    | m == 0            = X []
    | otherwise         = X [m]
    where
      m = fromInteger n
  negate (X xs)         = X $ map negate xs

-- | Polynomial from list
getPoly :: GaloisField k => [k] -> Polynomial k
getPoly = X . reverse . dropWhile (== 0) . reverse

-- | Polynomial degree
degree :: Polynomial k -> Int
degree (X xs) = length xs

{-# INLINE last #-}
-- | Polynomial leading coefficient
last :: GaloisField k => Polynomial k -> k
last (X [])     = 0
last (X [x])    = x
last (X (x:xs)) = last $ X xs

{-# INLINE polyDiv #-}
-- | Polynomial divide
polyDiv :: forall k . GaloisField k
  => Polynomial k -> Polynomial k -> (Polynomial k, Polynomial k)
polyDiv ns ds = polyQR (0, ns)
  where
    polyQR :: (Polynomial k, Polynomial k) -> (Polynomial k, Polynomial k)
    polyQR qr@(qs, rs)
      | degree rs < degree ds = qr
      | otherwise             = polyQR (qs + ts, rs - ts * ds)
      where
        ts = X $ replicate (degree rs - degree ds) 0 ++ [last rs / last ds]

{-# INLINE polyInv #-}
-- | Polynomial inverse
polyInv :: forall k . GaloisField k
  => Polynomial k -> Polynomial k -> Maybe (Polynomial k)
polyInv (X [x]) _ = Just $ X [recip x]
polyInv xs ps     = case extGCD ps xs of
  (X [y], (X ys, _)) -> Just . X $ map (/ y) ys
  _                  -> Nothing
  where
    extGCD :: Polynomial k -> Polynomial k
      -> (Polynomial k, (Polynomial k, Polynomial k))
    extGCD y 0 = (y, (0, 1))
    extGCD y x = (g, (t - s * q, s))
      where
        (q, r)      = polyDiv y x
        (g, (s, t)) = extGCD x r
