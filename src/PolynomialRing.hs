{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module PolynomialRing
  ( R(..)
  , polyDiv
  , polyInv
  ) where

import Protolude

-- | Polynomial rings
newtype R k = R [k]
  deriving (Eq, Show, Generic, NFData)

-- | Polynomial ring is a ring
instance (Eq k, Fractional k) => Num (R k) where
  {-# INLINE (+) #-}
  R []     + R xs       = R xs
  R xs     + R []       = R xs
  R (x:xs) + R (y:ys)
    | z == 0 && null zs = 0
    | otherwise         = R $ z : zs
    where
      z    = x + y
      R zs = R xs + R ys
  {-# INLINE (*) #-}
  R []     * _          = 0
  _        * R []       = 0
  R (x:xs) * R ys
    | null xs           = ws
    | otherwise         = ws + R (0 : zs)
    where
      ws   = R $ map (* x) ys
      R zs = R xs * R ys
  abs xs                = xs
  signum xs             = xs
  {-# INLINE fromInteger #-}
  fromInteger n
    | m == 0            = R []
    | otherwise         = R [m]
    where
      m = fromInteger n
  negate (R xs)         = R $ map negate xs

{-# INLINABLE degree #-}
-- | Polynomial degree
degree :: R k -> Int
degree (R xs) = length xs

{-# INLINABLE last #-}
-- | Polynomial leading coefficient
last :: Num k => R k -> k
last (R [])     = 0
last (R [x])    = x
last (R (x:xs)) = last $ R xs

{-# INLINABLE polyDiv #-}
-- | Polynomial divide
polyDiv :: forall k . (Eq k, Fractional k) => R k -> R k -> (R k, R k)
polyDiv ns ds = polyQR (0, ns)
  where
    polyQR :: (R k, R k) -> (R k, R k)
    polyQR qr@(qs, rs)
      | degree rs < degree ds = qr
      | otherwise             = polyQR (qs + ts, rs - ts * ds)
      where
        ts = R $ replicate (degree rs - degree ds) 0 ++ [last rs / last ds]

{-# INLINABLE polyInv #-}
-- | Polynomial inverse
polyInv :: forall k . (Eq k, Fractional k) => R k -> R k -> Maybe (R k)
polyInv (R xs) (R ps) = case extGCD (R ps) (R xs) of
  (R [_], (ys, _)) -> Just ys
  _                -> Nothing
  where
    extGCD :: R k -> R k -> (R k, (R k, R k))
    extGCD y 0 = (y, (0, 1))
    extGCD y x = (g, (t - s * q, s))
      where
        (q, r)      = polyDiv y x
        (g, (s, t)) = extGCD x r
