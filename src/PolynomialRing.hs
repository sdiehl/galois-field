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
  X xs + X ys   = X (add xs ys)
    where
      add :: [k] -> [k] -> [k]
      add []     as     = as
      add as     []     = as
      add (a:as) (b:bs) = let c  = a + b
                              cs = add as bs
                          in if c == 0 && null cs then [] else c : cs
  {-# INLINE (+) #-}
  X xs * X ys   = X (mul xs ys)
    where
      mul :: [k] -> [k] -> [k]
      mul []     _  = []
      mul _      [] = []
      mul (a:as) bs = let ds = map (* a) bs
                          cs = mul as bs
                      in if null as then ds else add ds (0 : cs)
        where
          add :: [k] -> [k] -> [k]
          add []     cs     = cs
          add cs     []     = cs
          add (c:cs) (d:ds) = let e  = c + d
                                  es = add cs ds
                              in if e == 0 && null es then [] else e : es
  {-# INLINE (*) #-}
  X xs - X ys   = X (sub xs ys)
    where
      sub :: [k] -> [k] -> [k]
      sub []     as     = map negate as
      sub as     []     = as
      sub (a:as) (b:bs) = let c  = a - b
                              cs = sub as bs
                          in if c == 0 && null cs then [] else c : cs
  {-# INLINE (-) #-}
  negate (X xs) = X (map negate xs)
  {-# INLINE negate #-}
  fromInteger n = X (let m = fromInteger n in if m == 0 then [] else [m])
  {-# INLINABLE fromInteger #-}
  abs           = panic "not implemented."
  signum        = panic "not implemented."

-- | Polynomial degree
degree :: Polynomial k -> Int
degree (X xs) = length xs
{-# INLINE degree #-}

-- | Polynomial leading coefficient
last :: GaloisField k => Polynomial k -> k
last (X [])     = 0
last (X [x])    = x
last (X (_:xs)) = last (X xs)
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
{-# INLINABLE toPoly #-}
