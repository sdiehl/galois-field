module PolynomialRing
  ( Polynomial(..)
  , cut
  , polyInv
  , polyMul
  , polyQR
  , toPoly
  ) where

import Protolude

import GaloisField (GaloisField(..))

newtype Polynomial k = X [k]
  deriving (Eq, Generic, NFData, Show)

instance GaloisField k => Num (Polynomial k) where
  X xs + X ys   = X (polyAdd xs ys)
  {-# INLINE (+) #-}
  X xs * X ys   = X (polyMul xs ys)
  {-# INLINE (*) #-}
  X xs - X ys   = X (polySub xs ys)
  {-# INLINE (-) #-}
  negate (X xs) = X (map negate xs)
  {-# INLINE negate #-}
  fromInteger n = X (let m = fromInteger n in if m == 0 then [] else [m])
  {-# INLINABLE fromInteger #-}
  abs           = panic "not implemented."
  signum        = panic "not implemented."

polyAdd :: GaloisField k => [k] -> [k] -> [k]
polyAdd xs     []     = xs
polyAdd []     ys     = ys
polyAdd (x:xs) (y:ys) = let z  = x + y
                            zs = polyAdd xs ys
                        in if z == 0 && null zs then [] else z : zs
{-# INLINE polyAdd #-}

polyMul :: GaloisField k => [k] -> [k] -> [k]
polyMul _      [] = []
polyMul []     _  = []
polyMul (x:xs) ys = let ws = map (* x) ys
                        zs = polyMul xs ys
                    in if null xs then ws else polyAdd ws (0 : zs)
{-# INLINE polyMul #-}

polySub :: GaloisField k => [k] -> [k] -> [k]
polySub xs     []     = xs
polySub []     ys     = map negate ys
polySub (x:xs) (y:ys) = let z  = x - y
                            zs = polySub xs ys
                        in if z == 0 && null zs then [] else z : zs
{-# INLINE polySub #-}

polyQR :: forall k . GaloisField k => [k] -> [k] -> ([k], [k])
polyQR xs ys = polyGCD ([], xs)
  where
    m :: Int
    m = length ys
    polyGCD :: ([k], [k]) -> ([k], [k])
    polyGCD qr@(qs, rs)
      | n < 0     = qr
      | otherwise = polyGCD (polyAdd qs ts, polySub rs (polyMul ts ys))
      where
        n :: Int
        n = length rs - m
        ts :: [k]
        ts = replicate n 0 ++ [last rs / last ys]
        last :: [k] -> k
        last []     = 0
        last [z]    = z
        last (_:zs) = last zs
{-# INLINE polyQR #-}

polyInv :: forall k . GaloisField k => [k] -> [k] -> Maybe [k]
polyInv [x] _ = Just [recip x]
polyInv xs ps = case extGCD (ps, xs) of
  ([y], (ys, _)) -> Just (map (/ y) ys)
  _              -> Nothing
  where
    extGCD :: ([k], [k]) -> ([k], ([k], [k]))
    extGCD (y, []) = (y, ([], [1]))
    extGCD (y, x)  = (g, (polySub t (polyMul s q), s))
      where
        (q, r)      = polyQR y x
        (g, (s, t)) = extGCD (x, r)
{-# INLINE polyInv #-}

cut :: GaloisField k => [k] -> [k]
cut = reverse . dropWhile (== 0) . reverse
{-# INLINABLE cut #-}

toPoly :: GaloisField k => [k] -> Polynomial k
toPoly = X . cut
{-# INLINABLE toPoly #-}
