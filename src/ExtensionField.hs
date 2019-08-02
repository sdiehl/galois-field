module ExtensionField
  ( ExtensionField
  , IrreducibleMonic(split)
  , fromField
  , fromList
  , t
  , x
  ) where

import Protolude

import Control.Monad.Random (Random(..), getRandom)
import Test.Tasty.QuickCheck (Arbitrary(..), vector)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (GaloisField(..))

-------------------------------------------------------------------------------
-- Extension field type
-------------------------------------------------------------------------------

-- | Extension fields @GF(p^q)[X]/\<f(X)\>@ for @p@ prime, @q@ positive, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@.
newtype ExtensionField k im = EF (Polynomial k)
  deriving (Eq, Generic, NFData, Ord, Read, Show)

-- | Irreducible monic splitting polynomial @f(X)@ of extension field.
class IrreducibleMonic k im where
  {-# MINIMAL split #-}
  -- | Splitting polynomial @f(X)@.
  split :: ExtensionField k im -> Polynomial k
  -- | Splitting polynomial list.
  plist :: ExtensionField k im -> [k]
  plist = (\(X xs) -> xs) . split

-- Extension fields are Galois fields.
instance (GaloisField k, IrreducibleMonic k im)
  => GaloisField (ExtensionField k im) where
  char          = const (char (witness :: k))
  {-# INLINE char #-}
  deg w         = deg (witness :: k) * (length (plist w) - 1)
  {-# INLINE deg #-}
  frob          = pow <*> char
  {-# INLINE frob #-}
  pow w@(EF (X y)) n
    | n < 0     = pow (recip w) (-n)
    | otherwise = EF (X (pow' [1] y n))
    where
      mul = (.) (snd . flip polyQR (plist w)) . polyMul
      pow' ws zs m
        | m == 0    = ws
        | m == 1    = mul ws zs
        | even m    = pow' ws (mul zs zs) (div m 2)
        | otherwise = pow' (mul ws zs) (mul zs zs) (div m 2)
  {-# INLINE pow #-}
  quad          = panic "not implemented."
  {-# INLINE quad #-}
  rnd           = getRandom
  {-# INLINE rnd #-}
  sr            = panic "not implemented."
  {-# INLINE sr #-}

-------------------------------------------------------------------------------
-- Extension field conversions
-------------------------------------------------------------------------------

-- Polynomial rings.
newtype Polynomial k = X [k]
  deriving (Eq, Generic, NFData, Ord, Read, Show)

-- Polynomial rings are rings.
instance GaloisField k => Num (Polynomial k) where
  X y + X z     = X (polyAdd y z)
  {-# INLINE (+) #-}
  X y * X z     = X (polyMul y z)
  {-# INLINE (*) #-}
  X y - X z     = X (polySub y z)
  {-# INLINE (-) #-}
  negate (X y)  = X (map negate y)
  {-# INLINE negate #-}
  fromInteger n = X (let m = fromInteger n in if m == 0 then [] else [m])
  {-# INLINABLE fromInteger #-}
  abs           = panic "not implemented."
  signum        = panic "not implemented."

-- | Convert from field element to list representation.
fromField :: ExtensionField k im -> [k]
fromField (EF (X y)) = y
{-# INLINABLE fromField #-}

-- | Convert from list representation to field element.
fromList :: forall k im . (GaloisField k, IrreducibleMonic k im)
  => [k] -> ExtensionField k im
fromList = EF . X . snd . flip polyQR (plist w) . dropZero
  where
    w = witness :: ExtensionField k im
{-# INLINABLE fromList #-}

-- | Descend tower of indeterminate variables.
t :: Polynomial k -> Polynomial (ExtensionField k im)
t = X . return . EF
{-# INLINE t #-}

-- | Current indeterminate variable.
x :: GaloisField k => Polynomial k
x = X [0, 1]
{-# INLINE x #-}

-------------------------------------------------------------------------------
-- Extension field instances
-------------------------------------------------------------------------------

-- Extension fields are arbitrary.
instance (Arbitrary k, GaloisField k, IrreducibleMonic k im)
  => Arbitrary (ExtensionField k im) where
  arbitrary = fromList <$>
    vector (length (plist (witness :: ExtensionField k im)) - 1)

-- Extension fields are fields.
instance (GaloisField k, IrreducibleMonic k im)
  => Fractional (ExtensionField k im) where
  recip w@(EF (X y))  = EF (X (polyInv y (plist w)))
  {-# INLINE recip #-}
  fromRational (y:%z) = fromInteger y / fromInteger z
  {-# INLINABLE fromRational #-}

-- Extension fields are rings.
instance (GaloisField k, IrreducibleMonic k im)
  => Num (ExtensionField k im) where
  EF y + EF z             = EF (y + z)
  {-# INLINE (+) #-}
  w@(EF (X y)) * EF (X z) = EF (X (snd (polyQR (polyMul y z) (plist w))))
  {-# INLINE (*) #-}
  EF y - EF z             = EF (y - z)
  {-# INLINE (-) #-}
  negate (EF y)           = EF (-y)
  {-# INLINE negate #-}
  fromInteger             = EF . fromInteger
  {-# INLINABLE fromInteger #-}
  abs                     = panic "not implemented."
  signum                  = panic "not implemented."

-- Extension fields are pretty.
instance (GaloisField k, IrreducibleMonic k im)
  => Pretty (ExtensionField k im) where
  pretty (EF (X y)) = pretty y

-- Extension fields are random.
instance (GaloisField k, IrreducibleMonic k im)
  => Random (ExtensionField k im) where
  random  = first (EF . X . dropZero) . unfold (length (plist w) - 1) []
    where
      w = witness :: ExtensionField k im
      unfold n ys g
        | n <= 0    = (ys, g)
        | otherwise = case random g of
          (y, g') -> unfold (n - 1) (y : ys) g'
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Extension field arithmetic
-------------------------------------------------------------------------------

-- Polynomial drop zeroes.
dropZero :: GaloisField k => [k] -> [k]
dropZero = reverse . dropWhile (== 0) . reverse
{-# INLINABLE dropZero #-}

-- Polynomial addition.
polyAdd :: GaloisField k => [k] -> [k] -> [k]
polyAdd ys     []     = ys
polyAdd []     zs     = zs
polyAdd (y:ys) (z:zs) = let w  = y + z
                            ws = polyAdd ys zs
                        in if w == 0 && null ws then [] else w : ws
{-# INLINE polyAdd #-}

-- Polynomial multiplication.
polyMul :: GaloisField k => [k] -> [k] -> [k]
polyMul _      [] = []
polyMul []     _  = []
polyMul (y:ys) zs = let ws  = map (* y) zs
                        ws' = polyMul ys zs
                    in if null ys then ws else polyAdd ws (0 : ws')
{-# INLINE polyMul #-}

-- Polynomial subtraction.
polySub :: GaloisField k => [k] -> [k] -> [k]
polySub ys     []     = ys
polySub []     zs     = map negate zs
polySub (y:ys) (z:zs) = let w  = y - z
                            ws = polySub ys zs
                        in if w == 0 && null ws then [] else w : ws
{-# INLINE polySub #-}

-- Polynomial quotient and remainder.
polyQR :: forall k . GaloisField k => [k] -> [k] -> ([k], [k])
polyQR ys zs = polyGCD ([], ys)
  where
    z = last zs :: k
    m = length zs :: Int
    last :: [k] -> k
    last []     = 0
    last [w]    = w
    last (_:ws) = last ws
    polyGCD :: ([k], [k]) -> ([k], [k])
    polyGCD qr@(qs, rs)
      | n < 0     = qr
      | otherwise = polyGCD (polyAdd qs ts, polySub rs (polyMul ts zs))
      where
        r = last rs :: k
        n = length rs - m :: Int
        ts = replicate n 0 ++ [r / z] :: [k]
{-# INLINE polyQR #-}

-- Polynomial inverse.
polyInv :: forall k . GaloisField k => [k] -> [k] -> [k]
polyInv [y] _ = [recip y]
polyInv ys zs = case extGCD (zs, ys) of
  ([w], (ws, _)) -> map (/ w) ws
  _              -> panic "no multiplicative inverse."
  where
    extGCD :: ([k], [k]) -> ([k], ([k], [k]))
    extGCD (y, []) = (y, ([], [1]))
    extGCD (y, z)  = (g, (polySub v (polyMul u q), u))
      where
        (q, r)      = polyQR y z
        (g, (u, v)) = extGCD (z, r)
{-# INLINE polyInv #-}
