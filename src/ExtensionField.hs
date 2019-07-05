module ExtensionField
  ( ExtensionField
  , IrreducibleMonic(..)
  , fromField
  , fromList
  , t
  , x
  ) where

import Protolude

import Control.Monad.Random (Random(..), getRandom)
import Test.Tasty.QuickCheck (Arbitrary(..), choose, sized)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (GaloisField(..))
import PolynomialRing (Polynomial(..), cut, polyInv, polyMul, polyQR)

-- | Irreducible monic splitting polynomial of extension field
class IrreducibleMonic k im where
  {-# MINIMAL split #-}
  split :: ExtensionField k im -> Polynomial k -- ^ Splitting polynomial

-- | Extension fields @GF(p^q)[X]/<f(X)>@ for @p@ prime, @q@ positive, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@
newtype ExtensionField k im = EF (Polynomial k)
  deriving (Eq, Generic, NFData, Show)

-- | Extension fields are arbitrary
instance (Arbitrary k, GaloisField k, IrreducibleMonic k im)
  => Arbitrary (ExtensionField k im) where
  arbitrary = fromList <$> sized (const poly)
    where
      poly = choose (1, length xs - 1) >>= mapM (const arbitrary) . enumFromTo 1
        where
          X xs = split (witness :: ExtensionField k im)

-- | Extension fields are fields
instance (GaloisField k, IrreducibleMonic k im)
  => Fractional (ExtensionField k im) where
  recip (EF (X ys))   = case polyInv ys xs of
    Just zs -> EF (X zs)
    _       -> panic "no multiplicative inverse."
    where
      X xs = split (witness :: ExtensionField k im)
  {-# INLINE recip #-}
  fromRational (y:%z) = fromInteger y / fromInteger z
  {-# INLINABLE fromRational #-}

-- | Extension fields are Galois fields
instance (GaloisField k, IrreducibleMonic k im)
  => GaloisField (ExtensionField k im) where
  char          = const (char (witness :: k))
  {-# INLINE char #-}
  deg           = const (deg (witness :: k) * length xs - 1)
    where
      X xs = split (witness :: ExtensionField k im)
  {-# INLINE deg #-}
  pow y@(EF (X ys)) n
    | n < 0     = pow (recip y) (-n)
    | otherwise = EF (X (pow' [1] ys n))
    where
      X xs = split (witness :: ExtensionField k im)
      mul = (.) (snd . flip polyQR xs) . polyMul
      pow' ws zs m
        | m == 0    = ws
        | m == 1    = mul ws zs
        | even m    = pow' ws (mul zs zs) (div m 2)
        | otherwise = pow' (mul ws zs) (mul zs zs) (div m 2)
  {-# INLINE pow #-}
  rnd           = getRandom
  {-# INLINE rnd #-}

-- | Extension fields are rings
instance (GaloisField k, IrreducibleMonic k im)
  => Num (ExtensionField k im) where
  EF y + EF z           = EF (y + z)
  {-# INLINE (+) #-}
  EF (X ys) * EF (X zs) = EF (X (snd (polyQR (polyMul ys zs) xs)))
    where
      X xs = split (witness :: ExtensionField k im)
  {-# INLINE (*) #-}
  EF y - EF z           = EF (y - z)
  {-# INLINE (-) #-}
  negate (EF y)         = EF (-y)
  {-# INLINE negate #-}
  fromInteger           = EF . fromInteger
  {-# INLINABLE fromInteger #-}
  abs                   = panic "not implemented."
  signum                = panic "not implemented."

-- | Extension fields are pretty
instance (GaloisField k, IrreducibleMonic k im)
  => Pretty (ExtensionField k im) where
  pretty (EF y) = pretty y

-- | Extension fields are random
instance (GaloisField k, IrreducibleMonic k im)
  => Random (ExtensionField k im) where
  random  = first (EF . X . cut) . unfold (length xs - 1) []
    where
      X xs = split (witness :: ExtensionField k im)
      unfold n ys g = if n <= 0 then (ys, g) else
        let (y, g') = random g in unfold (n - 1) (y : ys) g'
  randomR = panic "not implemented."

-- | List from field
fromField :: ExtensionField k im -> [k]
fromField (EF (X xs)) = xs
{-# INLINABLE fromField #-}

-- | Field from list
fromList :: forall k im . (GaloisField k, IrreducibleMonic k im)
  => [k] -> ExtensionField k im
fromList = EF . X . snd . flip polyQR xs . cut
  where
    X xs = split (witness :: ExtensionField k im)
{-# INLINABLE fromList #-}

-- | Current indeterminate variable
x :: GaloisField k => Polynomial k
x = X [0, 1]
{-# INLINE x #-}

-- | Descend variable tower
t :: Polynomial k -> Polynomial (ExtensionField k im)
t = X . return . EF
{-# INLINE t #-}
