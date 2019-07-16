module AdicField
  ( AdicField
  ) where

import Protolude

import Control.Monad.Random (Random(..), getRandom)
import Test.Tasty.QuickCheck (Arbitrary(..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import GaloisField (GaloisField(..))

-------------------------------------------------------------------------------
-- Adic field type
-------------------------------------------------------------------------------

-- | Adic fields @GF(p^q)[X]/\<f(X)\>@ for @p@ prime, @q@ positive, and
-- @f(X)@ irreducible monic in @GF(p^q)[X]@.
newtype AdicField k (im :: Nat) = EF Integer
  deriving (Eq, Generic, Integral, NFData, Ord, Read, Real, Show)

-- Adic fields are Galois fields.
instance (GaloisField k, KnownNat im) => GaloisField (AdicField k im) where

  char          = const (char (witness :: k))
  {-# INLINE char #-}

  deg           = snd . (adicLead <*> natVal)
  {-# INLINE deg #-}

  frob          = pow <*> char
  {-# INLINE frob #-}

  fromZ         = EF . snd . flip (adicQR w) (natVal w)
    where
      w = witness :: AdicField k im
  {-# INLINE fromZ #-}

  toZ (EF x)    = x
  {-# INLINE toZ #-}

  add           = flip adicZip (add (witness :: k))
  {-# INLINE add #-}

  sub           = flip adicZip (sub (witness :: k))
  {-# INLINE sub #-}

  mul w x y
    | x < p     = adicMap w (mul w' x) y
    | y < p     = adicMap w (mul w' y) x
    | otherwise = mul' x y
    where
      w' = witness :: k
      p = order w' :: Integer
      mul' :: Integer -> Integer -> Integer
      mul' x' y' = case quotRem x' p of
        (0, r) -> adicMap w (mul w' r) y'
        (q, r) -> add w (adicMap w (mul w' r) y') (p * mul' q y')
  {-# INLINE mul #-}

  neg           = flip adicMap (neg (witness :: k))
  {-# INLINE neg #-}

  inv w         = inv'
    where
      w' = witness :: k
      p = order w' :: Integer
      q = natVal w :: Integer
      inv' :: Integer -> Integer
      inv' x = case adicGCD w x q of
        (r, s)
          | r == 0 || r > p -> panic "no multiplicative inverse."
          | otherwise       -> adicMap w (flip (dvd w') r) s
  {-# INLINE inv #-}

  dvd w x y
    | y < p     = adicMap w (dvd w' p) x
    | otherwise = mul w x (inv w y)
    where
      w' = witness :: k
      p = order w' :: Integer

  pow           = (^)
  {-# INLINE pow #-}

  rnd           = getRandom
  {-# INLINE rnd #-}

-------------------------------------------------------------------------------
-- Adic field instances
-------------------------------------------------------------------------------

-- Adic fields are arbitrary.
instance (GaloisField k, KnownNat im) => Arbitrary (AdicField k im) where
  arbitrary = EF <$> choose (0, order (witness :: AdicField k im) - 1)

-- Adic fields are bounded.
instance (GaloisField k, KnownNat im) => Bounded (AdicField k im) where
  minBound = 0
  maxBound = -1

-- Adic fields are enumerable.
instance (GaloisField k, KnownNat im) => Enum (AdicField k im) where
  fromEnum = fromIntegral . toZ
  {-# INLINABLE fromEnum #-}
  toEnum   = fromZ . fromIntegral
  {-# INLINABLE toEnum #-}

-- Adic fields are fields.
instance (GaloisField k, KnownNat im) => Fractional (AdicField k im) where
  w@(EF x) / EF y     = fromZ (dvd w x y)
  {-# INLINE (/) #-}
  recip w@(EF x)      = fromZ (inv w x)
  {-# INLINE recip #-}
  fromRational (x:%y) = fromZ x / fromZ y
  {-# INLINABLE fromRational #-}

-- Adic fields are rings.
instance (GaloisField k, KnownNat im) => Num (AdicField k im) where
  w@(EF x) + EF y = EF (add w x y)
  {-# INLINE (+) #-}
  w@(EF x) * EF y = fromZ (mul w x y)
  {-# INLINE (*) #-}
  w@(EF x) - EF y = EF (sub w x y)
  {-# INLINE (-) #-}
  negate w@(EF x) = EF (neg w x)
  {-# INLINE negate #-}
  fromInteger     = fromZ
  {-# INLINABLE fromInteger #-}
  abs             = panic "not implemented."
  signum          = panic "not implemented."

-- Adic fields are pretty.
instance (GaloisField k, KnownNat im) => Pretty (AdicField k im) where
  pretty (EF x) = pretty x

-- Adic fields are random.
instance (GaloisField k, KnownNat im) => Random (AdicField k im) where
  random  = first EF . randomR (0, order (witness :: AdicField k im) - 1)
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Adic field functions
-------------------------------------------------------------------------------

-- | @p^q@-adic last and length.
adicLead :: forall k im . (GaloisField k, KnownNat im)
  => AdicField k im -- ^ Witness of @p^q@.
  -> Integer        -- ^ Integer @x@.
  -> (Integer, Int) -- ^ Last and length of @x@.
adicLead _ = adicLead' (order (witness :: k))
  where
    adicLead' :: Integer -> Integer -> (Integer, Int)
    adicLead' p x
      | x < p     = (x, 0)
      | otherwise = case adicLead' (p * p) x of
        (_, l) -> let l' = 2 * l in adicLead'' (quot x (p ^ l'), l')
      where
        adicLead'' :: (Integer, Int) -> (Integer, Int)
        adicLead'' yn@(y, n)
          | y < p     = yn
          | otherwise = adicLead'' (quot y p, n + 1)
{-# INLINE adicLead #-}

-- | @p^q@-adic map.
adicMap :: forall k im . (GaloisField k, KnownNat im)
  => AdicField k im       -- ^ Witness of @p^q@.
  -> (Integer -> Integer) -- ^ Mapper in @p@.
  -> (Integer -> Integer) -- ^ Mapper in @p^q@.
adicMap _ f = adicMap'
  where
    p = order (witness :: k) :: Integer
    adicMap' :: Integer -> Integer
    adicMap' x = case quotRem x p of
      (0, r) -> f r
      (q, r) -> f r + p * adicMap' q
{-# INLINE adicMap #-}

-- | @p^q@-adic zip.
adicZip :: forall k im . (GaloisField k, KnownNat im)
  => AdicField k im                  -- ^ Witness of @p^q@.
  -> (Integer -> Integer -> Integer) -- ^ Zipper in @p@.
  -> (Integer -> Integer -> Integer) -- ^ Zipper in @p^q@.
adicZip _ f = adicZip'
  where
    p = order (witness :: k) :: Integer
    adicZip' :: Integer -> Integer -> Integer
    adicZip' x y = case (quotRem x p, quotRem y p) of
      ((0, r), ( 0, r')) -> f r r'
      ((q, r), (q', r')) -> f r r' + p * adicZip' q q'
{-# INLINE adicZip #-}

-- | @p^q@-adic quotient and remainder.
adicQR :: forall k im . (GaloisField k, KnownNat im)
  => AdicField k im     -- ^ Witness of @p^q@.
  -> Integer            -- ^ Dividend @x@.
  -> Integer            -- ^ Divisor @y@.
  -> (Integer, Integer) -- ^ Quotient and remainder of division of @x@ by @y@.
adicQR w x y
  | y == 0    = panic "divisor is zero."
  | y < p     = (adicMap w (flip (dvd w') y) x, 0)
  | otherwise = adicQR' (0, x)
  where
    w' = witness :: k
    p = order w' :: Integer
    (yCoeff, yDeg) = adicLead w y :: (Integer, Int)
    adicQR' :: (Integer, Integer) -> (Integer, Integer)
    adicQR' qr@(q, r)
      | lDeg < 0  = qr
      | otherwise = adicQR' (add w q lTerm, sub w r (mul w y lTerm))
      where
        (rCoeff, rDeg) = adicLead w r :: (Integer, Int)
        lCoeff = dvd w' rCoeff yCoeff :: Integer
        lDeg = rDeg - yDeg :: Int
        lTerm = lCoeff * p ^ lDeg :: Integer
{-# INLINE adicQR #-}

-- | @p^q@-adic extended euclidean algorithm.
adicGCD :: forall k im . (GaloisField k, KnownNat im)
  => AdicField k im     -- ^ Witness of @p^q@.
  -> Integer            -- ^ Integer @x@.
  -> Integer            -- ^ Modulus @y@.
  -> (Integer, Integer) -- ^ Integers @r@ and @s@ such that @sx = r mod y@.
adicGCD w x y = adicGCD' 0 1 y x
  where
    adicGCD' :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
    adicGCD' s s' r r'
      | r' == 0   = (r, s)
      | otherwise = adicGCD' s' (sub w s (mul w q s')) r' (sub w r (mul w q r'))
      where
        q = fst (adicQR w r r')
{-# INLINE adicGCD #-}
