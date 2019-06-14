{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExtensionFieldTests where

import Protolude

import Test.Tasty
import Test.Tasty.QuickCheck

import ExtensionField
import GaloisField
import GaloisFieldTests
import PolynomialRingTests
import PrimeField
import PrimeFieldTests

instance (Arbitrary k, GaloisField k, IrreducibleMonic ps)
  => Arbitrary (ExtensionField k ps) where
  arbitrary = EF <$> arbitrary

instance IrreducibleMonic '[1, 1] where splitting = Poly [1, 1]
instance IrreducibleMonic '[1, 1, 0] where splitting = Poly [1, 1, 0]
instance IrreducibleMonic '[1, 0, 1] where splitting = Poly [1, 0, 1]
instance IrreducibleMonic '[1, 0] where splitting = Poly [1, 0]
instance IrreducibleMonic '[2, 1] where splitting = Poly [2, 1]
instance IrreducibleMonic '[2, 2] where splitting = Poly [2, 2]

test_4 = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 2) [1, 1])) "F4"

test_8' = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 2) [1, 0, 1])) "F8'"

test_8 = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 2) [1, 1, 0])) "F8"

test_9 = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 3) [1, 0])) "F9"

test_9' = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 3) [2, 1])) "F9'"

test_9'' = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 3) [2, 2])) "F9''"

test_0 = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 2147483647) [1, 0])) "F2M0"

test_1 = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 2305843009213693951) [1, 0])) "F2M1"

test_2 = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 618970019642690137449562111) [1, 0])) "F2M2"

test_3 = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 162259276829213363391578010288127) [1, 0])) "F2M3"

test_4' = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 170141183460469231731687303715884105727) [1, 0])) "F2M4"

test_V = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 20988936657440586486151264256610222593863921) [1, 0])) "FVL2"

test_X = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 5210644015679228794060694325390955853335898483908056458352183851018372555735221) [1, 0])) "FXL2"

test_Z = fieldAxioms (Proxy :: Proxy (ExtensionField (PrimeField 741640062627530801524787141901937474059940781097519023905821316144415759504705008092818711693940737) [1, 0])) "FZL2"
