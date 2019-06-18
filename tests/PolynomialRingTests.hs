module PolynomialRingTests where

import Protolude

import Test.Tasty.QuickCheck

import GaloisField
import GaloisFieldTests
import PolynomialRing
import PrimeField
import PrimeFieldTests

instance (Arbitrary k, GaloisField k) => Arbitrary (Polynomial k) where
  arbitrary = toPoly <$> arbitrary

test_S2X = ringAxioms (Proxy :: Proxy (Polynomial (PrimeField 2))) "FS2[X]"

test_S3X = ringAxioms (Proxy :: Proxy (Polynomial (PrimeField 3))) "FS3[X]"

test_S5X = ringAxioms (Proxy :: Proxy (Polynomial (PrimeField 5))) "FS5[X]"

test_S7X = ringAxioms (Proxy :: Proxy (Polynomial (PrimeField 7))) "FS7[X]"

test_M0X = ringAxioms (Proxy :: Proxy (Polynomial (PrimeField 2147483647))) "FM0[X]"

test_M1X = ringAxioms (Proxy :: Proxy (Polynomial (PrimeField 2305843009213693951))) "FM1[X]"

test_M2X = ringAxioms (Proxy :: Proxy (Polynomial (PrimeField 618970019642690137449562111))) "FM2[X]"

test_M3X = ringAxioms (Proxy :: Proxy (Polynomial (PrimeField 162259276829213363391578010288127))) "FM3[X]"

test_M4X = ringAxioms (Proxy :: Proxy (Polynomial (PrimeField 170141183460469231731687303715884105727))) "FM4[X]"

test_VLX = ringAxioms (Proxy :: Proxy (Polynomial (PrimeField 20988936657440586486151264256610222593863921))) "FVL[X]"

test_XLX = ringAxioms (Proxy :: Proxy (Polynomial (PrimeField 5210644015679228794060694325390955853335898483908056458352183851018372555735221))) "FXL[X]"

test_ZLX = ringAxioms (Proxy :: Proxy (Polynomial (PrimeField 741640062627530801524787141901937474059940781097519023905821316144415759504705008092818711693940737))) "FZL[X]"
