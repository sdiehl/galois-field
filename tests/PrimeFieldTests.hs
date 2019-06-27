module PrimeFieldTests where

import Protolude

import Test.Tasty.QuickCheck

import PrimeField
import GaloisFieldTests

test_S2 = fieldAxioms (Proxy :: Proxy (PrimeField 2)) "FS2"

test_S3 = fieldAxioms (Proxy :: Proxy (PrimeField 3)) "FS3"

test_S5 = fieldAxioms (Proxy :: Proxy (PrimeField 5)) "FS5"

test_S7 = fieldAxioms (Proxy :: Proxy (PrimeField 7)) "FS7"

test_M0 = fieldAxioms (Proxy :: Proxy (PrimeField 2147483647)) "FM0"

test_M1 = fieldAxioms (Proxy :: Proxy (PrimeField 2305843009213693951)) "FM1"

test_M2 = fieldAxioms (Proxy :: Proxy (PrimeField 618970019642690137449562111)) "FM2"

test_M3 = fieldAxioms (Proxy :: Proxy (PrimeField 162259276829213363391578010288127)) "FM3"

test_M4 = fieldAxioms (Proxy :: Proxy (PrimeField 170141183460469231731687303715884105727)) "FM4"

test_VL = fieldAxioms (Proxy :: Proxy (PrimeField 20988936657440586486151264256610222593863921)) "FVL"

test_XL = fieldAxioms (Proxy :: Proxy (PrimeField 5210644015679228794060694325390955853335898483908056458352183851018372555735221)) "FXL"

test_ZL = fieldAxioms (Proxy :: Proxy (PrimeField 741640062627530801524787141901937474059940781097519023905821316144415759504705008092818711693940737)) "FZL"
