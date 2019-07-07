module PrimeFieldTests where

import Protolude

import PrimeField
import Test.Tasty

import GaloisFieldTests

type FS2 = PrimeField 2
type FS3 = PrimeField 3
type FS5 = PrimeField 5
type FS7 = PrimeField 7

type FM0 = PrimeField 2147483647
type FM1 = PrimeField 2305843009213693951
type FM2 = PrimeField 618970019642690137449562111
type FM3 = PrimeField 162259276829213363391578010288127
type FM4 = PrimeField 170141183460469231731687303715884105727

type FVL = PrimeField 20988936657440586486151264256610222593863921
type FXL = PrimeField 5210644015679228794060694325390955853335898483908056458352183851018372555735221
type FZL = PrimeField 741640062627530801524787141901937474059940781097519023905821316144415759504705008092818711693940737

type Fq = PrimeField 21888242871839275222246405745257275088696311157297823662689037894645226208583


test_S2 :: TestTree
test_S2 = fieldAxioms (witness :: FS2) "FS2"

test_S3 :: TestTree
test_S3 = fieldAxioms (witness :: FS3) "FS3"

test_S5 :: TestTree
test_S5 = fieldAxioms (witness :: FS5) "FS5"

test_S7 :: TestTree
test_S7 = fieldAxioms (witness :: FS7) "FS7"

test_M0 :: TestTree
test_M0 = fieldAxioms (witness :: FM0) "FM0"

test_M1 :: TestTree
test_M1 = fieldAxioms (witness :: FM1) "FM1"

test_M2 :: TestTree
test_M2 = fieldAxioms (witness :: FM2) "FM2"

test_M3 :: TestTree
test_M3 = fieldAxioms (witness :: FM3) "FM3"

test_M4 :: TestTree
test_M4 = fieldAxioms (witness :: FM4) "FM4"

test_VL :: TestTree
test_VL = fieldAxioms (witness :: FVL) "FVL"

test_XL :: TestTree
test_XL = fieldAxioms (witness :: FXL) "FXL"

test_ZL :: TestTree
test_ZL = fieldAxioms (witness :: FZL) "FZL"
