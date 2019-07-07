module PolynomialRingTests where

import Protolude

import PolynomialRing
import Test.Tasty

import PrimeFieldTests
import GaloisFieldTests

test_S2X :: TestTree
test_S2X = ringAxioms (witness :: Polynomial FS2) "FS2[X]"

test_S3X :: TestTree
test_S3X = ringAxioms (witness :: Polynomial FS3) "FS3[X]"

test_S5X :: TestTree
test_S5X = ringAxioms (witness :: Polynomial FS5) "FS5[X]"

test_S7X :: TestTree
test_S7X = ringAxioms (witness :: Polynomial FS7) "FS7[X]"

test_M0X :: TestTree
test_M0X = ringAxioms (witness :: Polynomial FM0) "FM0[X]"

test_M1X :: TestTree
test_M1X = ringAxioms (witness :: Polynomial FM1) "FM1[X]"

test_M2X :: TestTree
test_M2X = ringAxioms (witness :: Polynomial FM2) "FM2[X]"

test_M3X :: TestTree
test_M3X = ringAxioms (witness :: Polynomial FM3) "FM3[X]"

test_M4X :: TestTree
test_M4X = ringAxioms (witness :: Polynomial FM4) "FM4[X]"

test_VLX :: TestTree
test_VLX = ringAxioms (witness :: Polynomial FVL) "FVL[X]"

test_XLX :: TestTree
test_XLX = ringAxioms (witness :: Polynomial FXL) "FXL[X]"

test_ZLX :: TestTree
test_ZLX = ringAxioms (witness :: Polynomial FZL) "FZL[X]"
