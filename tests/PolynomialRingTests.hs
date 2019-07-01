module PolynomialRingTests where

import Protolude

import PolynomialRing
import Test.Tasty

import PrimeFieldTests
import GaloisFieldTests

test_S2X :: TestTree
test_S2X = ringAxioms (Proxy :: Proxy (Polynomial FS2)) "FS2[X]"

test_S3X :: TestTree
test_S3X = ringAxioms (Proxy :: Proxy (Polynomial FS3)) "FS3[X]"

test_S5X :: TestTree
test_S5X = ringAxioms (Proxy :: Proxy (Polynomial FS5)) "FS5[X]"

test_S7X :: TestTree
test_S7X = ringAxioms (Proxy :: Proxy (Polynomial FS7)) "FS7[X]"

test_M0X :: TestTree
test_M0X = ringAxioms (Proxy :: Proxy (Polynomial FM0)) "FM0[X]"

test_M1X :: TestTree
test_M1X = ringAxioms (Proxy :: Proxy (Polynomial FM1)) "FM1[X]"

test_M2X :: TestTree
test_M2X = ringAxioms (Proxy :: Proxy (Polynomial FM2)) "FM2[X]"

test_M3X :: TestTree
test_M3X = ringAxioms (Proxy :: Proxy (Polynomial FM3)) "FM3[X]"

test_M4X :: TestTree
test_M4X = ringAxioms (Proxy :: Proxy (Polynomial FM4)) "FM4[X]"

test_VLX :: TestTree
test_VLX = ringAxioms (Proxy :: Proxy (Polynomial FVL)) "FVL[X]"

test_XLX :: TestTree
test_XLX = ringAxioms (Proxy :: Proxy (Polynomial FXL)) "FXL[X]"

test_ZLX :: TestTree
test_ZLX = ringAxioms (Proxy :: Proxy (Polynomial FZL)) "FZL[X]"
