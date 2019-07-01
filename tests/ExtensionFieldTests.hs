module ExtensionFieldTests where

import Protolude

import ExtensionField
import Test.Tasty

import GaloisFieldTests
import PrimeFieldTests

data P11
instance IrreducibleMonic FS2 P11 where
  split _ = x^2 + x + 1
type FS4 = ExtensionField FS2 P11
test_S4 :: TestTree
test_S4 = fieldAxioms (Proxy :: Proxy FS4) "FS4"

data P110
instance IrreducibleMonic FS2 P110 where
  split _ = x^3 + x + 1
type FS8 = ExtensionField FS2 P110
test_S8 :: TestTree
test_S8 = fieldAxioms (Proxy :: Proxy FS8) "FS8"

data P101
instance IrreducibleMonic FS2 P101 where
  split _ = x^3 + x^2 + 1
type FS8' = ExtensionField FS2 P101
test_S8' :: TestTree
test_S8' = fieldAxioms (Proxy :: Proxy FS8') "FS8'"

data P10
instance IrreducibleMonic FS3 P10 where
  split _ = x^2 + 1
type FS9 = ExtensionField FS3 P10
test_S9 :: TestTree
test_S9 = fieldAxioms (Proxy :: Proxy FS9) "FS9"

data P21
instance IrreducibleMonic FS3 P21 where
  split _ = x^2 + x - 1
type FS9' = ExtensionField FS3 P21
test_S9' :: TestTree
test_S9' = fieldAxioms (Proxy :: Proxy FS9') "FS9'"

data P22
instance IrreducibleMonic FS3 P22 where
  split _ = x^2 - x - 1
type FS9'' = ExtensionField FS3 P22
test_S9'' :: TestTree
test_S9'' = fieldAxioms (Proxy :: Proxy FS9'') "FS9''"

instance IrreducibleMonic FM0 P10 where
  split _ = x^2 + 1
type FL0 = ExtensionField FM0 P10
test_L0 :: TestTree
test_L0 = fieldAxioms (Proxy :: Proxy FL0) "FL0"

instance IrreducibleMonic FM1 P10 where
  split _ = x^2 + 1
type FL1 = ExtensionField FM1 P10
test_L1 :: TestTree
test_L1 = fieldAxioms (Proxy :: Proxy FL1) "FL1"

instance IrreducibleMonic FM2 P10 where
  split _ = x^2 + 1
type FL2 = ExtensionField FM2 P10
test_L2 :: TestTree
test_L2 = fieldAxioms (Proxy :: Proxy FL2) "FL2"

instance IrreducibleMonic FM3 P10 where
  split _ = x^2 + 1
type FL3 = ExtensionField FM3 P10
test_L3 :: TestTree
test_L3 = fieldAxioms (Proxy :: Proxy FL3) "FL3"

instance IrreducibleMonic FM4 P10 where
  split _ = x^2 + 1
type FL4 = ExtensionField FM4 P10
test_L4 :: TestTree
test_L4 = fieldAxioms (Proxy :: Proxy FL4) "FL4"

instance IrreducibleMonic FVL P10 where
  split _ = x^2 + 1
type FV2 = ExtensionField FVL P10
test_V2 :: TestTree
test_V2 = fieldAxioms (Proxy :: Proxy FV2) "FV2"

instance IrreducibleMonic FXL P10 where
  split _ = x^2 + 1
type FX2 = ExtensionField FXL P10
test_X2 :: TestTree
test_X2 = fieldAxioms (Proxy :: Proxy FX2) "FX2"

instance IrreducibleMonic FZL P10 where
  split _ = x^2 + 1
type FZ2 = ExtensionField FZL P10
test_Z2 :: TestTree
test_Z2 = fieldAxioms (Proxy :: Proxy FZ2) "FZ2"

data Pu
instance IrreducibleMonic Fq Pu where
  split _ = x^2 + 1
type Fq2 = ExtensionField Fq Pu
test_Fq2 :: TestTree
test_Fq2 = fieldAxioms (Proxy :: Proxy Fq2) "Fq2"

data Pv
instance IrreducibleMonic Fq2 Pv where
  split _ = x^3 - (9 + t x)
type Fq6 = ExtensionField Fq2 Pv
test_Fq6 :: TestTree
test_Fq6 = fieldAxioms (Proxy :: Proxy Fq6) "Fq6"

data Pw
instance IrreducibleMonic Fq6 Pw where
  split _ = x^2 - t x
type Fq12 = ExtensionField Fq6 Pw
test_Fq12 :: TestTree
test_Fq12 = fieldAxioms (Proxy :: Proxy Fq12) "Fq12"
