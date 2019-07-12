module ExtensionFieldTests where

import Protolude

import ExtensionField
import PolynomialRing
import Test.Tasty

import GaloisFieldTests
import PrimeFieldTests

data P11
instance IrreducibleMonic FS2 P11 where
  split _ = x ^ (2 :: Int) + x + 1
type FS4 = ExtensionField FS2 P11

data P110
instance IrreducibleMonic FS2 P110 where
  split _ = x ^ (3 :: Int) + x + 1
type FS8 = ExtensionField FS2 P110

data P101
instance IrreducibleMonic FS2 P101 where
  split _ = x ^ (3 :: Int) + x ^ (2 :: Int) + 1
type FS8' = ExtensionField FS2 P101

data P10
instance IrreducibleMonic FS3 P10 where
  split _ = x ^ (2 :: Int) + 1
type FS9 = ExtensionField FS3 P10

data P21
instance IrreducibleMonic FS3 P21 where
  split _ = x ^ (2 :: Int) + x - 1
type FS9' = ExtensionField FS3 P21

data P22
instance IrreducibleMonic FS3 P22 where
  split _ = x ^ (2 :: Int) - x - 1
type FS9'' = ExtensionField FS3 P22

instance IrreducibleMonic FM0 P10 where
  split _ = x ^ (2 :: Int) + 1
type FL0 = ExtensionField FM0 P10

instance IrreducibleMonic FM1 P10 where
  split _ = x ^ (2 :: Int) + 1
type FL1 = ExtensionField FM1 P10

instance IrreducibleMonic FM2 P10 where
  split _ = x ^ (2 :: Int) + 1
type FL2 = ExtensionField FM2 P10

instance IrreducibleMonic FM3 P10 where
  split _ = x ^ (2 :: Int) + 1
type FL3 = ExtensionField FM3 P10

instance IrreducibleMonic FM4 P10 where
  split _ = x ^ (2 :: Int) + 1
type FL4 = ExtensionField FM4 P10

instance IrreducibleMonic FVL P10 where
  split _ = x ^ (2 :: Int) + 1
type FV2 = ExtensionField FVL P10

instance IrreducibleMonic FXL P10 where
  split _ = x ^ (2 :: Int) + 1
type FX2 = ExtensionField FXL P10

instance IrreducibleMonic FZL P10 where
  split _ = x ^ (2 :: Int) + 1
type FZ2 = ExtensionField FZL P10

data Pu
instance IrreducibleMonic Fq Pu where
  split _ = x ^ (2 :: Int) + 1
type Fq2 = ExtensionField Fq Pu

data Pv
instance IrreducibleMonic Fq2 Pv where
  split _ = x ^ (3 :: Int) - (9 + t x)
type Fq6 = ExtensionField Fq2 Pv

data Pw
instance IrreducibleMonic Fq6 Pw where
  split _ = x ^ (2 :: Int) - t x
type Fq12 = ExtensionField Fq6 Pw

testExtensionField :: TestTree
testExtensionField = testGroup "Extension fields"
  [ testGroup "Polynomial rings"
    [ ringAxioms "FS2[X]" (witness :: Polynomial FS2)
    , ringAxioms "FS3[X]" (witness :: Polynomial FS3)
    , ringAxioms "FS5[X]" (witness :: Polynomial FS5)
    , ringAxioms "FS7[X]" (witness :: Polynomial FS7)
    , ringAxioms "FM0[X]" (witness :: Polynomial FM0)
    , ringAxioms "FM1[X]" (witness :: Polynomial FM1)
    , ringAxioms "FM2[X]" (witness :: Polynomial FM2)
    , ringAxioms "FM3[X]" (witness :: Polynomial FM3)
    , ringAxioms "FM4[X]" (witness :: Polynomial FM4)
    , ringAxioms "FVL[X]" (witness :: Polynomial FVL)
    , ringAxioms "FXL[X]" (witness :: Polynomial FXL)
    , ringAxioms "FZL[X]" (witness :: Polynomial FZL)
    ]
  , fieldAxioms "FS4"   (witness :: FS4  )
  , fieldAxioms "FS8"   (witness :: FS8  )
  , fieldAxioms "FS8'"  (witness :: FS8' )
  , fieldAxioms "FS9"   (witness :: FS9  )
  , fieldAxioms "FS9'"  (witness :: FS9' )
  , fieldAxioms "FS9''" (witness :: FS9'')
  , fieldAxioms "FL0"   (witness :: FL0  )
  , fieldAxioms "FL1"   (witness :: FL1  )
  , fieldAxioms "FL2"   (witness :: FL2  )
  , fieldAxioms "FL3"   (witness :: FL3  )
  , fieldAxioms "FL4"   (witness :: FL4  )
  , fieldAxioms "FV2"   (witness :: FV2  )
  , fieldAxioms "FX2"   (witness :: FX2  )
  , fieldAxioms "FZ2"   (witness :: FZ2  )
  , fieldAxioms "Fq2"   (witness :: Fq2  )
  , fieldAxioms "Fq6"   (witness :: Fq6  )
  , fieldAxioms "Fq12"  (witness :: Fq12 )
  ]
