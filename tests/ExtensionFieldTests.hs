module ExtensionFieldTests where

import Protolude

import ExtensionField
import Test.Tasty

import GaloisFieldTests
import PrimeFieldTests

data P111
instance IrreducibleMonic FS2 P111 where
  split _ = [1, 1, 1]
type FS4 = ExtensionField FS2 P111

data P1101
instance IrreducibleMonic FS2 P1101 where
  split _ = [1, 1, 0, 1]
type FS8 = ExtensionField FS2 P1101

data P1011
instance IrreducibleMonic FS2 P1011 where
  split _ = [1, 0, 1, 1]
type FS8' = ExtensionField FS2 P1011

data P101
instance IrreducibleMonic FS3 P101 where
  split _ = [1, 0, 1]
type FS9 = ExtensionField FS3 P101

data P211
instance IrreducibleMonic FS3 P211 where
  split _ = [2, 1, 1]
type FS9' = ExtensionField FS3 P211

data P221
instance IrreducibleMonic FS3 P221 where
  split _ = [2, 2, 1]
type FS9'' = ExtensionField FS3 P221

instance IrreducibleMonic FM0 P101 where
  split _ = [1, 0, 1]
type FL0 = ExtensionField FM0 P101

instance IrreducibleMonic FM1 P101 where
  split _ = [1, 0, 1]
type FL1 = ExtensionField FM1 P101

instance IrreducibleMonic FM2 P101 where
  split _ = [1, 0, 1]
type FL2 = ExtensionField FM2 P101

instance IrreducibleMonic FM3 P101 where
  split _ = [1, 0, 1]
type FL3 = ExtensionField FM3 P101

instance IrreducibleMonic FM4 P101 where
  split _ = [1, 0, 1]
type FL4 = ExtensionField FM4 P101

instance IrreducibleMonic FVL P101 where
  split _ = [1, 0, 1]
type FV2 = ExtensionField FVL P101

instance IrreducibleMonic FXL P101 where
  split _ = [1, 0, 1]
type FX2 = ExtensionField FXL P101

instance IrreducibleMonic FZL P101 where
  split _ = [1, 0, 1]
type FZ2 = ExtensionField FZL P101

data Pu
instance IrreducibleMonic Fq Pu where
  split _ = [1, 0, 1]
type Fq2 = ExtensionField Fq Pu

data Pv
instance IrreducibleMonic Fq2 Pv where
  split _ = [-9 - x, 0, 0, 1]
type Fq6 = ExtensionField Fq2 Pv

data Pw
instance IrreducibleMonic Fq6 Pw where
  split _ = [-x, 0, 1]
type Fq12 = ExtensionField Fq6 Pw

testExtensionField :: TestTree
testExtensionField = testGroup "Extension fields"
  [ fieldAxioms "FS4"   (witness :: FS4  )
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
