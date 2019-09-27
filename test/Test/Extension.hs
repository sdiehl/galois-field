module Test.Extension where

import Protolude

import Data.Field.Galois
import Test.Tasty

import Test.Galois
import Test.Prime

data P111
instance IrreducibleMonic P111 FS2 where
  poly _ = X2 + X + 1
type FS4 = Extension P111 FS2

data P1101
instance IrreducibleMonic P1101 FS2 where
  poly _ = X3 + X + 1
type FS8 = Extension P1101 FS2

data P1011
instance IrreducibleMonic P1011 FS2 where
  poly _ = X3 + X2 + 1
type FS8' = Extension P1011 FS2

data P101
instance IrreducibleMonic P101 FS3 where
  poly _ = X2 + 1
type FS9 = Extension P101 FS3

data P211
instance IrreducibleMonic P211 FS3 where
  poly _ = X2 + X - 1
type FS9' = Extension P211 FS3

data P221
instance IrreducibleMonic P221 FS3 where
  poly _ = X2 - X - 1
type FS9'' = Extension P221 FS3

instance IrreducibleMonic P101 FM0 where
  poly _ = X2 + 1
type FL0 = Extension P101 FM0

instance IrreducibleMonic P101 FM1 where
  poly _ = X2 + 1
type FL1 = Extension P101 FM1

instance IrreducibleMonic P101 FM2 where
  poly _ = X2 + 1
type FL2 = Extension P101 FM2

instance IrreducibleMonic P101 FM3 where
  poly _ = X2 + 1
type FL3 = Extension P101 FM3

instance IrreducibleMonic P101 FM4 where
  poly _ = X2 + 1
type FL4 = Extension P101 FM4

instance IrreducibleMonic P101 FVL where
  poly _ = X2 + 17
type FV2 = Extension P101 FVL

instance IrreducibleMonic P101 FXL where
  poly _ = X2 + 17
type FX2 = Extension P101 FXL

instance IrreducibleMonic P101 FZL where
  poly _ = X2 + 17
type FZ2 = Extension P101 FZL

data PU
instance IrreducibleMonic PU Fq where
  poly _ = X2 + 1
type Fq2 = Extension PU Fq

data PV
instance IrreducibleMonic PV Fq2 where
  poly _ = X3 - 9 - Y X
type Fq6 = Extension PV Fq2

data PW
instance IrreducibleMonic PW Fq6 where
  poly _ = X2 - Y X
type Fq12 = Extension PW Fq6

testExtension :: TestTree
testExtension = testGroup "Extension fields"
  [ test' "FS4"   (witness :: FS4  ) -- not implemented.
  , test  "FS8"   (witness :: FS8  )
  , test  "FS8'"  (witness :: FS8' )
  , test  "FS9"   (witness :: FS9  )
  , test  "FS9'"  (witness :: FS9' )
  , test  "FS9''" (witness :: FS9'')
  , test  "FL0"   (witness :: FL0  )
  , test  "FL1"   (witness :: FL1  )
  , test  "FL2"   (witness :: FL2  )
  , test  "FL3"   (witness :: FL3  )
  , test  "FL4"   (witness :: FL4  )
  , test  "FV2"   (witness :: FV2  )
  , test  "FX2"   (witness :: FX2  )
  , test  "FZ2"   (witness :: FZ2  )
  , test  "Fq2"   (witness :: Fq2  )
  , test' "Fq6"   (witness :: Fq6  ) -- time out.
  , test' "Fq12"  (witness :: Fq12 ) -- time out.
  ]
