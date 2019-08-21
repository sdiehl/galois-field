module Test.Prime where

import Protolude

import Data.Field.Galois.Prime
import Test.Tasty

import Test.Galois

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

testPrimeField :: TestTree
testPrimeField = testGroup "Prime fields"
  [ test "FS2" (witness :: FS2)
  , test "FS3" (witness :: FS3)
  , test "FS5" (witness :: FS5)
  , test "FS7" (witness :: FS7)
  , test "FM0" (witness :: FM0)
  , test "FM1" (witness :: FM1)
  , test "FM2" (witness :: FM2)
  , test "FM3" (witness :: FM3)
  , test "FM4" (witness :: FM4)
  , test "FVL" (witness :: FVL)
  , test "FXL" (witness :: FXL)
  , test "FZL" (witness :: FZL)
  ]
