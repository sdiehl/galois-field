module Bench.Extension where

import Protolude

import Control.Monad.Random
import Criterion.Main
import Data.Field.Galois

import Bench.Galois
import Bench.Prime

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

fq12 :: Fq12
fq12 = evalRand getRandom $ mkStdGen 0

fq12' :: Fq12
fq12' = evalRand getRandom $ mkStdGen 1

benchExtension :: Benchmark
benchExtension = benchmark "Extension" fq12 fq12'
