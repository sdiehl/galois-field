module ExtensionFieldBenchmarks where

import Protolude

import Control.Monad.Random
import Criterion.Main
import ExtensionField

import GaloisFieldBenchmarks
import PrimeFieldBenchmarks

data Pu
instance IrreducibleMonic Fq Pu where
  split _ = X2 + 1
type Fq2 = ExtensionField Fq Pu

data Pv
instance IrreducibleMonic Fq2 Pv where
  split _ = X3 - 9 - Y X
type Fq6 = ExtensionField Fq2 Pv

data Pw
instance IrreducibleMonic Fq6 Pw where
  split _ = X2 - Y X
type Fq12 = ExtensionField Fq6 Pw

fq12 :: Fq12
fq12 = evalRand getRandom $ mkStdGen 0

fq12' :: Fq12
fq12' = evalRand getRandom $ mkStdGen 1

benchmarkExtensionField :: Benchmark
benchmarkExtensionField = benchmark "ExtensionField Fq12" fq12 fq12'
