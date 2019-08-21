module Bench.Binary where

import Protolude

import Control.Monad.Random
import Criterion.Main
import Data.Field.Galois.Binary

import Bench.Galois

type F2m = BinaryField 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425

f2m :: F2m
f2m = evalRand getRandom $ mkStdGen 0

f2m' :: F2m
f2m' = evalRand getRandom $ mkStdGen 1

benchmarkBinaryField :: Benchmark
benchmarkBinaryField = benchmark "BinaryField F2m" f2m f2m'
