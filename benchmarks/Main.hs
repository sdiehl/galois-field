module Main where

import Protolude

import Criterion.Main

import BinaryFieldBenchmarks
import ExtensionFieldBenchmarks
import PrimeFieldBenchmarks

main :: IO ()
main = defaultMain
  [benchmarkBinaryField, benchmarkExtensionField, benchmarkPrimeField]
