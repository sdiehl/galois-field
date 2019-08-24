module Main where

import Protolude

import Criterion.Main

import Bench.Binary
import Bench.Extension
import Bench.Prime

main :: IO ()
main = defaultMain
  [benchmarkBinary, benchmarkExtension, benchmarkPrime]
