module Main where

import Protolude

import Criterion.Main

import Bench.Binary
import Bench.Extension
import Bench.Prime

main :: IO ()
main = defaultMain
  [benchBinary, benchExtension, benchPrime]
