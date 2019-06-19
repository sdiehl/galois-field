module Main where

import Protolude

import Criterion.Main

import Benchmarks

main :: IO ()
main = defaultMain [benchmarks]
