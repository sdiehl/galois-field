module GaloisFieldBenchmarks where

import Protolude

import Criterion.Main
import GaloisField
import GHC.Base

benchmark :: GaloisField k => String -> k -> k -> Benchmark
benchmark s a b = bgroup s
  [ bench "Addition" $
    nf (uncurry (+)) (a, b)
  , bench "Multiplication" $
    nf (uncurry (*)) (a, b)
  , bench "Negation" $
    nf negate a
  , bench "Subtraction" $
    nf (uncurry (-)) (a, b)
  , bench "Inversion" $
    nf recip a
  , bench "Division" $
    nf (uncurry (/)) (a, b)
  ]
