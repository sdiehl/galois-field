module GaloisFieldBenchmarks where

import Protolude

import Criterion.Main
import GaloisField
import GHC.Base

benchmark :: GaloisField k => String -> k -> k -> Benchmark
benchmark s a b = bgroup s
  [ bench "Addition" $
    whnf (uncurry (+)) (a, b)
  , bench "Multiplication" $
    whnf (uncurry (*)) (a, b)
  , bench "Negation" $
    whnf negate a
  , bench "Subtraction" $
    whnf (uncurry (-)) (a, b)
  , bench "Inversion" $
    whnf recip a
  , bench "Division" $
    whnf (uncurry (/)) (a, b)
  ]
