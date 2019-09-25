module Bench.Galois where

import Protolude

import Criterion.Main
import Data.Field.Galois
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
  , bench "Frobenius endomorphism" $
    nf frob a
  , bench "Square root" $
    nf sr a
  ]
