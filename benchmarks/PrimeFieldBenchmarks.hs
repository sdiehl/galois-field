module PrimeFieldBenchmarks where

import Criterion.Main
import PrimeField

import GaloisFieldBenchmarks

type Fq = PrimeField 21888242871839275222246405745257275088696311157297823662689037894645226208583

fq :: Fq
fq = 5216004179354450092383934373463611881445186046129513844852096383579774061693

fq' :: Fq
fq' = 10757805228921058098980668000791497318123219899766237205512608761387909753942

benchmarkPrimeField :: Benchmark
benchmarkPrimeField = benchmark "PrimeField Fq" fq fq'