module Main where

import Protolude

import Test.Tasty

import Test.Binary
import Test.Extension
import Test.Prime

main :: IO ()
main = defaultMain $
  testGroup "Tests" [testPrimeField, testExtensionField, testBinaryField]
