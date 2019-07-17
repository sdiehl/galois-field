module Main where

import Protolude

import Test.Tasty

import BinaryFieldTests
import ExtensionFieldTests
import PrimeFieldTests

main :: IO ()
main = defaultMain $
  testGroup "Tests" [testPrimeField, testExtensionField, testBinaryField]
