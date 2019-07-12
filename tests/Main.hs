module Main where

import Protolude

import Test.Tasty

import ExtensionFieldTests
import PrimeFieldTests

main :: IO ()
main = defaultMain $ testGroup "Tests" [testPrimeField, testExtensionField]
