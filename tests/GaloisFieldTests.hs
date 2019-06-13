{-# LANGUAGE ScopedTypeVariables #-}

module GaloisFieldTests where

import Protolude

import Test.Tasty
import Test.Tasty.QuickCheck

import GaloisField

associativity :: GaloisField k => (k -> k -> k) -> k -> k -> k -> Bool
associativity op x y z = op x (op y z) == op (op x y) z

commutativity :: GaloisField k => (k -> k -> k) -> k -> k -> Bool
commutativity op x y = op x y == op y x

distributivity :: GaloisField k => (k -> k -> k) -> (k -> k -> k) -> k -> k -> k -> Bool
distributivity op op' x y z = op (op' x y) z == op' (op x z) (op y z)
                           && op x (op' y z) == op' (op x y) (op x z)

identities :: GaloisField k => (k -> k -> k) -> k -> k -> Bool
identities op e x = op x e == x && op e x == x

inverses :: GaloisField k => (k -> k -> k) -> (k -> k) -> k -> k -> Bool
inverses op inv e x = op x (inv x) == e && op (inv x) x == e

axioms :: forall k . (Arbitrary k, GaloisField k)
  => Proxy k -> TestName -> TestTree
axioms _ descr = testGroup ("Test field axioms of " <> descr)
  [ testProperty "commutativity of addition"
    $ commutativity ((+) :: k -> k -> k)
  , testProperty "commutativity of multiplication"
    $ commutativity ((*) :: k -> k -> k)
  , testProperty "associativity of addition"
    $ associativity ((+) :: k -> k -> k)
  , testProperty "associativity of multiplication"
    $ associativity ((*) :: k -> k -> k)
  , testProperty "distributivity of multiplication over addition"
    $ distributivity ((*) :: k -> k -> k) (+)
  , testProperty "additive identity"
    $ identities ((+) :: k -> k -> k) 0
  , testProperty "multiplicative identity"
    $ identities ((*) :: k -> k -> k) 1
  , testProperty "additive inverses"
    $ inverses ((+) :: k -> k -> k) negate 0
  , testProperty "multiplicative inverses"
    $ \n -> n /= 0 ==> inverses ((*) :: k -> k -> k) recip 1 n
  ]
