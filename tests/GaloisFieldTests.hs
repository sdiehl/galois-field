module GaloisFieldTests where

import Protolude

import Test.Tasty
import Test.Tasty.QuickCheck

associativity :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associativity op x y z = op x (op y z) == op (op x y) z

commutativity :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutativity op x y = op x y == op y x

distributivity :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
distributivity op op' x y z = op (op' x y) z == op' (op x z) (op y z)
                           && op x (op' y z) == op' (op x y) (op x z)

identities :: Eq a => (a -> a -> a) -> a -> a -> Bool
identities op e x = op x e == x && op e x == x

inverses :: Eq a => (a -> a -> a) -> (a -> a) -> a -> a -> Bool
inverses op inv e x = op x (inv x) == e && op (inv x) x == e

ringAxioms :: forall r . (Arbitrary r, Eq r, Num r, Show r)
  => TestName -> r -> TestTree
ringAxioms s _ = testGroup ("Ring axioms of " <> s)
  [ testProperty "commutativity of addition"
    $ commutativity ((+) :: r -> r -> r)
  , testProperty "commutativity of multiplication"
    $ commutativity ((*) :: r -> r -> r)
  , testProperty "associativity of addition"
    $ associativity ((+) :: r -> r -> r)
  , testProperty "associativity of multiplication"
    $ associativity ((*) :: r -> r -> r)
  , testProperty "distributivity of multiplication over addition"
    $ distributivity ((*) :: r -> r -> r) (+)
  , testProperty "additive identity"
    $ identities ((+) :: r -> r -> r) 0
  , testProperty "multiplicative identity"
    $ identities ((*) :: r -> r -> r) 1
  , testProperty "additive inverses"
    $ inverses ((+) :: r -> r -> r) negate 0
  ]

fieldAxioms :: forall k . (Arbitrary k, Eq k, Fractional k, Show k)
  => TestName -> k -> TestTree
fieldAxioms s k = testGroup ("Field axioms of " <> s)
  [ ringAxioms s k
  , testProperty "multiplicative inverses"
    $ \n -> n /= 0 ==> inverses ((*) :: k -> k -> k) recip 1 n
  ]
