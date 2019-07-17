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

fieldAxioms :: forall k . (Arbitrary k, Eq k, Fractional k, Show k)
  => TestName -> k -> TestTree
fieldAxioms s _ = testGroup ("Field axioms of " <> s)
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
