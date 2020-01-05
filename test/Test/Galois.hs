module Test.Galois where

import Protolude

import Data.Field.Galois
import Test.Tasty
import Test.Tasty.QuickCheck

annihilation :: Eq a => (a -> a -> a) -> a -> a -> Bool
annihilation op e x = op x e == e && op e x == e

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

groupAxioms :: forall g . (Arbitrary g, Eq g, Show g)
  => (g -> g -> g) -> (g -> g) -> g -> (g -> Bool) -> [TestTree]
groupAxioms add inv id cond =
  [ testProperty "associativity" $
    associativity add
  , testProperty "commutativity" $
    commutativity add
  , testProperty "identity" $
    identities add id
  , testProperty "inverses" $
    \x -> cond x ==> inverses add inv id x
  ]

fieldAxioms :: forall k . GaloisField k => k -> TestTree
fieldAxioms _ = testGroup "Field axioms"
  [ testGroup "additive group axioms" $
    groupAxioms (+) negate (0 :: k) (const True)
  , testGroup "multiplicative group axioms" $
    groupAxioms (*) recip (1 :: k) (/= 0)
  , testProperty "distributivity of multiplication over addition" $
    distributivity ((*) :: k -> k -> k) (+)
  , testProperty "multiplicative annihilation" $
    annihilation ((*) :: k -> k -> k) 0
  ]

frobeniusEndomorphisms :: forall k . GaloisField k => k -> TestTree
frobeniusEndomorphisms _ = testGroup "Frobenius endomorphisms"
  [ testProperty "frobenius endomorphisms are characteristic powers" $
    \(x :: k) -> frob x == pow x (char (witness :: k))
  , testProperty "frobenius endomorphisms are ring homomorphisms" $
    \(x :: k) (y :: k) (z :: k) -> frob (x * y + z) == frob x * frob y + frob z
  ]

squareRoots :: forall k . GaloisField k => k -> TestTree
squareRoots _ = localOption (QuickCheckMaxRatio 100)
  . localOption (QuickCheckTests 10) $ testGroup "Square roots"
  [ testProperty "squares of square roots" $
    \(x :: k) -> qr x
    ==> ((join (*) <$> sr x) == Just x)
  , testProperty "solutions of quadratic equations" $
    \(a :: k) (b :: k) (c :: k) -> a /= 0 && isJust (quad a b c)
    ==> (((\x -> (a * x + b) * x + c) <$> quad a b c) == Just 0)
  ]

test :: forall k . GaloisField k => TestName -> k -> TestTree
test s x = testGroup s [fieldAxioms x, frobeniusEndomorphisms x, squareRoots x]

test' :: forall k . GaloisField k => TestName -> k -> TestTree
test' s x = testGroup s [fieldAxioms x, frobeniusEndomorphisms x]
