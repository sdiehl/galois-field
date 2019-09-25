# Change log for galois-field

## 1.0.0

* Refactor library structure from `GaloisField` to `Data.Field.Galois`.
* Add `Semiring` dependency for Galois fields.
* Rename `PrimeField` to `Prime` and add `PrimeField` class.
* Rename `ExtensionField` to `Extension` and add `ExtensionField` class.
* Rename `BinaryField` to `Binary` and add `BinaryField` class.
* Rename `split` to `poly` and swap `IrreducibleMonic` parameters.
* Rename `toInt`, `toField`, `fromField` to `from`, `to` conversion functions.
* Replace `Integer` with `Natural`.
* Add `CyclicSubgroup` class with generator function.
* Add `RootsOfUnity` type with cofactor, check, and conversion functions.
* Add `TowerOfFields` class with embed and scalar multiplication functions.
* Add `Bounded` instances for prime fields and binary fields.
* Add `Enum` instances for prime fields and binary fields.
* Add `Group` instances for Galois fields.
* Add `Hashable` instances for prime fields and binary fields.
* Add `Integral` instances for prime fields and binary fields.
* Add `IsList` instances for Galois fields.
* Add `Real` instances for prime fields and binary fields.
* Add `rndR` function for Galois fields.
* Add `conj` function for extension fields.
* Add minor optimisations to exponentiation with `SPECIALISE`.
* Add major optimisations to `frob` function.
* Add pattern synonyms for field elements.

## 0.4.1

* Add compilation optimisations with `INLINABLE`.

## 0.4.0

* Add `Poly` dependency for extension fields.
* Add `qnr` function for Galois fields.
* Add `qr` function for Galois fields.
* Add `quad` function for extension fields and binary fields.
* Add `sr` function for extension fields and binary fields.
* Add `Semiring` instances for Galois fields.
* Add `Ord` instances for Galois fields.
* Add minor optimisations to exponentiation with `RULES`.
* Add pattern synonyms for monic monomials.

## 0.3.0

* Add complete implementation of binary fields.
* Add `quad` function for prime fields.
* Add `sr` function for prime fields.

## 0.2.1

* Add preliminary implementation of binary fields.
* Add `frob` function for Galois fields.
* Add minor improvements to documentation.

## 0.2.0

* Add `deg` function for Galois fields.
* Add `order` function for Galois fields.
* Add `pow` function for Galois fields.
* Add `rnd` function for Galois fields.
* Add `Random` instances for Galois fields.

## 0.1.1

* Add `Arbitrary` instances for Galois fields.
* Add `Bits` instances for prime fields.
* Add `Pretty` instances for Galois fields.
* Add minor optimisations to multiplication and inversion with `INLINE`.

## 0.1.0

* Initial release.
