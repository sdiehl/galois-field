<p align="center">
  <a href="https://www.adjoint.io">
    <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
  </a>
</p>

[![CircleCI](https://circleci.com/gh/adjoint-io/galois-field.svg?style=svg)](https://circleci.com/gh/adjoint-io/galois-field)
[![Hackage](https://img.shields.io/hackage/v/galois-field.svg)](https://hackage.haskell.org/package/galois-field)

# Galois Field

An efficient implementation of Galois fields used in cryptography research.

## Technical background

A **Galois field** GF(p^q), for prime p and positive q, is a *field* (GF(p^q), +, \*, 0, 1) of finite *order*. Explicitly,
- (GF(p^q), +, 0) is an abelian group,
- (GF(p^q) \\ \{0\}, \*, 1) is an abelian group,
- \* is distributive over +, and
- \#GF(p^q) is finite.

### Prime fields

Any Galois field has a unique *characteristic* p, the minimum positive p such that p(1) = 1 + ... + 1 = 0, and p is prime. The smallest Galois field of characteristic p is a **prime field**, and any Galois field of characteristic p is a *finite-dimensional vector space* over its prime subfield.

For example, GF(4) is a Galois field of characteristic 2 that is a two-dimensional vector space over the prime subfield GF(2) = Z / 2Z.

### Extension fields

Any Galois field has order a prime power p^q for prime p and positive q, and there is a Galois field GF(p^q) of any prime power order p^q that is *unique up to non-unique isomorphism*. Any Galois field GF(p^q) can be constructed as an **extension field** over a smaller Galois subfield GF(p^r), through the identification GF(p^q) = GF(p^r)[X] / \<f(X)\> for an *irreducible monic polynomial* f(X) of degree q - r + 1 in the *polynomial ring* GF(p^r)[X].

For example, GF(4) has order 2^2 and can be constructed as an extension field GF(2)[X] / \<f(X)\> where f(X) = X^2 + X + 1 is an irreducible monic quadratic polynomial in GF(2)[X].

### Binary fields

A Galois field of the form GF(2^m) for big positive m is a sum of X^n for a non-empty set of 0 \< n \< m. For computational efficiency in cryptography, an element of a **binary field** can be represented by an integer that represents a bit string. It should always be used when the field characteristic is 2.

For example, X^8 + X^4 + X^3 + X + 1 can be represented as the integer 283 that represents the bit string 100011011.

## Example usage

Include the following required language extensions.
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
```
Import the following functions at minimum.
```haskell
import Data.Field.Galois (Prime, Extension, IrreducibleMonic(poly), Binary,
                          pattern X, pattern X2, pattern X3, pattern Y)
```

### Prime fields

The following type declaration creates a prime field of a given characteristic.
```haskell
type Fq = Prime 21888242871839275222246405745257275088696311157297823662689037894645226208583
```
Note that the characteristic given *must* be prime.

Galois field arithmetic can then be performed in this prime field.
```haskell
fq :: Fq
fq = 5216004179354450092383934373463611881445186046129513844852096383579774061693

fq' :: Fq
fq' = 10757805228921058098980668000791497318123219899766237205512608761387909753942

arithmeticFq :: (Fq, Fq, Fq, Fq)
arithmeticFq = (fq + fq', fq - fq', fq * fq', fq / fq')
```

### Extension fields

The following data type declaration creates a polynomial given an irreducible monic polynomial.
```haskell
data P2
instance IrreducibleMonic P2 Fq where
  poly _ = X2 + 1
```
The following type declaration then creates an extension field with this polynomial.
```haskell
type Fq2 = Extension P2 Fq
```
Note that the polynomial given *must* be irreducible and monic in the prime field.

Similarly, further extension fields can be constructed iteratively as follows.
```haskell
data P6
instance IrreducibleMonic P6 Fq2 where
  poly _ = X3 - (9 + Y X)

type Fq6 = Extension P6 Fq2

data P12
instance IrreducibleMonic P12 Fq6 where
  poly _ = X2 - Y X

type Fq12 = Extension P12 Fq6
```
Note that `X, X2, X3` accesses the current indeterminate variables and `Y` descends the tower of indeterminate variables.

Galois field arithmetic can then be performed in this extension field.
```haskell
fq12 :: Fq12
fq12 =
  [ [ [ 4025484419428246835913352650763180341703148406593523188761836807196412398582
      , 5087667423921547416057913184603782240965080921431854177822601074227980319916
      ]
    , [ 8868355606921194740459469119392835913522089996670570126495590065213716724895
      , 12102922015173003259571598121107256676524158824223867520503152166796819430680
      ]
    , [ 92336131326695228787620679552727214674825150151172467042221065081506740785
      , 5482141053831906120660063289735740072497978400199436576451083698548025220729
      ]
    ]
  , [ [ 7642691434343136168639899684817459509291669149586986497725240920715691142493
      , 1211355239100959901694672926661748059183573115580181831221700974591509515378
      ]
    , [ 20725578899076721876257429467489710434807801418821512117896292558010284413176
      , 17642016461759614884877567642064231230128683506116557502360384546280794322728
      ]
    , [ 17449282511578147452934743657918270744212677919657988500433959352763226500950
      , 1205855382909824928004884982625565310515751070464736233368671939944606335817
      ]
    ]
  ]

fq12' :: Fq12
fq12' =
  [ [ [ 495492586688946756331205475947141303903957329539236899715542920513774223311
      , 9283314577619389303419433707421707208215462819919253486023883680690371740600
      ]
    , [ 11142072730721162663710262820927009044232748085260948776285443777221023820448
      , 1275691922864139043351956162286567343365697673070760209966772441869205291758
      ]
    , [ 20007029371545157738471875537558122753684185825574273033359718514421878893242
      , 9839139739201376418106411333971304469387172772449235880774992683057627654905
      ]
    ]
  , [ [ 9503058454919356208294350412959497499007919434690988218543143506584310390240
      , 19236630380322614936323642336645412102299542253751028194541390082750834966816
      ]
    , [ 18019769232924676175188431592335242333439728011993142930089933693043738917983
      , 11549213142100201239212924317641009159759841794532519457441596987622070613872
      ]
    , [ 9656683724785441232932664175488314398614795173462019188529258009817332577664
      , 20666848762667934776817320505559846916719041700736383328805334359135638079015
      ]
    ]
  ]

arithmeticFq12 :: (Fq12, Fq12, Fq12, Fq12)
arithmeticFq12 = (fq12 + fq12', fq12 - fq12', fq12 * fq12', fq12 / fq12')
```
Note that
```
a + bX + (c + dX)Y + (e + fX)Y^2 + (g + hX + (i + jX)Y + (k + lX)Y^2)Z
```
where `X, Y, Z` is a tower of indeterminate variables, is constructed by
```haskell
[ [ [a, b], [c, d], [e, f] ]
, [ [g, h], [i, j], [k, l] ] ] :: Fq12
```

### Binary fields

The following type declaration creates a binary field modulo a given irreducible binary polynomial.
```haskell
type F2m = Binary 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425
```
Note that the polynomial given *must* be irreducible in F2.

Galois field arithmetic can then be performed in this binary field.
```haskell
f2m :: F2m
f2m = 0x303001d34b856296c16c0d40d3cd7750a93d1d2955fa80aa5f40fc8db7b2abdbde53950f4c0d293cdd711a35b67fb1499ae60038614f1394abfa3b4c850d927e1e7769c8eec2d19

f2m' :: F2m
f2m' = 0x37bf27342da639b6dccfffeb73d69d78c6c27a6009cbbca1980f8533921e8a684423e43bab08a576291af8f461bb2a8b3531d2f0485c19b16e2f1516e23dd3c1a4827af1b8ac15b

arithmeticF2m :: (F2m, F2m, F2m, F2m)
arithmeticF2m = (f2m + f2m', f2m - f2m', f2m * f2m', f2m / f2m')
```

## Disclaimer

This is experimental code meant for research-grade projects only. Please do not
use this code in production until it has matured significantly.

## License

```
Copyright (c) 2019 Adjoint Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
OR OTHER DEALINGS IN THE SOFTWARE.
```
