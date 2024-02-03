# Galois Field

[![Hackage](https://img.shields.io/hackage/v/galois-field.svg)](https://hackage.haskell.org/package/galois-field)

An efficient implementation of Galois fields used in cryptography research.

## Technical background

A **Galois field** <img src="/tex/e749ad7d18f2855210ea115451c98828.svg?invert_in_darkmode&sanitize=true" align=middle width=51.94591709999999pt height=24.65753399999998pt/>, for prime <img src="/tex/2ec6e630f199f589a2402fdf3e0289d5.svg?invert_in_darkmode&sanitize=true" align=middle width=8.270567249999992pt height=14.15524440000002pt/> and positive <img src="/tex/d5c18a8ca1894fd3a7d25f242cbe8890.svg?invert_in_darkmode&sanitize=true" align=middle width=7.928106449999989pt height=14.15524440000002pt/>, is a *field* (<img src="/tex/e749ad7d18f2855210ea115451c98828.svg?invert_in_darkmode&sanitize=true" align=middle width=51.94591709999999pt height=24.65753399999998pt/>, +, <img src="/tex/bdbf342b57819773421273d508dba586.svg?invert_in_darkmode&sanitize=true" align=middle width=12.785434199999989pt height=19.1781018pt/>, 0, 1) of finite *order*. Explicitly,
- (<img src="/tex/e749ad7d18f2855210ea115451c98828.svg?invert_in_darkmode&sanitize=true" align=middle width=51.94591709999999pt height=24.65753399999998pt/>, +, 0) is an abelian group,
- (<img src="/tex/1bd3c330eb2232e45ee8cc54a23d716b.svg?invert_in_darkmode&sanitize=true" align=middle width=84.82275614999998pt height=24.65753399999998pt/>, <img src="/tex/bdbf342b57819773421273d508dba586.svg?invert_in_darkmode&sanitize=true" align=middle width=12.785434199999989pt height=19.1781018pt/>, 1) is an abelian group,
- <img src="/tex/bdbf342b57819773421273d508dba586.svg?invert_in_darkmode&sanitize=true" align=middle width=12.785434199999989pt height=19.1781018pt/> is distributive over +, and
- <img src="/tex/dc7ba567ce3ac714a62c171a3db2f3a4.svg?invert_in_darkmode&sanitize=true" align=middle width=51.94591709999999pt height=24.65753399999998pt/> is finite.

### Prime fields

Any Galois field has a unique *characteristic* <img src="/tex/2ec6e630f199f589a2402fdf3e0289d5.svg?invert_in_darkmode&sanitize=true" align=middle width=8.270567249999992pt height=14.15524440000002pt/>, the minimum positive <img src="/tex/2ec6e630f199f589a2402fdf3e0289d5.svg?invert_in_darkmode&sanitize=true" align=middle width=8.270567249999992pt height=14.15524440000002pt/> such that <img src="/tex/c89470a5c4f7e9473c0ebeb6148bdf5d.svg?invert_in_darkmode&sanitize=true" align=middle width=157.12847205pt height=24.65753399999998pt/>, and <img src="/tex/2ec6e630f199f589a2402fdf3e0289d5.svg?invert_in_darkmode&sanitize=true" align=middle width=8.270567249999992pt height=14.15524440000002pt/> is prime. The smallest Galois field of characteristic <img src="/tex/2ec6e630f199f589a2402fdf3e0289d5.svg?invert_in_darkmode&sanitize=true" align=middle width=8.270567249999992pt height=14.15524440000002pt/> is a **prime field**, and any Galois field of characteristic <img src="/tex/2ec6e630f199f589a2402fdf3e0289d5.svg?invert_in_darkmode&sanitize=true" align=middle width=8.270567249999992pt height=14.15524440000002pt/> is a *finite-dimensional vector space* over its prime subfield.

For example, <img src="/tex/b566101ab803ed6b496286a86485ba7d.svg?invert_in_darkmode&sanitize=true" align=middle width=44.634829799999984pt height=24.65753399999998pt/> is a Galois field of characteristic 2 that is a two-dimensional vector space over the prime subfield <img src="/tex/01352daed7334b69544be287fa1dc7af.svg?invert_in_darkmode&sanitize=true" align=middle width=104.90876054999998pt height=24.65753399999998pt/>.

### Extension fields

Any Galois field has order a prime power <img src="/tex/f5bb40e395f7fadf8da15b1de7cb359e.svg?invert_in_darkmode&sanitize=true" align=middle width=14.70840524999999pt height=21.839370299999988pt/> for prime <img src="/tex/2ec6e630f199f589a2402fdf3e0289d5.svg?invert_in_darkmode&sanitize=true" align=middle width=8.270567249999992pt height=14.15524440000002pt/> and positive <img src="/tex/d5c18a8ca1894fd3a7d25f242cbe8890.svg?invert_in_darkmode&sanitize=true" align=middle width=7.928106449999989pt height=14.15524440000002pt/>, and there is a Galois field <img src="/tex/e749ad7d18f2855210ea115451c98828.svg?invert_in_darkmode&sanitize=true" align=middle width=51.94591709999999pt height=24.65753399999998pt/> of any prime power order <img src="/tex/f5bb40e395f7fadf8da15b1de7cb359e.svg?invert_in_darkmode&sanitize=true" align=middle width=14.70840524999999pt height=21.839370299999988pt/> that is *unique up to non-unique isomorphism*. Any Galois field <img src="/tex/e749ad7d18f2855210ea115451c98828.svg?invert_in_darkmode&sanitize=true" align=middle width=51.94591709999999pt height=24.65753399999998pt/> can be constructed as an **extension field** over a smaller Galois subfield <img src="/tex/7f17ac2b8db44db36d376e44dbca459b.svg?invert_in_darkmode&sanitize=true" align=middle width=51.96550754999999pt height=24.65753399999998pt/>, through the identification <img src="/tex/676c07f4913e5983d30644120a2190a0.svg?invert_in_darkmode&sanitize=true" align=middle width=208.38632594999999pt height=24.65753399999998pt/> for an *irreducible monic polynomial* <img src="/tex/161805ece9a8142e4ebe9d356fd0f763.svg?invert_in_darkmode&sanitize=true" align=middle width=37.51151249999999pt height=24.65753399999998pt/> of degree <img src="/tex/610a0cab14ccd2bc1d2080214ad87a19.svg?invert_in_darkmode&sanitize=true" align=middle width=64.20263354999999pt height=21.18721440000001pt/> in the *polynomial ring* <img src="/tex/16fd46b712782718e9e17184e9d36aa0.svg?invert_in_darkmode&sanitize=true" align=middle width=76.00662299999999pt height=24.65753399999998pt/>.

For example, <img src="/tex/b566101ab803ed6b496286a86485ba7d.svg?invert_in_darkmode&sanitize=true" align=middle width=44.634829799999984pt height=24.65753399999998pt/> has order <img src="/tex/ec4089d7f3fb410f521723b967e41a69.svg?invert_in_darkmode&sanitize=true" align=middle width=14.771756999999988pt height=26.76175259999998pt/> and can be constructed as an extension field <img src="/tex/eff7dfe6e7384ba3c57607f1376fda7b.svg?invert_in_darkmode&sanitize=true" align=middle width=127.19210129999998pt height=24.65753399999998pt/> where <img src="/tex/14204002b85c3f046fbde37b519e83c8.svg?invert_in_darkmode&sanitize=true" align=middle width=145.02252929999997pt height=26.76175259999998pt/> is an irreducible monic quadratic polynomial in <img src="/tex/b5b165971fad7e6b6c890db109b9d2fd.svg?invert_in_darkmode&sanitize=true" align=middle width=68.67594524999998pt height=24.65753399999998pt/>.

### Binary fields

A Galois field of the form <img src="/tex/f4687471921caacf38fd0e0667005c1f.svg?invert_in_darkmode&sanitize=true" align=middle width=57.12159419999999pt height=24.65753399999998pt/> for big positive <img src="/tex/0e51a2dede42189d77627c4d742822c3.svg?invert_in_darkmode&sanitize=true" align=middle width=14.433101099999991pt height=14.15524440000002pt/> is a sum of <img src="/tex/aedfd2f0682e37eedb201bcd2ca04442.svg?invert_in_darkmode&sanitize=true" align=middle width=23.034689699999987pt height=22.465723500000017pt/> for a non-empty set of <img src="/tex/ddce1c5958e2d066abe368e96fa697db.svg?invert_in_darkmode&sanitize=true" align=middle width=76.35444795pt height=21.18721440000001pt/>. For computational efficiency in cryptography, an element of a **binary field** can be represented by an integer that represents a bit string. It should always be used when the field characteristic is 2.

For example, <img src="/tex/72891ad284e66d0f200979d5f8209d4e.svg?invert_in_darkmode&sanitize=true" align=middle width=170.34202019999998pt height=26.76175259999998pt/> can be represented as the integer 283 that represents the bit string 100011011.

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

<img src="/tex/7ddc69d41c4a5172b739624242de9306.svg?invert_in_darkmode&sanitize=true" align=middle width=548.77807545pt height=26.76175259999998pt/>

where <img src="/tex/cbfb1b2a33b28eab8a3e59464768e810.svg?invert_in_darkmode&sanitize=true" align=middle width=14.908688849999992pt height=22.465723500000017pt/>, <img src="/tex/91aac9730317276af725abd8cef04ca9.svg?invert_in_darkmode&sanitize=true" align=middle width=13.19638649999999pt height=22.465723500000017pt/>, <img src="/tex/5b51bd2e6f329245d425b8002d7cf942.svg?invert_in_darkmode&sanitize=true" align=middle width=12.397274999999992pt height=22.465723500000017pt/> is a tower of indeterminate variables, is constructed by
```haskell
[ [ [a, b], [c, d], [e, f] ]
, [ [g, h], [i, j], [k, l] ] ] :: Fq12
```

### Binary fields

The following type declaration creates a binary field modulo a given irreducible binary polynomial.

```haskell
type F2m = Binary 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425
```

Note that the polynomial given *must* be irreducible in <img src="/tex/d0b03602a2acb8c4b039c03c890f1729.svg?invert_in_darkmode&sanitize=true" align=middle width=16.59823274999999pt height=22.648391699999998pt/>.

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
Copyright (c) 2019-2024 Stephen Diehl.

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
