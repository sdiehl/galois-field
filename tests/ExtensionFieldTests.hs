module ExtensionFieldTests where

import Protolude

import Test.Tasty
import Test.Tasty.QuickCheck

import ExtensionField
import GaloisField
import GaloisFieldTests
import PolynomialRing
import PolynomialRingTests
import PrimeField
import PrimeFieldTests

instance (Arbitrary k, GaloisField k, IrreducibleMonic k ps)
  => Arbitrary (ExtensionField k ps) where
  arbitrary = fromPoly . PolynomialRing.fromList <$> sized (const poly)
    where
      poly = choose (1, degree (split (witness :: (k, ps))) - 1)
        >>= mapM (const arbitrary) . enumFromTo 1

type F2 = PrimeField 2
data P11; instance IrreducibleMonic F2 P11 where split _ = x^2 + x + 1
test_4 = fieldAxioms (Proxy :: Proxy (ExtensionField F2 P11)) "F4"
data P110; instance IrreducibleMonic F2 P110 where split _ = x^3 + x + 1
test_8 = fieldAxioms (Proxy :: Proxy (ExtensionField F2 P110)) "F8"
data P101; instance IrreducibleMonic F2 P101 where split _ = x^3 + x^2 + 1
test_8' = fieldAxioms (Proxy :: Proxy (ExtensionField F2 P110)) "F8'"

type F3 = PrimeField 3
data P10; instance IrreducibleMonic F3 P10 where split _ = x^2 + 1
test_9 = fieldAxioms (Proxy :: Proxy (ExtensionField F3 P10)) "F9"
data P21; instance IrreducibleMonic F3 P21 where split _ = x^2 + x - 1
test_9' = fieldAxioms (Proxy :: Proxy (ExtensionField F3 P21)) "F9'"
data P22; instance IrreducibleMonic F3 P22 where split _ = x^2 - x - 1
test_9'' = fieldAxioms (Proxy :: Proxy (ExtensionField F3 P22)) "F9''"

type FA = PrimeField 2147483647
instance IrreducibleMonic FA P10 where split _ = x^2 + 1
test_A = fieldAxioms (Proxy :: Proxy (ExtensionField FA P10)) "FA2"

type FB = PrimeField 2305843009213693951
instance IrreducibleMonic FB P10 where split _ = x^2 + 1
test_B = fieldAxioms (Proxy :: Proxy (ExtensionField FB P10)) "FB2"

type FC = PrimeField 618970019642690137449562111
instance IrreducibleMonic FC P10 where split _ = x^2 + 1
test_C = fieldAxioms (Proxy :: Proxy (ExtensionField FC P10)) "FC2"

type FD = PrimeField 162259276829213363391578010288127
instance IrreducibleMonic FD P10 where split _ = x^2 + 1
test_D = fieldAxioms (Proxy :: Proxy (ExtensionField FD P10)) "FD2"

type FE = PrimeField 170141183460469231731687303715884105727
instance IrreducibleMonic FE P10 where split _ = x^2 + 1
test_E = fieldAxioms (Proxy :: Proxy (ExtensionField FE P10)) "FE2"

type FV = PrimeField 20988936657440586486151264256610222593863921
instance IrreducibleMonic FV P10 where split _ = x^2 + 1
test_V = fieldAxioms (Proxy :: Proxy (ExtensionField FV P10)) "FV2"

type FX = PrimeField 5210644015679228794060694325390955853335898483908056458352183851018372555735221
instance IrreducibleMonic FX P10 where split _ = x^2 + 1
test_X = fieldAxioms (Proxy :: Proxy (ExtensionField FX P10)) "FX2"

type FZ = PrimeField 741640062627530801524787141901937474059940781097519023905821316144415759504705008092818711693940737
instance IrreducibleMonic FZ P10 where split _ = x^2 + 1
test_Z = fieldAxioms (Proxy :: Proxy (ExtensionField FZ P10)) "FZ2"

type Fq = PrimeField 21888242871839275222246405745257275088696311157297823662689037894645226208583
data Pu
instance IrreducibleMonic Fq Pu where split _ = x^2 + 1
type Fq2 = ExtensionField Fq Pu
test_Fq2 = fieldAxioms (Proxy :: Proxy Fq2) "Fq2"
data Pv
instance IrreducibleMonic Fq2 Pv where split _ = x^3 - (9 + t x)
type Fq6 = ExtensionField Fq2 Pv
test_Fq6 = fieldAxioms (Proxy :: Proxy Fq6) "Fq6"
data Pw
instance IrreducibleMonic Fq6 Pw where split _ = x^2 - t x
type Fq12 = ExtensionField Fq6 Pw
test_Fq12 = fieldAxioms (Proxy :: Proxy Fq12) "Fq12"
