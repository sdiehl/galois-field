module Benchmarks where

import Protolude

import Criterion.Main

import ExtensionField
import PrimeField

type Fq = PrimeField 21888242871839275222246405745257275088696311157297823662689037894645226208583

fq :: Fq
fq = 5216004179354450092383934373463611881445186046129513844852096383579774061693

fq' :: Fq
fq' = 10757805228921058098980668000791497318123219899766237205512608761387909753942

data Pu
instance IrreducibleMonic Fq Pu where split _ = x^2 + 1
type Fq2 = ExtensionField Fq Pu

fq2 :: Fq2
fq2 = fromList
  [ 19908898611787582971615951530393785823319364696376311494770162270472288380562
  , 2444690988583914246674870181013910409542697083717824402984851238236041783759
  ]

fq2' :: Fq2
fq2' = fromList
  [ 176307305890807650390915550856467756101144733976249050387177647283239486934
  , 9913547941088878400547309488585076816688958962210000330808066250849942240036
  ]

data Pv
instance IrreducibleMonic Fq2 Pv where split _ = x^3 - (9 + t x)
type Fq6 = ExtensionField Fq2 Pv

fq6 :: Fq6
fq6 = fromList
  [ fromList
    [ 8727269669017421992537561450387212506711577304101544328736696625792447584819
    , 14548604791762199086915107662335514800873255588931510951007415299299859294564
    ]
  , fromList
    [ 12226353852518517213098257637254082040554292743096797524265221809863992104040
    , 12690801089710533803594523982915673248220237967492611523932652691226365708512
    ]
  , fromList
    [ 18336930404004840796680535059992401039831316705513753839479258873269709495858
    , 21634580953983557175729336703450663797341055784728343534694506874757389871868
    ]
  ]

fq6' :: Fq6
fq6' = fromList
  [ fromList
    [ 21427158918811764040959407626476119248515601360702754918240300689672054041331
    , 12750457256357562507331331307761996193149796736574153338180573114576232473092
    ]
  , fromList
    [ 19307896751125425658868292427117755307914453765471505616446813557567103424424
    , 11511704315039881938763578963465960361806962511008317843374696569679546862720
    ]
  , fromList
    [ 16856354813335682789816416666746807604324955216244680818919639213184967817815
    , 10563739714379631354612735346769824530666877338817980746884577737330686430079
    ]
  ]

data Pw
instance IrreducibleMonic Fq6 Pw where split _ = x^2 - t x
type Fq12 = ExtensionField Fq6 Pw

fq12 :: Fq12
fq12 = fromList
  [ fromList
    [ fromList
      [ 4025484419428246835913352650763180341703148406593523188761836807196412398582
      , 5087667423921547416057913184603782240965080921431854177822601074227980319916
      ]
    , fromList
      [ 8868355606921194740459469119392835913522089996670570126495590065213716724895
      , 12102922015173003259571598121107256676524158824223867520503152166796819430680
      ]
    , fromList
      [ 92336131326695228787620679552727214674825150151172467042221065081506740785
      , 5482141053831906120660063289735740072497978400199436576451083698548025220729
      ]
    ]
  , fromList
    [ fromList
      [ 7642691434343136168639899684817459509291669149586986497725240920715691142493
      , 1211355239100959901694672926661748059183573115580181831221700974591509515378
      ]
    , fromList
      [ 20725578899076721876257429467489710434807801418821512117896292558010284413176
      , 17642016461759614884877567642064231230128683506116557502360384546280794322728
      ]
    , fromList
      [ 17449282511578147452934743657918270744212677919657988500433959352763226500950
      , 1205855382909824928004884982625565310515751070464736233368671939944606335817
      ]
    ]
  ]

fq12' :: Fq12
fq12' = fromList
  [ fromList
    [ fromList
      [ 495492586688946756331205475947141303903957329539236899715542920513774223311
      , 9283314577619389303419433707421707208215462819919253486023883680690371740600
      ]
    , fromList
      [ 11142072730721162663710262820927009044232748085260948776285443777221023820448
      , 1275691922864139043351956162286567343365697673070760209966772441869205291758
      ]
    , fromList
      [ 20007029371545157738471875537558122753684185825574273033359718514421878893242
      , 9839139739201376418106411333971304469387172772449235880774992683057627654905
      ]
    ]
  , fromList
    [ fromList
      [ 9503058454919356208294350412959497499007919434690988218543143506584310390240
      , 19236630380322614936323642336645412102299542253751028194541390082750834966816
      ]
    , fromList
      [ 18019769232924676175188431592335242333439728011993142930089933693043738917983
      , 11549213142100201239212924317641009159759841794532519457441596987622070613872
      ]
    , fromList
      [ 9656683724785441232932664175488314398614795173462019188529258009817332577664
      , 20666848762667934776817320505559846916719041700736383328805334359135638079015
      ]
    ]
  ]

benchmarks :: Benchmark
benchmarks = bgroup "GaloisField"
  [ bgroup "PrimeField"
    [ bgroup "Fq"
      [ bench "Addition"
        $ whnf (uncurry (+)) (fq, fq')
      , bench "Subtraction"
        $ whnf (uncurry (-)) (fq, fq')
      , bench "Multiplication"
        $ whnf (uncurry (*)) (fq, fq')
      , bench "Division"
        $ whnf (uncurry (/)) (fq, fq')
      ]
    ]
  , bgroup "ExtensionField"
    [ bgroup "Fq2"
      [ bench "Addition"
        $ whnf (uncurry (+)) (fq2, fq2')
      , bench "Subtraction"
        $ whnf (uncurry (-)) (fq2, fq2')
      , bench "Multiplication"
        $ whnf (uncurry (*)) (fq2, fq2')
      , bench "Division"
        $ whnf (uncurry (/)) (fq2, fq2')
      ]
    , bgroup "Fq6"
      [ bench "Addition"
        $ whnf (uncurry (+)) (fq6, fq6')
      , bench "Subtraction"
        $ whnf (uncurry (-)) (fq6, fq6')
      , bench "Multiplication"
        $ whnf (uncurry (*)) (fq6, fq6')
      , bench "Division"
        $ whnf (uncurry (/)) (fq6, fq6')
      ]
    , bgroup "Fq12"
      [ bench "Addition"
        $ whnf (uncurry (+)) (fq12, fq12')
      , bench "Subtraction"
        $ whnf (uncurry (-)) (fq12, fq12')
      , bench "Multiplication"
        $ whnf (uncurry (*)) (fq12, fq12')
      , bench "Division"
        $ whnf (uncurry (/)) (fq12, fq12')
      ]
    ]
  ]
