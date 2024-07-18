::  tests/lib/math/hoon
/+  math,
    *test
|%
::  +rs
++  test-rs-sea
  ;:  weld
  %+  expect-eq
    !>  [%f s=%.y e=-23 a=8.388.608]
    !>  (sea:rs:math .1)
  %+  expect-eq
    !>  [%f s=%.y e=-23 a=9.227.469]
    !>  (sea:rs:math .1.1)
  ==
++  test-rs-bit
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (bit:rs:math [%f s=%.y e=-23 a=8.388.608])
  %+  expect-eq
    !>  .1.1
    !>  (bit:rs:math [%f s=%.y e=-23 a=9.227.469])
  ==
++  test-rs-sun
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (sun:rs:math 1)
  %+  expect-eq
    !>  .1e3
    !>  (sun:rs:math 1.000)
  ==
++  test-rs-san
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (san:rs:math --1)
  %+  expect-eq
    !>  .-1
    !>  (san:rs:math -1)
  ==
++  test-rs-toi
  ;:  weld
  %+  expect-eq
    !>  `--1
    !>  (toi:rs:math .1)
  %+  expect-eq
    !>  `-1
    !>  (toi:rs:math .-1)
  ==
++  test-rs-drg
  ;:  weld
  %+  expect-eq
    !>  [%d s=%.y e=--0 a=1]
    !>  (drg:rs:math .1)
  %+  expect-eq
    !>  [%d s=%.y e=-1 a=11]
    !>  (drg:rs:math .1.1)
  ==
++  test-rs-grd
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (grd:rs:math [%d s=%.y e=--0 a=1])
  %+  expect-eq
    !>  .1.1
    !>  (grd:rs:math [%d s=%.y e=-1 a=11])
  ==
++  test-rs-lth
  ;:  weld
  %+  expect-eq
    !>  %.y
    !>  (lth:rs:math .1 .2)
  %+  expect-eq
    !>  %.n
    !>  (lth:rs:math .2 .1)
  %+  expect-eq
    !>  %.n
    !>  (lth:rs:math .1 .1)
  ==
++  test-rs-lte
  ;:  weld
  %+  expect-eq
    !>  %.y
    !>  (lte:rs:math .1 .2)
  %+  expect-eq
    !>  %.n
    !>  (lte:rs:math .2 .1)
  %+  expect-eq
    !>  %.y
    !>  (lte:rs:math .1 .1)
  ==
++  test-rs-leq
  ;:  weld
  %+  expect-eq
    !>  %.y
    !>  (leq:rs:math .1 .2)
  %+  expect-eq
    !>  %.n
    !>  (leq:rs:math .2 .1)
  %+  expect-eq
    !>  %.y
    !>  (leq:rs:math .1 .1)
  ==
++  test-rs-equ
  ;:  weld
  %+  expect-eq
    !>  %.n
    !>  (equ:rs:math .1 .2)
  %+  expect-eq
    !>  %.n
    !>  (equ:rs:math .2 .1)
  %+  expect-eq
    !>  %.y
    !>  (equ:rs:math .1 .1)
  ==
++  test-rs-gth
  ;:  weld
  %+  expect-eq
    !>  %.n
    !>  (gth:rs:math .1 .2)
  %+  expect-eq
    !>  %.y
    !>  (gth:rs:math .2 .1)
  %+  expect-eq
    !>  %.n
    !>  (gth:rs:math .1 .1)
  ==
++  test-rs-gte
  ;:  weld
  %+  expect-eq
    !>  %.n
    !>  (gte:rs:math .1 .2)
  %+  expect-eq
    !>  %.y
    !>  (gte:rs:math .2 .1)
  %+  expect-eq
    !>  %.y
    !>  (gte:rs:math .1 .1)
  ==
++  test-rs-geq
  ;:  weld
  %+  expect-eq
    !>  %.n
    !>  (geq:rs:math .1 .2)
  %+  expect-eq
    !>  %.y
    !>  (geq:rs:math .2 .1)
  %+  expect-eq
    !>  %.y
    !>  (geq:rs:math .1 .1)
  ==
++  test-rs-neq
  ;:  weld
  %+  expect-eq
    !>  %.y
    !>  (neq:rs:math .1 .2)
  %+  expect-eq
    !>  %.y
    !>  (neq:rs:math .2 .1)
  %+  expect-eq
    !>  %.n
    !>  (neq:rs:math .1 .1)
  ==
++  test-rs-isclose
  ;:  weld
  %+  expect-eq
    !>  %.y
    !>  (isclose:rs:math .1 .1)
  %+  expect-eq
    !>  %.n
    !>  (isclose:rs:math .1 .1.1)
  ==
++  test-rs-allclose
  ;:  weld
  %+  expect-eq
    !>  %.n
    !>  (~(allclose rs:math [%z .1e-8]) .1 ~[.1 .1.000001])
  %+  expect-eq
    !>  %.n
    !>  (~(allclose rs:math [%z .1e-8]) .1 ~[.1 .1.000001])
  %+  expect-eq
    !>  %.y
    !>  (allclose:rs:math .1 ~[.1])
  ==
++  test-rs-isint
  ;:  weld
  %+  expect-eq
    !>  %.y
    !>  (isint:rs:math .1)
  %+  expect-eq
    !>  %.n
    !>  (isint:rs:math .1.1)
  ==
++  test-rs-add
  ;:  weld
  %+  expect-eq
    !>  .3
    !>  (add:rs:math .1 .2)
  ==
++  test-rs-sub
  ;:  weld
  %+  expect-eq
    !>  .-1
    !>  (sub:rs:math .1 .2)
  ==
++  test-rs-mul
  ;:  weld
  %+  expect-eq
    !>  .2
    !>  (mul:rs:math .1 .2)
  ==
++  test-rs-div
  ;:  weld
  %+  expect-eq
    !>  .0.5
    !>  (div:rs:math .1 .2)
  ==
++  test-rs-fma
  ;:  weld
  %+  expect-eq
    !>  .5
    !>  (fma:rs:math .1 .2 .3)
  %+  expect-eq
    !>  .10
    !>  (fma:rs:math .2 .3 .4)
  ==
++  test-rs-sig
  ;:  weld
  %+  expect-eq
    !>  %.y
    !>  (sig:rs:math .1)
  %+  expect-eq
    !>  %.n
    !>  (sig:rs:math .-1)
  ==
++  test-rs-sgn
  ;:  weld
  %+  expect-eq
    !>  %.y
    !>  (sgn:rs:math .1)
  %+  expect-eq
    !>  %.n
    !>  (sgn:rs:math .-1)
  ==
++  test-rs-neg
  ;:  weld
  %+  expect-eq
    !>  .-1
    !>  (neg:rs:math .1)
  %+  expect-eq
    !>  .1
    !>  (neg:rs:math .-1)
  ==
++  test-rs-factorial
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (factorial:rs:math .1)
  %+  expect-eq
    !>  .2
    !>  (factorial:rs:math .2)
  %+  expect-eq
    !>  .6
    !>  (factorial:rs:math .3)
  %+  expect-eq
    !>  .24
    !>  (factorial:rs:math .4)
  %+  expect-eq
    !>  .120
    !>  (factorial:rs:math .5)
  ==
++  test-rs-abs
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (abs:rs:math .1)
  %+  expect-eq
    !>  .1
    !>  (abs:rs:math .-1)
  ==
++  test-rs-exp
  ;:  weld
  %+  expect-eq
    !>  .2.7182808
    !>  (exp:rs:math .1)
  %+  expect-eq
    !>  .7.389052
    !>  (exp:rs:math .2)
  %+  expect-eq
    !>  .7.389053
    !>  (~(exp rs:math [%z .1e-8]) .2)
  %+  expect-eq
    !>  .inf
    !>  (exp:rs:math .inf)
  ==
++  test-rs-sin
  ;:  weld
  %+  expect-eq
    !>  .0.84147096
    !>  (sin:rs:math .1)
  %+  expect-eq
    !>  .0.9092974
    !>  (sin:rs:math .2)
  %+  expect-eq
    !>  .3.1609193e-7
    !>  (sin:rs:math pi:rs:math)
  %+  expect-eq
    !>  .0.90929735
    !>  (~(sin rs:math [%z .1e-8]) .2)
  ==
++  test-rs-cos
  ;:  weld
  %+  expect-eq
    !>  .0.5403022
    !>  (cos:rs:math .1)
  %+  expect-eq
    !>  .-0.41614664
    !>  (cos:rs:math .2)
  %+  expect-eq
    !>  .-0.9999998
    !>  (cos:rs:math pi:rs:math)
  %+  expect-eq
    !>  .-0.41614679
    !>  (~(cos rs:math [%z .1e-8]) .2)
  ==
++  test-rs-tan
  ;:  weld
  %+  expect-eq
    !>  .1.5574079
    !>  (tan:rs:math .1)
  %+  expect-eq
    !>  .-2.1850407
    !>  (tan:rs:math .2)
  %+  expect-eq
    !>  .-3.1609196e-7
    !>  (tan:rs:math pi:rs:math)
  %+  expect-eq
    !>  .-2.1850398
    !>  (~(tan rs:math [%z .1e-8]) .2)
  ==
++  test-rs-pow-n
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (pow-n:rs:math .1 .2)
  %+  expect-eq
    !>  .4
    !>  (pow-n:rs:math .2 .2)
  %+  expect-eq
    !>  .8
    !>  (pow-n:rs:math .2 .3)
  ==
++  test-rs-log
  ;:  weld
  %+  expect-eq
    !>  .0
    !>  (log:rs:math .1)
  %+  expect-eq
    !>  .0.69314677
    !>  (log:rs:math .2)
  %+  expect-eq
    !>  .0.6931469
    !>  (~(log rs:math [%z .1e-8]) .2)
  %+  expect-eq
    !>  .inf
    !>  (log:rs:math .inf)
  %+  expect-eq
    !>  .0.999998
    !>  (log:rs:math e:rs:math)
  %+  expect-eq
    !>  .0.9999994
    !>  (~(log rs:math [%z .1e-8]) e:rs:math)
  ==
++  test-rs-pow
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (pow:rs:math .1 .2)
  %+  expect-eq
    !>  .4
    !>  (pow:rs:math .2 .2)
  %+  expect-eq
    !>  .11.313682
    !>  (pow:rs:math .2 .3.5)
  %+  expect-eq
    !>  .11.313687
    !>  (~(pow rs:math [%z .1e-8]) .2 .3.5)
  ==
++  test-rs-sqrt
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (sqrt:rs:math .1)
  %+  expect-eq
    !>  .1.4142128
    !>  (sqrt:rs:math .2)
  %+  expect-eq
    !>  .1.414213
    !>  (~(sqrt rs:math [%z .1e-8]) .2)
  ==
++  test-rs-sqt
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (sqt:rs:math .1)
  %+  expect-eq
    !>  .1.4142128
    !>  (sqt:rs:math .2)
  %+  expect-eq
    !>  .1.414213
    !>  (~(sqt rs:math [%z .1e-8]) .2)
  ==
++  test-rs-cbrt
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (cbrt:rs:math .1)
  %+  expect-eq
    !>  .1.2599205
    !>  (cbrt:rs:math .2)
  %+  expect-eq
    !>  .1.2599207
    !>  (~(cbrt rs:math [%z .1e-8]) .2)
  ==
++  test-rs-cbt
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (cbt:rs:math .1)
  %+  expect-eq
    !>  .1.2599205
    !>  (cbt:rs:math .2)
  %+  expect-eq
    !>  .1.2599207
    !>  (~(cbt rs:math [%z .1e-8]) .2)
  ==
++  test-rs-arg
  ;:  weld
  %+  expect-eq
    !>  .1
    !>  (arg:rs:math .1)
  %+  expect-eq
    !>  .1
    !>  (arg:rs:math .-1)
  ==
--
