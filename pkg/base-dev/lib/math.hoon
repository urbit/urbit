  ::
::::  Mathematical library
::
::  Pure Hoon implementations are naive formally correct algorithms,
::  awaiting efficient jetting with GNU Scientific Library.
::
/+  twoc
|%
++  rs
  ^|
  |_  $:  r=$?(%n %u %d %z)   :: round nearest, up, down, to zero
          rtol=_.1e-5         :: relative tolerance for precision of operations
      ==
  ::  mathematics constants to single precision
  ::    +tau:  @rs
  ::
  ::  Returns the value 2*pi (OEIS A019692).
  ::    Examples
  ::      > tau
  ::      .6.2831855
  ::  Source
  ++  tau  .6.2831855
  ::    +pi:  @rs
  ::
  ::  Returns the value pi (OEIS A000796).
  ::    Examples
  ::      > pi
  ::      .3.1415927
  ::  Source
  ++  pi  .3.1415927
  ::    +e:  @rs
  ::
  ::  Returns the value e (Euler's constant) (OEIS A001113).
  ::    Examples
  ::      > e
  ::      .2.7182817
  ::  Source
  ++  e  .2.7182817
  ::    +phi:  @rs
  ::
  ::  Returns the value phi (golden ratio) (OEIS A001622).  
  ::    Examples
  ::      > phi
  ::      .1.618034
  ::  Source
  ++  phi  .1.618034
  ::    +sqt2:  @rs
  ::
  ::  Returns the value sqrt(2) (OEIS A002193).
  ::    Examples
  ::      > sqt2
  ::      .1.4142135
  ::  Source
  ++  sqt2  .1.4142135
  ::    +invsqt2:  @rs
  ::
  ::  Returns the value 1/sqrt(2) (OEIS A010503).
  ::    Examples
  ::      > invsqt2
  ::      .70710677
  ::  Source
  ++  invsqt2  .70710677
  ::    +log2:  @rs
  ::
  ::  Returns the value log(2) (OEIS A002162).
  ::    Examples
  ::      > log2
  ::      .6931472
  ::  Source
  ++  log2  .0.6931472
  ::    +invlog2:  @rs
  ::
  ::  Returns the value 1/log(2).
  ::    Examples
  ::      > invlog2
  ::      1.442695
  ::  Source
  ++  invlog2  .1.442695
  ::    +log10:  @rs
  ::
  ::  Returns the value log(10) (OEIS A002392).
  ::    Examples
  ::      > log10
  ::      .2.3025851
  ::  Source
  ++  log10  .2.3025851
  ::    +huge:  @rs
  ::
  ::  Returns the value of the largest representable number.
  ::    Examples
  ::      > huge
  ::      .3.4028235e+38
  ::  Source
  ++  huge  `@rs`0x7f80.0000  ::  3.40282346638528859812e+38
  ::    +tiny:  @rs
  ::
  ::  Returns the value of the smallest representable normal number.
  ::    Examples
  ::      > tiny
  ::      .1.1754944e-38
  ::  Source
  ++  tiny  `@rs`0x1          ::  1.40129846432481707092e-45
  ::
  ::  Operations
  ::
  ::    +sea:  @rs -> fn
  ::
  ::  Returns the +$fn representation of a floating-point atom.
  ::    Examples
  ::      > (sea .1)
  ::      [%f s=%.y e=-23 a=8.388.608]
  ::      > (sea .1.1)
  ::      [%f s=%.y e=-23 a=9.227.469]
  ::  Source
  ++  sea  sea:^rs
  ::    +bit:  fn -> @rs
  ::
  ::  Returns the floating-point atom of a +$fn representation.
  ::    Examples
  ::      > (bit [%f s=%.y e=-23 a=8.388.608])
  ::      .1
  ::      > (bit [%f s=%.y e=-23 a=9.227.469])
  ::      .1.1
  ::  Source
  ++  bit  bit:^rs
  ::    +sun:  @ud -> @rs
  ::
  ::  Returns the floating-point atom of an unsigned integer atom.
  ::    Examples
  ::      > (sun 1)
  ::      .1
  ::      > (sun 1.000)
  ::      .1e3
  ::  Source
  ++  sun  sun:^rs
  ::    +san:  @sd -> @rs
  ::
  ::  Returns the floating-point atom of a signed integer atom.
  ::    Examples
  ::      > (san --1)
  ::      .1
  ::      > (san -1)
  ::      .-1
  ::  Source
  ++  san  san:^rs
  ::++  exp  exp:^rs  :: no pass-through because of exp function
  ::    +toi:  @rs -> @sd
  ::
  ::  Returns the unitized signed integer atom of a rounded floating-point atom.
  ::    Examples
  ::      > (toi .1)
  ::      [~ --1]
  ::      > (toi .1.1)
  ::      [~ --1]
  ::  Source
  ++  toi  toi:^rs
  ::    +drg:  @rs -> dn
  ::
  ::  Returns the decimal form of a floating-point atom using the Dragon4
  ::  algorithm.
  ::    Examples
  ::      > (drg .1)
  ::      [%d s=%.y e=--0 a=1]
  ::      > (drg .1.1)
  ::      [%d s=%.y e=-1 a=11]
  ::  Source
  ++  drg  drg:^rs
  ::    +grd:  dn -> @rs
  ::
  ::  Returns the floating-point atom of a decimal form.
  ::    Examples
  ::      > (grd [%d s=%.y e=--0 a=1])
  ::      .1
  ::      > (grd [%d s=%.y e=-1 a=11])
  ::      .1.1
  ::  Source
  ++  grd  grd:^rs
  ::
  ::  Comparison
  ::
  ::    +lth:  [@rs @rs] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, less than.
  ::    Examples
  ::      > (lth .1 .2)
  ::      %.y
  ::      > (lth .2 .1)
  ::      %.n
  ::      > (lth .1 .1)
  ::      %.n
  ::  Source
  ++  lth  lth:^rs
  ::    +lte:  [@rs @rs] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, less than or equal to.
  ::    Examples
  ::      > (lte .1 .2)
  ::      %.y
  ::      > (lte .2 .1)
  ::      %.n
  ::      > (lte .1 .1)
  ::      %.y
  ::  Source
  ++  lte  lte:^rs
  ::    +leq:  [@rs @rs] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, less than or equal to.
  ::  Alias for +lte.
  ::    Examples
  ::      > (leq .1 .2)
  ::      %.y
  ::      > (leq .2 .1)
  ::      %.n
  ::      > (leq .1 .1)
  ::      %.y
  ::  Source
  ++  leq  lte:^rs
  ::    +equ:  [@rs @rs] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, equal to.
  ::    Examples
  ::      > (equ .1 .2)
  ::      %.n
  ::      > (equ .2 .1)
  ::      %.n
  ::      > (equ .1 .1)
  ::      %.y
  ::  Source
  ++  equ  equ:^rs
  ::    +gth:  [@rs @rs] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, greater than.
  ::    Examples
  ::      > (gth .1 .2)
  ::      %.n
  ::      > (gth .2 .1)
  ::      %.y
  ::      > (gth .1 .1)
  ::      %.n
  ::  Source
  ++  gth  gth:^rs
  ::    +gte:  [@rs @rs] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, greater than or equal to.
  ::    Examples
  ::      > (gte .1 .2)
  ::      %.n
  ::      > (gte .2 .1)
  ::      %.y
  ::      > (gte .1 .1)
  ::      %.y
  ::  Source
  ++  gte  gte:^rs
  ::    +geq:  [@rs @rs] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, greater than or equal to.
  ::  Alias for +gte.
  ::    Examples
  ::      > (geq .1 .2)
  ::      %.n
  ::      > (geq .2 .1)
  ::      %.y
  ::      > (geq .1 .1)
  ::      %.y
  ::  Source
  ++  geq  gte:^rs
  ::    +neq:  [@rs @rs] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, not equal to.
  ::    Examples
  ::      > (neq .1 .2)
  ::      %.y
  ::      > (neq .2 .1)
  ::      %.y
  ::      > (neq .1 .1)
  ::      %.n
  ::  Source
  ++  neq  |=([a=@rs b=@rs] ^-(? !(equ:^rs a b)))
  ::    +isclose:  [@rs @rs] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, within a relative
  ::  tolerance (provided by the +rs door).
  ::    Examples
  ::      > (isclose .1 .2)
  ::      %.n
  ::      > (isclose .1 .1000001)
  ::      %.y
  ::      > (~(isclose rs [%z .1e-8]) .1 .1000001)
  ::      %.n
  ::  Source
  ++  isclose
    |=  [p=@rs r=@rs]
    (lth (abs (sub p r)) rtol)
  ::    +allclose:  [@rs (list @rs)] -> ?
  ::
  ::  Returns the comparison of a floating-point atom to a list of floating-
  ::  point atoms, within a relative tolerance (provided by the +rs door).
  ::    Examples
  ::      > (allclose .1 ~[.1 .2])
  ::      %.n
  ::      > (allclose .1 ~[.1 .1.000001])
  ::      %.y
  ::      > (~(allclose rs [%z .1e-8]) .1 ~[.1 .1000001])
  ::      %.n
  ::  Source
  ++  allclose
    |=  [p=@rs q=(list @rs)]
    =/  i  0
    =/  n  (lent q)
    |-  ^-  ?
    ?:  =(n i)
      %.y
    ?.  (isclose p (snag i q))
      %.n
    $(i +(i))
  ::    +isint:  @rs -> ?
  ::
  ::  Returns whether a floating-point value is an integer (no fractional part).
  ::    Examples
  ::      > (isint .1)
  ::      %.y
  ::      > (isint .1.1)
  ::      %.n
  ::  Source
  ++  isint
    |=  x=@rs  ^-  ?
    (equ x (san (need (toi x))))
  ::
  ::  Algebraic
  ::
  ::    +add:  [@rs @rs] -> @rs
  ::
  ::  Returns the sum of two floating-point atoms.
  ::    Examples
  ::      > (add .1 .2)
  ::      .3
  ::  Source
  ++  add  add:^rs
  ::    +sub:  [@rs @rs] -> @rs
  ::
  ::  Returns the difference of two floating-point atoms.
  ::    Examples
  ::      > (sub .1 .2)
  ::      .-1
  ::  Source
  ++  sub  sub:^rs
  ::    +mul:  [@rs @rs] -> @rs
  ::
  ::  Returns the product of two floating-point atoms.
  ::    Examples
  ::      > (mul .1 .2)
  ::      .2
  ::      > (mul .2 .3)
  ::      .6
  ::  Source
  ++  mul  mul:^rs
  ::    +div:  [@rs @rs] -> @rs
  ::
  ::  Returns the quotient of two floating-point atoms.
  ::    Examples
  ::      > (div .1 .2)
  ::      .0.5
  ::  Source
  ++  div  div:^rs
  ::  +mod:  [@rs @rs] -> @rs
  ::
  ::  Returns the modulus of two floating-point atoms.
  ::    Examples
  ::      > (mod .1 .2)
  ::      .1
  ::      > (mod .100 .8)
  ::      .4
  ::  Source
  ++  mod
    |=  [a=@rs b=@rs]  ^-  @rs
    ?:  (lth a .0)
      (sub b (mod (neg a) b))
    (sub a (mul b (san (need (toi (div a b))))))  ::  a - b * floor(a / b)
  ::    +fma:  [@rs @rs @rs] -> @rs
  ::
  ::  Returns the fused multiply-add of three floating-point atoms.
  ::    Examples
  ::      > (fma .1 .2 .3)
  ::      .5
  ::      > (fma .2 .3 .4)
  ::      .10
  ::  Source
  ++  fma  fma:^rs
  ::    +sig:  @rs -> ?
  ::
  ::  Returns the sign of a floating-point atom.
  ::    Examples
  ::      > (sig .1)
  ::      %.y
  ::      > (sig .-1)
  ::      %.n
  ::  Source
  ++  sig  |=(x=@rs =(0 (rsh [0 31] x)))
  ::    +sgn:  @rs -> ?
  ::
  ::  Returns the sign of a floating-point atom.
  ::  Alias for +sig.
  ::    Examples
  ::      > (sgn .1)
  ::      %.y
  ::      > (sgn .-1)
  ::      %.n
  ::  Source
  ++  sgn  sig
  ::    +neg:  @rs -> @rs
  ::
  ::  Returns the negation of a floating-point atom.
  ::    Examples
  ::      > (neg .1)
  ::      .-1
  ::      > (neg .-1)
  ::      .1
  ::  Source
  ++  neg  |=(x=@rs (sub .0 x))
  ::    +factorial:  @rs -> @rs
  ::
  ::  Returns the factorial of a floating-point atom.  Assumes integer input.
  ::    Examples
  ::      > (factorial .1)
  ::      .1
  ::      > (factorial .2)
  ::      .2
  ::      > (factorial .3)
  ::      .6
  ::  Source
  ++  factorial
    |=  x=@rs  ^-  @rs
    ?>  (gte x .0)
    =/  t=@rs  .1
    ?:  (isclose x .0)
      t
    |-  ^-  @rs
    ?:  (isclose x .1)
      t
    $(x (sub x .1), t (mul t x))
  ::    +abs:  @rs -> @rs
  ::
  ::  Returns the absolute value of a floating-point atom.
  ::    Examples
  ::      > (abs .1)
  ::      .1
  ::      > (abs .-1)
  ::      .1
  ::  Source
  ++  abs
    |=  x=@rs  ^-  @rs
    ?:((sgn x) x (neg x))
  ::    +exp:  @rs -> @rs
  ::
  ::  Returns the exponential of a floating-point atom.
  ::    Examples
  ::      > (exp .1)
  ::      .2.7182808
  ::      > (exp .2)
  ::      .7.389052
  ::      > (~(exp rs [%z .1e-8]) .2)
  ::      .7.389053
  ::      > (exp .inf)
  ::      .inf
  ::  Source
  ++  exp
    |=  x=@rs  ^-  @rs
    ::  filter out non-finite arguments
    ?:  =(x 0x0)  .1
    ::    check infinities
    ?:  =(x 0x7f80.0000)  `@rs`0x7f80.0000  :: exp(+inf) -> inf
    ?:  =(x 0xff80.0000)  .0.0              :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gte (dis 0x7fc0.0000 x) 0)  `@rs`0x7fc0.0000  :: exp(NaN) -> NaN
    ::    check overflow to infinity
    =/  o-threshold  `@rs`0x42b0.c0a8  ::  88.72283905206835, value above which exp(x) overflows
    ?:  (gth x o-threshold)  (mul huge huge)
    ::    check underflow to zero
    =/  u-threshold  `@rs`0xc2b0.c0a8  ::  -88.72283905206835, value below which exp(x) underflows
    ?:  (lth x u-threshold)  (mul tiny tiny)
    ::  otherwise, use Taylor series
    =/  p   .1
    =/  po  .-1
    =/  i   .1
    |-  ^-  @rs
    ?:  (lth (abs (sub po p)) rtol)
      p
    $(i (add i .1), p (add p (div (pow-n x i) (factorial i))), po p)
  ::    +sin:  @rs -> @rs
  ::
  ::  Returns the sine of a floating-point atom.
  ::    Examples
  ::    > (sin .1)
  ::    .0.84147096
  ::    > (sin .2)
  ::    .0.9092974
  ::    > (sin pi)
  ::    .3.1609193e-7
  ::  Source
  ++  sin
    |=  x=@rs  ^-  @rs
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(x 0x7f80.0000)  `@rs`0x7fc0.0000  :: sin(+inf) -> NaN
    ?:  =(x 0xff80.0000)  `@rs`0x7fc0.0000  :: sin(-inf) -> NaN
    ::    check NaN
    ?.  (^gte (dis 0x7fc0.0000 x) 0)  `@rs`0x7fc0.0000  :: sin(NaN) -> NaN
    ::  map into domain
    =.  x  (mod x tau)
    ::  otherwise, use Taylor series
    =/  p   x
    =/  po  .-2
    =/  i   1
    =/  term  x
    |-  ^-  @rs
    ?.  (gth (abs term) rtol)
      p
    =/  i2  (add (sun i) (sun i))
    =.  term  (mul (neg term) (div (mul x x) (mul i2 (add i2 .1))))
    $(i +(i), p (add p term), po p)
  ::    +cos:  @rs -> @rs
  ::
  ::  Returns the cosine of a floating-point atom.
  ::    Examples
  ::      > (cos .1)
  ::      .0.5403022
  ::     > (cos .2)
  ::      .-0.41614664
  ::     > (cos pi)
  ::      .-0.9999998
  ::  Source
  ++  cos
    |=  x=@rs  ^-  @rs
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(x 0x7f80.0000)  `@rs`0x7fc0.0000  :: sin(+inf) -> NaN
    ?:  =(x 0xff80.0000)  `@rs`0x7fc0.0000  :: sin(-inf) -> NaN
    ::    check NaN
    ?.  (^gte (dis 0x7fc0.0000 x) 0)  `@rs`0x7fc0.0000  :: sin(NaN) -> NaN
    ::  map into domain
    =.  x  (mod x tau)
    ::  otherwise, use Taylor series
    =/  p   .1
    =/  po  .-2
    =/  i   1
    =/  term  .1
    |-  ^-  @rs
    ?.  (gth (abs term) rtol)
      p
    =/  i2  (add (sun i) (sun i))
    =.  term  (mul (neg term) (div (mul x x) (mul i2 (sub i2 .1))))
    $(i +(i), p (add p term), po p)
  ::    +tan:  @rs -> @rs
  ::
  ::  Returns the tangent of a floating-point atom.
  ::    Examples
  ::      > (tan .1)
  ::      .1.5574079
  ::      > (tan .2)
  ::      .-2.1850407
  ::      > (tan pi)
  ::      .-7.0094916e-7
  ::  Source
  ++  tan
    |=  x=@rs  ^-  @rs
    (div (sin x) (cos x))
  ::    +pow-n:  [@rs @rs] -> @rs
  ::
  ::  Returns the power of a floating-point atom to an integer exponent.
  ::    Examples
  ::      > (pow-n .1 .2)
  ::      .1
  ::      > (pow-n .2 .2)
  ::      .4
  ::      > (pow-n .2 .3)
  ::      .8
  ::  Source
  ++  pow-n
    |=  [x=@rs n=@rs]  ^-  @rs
    ?:  =(n .0)  .1
    ?>  &((gth n .0) (isint n))
    =/  p  x
    |-  ^-  @rs
    ?:  (lth n .2)
      p
    $(n (sub n .1), p (mul p x))
  ::    +log:  @rs -> @rs
  ::
  ::  Returns the natural logarithm of a floating-point atom.
  ::    Examples
  ::      > (log .1)
  ::      .0
  ::      > (log .2)
  ::      .0.69314677
  ::      > (~(log rs [%z .1e-8]) .2)
  ::      .0.6931469
  ::      > (log .inf)
  ::      .inf
  ::      > (log:rs:math e:rs:math)
  ::      .0.999998
  ::      > (~(log rs:math [%z .1e-8]) e:rs:math)
  ::      .0.9999994
  ::  Source
  ++  log
    |=  z=@rs  ^-  @rs
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(z 0x7f80.0000)  `@rs`0x7f80.0000  :: log(+inf) -> inf
    ?:  =(z 0xff80.0000)  `@rs`0x7fc0.0000  :: log(-inf) -> NaN
    ::    check NaN
    ?.  (^gte (dis 0x7fc0.0000 z) 0)  `@rs`0x7fc0.0000  :: exp(NaN) -> NaN
    ::  otherwise, use Taylor series
    =/  p   .0
    =/  po  .-1
    =/  i   .0
    |-  ^-  @rs
    ?:  (lth (abs (sub po p)) rtol)
      (mul (div (mul .2 (sub z .1)) (add z .1)) p)
    =/  term1  (div .1 (add .1 (mul .2 i)))
    =/  term2  (mul (sub z .1) (sub z .1))
    =/  term3  (mul (add z .1) (add z .1))
    =/  term  (mul term1 (pow-n (div term2 term3) i))
    $(i (add i .1), p (add p term), po p)
  ::    +pow:  [@rs @rs] -> @rs
  ::
  ::  Returns the power of a floating-point atom to a floating-point exponent.
  ::    Examples
  ::      > (pow .1 .2)
  ::      .1
  ::      > (pow .2 .2)
  ::      .4
  ::      > (pow .2 .3.5)
  ::      .11.313682
  ::      > (~(pow rs:math [%z .1e-8]) .2 .3.5)
  ::      .11.313687
  ::  Source
  ++  pow
    |=  [x=@rs n=@rs]  ^-  @rs
    ::  fall through on integers (faster)
    ?:  =(n (san (need (toi n))))  (pow-n x (san (need (toi n))))
    (exp (mul n (log x)))
  ::    +sqrt:  @rs -> @rs
  ::
  ::  Returns the square root of a floating-point atom.
  ::  Alias for +sqt.
  ::    Examples
  ::      > (sqrt .1)
  ::      .1
  ::      > (sqrt .2)
  ::      .1.4142128
  ::      > (~(sqrt rs [%z .1e-8]) .2)
  ::      .1.414213
  ::  Source
  ++  sqrt  sqt
  ::    +sqt:  @rs -> @rs
  ::
  ::  Returns the square root of a floating-point atom.
  ::    Examples
  ::      > (sqt .1)
  ::      .1
  ::      > (sqt .2)
  ::      .1.4142128
  ::      > (~(sqt rs [%z .1e-8]) .2)
  ::      .1.414213
  ::  Source
  ++  sqt
    |=  x=@rs  ^-  @rs
    ?>  (sgn x)
    (pow x .0.5)
  ::    +cbrt:  @rs -> @rs
  ::
  ::  Returns the cube root of a floating-point atom.
  ::  Alias for +cbt.
  ::    Examples
  ::      > (cbrt .1)
  ::      .1
  ::      > (cbrt .2)
  ::      .1.2599205
  ::      > (~(cbrt rs [%z .1e-8]) .2)
  ::      .1.2599207
  ::  Source
  ++  cbrt  cbt
  ::    +cbt:  @rs -> @rs
  ::
  ::  Returns the cube root of a floating-point atom.
  ::    Examples
  ::      > (cbt .1)
  ::      .1
  ::      > (cbt .2)
  ::      .1.2599205
  ::      > (~(cbt rs [%z .1e-8]) .2)
  ::      .1.2599207
  ::  Source
  ++  cbt
    |=  x=@rs  ^-  @rs
    ?>  (sgn x)
    (pow x .0.33333333)
  ::    +arg:  @rs -> @rs
  ::
  ::  Returns the argument of a floating-point atom (real argument = absolute
  ::  value).
  ::    Examples
  ::      > (arg .1)
  ::      .1
  ::      > (arg .-1)
  ::      .1
  ::  Source
  ++  arg  abs
  --
::  double precision
++  rd
  ^|
  |_  $:  r=$?(%n %u %d %z)   :: round nearest, up, down, to zero
          rtol=_.~1e-10       :: relative tolerance for precision of operations
      ==
  ::  mathematics constants to single precision
  ::    +tau:  @rd
  ::
  ::  Returns the value 2*pi (OEIS A019692).
  ::    Examples
  ::      > tau
  ::      .~6.283185307179586
  ::  Source
  ++  tau  .~6.283185307179586
  ::    +pi:  @rd
  ::
  ::  Returns the value pi (OEIS A000796).
  ::    Examples
  ::      > pi
  ::      .~3.141592653589793
  ::  Source
  ++  pi  .~3.141592653589793
  ::    +e:  @rd
  ::
  ::  Returns the value e (Euler's constant) (OEIS A001113).
  ::    Examples
  ::      > e
  ::      .~2.718281828459045
  ::  Source
  ++  e  .~2.718281828459045
  ::    +phi:  @rd
  ::
  ::  Returns the value phi (golden ratio) (OEIS A001622).
  ::    Examples
  ::      > phi
  ::      .~1.618033988749895
  ::  Source
  ++  phi  .~1.618033988749895
  ::    +sqt2:  @rd
  ::
  ::  Returns the value sqrt(2) (OEIS A002193).
  ::    Examples
  ::      > sqt2
  ::      .~1.414213562373095
  ::  Source
  ++  sqt2  .~1.4142135623730951
  ::    +invsqt2:  @rd
  ::
  ::  Returns the value 1/sqrt(2) (OEIS A010503).
  ::    Examples
  ::      > invsqt2
  ::      .~0.7071067811865476
  ::  Source
  ++  invsqt2  .~0.7071067811865476
  ::    +log2:  @rd
  ::
  ::  Returns the value log(2) (OEIS A002162).
  ::    Examples
  ::      > log2
  ::      .~0.6931471805599453
  ::  Source
  ++  log2  .~0.6931471805599453
  ::    +invlog2:  @rd
  ::
  ::  Returns the value 1/log(2).
  ::    Examples
  ::      > invlog2
  ::      .~1.4426950408889634
  ::  Source
  ++  invlog2  .~1.4426950408889634
  ::    +log10:  @rd
  ::
  ::  Returns the value log(10) (OEIS A002392).
  ::    Examples
  ::      > log10
  ::      .~2.302585092994046
  ::  Source
  ++  log10  .~2.302585092994046
  ::
  ::    +huge:  @rd
  ::
  ::  Returns the value of the largest representable number.
  ::    Examples
  ::      > huge
  ::      .~1.7976931348623157e+308
  ::  Source
  ++  huge  `@rd`0x7fef.ffff.ffff.ffff  ::  1.79769313486231570815e+308
  ::    +tiny:  @rd
  ::
  ::  Returns the value of the smallest representable normal number.
  ::    Examples
  ::      > tiny
  ::      .~2.2250738585072014e-308
  ::  Source
  ++  tiny  `@rd`0x10.0000.0000.0000    ::  2.22507385850720138309e-308
  ::
  ::  Operations
  ::
  ::    +sea:  @rd -> fn
  ::
  ::  Returns the +$fn representation of a floating-point atom.
  ::    Examples
  ::      > (sea .~1)
  ::      [%f s=%.y e=-52 a=4.503.599.627.370.496]
  ::      > (sea .~1.1)
  ::      [%f s=%.y e=-52 a=4.953.959.590.107.546]
  ::  Source
  ++  sea  sea:^rd
  ::    +bit:  fn -> @rd
  ::
  ::  Returns the floating-point atom of a +$fn representation.
  ::    Examples
  ::      > (bit [%f s=%.y e=-52 a=4.503.599.627.370.496])
  ::      .~1
  ::      > (bit [%f s=%.y e=-52 a=4.953.959.590.107.546])
  ::      .~1.1
  ::  Source
  ++  bit  bit:^rd
  ::    +sun:  @ud -> @rd
  ::
  ::  Returns the floating-point atom of an unsigned integer atom.
  ::    Examples
  ::      > (sun 1)
  ::      .~1
  ::      > (sun 1.000)
  ::      .~1e3
  ::  Source
  ++  sun  sun:^rd
  ::    +san:  @sd -> @rd
  ::
  ::  Returns the floating-point atom of a signed integer atom.
  ::    Examples
  ::      > (san --1)
  ::      .~1
  ::      > (san -1)
  ::      .~-1
  ::  Source
  ++  san  san:^rd
  ::++  exp  exp:^rd  :: no pass-through because of exp function
  ::    +toi:  @rd -> @sd
  ::
  ::  Returns the unitized signed integer atom of a rounded floating-point atom.
  ::    Examples
  ::      > (toi .~1)
  ::      [~ --1]
  ::      > (toi .~1.1)
  ::      [~ --1]
  ::  Source
  ++  toi  toi:^rd
  ::    +drg:  @rd -> dn
  ::
  ::  Returns the decimal form of a floating-point atom using the Dragon4
  ::  algorithm.
  ::    Examples
  ::      > (drg .~1)
  ::      [%d s=%.y e=--0 a=1]
  ::      > (drg .~1.1)
  ::      [%d s=%.y e=-1 a=11]
  ::  Source
  ++  drg  drg:^rd
  ::    +grd:  dn -> @rd
  ::
  ::  Returns the floating-point atom of a decimal form.
  ::    Examples
  ::      > (grd [%d s=%.y e=--0 a=1])
  ::      .~1
  ::      > (grd [%d s=%.y e=-1 a=11])
  ::      .~1.1
  ::  Source
  ++  grd  grd:^rd
  ::
  ::  Comparison
  ::
  ::    +lth:  [@rd @rd] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, less than.
  ::    Examples
  ::      > (lth .~1 .~2)
  ::      %.y
  ::      > (lth .~2 .~1)
  ::      %.n
  ::      > (lth .~1 .~1)
  ::      %.n
  ::  Source
  ++  lth  lth:^rd
  ::    +lte:  [@rd @rd] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, less than or equal to.
  ::    Examples
  ::      > (lte .~1 .~2)
  ::      %.y
  ::      > (lte .~2 .~1)
  ::      %.n
  ::      > (lte .~1 .~1)
  ::      %.y
  ::  Source
  ++  lte  lte:^rd
  ::    +leq:  [@rd @rd] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, less than or equal to.
  ::  Alias for +lte.
  ::    Examples
  ::      > (leq .~1 .~2)
  ::      %.y
  ::      > (leq .~2 .~1)
  ::      %.n
  ::      > (leq .~1 .~1)
  ::      %.y
  ::  Source
  ++  leq  lte:^rd
  ::    +equ:  [@rd @rd] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, equal to.
  ::    Examples
  ::      > (equ .~1 .~2)
  ::      %.n
  ::      > (equ .~2 .~1)
  ::      %.n
  ::      > (equ .~1 .~1)
  ::      %.y
  ::  Source
  ++  equ  equ:^rd
  ::    +gth:  [@rd @rd] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, greater than.
  ::    Examples
  ::      > (gth .~1 .~2)
  ::      %.n
  ::      > (gth .~2 .~1)
  ::      %.y
  ::      > (gth .~1 .~1)
  ::      %.n
  ::  Source
  ++  gth  gth:^rd
  ::    +gte:  [@rd @rd] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, greater than or equal to.
  ::    Examples
  ::      > (gte .~1 .~2)
  ::      %.n
  ::      > (gte .~2 .~1)
  ::      %.y
  ::      > (gte .~1 .~1)
  ::      %.y
  ::  Source
  ++  gte  gte:^rd
  ::    +geq:  [@rd @rd] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, greater than or equal to.
  ::  Alias for +gte.
  ::    Examples
  ::      > (geq .~1 .~2)
  ::      %.n
  ::      > (geq .~2 .~1)
  ::      %.y
  ::      > (geq .~1 .~1)
  ::      %.y
  ::  Source
  ++  geq  gte:^rd
  ::    +neq:  [@rd @rd] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, not equal to.
  ::    Examples
  ::      > (neq .~1 .~2)
  ::      %.y
  ::      > (neq .~2 .~1)
  ::      %.y
  ::      > (neq .~1 .~1)
  ::      %.n
  ::  Source
  ++  neq  |=([a=@rd b=@rd] ^-(? !(equ:^rd a b)))
  ::    +isclose:  [@rd @rd] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, within a relative
  ::  tolerance (provided by the +rd door).
  ::    Examples
  ::      > (isclose .~1 .~2)
  ::      %.n
  ::      > (isclose .~1 .~1.0000001)
  ::      %.n
  ::      > (~(isclose rd [%z .~1e-3]) .~1 .~1.0000001)
  ::      %.y
  ::  Source
  ++  isclose
    |=  [p=@rd r=@rd]
    (lth (abs (sub p r)) rtol)
  ::    +allclose:  [@rd (list @rd)] -> ?
  ::
  ::  Returns the comparison of a floating-point atom to a list of floating-
  ::  point atoms, within a relative tolerance (provided by the +rd door).
  ::    Examples
  ::      > (allclose .~1 ~[.~1 .~2])
  ::      %.n
  ::      > (allclose .~1 ~[.~1 .~1.0000001])
  ::      %.n
  ::      > (~(allclose rd [%z .~1e-3]) .~1 ~[.~1 .~1.0000001])
  ::      %.y
  ::  Source
  ++  allclose
    |=  [p=@rd q=(list @rd)]
    =/  i  0
    =/  n  (lent q)
    |-  ^-  ?
    ?:  =(n i)
      %.y
    ?.  (isclose p (snag i q))
      %.n
    $(i +(i))
  ::    +isint:  @rd -> ?
  ::
  ::  Returns whether a floating-point value is an integer (no fractional part).
  ::    Examples
  ::      > (isint .~1)
  ::      %.y
  ::      > (isint .~1.1)
  ::      %.n
  ::  Source
  ++  isint
    |=  x=@rd  ^-  ?
    (equ x (san (need (toi x))))
  ::
  ::  Algebraic
  ::
  ::    +add:  [@rd @rd] -> @rd
  ::
  ::  Returns the sum of two floating-point atoms.
  ::    Examples
  ::      > (add .~1 .~2)
  ::      .~3
  ::  Source
  ++  add  add:^rd
  ::    +sub:  [@rd @rd] -> @rd
  ::
  ::  Returns the difference of two floating-point atoms.
  ::    Examples
  ::      > (sub .~1 .~2)
  ::      .~-1
  ::  Source
  ++  sub  sub:^rd
  ::    +mul:  [@rd @rd] -> @rd
  ::
  ::  Returns the product of two floating-point atoms.
  ::    Examples
  ::      > (mul .~1 .~2)
  ::      .~2
  ::      > (mul .~2 .~3)
  ::      .~6
  ::  Source
  ++  mul  mul:^rd
  ::    +div:  [@rd @rd] -> @rd
  ::
  ::  Returns the quotient of two floating-point atoms.
  ::    Examples
  ::      > (div .~1 .~2)
  ::      .~0.5
  ::  Source
  ++  div  div:^rd
  ::    +fma:  [@rd @rd @rd] -> @rd
  ::
  ::  Returns the fused multiply-add of three floating-point atoms.
  ::    Examples
  ::      > (fma .~1 .~2 .~3)
  ::      .~5
  ::      > (fma .~2 .~3 .~4)
  ::      .~10
  ::  Source
  ++  fma  fma:^rd
  ::    +sig:  @rd -> ?
  ::
  ::  Returns the sign of a floating-point atom.
  ::    Examples
  ::      > (sig .~1)
  ::      %.y
  ::      > (sig .~-1)
  ::      %.n
  ::  Source
  ++  sig  |=(x=@rd =(0 (rsh [0 63] x)))
  ::    +sgn:  @rd -> ?
  ::
  ::  Returns the sign of a floating-point atom.
  ::  Alias for +sig.
  ::    Examples
  ::      > (sgn .~1)
  ::      %.y
  ::      > (sgn .~-1)
  ::      %.n
  ::  Source
  ++  sgn  sig
  ::    +neg:  @rd -> @rd
  ::
  ::  Returns the negation of a floating-point atom.
  ::    Examples
  ::      > (neg .~1)
  ::      .~-1
  ::      > (neg .~-1)
  ::      .~1
  ::  Source
  ++  neg  |=(x=@rd (sub .~0 x))
  ::    +factorial:  @rd -> @rd
  ::
  ::  Returns the factorial of a floating-point atom.  Assumes integer input.
  ::    Examples
  ::      > (factorial .~1)
  ::      .~1
  ::      > (factorial .~2)
  ::      .~2
  ::      > (factorial .~3)
  ::      .~6
  ::  Source
  ++  factorial
    |=  x=@rd  ^-  @rd
    ?>  (gte x .~0)
    =/  t=@rd  .~1
    ?:  (isclose x .~0)
      t
    |-  ^-  @rd
    ?:  (isclose x .~1)
      t
    $(x (sub x .~1), t (mul t x))
  ::    +abs:  @rd -> @rd
  ::
  ::  Returns the absolute value of a floating-point atom.
  ::    Examples
  ::      > (abs .~1)
  ::      .~1
  ::      > (abs .~-1)
  ::      .~1
  ::  Source
  ++  abs
    |=  x=@rd  ^-  @rd
    ?:((sgn x) x (neg x))
  ::    +exp:  @rd -> @rd
  ::
  ::  Returns the exponential of a floating-point atom.
  ::    Examples
  ::      > (exp .~1)
  ::      .~2.7182818284582266
  ::      > (exp .~2)
  ::      .~7.389056098925858
  ::      > (~(exp rd [%z .~1e-15]) .~2)
  ::      .~7.389056098930642
  ::      > (exp .~inf)
  ::      .inf
  ::  Source
  ++  exp
    |=  x=@rd  ^-  @rd
    ::  filter out non-finite arguments
    ?:  =(x 0x0)  .~1
    ::    check infinities
    ?:  =(x 0x7ff0.0000.0000.0000)  `@rd`0x7ff0.0000.0000.0000  :: exp(+inf) -> inf
    ?:  =(x 0xfff0.0000.0000.0000)  .~0.0                       :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gte (dis 0x7ff8.0000.0000.0000 x) 0)  `@rd`0x7ff8.0000.0000.0000  :: exp(NaN) -> NaN
    ::    check overflow to infinity
    =/  o-threshold  `@rd`0x4086.2e42.fefa.39ef  ::  709.782712893384, value above which exp(x) overflows
    ?:  (gth x o-threshold)  (mul huge huge)
    ::    check underflow to zero
    =/  u-threshold  `@rd`0xc086.2e42.fefa.39ef  ::  -709.782712893384, value below which exp(x) underflows
    ?:  (lth x u-threshold)  (mul tiny tiny)
    ::  otherwise, use Taylor series
    =/  p   .~1
    =/  po  .~-1
    =/  i   .~1
    |-  ^-  @rd
    ?:  (lth (abs (sub po p)) rtol)
      p
    $(i (add i .~1), p (add p (div (pow-n x i) (factorial i))), po p)
  ::    +sin:  @rd -> @rd
  ::
  ::  Returns the sine of a floating-point atom.
  ::    Examples
  ::    > (sin .~1)
  ::    .~0.8414709848078934
  ::    > (sin .~2)
  ::    .~0.9092974268256406
  ::    > (sin pi)
  ::    .~-1.698287706085482e-13
  ::  Source
  ++  sin
    |=  x=@rd  ^-  @rd
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(x 0x7ff0.0000.0000.0000)  `@rd`0x7ff8.0000.0000.0000  :: sin(+inf) -> NaN
    ?:  =(x 0xfff0.0000.0000.0000)  `@rd`0x7ff8.0000.0000.0000  :: sin(-inf) -> NaN
    ::    check NaN
    ?.  (^gte (dis 0x7ff8.0000.0000.0000 x) 0)  `@rd`0x7ff8.0000.0000.0000  :: sin(NaN) -> NaN
    ::  map into domain
    =.  x  (mod x tau)
    ::  otherwise, use Taylor series
    =/  p   x
    =/  po  .~-2
    =/  i   1
    =/  term  x
    |-  ^-  @rd
    ?.  (gth (abs term) rtol)
      p
    =/  i2  (add (sun i) (sun i))
    =.  term  (mul (neg term) (div (mul x x) (mul i2 (add i2 .~1))))
    $(i +(i), p (add p term), po p)
  ::    +cos:  @rd -> @rd
  ::
  ::  Returns the cosine of a floating-point atom.
  ::    Examples
  ::      > (cos .~1)
  ::      .~0.5403023058680917
  ::     > (cos .~2)
  ::      .~-0.41614683654756957
  ::     > (cos pi)
  ::      .~-1.0000000000013558
  ::  Source
  ++  cos
    |=  x=@rd  ^-  @rd
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(x 0x7ff0.0000.0000.0000)  `@rd`0x7ff8.0000.0000.0000  :: cos(+inf) -> NaN
    ?:  =(x 0xfff0.0000.0000.0000)  `@rd`0x7ff8.0000.0000.0000  :: cos(-inf) -> NaN
    ::    check NaN
    ?.  (^gte (dis 0x7ff8.0000.0000.0000 x) 0)  `@rd`0x7ff8.0000.0000.0000  :: exp(NaN) -> NaN
    ::  map into domain
    =.  x  (mod x tau)
    ::  otherwise, use Taylor series
    =/  p   .~1
    =/  po  .~-2
    =/  i   1
    =/  term  .~1
    |-  ^-  @rd
    ?.  (gth (abs term) rtol)
      p
    =/  i2  (add (sun i) (sun i))
    =.  term  (mul (neg term) (div (mul x x) (mul i2 (sub i2 .~1))))
    $(i +(i), p (add p term), po p)
  ::    +tan:  @rd -> @rd
  ::
  ::  Returns the tangent of a floating-point atom.
  ::    Examples
  ::      > (tan .~1)
  ::      .~1.5574077246550349
  ::      > (tan .~2)
  ::      .~-2.185039863259177
  ::      > (tan pi)
  ::      .~-2.6535896228476087e-6
  ::  Source
  ++  tan
    |=  x=@rd  ^-  @rd
    (div (sin x) (cos x))
  ::    +pow-n:  [@rd @rd] -> @rd
  ::
  ::  Returns the power of a floating-point atom to an integer exponent.
  ::    Examples
  ::      > (pow-n .1 .2)
  ::      .1
  ::      > (pow-n .2 .2)
  ::      .4
  ::      > (pow-n .2 .3)
  ::      .8
  ::  Source
  ++  pow-n
    |=  [x=@rd n=@rd]  ^-  @rd
    ?:  =(n .~0)  .~1
    ?>  &((gth n .~0) (isint n))
    =/  p  x
    |-  ^-  @rd
    ?:  (lth n .~2)
      p
    $(n (sub n .~1), p (mul p x))
  ::    +log:  @rd -> @rd
  ::
  ::  Returns the natural logarithm of a floating-point atom.
  ::    Examples
  ::      > (log .~1)
  ::      .~0
  ::      > (log .~2)
  ::      .~0.6931471805589156
  ::      > (~(log rd [%z .~1e-15]) .~2)
  ::      .~0.693147180559944
  ::      > (log .~inf)
  ::      .~inf
  ::  Source
  ++  log
    |=  z=@rd  ^-  @rd
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(z 0x7ff0.0000.0000.0000)  `@rd`0x7ff0.0000.0000.0000  :: exp(+inf) -> inf
    ?:  =(z 0xfff0.0000.0000.0000)  .~0.0                       :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gte (dis 0x7ff8.0000.0000.0000 z) 0)  `@rd`0x7ff8.0000.0000.0000  :: exp(NaN) -> NaN
    ::  otherwise, use Taylor series
    =/  p   .~0
    =/  po  .~-1
    =/  i   .~0
    |-  ^-  @rd
    ?:  (lth (abs (sub po p)) rtol)
      (mul (div (mul .~2 (sub z .~1)) (add z .~1)) p)
    =/  term1  (div .~1 (add .~1 (mul .~2 i)))
    =/  term2  (mul (sub z .~1) (sub z .~1))
    =/  term3  (mul (add z .~1) (add z .~1))
    =/  term  (mul term1 (pow-n (div term2 term3) i))
    $(i (add i .~1), p (add p term), po p)
  ::    +pow:  [@rd @rd] -> @rd
  ::
  ::  Returns the power of a floating-point atom to a floating-point exponent.
  ::    Examples
  ::      > (pow .~1 .~2)
  ::      .~1
  ::      > (pow .~2 .~2)
  ::      .~4
  ::      > (pow .~2 .~3.5)
  ::      .~11.313708498941306
  ::      > (~(pow rd [%z .~1e-15]) .~2 .~3.5)
  ::      .~11.313708498984685
  ::  Source
  ++  pow
    |=  [x=@rd n=@rd]  ^-  @rd
    ::  fall through on integers (faster)
    ?:  =(n (san (need (toi n))))  (pow-n x (san (need (toi n))))
    (exp (mul n (log x)))
  ::    +sqrt:  @rd -> @rd
  ::
  ::  Returns the square root of a floating-point atom.
  ::  Alias for +sqt.
  ::    Examples
  ::      > (sqrt .~1)
  ::      .~1
  ::      > (sqrt .~2)
  ::      .~1.4142135623721421
  ::      > (~(sqrt rd [%z .~1e-15]) .~2)
  ::      .~1.4142135623730923
  ::  Source
  ++  sqrt  sqt
  ::    +sqt:  @rd -> @rd
  ::
  ::  Returns the square root of a floating-point atom.
  ::    Examples
  ::      > (sqt .~1)
  ::      .~1
  ::      > (sqt .~2)
  ::      .~1.4142135623721421
  ::      > (~(sqt rd [%z .~1e-15]) .~2)
  ::      .~1.4142135623730923
  ::  Source
  ++  sqt
    |=  x=@rd  ^-  @rd
    ?>  (sgn x)
    (pow x .~0.5)
  ::    +cbrt:  @rd -> @rd
  ::
  ::  Returns the cube root of a floating-point atom.
  ::  Alias for +cbt.
  ::    Examples
  ::      > (cbrt .~1)
  ::      .~1
  ::      > (cbrt .~2)
  ::      .~1.2599210498943176
  ::      > (~(cbrt rd [%z .~1e-15]) .~2)
  ::      .~1.2599210498948716
  ::  Source
  ++  cbrt  cbt
  ::    +cbt:  @rd -> @rd
  ::
  ::  Returns the cube root of a floating-point atom.
  ::    Examples
  ::      > (cbt .~1)
  ::      .~1
  ::      > (cbt .~2)
  ::      .~1.2599210498943176
  ::      > (~(cbt rd [%z .~1e-15]) .~2)
  ::      .~1.2599210498948716
  ::  Source
  ++  cbt
    |=  x=@rd  ^-  @rd
    ?>  (sgn x)
    (pow x .~0.3333333333333333)
  ::    +arg:  @rd -> @rd
  ::
  ::  Returns the argument of a floating-point atom (real argument = absolute
  ::  value).
  ::    Examples
  ::      > (arg .~1)
  ::      .~1
  ::      > (arg .~-1)
  ::      .~1
  ::  Source
  ++  arg  abs
  --
::  half precision
++  rh
  ^|
  |_  $:  r=$?(%n %u %d %z)   :: round nearest, up, down, to zero
          rtol=_.~~1e-2       :: relative tolerance for precision of operations
      ==
  ::  mathematics constants to half precision
  ::    +tau:  @rh
  ::
  ::  Returns the value 2*pi (OEIS A019692).
  ::    Examples
  ::      > tau
  ::      .~~6.28
  ::  Source
  ++  tau  .~~6.28
  ::    +pi:  @rh
  ::
  ::  Returns the value pi (OEIS A000796).
  ::    Examples
  ::      > pi
  ::      .~~3.14
  ::  Source
  ++  pi  .~~3.14
  ::    +e:  @rh
  ::
  ::  Returns the value e (Euler's constant) (OEIS A001113).
  ::    Examples
  ::      > e
  ::      .~~2.72
  ::  Source
  ++  e  .~~2.719
  ::    +phi:  @rh
  ::
  ::  Returns the value phi (golden ratio) (OEIS A001622).
  ::    Examples
  ::      > phi
  ::      .~~1.62
  ::  Source
  ++  phi  .~~1.618
  ::    +sqt2:  @rh
  ::
  ::  Returns the value sqrt(2) (OEIS A002193).
  ::    Examples
  ::      > sqt2
  ::      .~~1.414
  ::  Source
  ++  sqt2  .~~1.414
  ::    +invsqt2:  @rh
  ::
  ::  Returns the value 1/sqrt(2) (OEIS A010503).
  ::    Examples
  ::      > invsqt2
  ::      .~~0.707
  ::  Source
  ++  invsqt2  .~~0.707
  ::    +log2:  @rh
  ::
  ::  Returns the value log(2) (OEIS A002162).
  ::    Examples
  ::      > log2
  ::      .~~0.693
  ::  Source
  ++  log2  .~~0.6934
  ::    +invlog2:  @rh
  ::
  ::  Returns the value 1/log(2).
  ::    Examples
  ::      > invlog2
  ::      .~~1.443
  ::  Source
  ++  invlog2  .~~1.443
  ::    +log10:  @rh
  ::
  ::  Returns the value log(10) (OEIS A002392).
  ::    Examples
  ::      > log10
  ::      .~~2.303
  ::  Source
  ++  log10  .~~2.303
  ::    +huge:  @rh
  ::
  ::  Returns the value of the largest representable number.
  ::    Examples
  ::      > huge
  ::      .~~6.55e+04
  ::  Source
  ++  huge  `@rh`0x7bff  ::  6.55e+04
  ::    +tiny:  @rh
  ::
  ::  Returns the value of the smallest representable normal number.
  ::    Examples
  ::      > tiny
  ::      .~~6.10e-05
  ::  Source
  ++  tiny  `@rh`0x1     ::  6e-08
  ::
  ::  Operations
  ::
  ::    +sea:  @rh -> fn
  ::
  ::  Returns the +$fn representation of a floating-point atom.
  ::    Examples
  ::      > (sea .~~1)
  ::      [%f s=%.y e=-10 a=1.024]
  ::      > (sea .~~1.1)
  ::      [%f s=%.y e=-10 a=1.126]
  ::  Source
  ++  sea  sea:^rh
  ::    +bit:  fn -> @rh
  ::
  ::  Returns the floating-point atom of a +$fn representation.
  ::    Examples
  ::      > (bit [%f s=%.y e=-10 a=1.024])
  ::      .~~1
  ::      > (bit [%f s=%.y e=-10 a=1.126])
  ::      .~~1.1
  ::  Source
  ++  bit  bit:^rh
  ::    +sun:  @ud -> @rh
  ::
  ::  Returns the floating-point atom of an unsigned integer atom.
  ::    Examples
  ::      > (sun 1)
  ::      .~~1
  ::      > (sun 1.000)
  ::      .~~1e3
  ::  Source
  ++  sun  sun:^rh
  ::    +san:  @sd -> @rh
  ::
  ::  Returns the floating-point atom of a signed integer atom.
  ::    Examples
  ::      > (san --1)
  ::      .~~1
  ::      > (san -1)
  ::      .~-1
  ::  Source
  ++  san  san:^rh
  ::++  exp  exp:^rh  :: no pass-through because of exp function
  ::    +toi:  @rh -> @sd
  ::
  ::  Returns the unitized signed integer atom of a rounded floating-point atom.
  ::    Examples
  ::      > (toi .~~1)
  ::      [~ --1]
  ::      > (toi .~~1.1)
  ::      [~ --1]
  ::  Source
  ++  toi  toi:^rh
  ::    +drg:  @rh -> dn
  ::
  ::  Returns the decimal form of a floating-point atom using the Dragon4
  ::  algorithm.
  ::    Examples
  ::      > (drg .~~1)
  ::      [%d s=%.y e=--0 a=1]
  ::      > (drg .~~1.1)
  ::      [%d s=%.y e=-1 a=11]
  ::  Source
  ++  drg  drg:^rh
  ::    +grd:  dn -> @rh
  ::
  ::  Returns the floating-point atom of a decimal form.
  ::    Examples
  ::      > (grd [%d s=%.y e=--0 a=1])
  ::      .~~1
  ::      > (grd [%d s=%.y e=-1 a=11])
  ::      .~~1.1
  ::  Source
  ++  grd  grd:^rh
  ::
  ::  Comparison
  ::
  ::    +lth:  [@rh @rh] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, less than.
  ::    Examples
  ::      > (lth .~~1 .~~2)
  ::      %.y
  ::      > (lth .~~2 .~~1)
  ::      %.n
  ::      > (lth .~~1 .~~1)
  ::      %.n
  ::  Source
  ++  lth  lth:^rh
  ::    +lte:  [@rh @rh] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, less than or equal to.
  ::    Examples
  ::      > (lte .~~1 .~~2)
  ::      %.y
  ::      > (lte .~~2 .~~1)
  ::      %.n
  ::      > (lte .~~1 .~~1)
  ::      %.y
  ::  Source
  ++  lte  lte:^rh
  ::    +leq:  [@rh @rh] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, less than or equal to.
  ::  Alias for +lte.
  ::    Examples
  ::      > (leq .~~1 .~~2)
  ::      %.y
  ::      > (leq .~~2 .~~1)
  ::      %.n
  ::      > (leq .~~1 .~~1)
  ::      %.y
  ::  Source
  ++  leq  lte:^rh
  ::    +equ:  [@rh @rh] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, equal to.
  ::    Examples
  ::      > (equ .~~1 .~~2)
  ::      %.n
  ::      > (equ .~~2 .~~1)
  ::      %.n
  ::      > (equ .~~1 .~~1)
  ::      %.y
  ::  Source
  ++  equ  equ:^rh
  ::    +gth:  [@rh @rh] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, greater than.
  ::    Examples
  ::      > (gth .~~1 .~~2)
  ::      %.n
  ::      > (gth .~~2 .~~1)
  ::      %.y
  ::      > (gth .~~1 .~~1)
  ::      %.n
  ::  Source
  ++  gth  gth:^rh
  ::    +gte:  [@rh @rh] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, greater than or equal to.
  ::    Examples
  ::      > (gte .~~1 .~~2)
  ::      %.n
  ::      > (gte .~~2 .~~1)
  ::      %.y
  ::      > (gte .~~1 .~~1)
  ::      %.y
  ::  Source
  ++  gte  gte:^rh
  ::    +geq:  [@rh @rh] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, greater than or equal to.
  ::  Alias for +gte.
  ::    Examples
  ::      > (geq .~~1 .~~2)
  ::      %.n
  ::      > (geq .~~2 .~~1)
  ::      %.y
  ::      > (geq .~~1 .~~1)
  ::      %.y
  ::  Source
  ++  geq  gte:^rh
  ::    +neq:  [@rh @rh] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, not equal to.
  ::    Examples
  ::      > (neq .~~1 .~~2)
  ::      %.y
  ::      > (neq .~~2 .~~1)
  ::      %.y
  ::      > (neq .~~1 .~~1)
  ::      %.n
  ::  Source
  ++  neq  |=([a=@rh b=@rh] ^-(? !(equ:^rh a b)))
  ::    +isclose:  [@rh @rh] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, within a relative
  ::  tolerance (provided by the +rh door).
  ::    Examples
  ::      > (isclose .~~1 .~~2)
  ::      %.n
  ::      > (isclose .~~1 .~~1.0000001)
  ::      %.n
  ::      > (~(isclose rh [%z .~~1e-3]) .~~1 .~~1.0001)
  ::      %.y
  ::  Source
  ++  isclose
    |=  [p=@rh r=@rh]
    (lth (abs (sub p r)) rtol)
  ::    +allclose:  [@rh (list @rh)] -> ?
  ::
  ::  Returns the comparison of a floating-point atom to a list of floating-
  ::  point atoms, within a relative tolerance (provided by the +rh door).
  ::    Examples
  ::      > (allclose .~~1 ~[.~~1 .~~2])
  ::      %.n
  ::      > (allclose .~~1 ~[.~~1 .~~1.0000001])
  ::      %.n
  ::      > (~(allclose rh [%z .~~1e-3]) .~~1 ~[.~~1 .~~1.0001])
  ::      %.y
  ::  Source
  ++  allclose
    |=  [p=@rh q=(list @rh)]
    =/  i  0
    =/  n  (lent q)
    |-  ^-  ?
    ?:  =(n i)
      %.y
    ?.  (isclose p (snag i q))
      %.n
    $(i +(i))
  ::    +isint:  @rh -> ?
  ::
  ::  Returns whether a floating-point value is an integer (no fractional part).
  ::    Examples
  ::      > (isint .~~1)
  ::      %.y
  ::      > (isint .~~1.1)
  ::      %.n
  ::  Source
  ++  isint
    |=  x=@rh  ^-  ?
    (equ x (san (need (toi x))))
  ::
  ::  Algebraic
  ::
  ::    +add:  [@rh @rh] -> @rh
  ::
  ::  Returns the sum of two floating-point atoms.
  ::    Examples
  ::      > (add .~~1 .~~2)
  ::      .~~3
  ::  Source
  ++  add  add:^rh
  ::    +sub:  [@rh @rh] -> @rh
  ::
  ::  Returns the difference of two floating-point atoms.
  ::    Examples
  ::      > (sub .~~1 .~~2)
  ::      .~~-1
  ::  Source
  ++  sub  sub:^rh
  ::    +mul:  [@rh @rh] -> @rh
  ::
  ::  Returns the product of two floating-point atoms.
  ::    Examples
  ::      > (mul .~~1 .~~2)
  ::      .~~2
  ::  Source
  ++  mul  mul:^rh
  ::    +div:  [@rh @rh] -> @rh
  ::
  ::  Returns the quotient of two floating-point atoms.
  ::    Examples
  ::      > (div .~~1 .~~2)
  ::      .~~0.5
  ::  Source
  ++  div  div:^rh
  ::    +fma:  [@rh @rh @rh] -> @rh
  ::
  ::  Returns the fused multiply-add of three floating-point atoms.
  ::    Examples
  ::      > (fma .~~1 .~~2 .~~3)
  ::      .~~5
  ::      > (fma .~~2 .~~3 .~~4)
  ::      .~~10
  ::  Source
  ++  fma  fma:^rh
  ::    +sig:  @rh -> ?
  ::
  ::  Returns the sign of a floating-point atom.
  ::    Examples
  ::      > (sig .~~1)
  ::      %.y
  ::      > (sig .~~-1)
  ::      %.n
  ::  Source
  ++  sig  |=(x=@rh =(0 (rsh [0 15] x)))
  ::    +sgn:  @rh -> ?
  ::
  ::  Returns the sign of a floating-point atom.
  ::  Alias for +sig.
  ::    Examples
  ::      > (sgn .~~1)
  ::      %.y
  ::      > (sgn .~~-1)
  ::      %.n
  ::  Source
  ++  sgn  sig
  ::    +neg:  @rh -> @rh
  ::
  ::  Returns the negation of a floating-point atom.
  ::    Examples
  ::      > (neg .~~1)
  ::      .~~-1
  ::      > (neg .~~-1)
  ::      .~~1
  ::  Source
  ++  neg  |=(x=@rh (sub .~~0 x))
  ::    +factorial:  @rh -> @rh
  ::
  ::  Returns the factorial of a floating-point atom.  Assumes integer input.
  ::    Examples
  ::      > (factorial .~~1)
  ::      .~~1
  ::      > (factorial .~~2)
  ::      .~~2
  ::      > (factorial .~~3)
  ::      .~~6
  ::  Source
  ++  factorial
    |=  x=@rh  ^-  @rh
    ?>  (gte x .~~0)
    =/  t=@rh  .~~1
    ?:  (isclose x .~~0)
      t
    |-  ^-  @rh
    ?:  (isclose x .~~1)
      t
    $(x (sub x .~~1), t (mul t x))
  ::    +abs:  @rh -> @rh
  ::
  ::  Returns the absolute value of a floating-point atom.
  ::    Examples
  ::      > (abs .~~1)
  ::      .~~1
  ::      > (abs .~~-1)
  ::      .~~1
  ::  Source
  ++  abs
    |=  x=@rh  ^-  @rh
    ?:((sgn x) x (neg x))
  ::    +exp:  @rh -> @rh
  ::
  ::  Returns the exponential of a floating-point atom.
  ::    Examples
  ::      > (exp .~~1)
  ::      .~~2.715
  ::      > (exp .~~2)
  ::      .~~7.375
  ::      > (~(exp rh [%z .~~1e-1]) .~~2)
  ::      .~~7.348
  ::      > (exp .~~inf)
  ::      .inf
  ::  Source
  ++  exp
    |=  x=@rh  ^-  @rh
    ::  filter out non-finite arguments
    ?:  =(x 0x0)  .~~1
    ::    check infinities
    ?:  =(x 0x7c00)  `@rh`0x7c00  :: exp(+inf) -> inf
    ?:  =(x 0xfc00)  .~~0.0       :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gte (dis 0x7e00 x) 0)  `@rh`0x7e00  :: exp(NaN) -> NaN
    ::    check overflow to infinity
    =/  o-threshold  `@rh`0x498c  ::  11.091265424003277, value above which exp(x) overflows
    ?:  (gth x o-threshold)  (mul huge huge)
    ::    check underflow to zero
    =/  u-threshold  `@rh`0xc98c  ::  -11.091265424003277, value below which exp(x) underflows
    ?:  (lth x u-threshold)  (mul tiny tiny)
    ::  otherwise, use Taylor series
    =/  p   .~~1
    =/  po  .~~-1
    =/  i   .~~1
    |-  ^-  @rh
    ?:  (lth (abs (sub po p)) rtol)
      p
    $(i (add i .~~1), p (add p (div (pow-n x i) (factorial i))), po p)
  ::    +sin:  @rh -> @rh
  ::
  ::  Returns the sine of a floating-point atom.
  ::    Examples
  ::    > (sin .~~1)
  ::    .~~0.8413
  ::    > (sin .~~2)
  ::    .~~0.9087
  ::    > (sin pi)
  ::    .~~3.437e-3
  ::  Source
  ++  sin
    |=  x=@rh  ^-  @rh
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(x 0x7c00)  `@rh`0x7e00  :: sin(+inf) -> NaN
    ?:  =(x 0xfc00)  `@rh`0x7e00  :: sin(-inf) -> NaN
    ::    check NaN
    ?.  (^gte (dis 0x7e00 x) 0)  `@rh`0x7e00  :: sin(NaN) -> NaN
    ::  map into domain
    =.  x  (mod x tau)
    ::  otherwise, use Taylor series
    =/  p   x
    =/  po  .~~-2
    =/  i   1
    =/  term  x
    |-  ^-  @rh
    ?.  (gth (abs term) rtol)
      p
    =/  i2  (add (sun i) (sun i))
    =.  term  (mul (neg term) (div (mul x x) (mul i2 (add i2 .~~1))))
    $(i +(i), p (add p term), po p)
  ::    +cos:  @rh -> @rh
  ::
  ::  Returns the cosine of a floating-point atom.
  ::    Examples
  ::      > (cos .~~1)
  ::      .~~0.54
  ::     > (cos .~~2)
  ::      .~~-0.4158
  ::     > (cos pi)
  ::      .~~-1.001
  ::  Source
  ++  cos
    |=  x=@rh  ^-  @rh
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(x 0x7c00)  `@rh`0x7e00  :: cos(+inf) -> NaN
    ?:  =(x 0xfc00)  `@rh`0x7e00  :: cos(-inf) -> NaN
    ::    check NaN
    ?.  (^gte (dis 0x7e00 x) 0)  `@rh`0x7e00  :: cos(NaN) -> NaN
    ::  map into domain
    =.  x  (mod x tau)
    ::  otherwise, use Taylor series
    =/  p   .~~1
    =/  po  .~~-2
    =/  i   1
    =/  term  .~~1
    |-  ^-  @rh
    ?.  (gth (abs term) rtol)
      p
    =/  i2  (add (sun i) (sun i))
    =.  term  (mul (neg term) (div (mul x x) (mul i2 (sub i2 .~~1))))
    $(i +(i), p (add p term), po p)
  ::    +tan:  @rh -> @rh
  ::
  ::  Returns the tangent of a floating-point atom.
  ::    Examples
  ::      > (tan .~~1)
  ::      .~~1.558
  ::      > (tan .~~2)
  ::      .~~-2.186
  ::      > (tan pi)
  ::      .~~-3.433e-3
  ::  Source
  ++  tan
    |=  x=@rh  ^-  @rh
    (div (sin x) (cos x))
  ::    +pow-n:  [@rh @rh] -> @rh
  ::
  ::  Returns the power of a floating-point atom to an integer exponent.
  ::    Examples
  ::      > (pow-n .~~1 .~~2)
  ::      .~~1
  ::      > (pow-n .~~2 .~~2)
  ::      .~~4
  ::      > (pow-n .~~2 .~~3)
  ::      .~~8
  ::  Source
  ++  pow-n
    |=  [x=@rh n=@rh]  ^-  @rh
    ?:  =(n .~~0)  .~~1
    ?>  &((gth n .~~0) (isint n))
    =/  p  x
    |-  ^-  @rh
    ?:  (lth n .~~2)
      p
    $(n (sub n .~~1), p (mul p x))
  ::    +log:  @rh -> @rh
  ::
  ::  Returns the natural logarithm of a floating-point atom.
  ::    Examples
  ::      > (log .~~1)
  ::      .~~0
  ::      > (log .~~2)
  ::      .~~0.6914
  ::      > (~(log rh [%z .~~1e-1]) .~~2)
  ::      .~~0.6904
  ++  log
    |=  z=@rh  ^-  @rh
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(z 0x7c00)  `@rh`0x7c00  :: exp(+inf) -> inf
    ?:  =(z 0xfc00)  .~~0.0       :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gte (dis 0x7e00 z) 0)  `@rh`0x7e00  :: exp(NaN) -> NaN
    ::  otherwise, use Taylor series
    =/  p   .~~0
    =/  po  .~~-1
    =/  i   .~~0
    |-  ^-  @rh
    ?:  (lth (abs (sub po p)) rtol)
      (mul (div (mul .~~2 (sub z .~~1)) (add z .~~1)) p)
    =/  term1  (div .~~1 (add .~~1 (mul .~~2 i)))
    =/  term2  (mul (sub z .~~1) (sub z .~~1))
    =/  term3  (mul (add z .~~1) (add z .~~1))
    =/  term  (mul term1 (pow-n (div term2 term3) i))
    $(i (add i .~~1), p (add p term), po p)
  ::    +pow:  [@rh @rh] -> @rh
  ::
  ::  Returns the power of a floating-point atom to a floating-point exponent.
  ::    Examples
  ::      > (pow .~~1 .~~2)
  ::      .~~1
  ::      > (pow .~~2 .~~2)
  ::      .~~4
  ::      > (~(pow rh:math [%z .~~1e-1]) .~~2 .~~3.5)
  ::      .~~11.14
  ::  Source
  ++  pow
    |=  [x=@rh n=@rh]  ^-  @rh
    ::  fall through on integers (faster)
    ?:  =(n (san (need (toi n))))  (pow-n x (san (need (toi n))))
    (exp (mul n (log x)))
  ::    +sqrt:  @rh -> @rh
  ::
  ::  Returns the square root of a floating-point atom.
  ::  Alias for +sqt.
  ::    Examples
  ::      > (sqrt .~~1)
  ::      .~~1
  ::      > (sqrt .~~2)
  ::      .~~1.412
  ::      > (~(sqrt rh [%z .~~1e-1]) .~~2)
  ::      .~~1.404
  ::  Source
  ++  sqrt  sqt
  ::    +sqt:  @rh -> @rh
  ::
  ::  Returns the square root of a floating-point atom.
  ::    Examples
  ::      > (sqt .~~1)
  ::      .~~1
  ::      > (sqt .~~2)
  ::      .~~1.412
  ::      > (~(sqt rh [%z .~~1e-1]) .~~2)
  ::      .~~1.404
  ::  Source
  ++  sqt
    |=  x=@rh  ^-  @rh
    ?>  (sgn x)
    (pow x .~~0.5)
  ::    +cbrt:  @rh -> @rh
  ::
  ::  Returns the cube root of a floating-point atom.
  ::  Alias for +cbt.
  ::    Examples
  ::      > (cbrt .~~1)
  ::      .~~1
  ::      > (cbrt .~~2)
  ::      .~~1.258
  ::      > (~(cbrt rh [%z .~~1e-1]) .~~2)
  ::      .~~1.256
  ::  Source
  ++  cbrt  cbt
  ::    +cbt:  @rh -> @rh
  ::
  ::  Returns the cube root of a floating-point atom.
  ::    Examples
  ::      > (cbt .~~1)
  ::      .~~1
  ::      > (cbt .~~2)
  ::      .~~1.258
  ::      > (~(cbt rh [%z .~~1e-1]) .~~2)
  ::      .~~1.256
  ::  Source
  ++  cbt
    |=  x=@rh  ^-  @rh
    ?>  (sgn x)
    (pow x .~~0.3333)
  ::    +arg:  @rh -> @rh
  ::
  ::  Returns the argument of a floating-point atom (real argument = absolute
  ::  value).
  ::    Examples
  ::      > (arg .~~1)
  ::      .~~1
  ::      > (arg .~-1)
  ::      .~~1
  ::  Source
  ++  arg  abs
  --
::  quad precision
++  rq
  ^|
  |_  $:  r=$?(%n %u %d %z)   :: round nearest, up, down, to zero
          rtol=_.~~~1e-20     :: relative tolerance for precision of operations
      ==
  ::  mathematics constants to quad precision
  ::    +tau:  @rq
  ::
  ::  Returns the value 2*pi (OEIS A019692).
  ::    Examples
  ::      > tau
  ::      .~~~6.2831853071795864769252867665590056
  ::  Source
  ++  tau  .~~~6.2831853071795864769252867665590056
  ::    +pi:  @rq
  ::
  ::  Returns the value pi (OEIS A000796).
  ::    Examples
  ::      > pi
  ::      .~~~3.1415926535897932384626433832795028
  ::  Source
  ++  pi  .~~~3.1415926535897932384626433832795028
  ::    +e:  @rq
  ::
  ::  Returns the value e (Euler's constant) (OEIS A001113).
  ::    Examples
  ::      > e
  ::      .~~~2.7182818284590452353602874713526623
  ::  Source
  ++  e  .~~~2.7182818284590452353602874713526623
  ::    +phi:  @rq
  ::
  ::  Returns the value phi (golden ratio) (OEIS A001622).
  ::    Examples
  ::      > phi
  ::      .~~~1.6180339887498948482045868343656382
  ::  Source
  ++  phi  .~~~1.6180339887498948482045868343656382
  ::    +sqt2:  @rq
  ::
  ::  Returns the value sqrt(2) (OEIS A002193).
  ::    Examples
  ::      > sqt2
  ::      .~~~1.414213562373095048801688724209698
  ::  Source
  ++  sqt2  .~~~1.414213562373095048801688724209698
  ::    +invsqt2:  @rq
  ::
  ::  Returns the value 1/sqrt(2) (OEIS A010503).
  ::    Examples
  ::      > invsqt2
  ::      .~~~0.707106781186547524400844362104849
  ::  Source
  ++  invsqt2  .~~~0.707106781186547524400844362104849
  ::    +log2:  @rq
  ::
  ::  Returns the value log(2) (OEIS A002162).
  ::    Examples
  ::      > log2
  ::      .~~~0.6931471805599453094172321214581766
  ::  Source
  ++  log2  .~~~0.6931471805599453094172321214581766
  ::    +invlog2:  @rq
  ::
  ::  Returns the value 1/log(2).
  ::    Examples
  ::      > invlog2
  ::      .~~~1.442695040888963387004650940070860
  ::  Source
  ++  invlog2  .~~~1.442695040888963387004650940070860  :: TODO check
  ::    +log10:  @rq
  ::
  ::  Returns the value log(10) (OEIS A002392).
  ::    Examples
  ::      > log10
  ::      .~~~2.302585092994045684017991454684364
  ::  Source
  ++  log10  .~~~2.302585092994045684017991454684364
  ::    +huge:  @rq
  ::
  ::  Returns the value of the largest representable number.
  ::    Examples
  ::      > huge
  ::      .~~~1.189731495357231765085759326628007e4932
  ::  Source
  ++  huge  `@rq`0x7ffe.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff  ::  1.18973149535723176508575932662800702e4932
  ::    +tiny:  @rq
  ::
  ::  Returns the value of the smallest representable normal number.
  ::    Examples
  ::      > tiny
  ::      .~~~3.3621031431120935062626778173217526e-4932
  ::  Source
  ++  tiny  `@rq`0x1.0000.0000.0000.0000.0000.0000.0000.0000     ::  3.36210314311209350626267781732175260e-4932
  ::
  ::  Operations
  ::
  ::    +sea:  @rq -> fn
  ::
  ::  Returns the +$fn representation of a floating-point atom.
  ::    Examples
  ::      > (sea .~~~1)
  ::      [%f s=%.y e=-112 a=5.192.296.858.534.827.628.530.496.329.220.096]
  ::      > (sea .~~~1.1)
  ::      [%f s=%.y e=-112 a=5.711.526.544.388.310.391.383.545.962.142.106]
  ::  Source
  ++  sea  sea:^rq
  ::    +bit:  fn -> @rq
  ::
  ::  Returns the floating-point atom of a +$fn representation.
  ::    Examples
  ::      > (bit [%f s=%.y e=-112 a=5.192.296.858.534.827.628.530.496.329.220.096])
  ::      .~~~1
  ::      > (bit [%f s=%.y e=-112 a=5.711.526.544.388.310.391.383.545.962.142.106])
  ::      .~~~1.1
  ::  Source
  ++  bit  bit:^rq
  ::    +sun:  @ud -> @rq
  ::
  ::  Returns the floating-point atom of an unsigned integer atom.
  ::    Examples
  ::      > (sun 1)
  ::      .~~~1
  ::      > (sun 1.000)
  ::      .~~~1e3
  ::  Source
  ++  sun  sun:^rq
  ::    +san:  @sd -> @rq
  ::
  ::  Returns the floating-point atom of a signed integer atom.
  ::    Examples
  ::      > (san --1)
  ::      .~~~1
  ::      > (san -1)
  ::      .~~~-1
  ::  Source
  ++  san  san:^rq
  ::++  exp  exp:^rq  :: no pass-through because of exp function
  ::    +toi:  @rq -> @sd
  ::
  ::  Returns the unitized signed integer atom of a rounded floating-point atom.
  ::    Examples
  ::      > (toi .~~~1)
  ::      [~ --1]
  ::      > (toi .~~~1.1)
  ::      [~ --1]
  ::  Source
  ++  toi  toi:^rq
  ::    +drg:  @rq -> dn
  ::
  ::  Returns the decimal form of a floating-point atom using the Dragon4
  ::  algorithm.
  ::    Examples
  ::      > (drg .~~~1)
  ::      [%d s=%.y e=--0 a=1]
  ::      > (drg .~~~1.1)
  ::      [%d s=%.y e=-1 a=11]
  ::  Source
  ++  drg  drg:^rq
  ::    +grd:  dn -> @rq
  ::
  ::  Returns the floating-point atom of a decimal form.
  ::    Examples
  ::      > (grd [%d s=%.y e=--0 a=1])
  ::      .~~~1
  ::      > (grd [%d s=%.y e=-1 a=11])
  ::      .~~~1.1
  ::  Source
  ++  grd  grd:^rq
  ::
  ::  Comparison
  ::
  ::    +lth:  [@rq @rq] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, less than.
  ::    Examples
  ::      > (lth .~~~1 .~~~2)
  ::      %.y
  ::      > (lth .~~~2 .~~~1)
  ::      %.n
  ::      > (lth .~~~1 .~~~1)
  ::      %.n
  ::  Source
  ++  lth  lth:^rq
  ::    +lte:  [@rq @rq] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, less than or equal to.
  ::    Examples
  ::      > (lte .~~~1 .~~~2)
  ::      %.y
  ::      > (lte .~~~2 .~~~1)
  ::      %.n
  ::      > (lte .~~~1 .~~~1)
  ::      %.y
  ::  Source
  ++  lte  lte:^rq
  ::    +leq:  [@rq @rq] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, less than or equal to.
  ::  Alias for +lte.
  ::    Examples
  ::      > (leq .~~~1 .~~~2)
  ::      %.y
  ::      > (leq .~~~2 .~~~1)
  ::      %.n
  ::      > (leq .~~~1 .~~~1)
  ::      %.y
  ::  Source
  ++  leq  lte:^rq
  ::    +equ:  [@rq @rq] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, equal to.
  ::    Examples
  ::      > (equ .~~~1 .~~~2)
  ::      %.n
  ::      > (equ .~~~2 .~~~1)
  ::      %.n
  ::      > (equ .~~~1 .~~~1)
  ::      %.y
  ::  Source
  ++  equ  equ:^rq
  ::    +gth:  [@rq @rq] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, greater than.
  ::    Examples
  ::      > (gth .~~~1 .~~~2)
  ::      %.n
  ::      > (gth .~~~2 .~~~1)
  ::      %.y
  ::      > (gth .~~~1 .~~~1)
  ::      %.n
  ::  Source
  ++  gth  gth:^rq
  ::    +gte:  [@rq @rq] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, greater than or equal to.
  ::    Examples
  ::      > (gte .~~~1 .~~~2)
  ::      %.n
  ::      > (gte .~~~2 .~~~1)
  ::      %.y
  ::      > (gte .~~~1 .~~~1)
  ::      %.y
  ::  Source
  ++  gte  gte:^rq
  ::    +geq:  [@rq @rq] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, greater than or equal to.
  ::  Alias for +gte.
  ::    Examples
  ::      > (geq .~~~1 .~~~2)
  ::      %.n
  ::      > (geq .~~~2 .~~~1)
  ::      %.y
  ::      > (geq .~~~1 .~~~1)
  ::      %.y
  ::  Source
  ++  geq  gte:^rq
  ::    +neq:  [@rq @rq] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, not equal to.
  ::    Examples
  ::      > (neq .~~~1 .~~~2)
  ::      %.y
  ::      > (neq .~~~2 .~~~1)
  ::      %.y
  ::      > (neq .~~~1 .~~~1)
  ::      %.n
  ::  Source
  ++  neq  |=([a=@rq b=@rq] ^-(? !(equ:^rq a b)))
  ::    +isclose:  [@rq @rq] -> ?
  ::
  ::  Returns the comparison of two floating-point atoms, within a relative
  ::  tolerance (provided by the +rq door).
  ::    Examples
  ::      > (isclose .~~~1 .~~~2)
  ::      %.n
  ::      > (isclose .~~~1 .~~~1.0000001)
  ::      %.n
  ::      > (~(isclose rq [%z .~~~1e-3]) .~~~1 .~~~1.0001)
  ::      %.y
  ::      > (~(isclose rq [%z .~~~1e-30]) .~~~1 .~~~1.0001)
  ::      %.n
  ::  Source
  ++  isclose
    |=  [p=@rq r=@rq]
    (lth (abs (sub p r)) rtol)
  ::    +allclose:  [@rq (list @rq)] -> ?
  ::
  ::  Returns the comparison of a floating-point atom to a list of floating-
  ::  point atoms, within a relative tolerance (provided by the +rq door).
  ::    Examples
  ::      > (allclose .~~~1 ~[.~~~1 .~~~2])
  ::      %.n
  ::      > (allclose .~~~1 ~[.~~~1 .~~~1.0000001])
  ::      %.n
  ::      > (~(allclose rq [%z .~~~1e-3]) .~~~1 ~[.~~~1 .~~~1.0001])
  ::      %.y
  ::  Source
  ++  allclose
    |=  [p=@rq q=(list @rq)]
    =/  i  0
    =/  n  (lent q)
    |-  ^-  ?
    ?:  =(n i)
      %.y
    ?.  (isclose p (snag i q))
      %.n
    $(i +(i))
  ::    +isint:  @rq -> ?
  ::
  ::  Returns whether a floating-point value is an integer (no fractional part).
  ::    Examples
  ::      > (isint .~~~1)
  ::      %.y
  ::      > (isint .~~~1.1)
  ::      %.n
  ::  Source
  ++  isint
    |=  x=@rq  ^-  ?
    (equ x (san (need (toi x))))
  ::
  ::  Algebraic
  ::
  ::    +add:  [@rq @rq] -> @rq
  ::
  ::  Returns the sum of two floating-point atoms.
  ::    Examples
  ::      > (add .~~~1 .~~~2)
  ::      .~~~3
  ::  Source
  ++  add  add:^rq
  ::    +sub:  [@rq @rq] -> @rq
  ::
  ::  Returns the difference of two floating-point atoms.
  ::    Examples
  ::      > (sub .~~~1 .~~~2)
  ::      .~~~-1
  ::  Source
  ++  sub  sub:^rq
  ::    +mul:  [@rq @rq] -> @rq
  ::
  ::  Returns the product of two floating-point atoms.
  ::    Examples
  ::      > (mul .~~~1 .~~~2)
  ::      .~~~2
  ::  Source
  ++  mul  mul:^rq
  ::    +div:  [@rq @rq] -> @rq
  ::
  ::  Returns the quotient of two floating-point atoms.
  ::    Examples
  ::      > (div .~~~1 .~~~2)
  ::      .~~~0.5
  ::  Source
  ++  div  div:^rq
  ::    +fma:  [@rq @rq @rq] -> @rq
  ::
  ::  Returns the fused multiply-add of three floating-point atoms.
  ::    Examples
  ::      > (fma .~~~1 .~~~2 .~~~3)
  ::      .~~~5
  ::      > (fma .~~~2 .~~~3 .~~~4)
  ::      .~~~10
  ::  Source
  ++  fma  fma:^rq
  ::    +sig:  @rq -> ?
  ::
  ::  Returns the sign of a floating-point atom.
  ::    Examples
  ::      > (sig .~~~1)
  ::      %.y
  ::      > (sig .~~~-1)
  ::      %.n
  ::  Source
  ++  sig  |=(x=@rq =(0 (rsh [0 127] x)))
  ::    +sgn:  @rq -> ?
  ::
  ::  Returns the sign of a floating-point atom.
  ::  Alias for +sig.
  ::    Examples
  ::      > (sgn .~~~1)
  ::      %.y
  ::      > (sgn .~~~-1)
  ::      %.n
  ::  Source
  ++  sgn  sig
  ::    +neg:  @rq -> @rq
  ::
  ::  Returns the negation of a floating-point atom.
  ::    Examples
  ::      > (neg .~~~1)
  ::      .~~~-1
  ::      > (neg .~~~-1)
  ::      .~~~1
  ::  Source
  ++  neg  |=(x=@rq (sub .~~~0 x))
  ::    +factorial:  @rq -> @rq
  ::
  ::  Returns the factorial of a floating-point atom.  Assumes integer input.
  ::    Examples
  ::      > (factorial .~~~1)
  ::      .~~~1
  ::      > (factorial .~~~2)
  ::      .~~~2
  ::      > (factorial .~~~3)
  ::      .~~~6
  ::  Source
  ++  factorial
    |=  x=@rq  ^-  @rq
    ?>  (gte x .~~~0)
    =/  t=@rq  .~~~1
    ?:  (isclose x .~~~0)
      t
    |-  ^-  @rq
    ?:  (isclose x .~~~1)
      t
    $(x (sub x .~~~1), t (mul t x))
  ::    +abs:  @rq -> @rq
  ::
  ::  Returns the absolute value of a floating-point atom.
  ::    Examples
  ::      > (abs .~~~1)
  ::      .~~~1
  ::      > (abs .~~~-1)
  ::      .~~~1
  ::  Source
  ++  abs
    |=  x=@rq  ^-  @rq
    ?:((sgn x) x (neg x))
  ::    +exp:  @rq -> @rq
  ::
  ::  Returns the exponential of a floating-point atom.
  ::    Examples
  ::      > (exp .~~~1)
  ::      .~~~2.7182818284590452353602471108690483
  ::      > (exp .~~~2)
  ::      .~~~7.389056098930650227230362414146335
  ::      > (~(exp rq [%z .~~~1e-20]) .~~~2)
  ::      .~~~7.389056098930650227230362414146335
  ::      > (exp .~~~inf)
  ::      .~~~inf
  ::  Source
  ++  exp
    |=  x=@rq  ^-  @rq
    ::  filter out non-finite arguments
    ?:  =(x 0x0)  .~~~1
    ::    check infinities
    ?:  =(x 0x7fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000  :: exp(+inf) -> inf
    ?:  =(x 0xffff.0000.0000.0000.0000.0000.0000.0000)  .~~~0.0      :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gte (dis 0x7fff.8000.0000.0000.0000.0000.0000.0000 x) 0)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000  :: exp(NaN) -> NaN
    ::    check overflow to infinity
    =/  o-threshold  `@rq`0x400c.62e4.2fef.a39e.f357.93c7.6730.0601  ::  1.135652340629414394949193107797e4, value above which exp(x) overflows
    ?:  (gth x o-threshold)  (mul huge huge)
    ::    check underflow to zero
    =/  u-threshold  `@rq`0xc00c.62e4.2fef.a39e.f357.93c7.6730.0601  ::  -1.135652340629414394949193107797e4, value below which exp(x) underflows
    ?:  (lth x u-threshold)  (mul tiny tiny)
    ::  otherwise, use Taylor series
    =/  p   .~~~1
    =/  po  .~~~-1
    =/  i   .~~~1
    |-  ^-  @rq
    ?:  (lth (abs (sub po p)) rtol)
      p
    $(i (add i .~~~1), p (add p (div (pow-n x i) (factorial i))), po p)
  ::    +sin:  @rq -> @rq
  ::
  ::  Returns the sine of a floating-point atom.
  ::    Examples
  ::    > (sin .~~~1)
  ::    .~~~0.8414709848078965066525022572525196
  ::    > (sin .~~~2)
  ::    .~~~0.9092974268256816953960201260866781
  ::    > (sin pi)
  ::    .~~~2.4143733100361875441251426417684949e-23
  ::  Source
  ++  sin
    |=  x=@rq  ^-  @rq
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(x 0x7fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000  :: sin(+inf) -> NaN
    ?:  =(x 0xffff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000  :: sin(-inf) -> NaN
    ::    check NaN
    ?.  (^gte (dis 0x7fff.8000.0000.0000.0000.0000.0000.0000 x) 0)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000  :: sin(NaN) -> NaN
    ::  map into domain
    =.  x  (mod x tau)
    ::  otherwise, use Taylor series
    =/  p   x
    =/  po  .~~~-2
    =/  i   1
    =/  term  x
    |-  ^-  @rq
    ?.  (gth (abs term) rtol)
      p
    =/  i2  (add (sun i) (sun i))
    =.  term  (mul (neg term) (div (mul x x) (mul i2 (add i2 .~~~1))))
    $(i +(i), p (add p term), po p)
  ::    +cos:  @rq -> @rq
  ::
  ::  Returns the cosine of a floating-point atom.
  ::    Examples
  ::      > (cos .~~~1)
  ::      .~~~0.5403023058681397174009349981817251
  ::     > (cos .~~~2)
  ::      .~~~-0.41614683654714238699756419777191616
  ::     > (cos pi)
  ::      .~~~-1.0000000000000000000000021077555518
  ::  Source
  ++  cos
    |=  x=@rq  ^-  @rq
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(x 0x7fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000  :: cos(+inf) -> NaN
    ?:  =(x 0xffff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000  :: cos(-inf) -> NaN
    ::    check NaN
    ?.  (^gte (dis 0x7fff.8000.0000.0000.0000.0000.0000.0000 x) 0)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000  :: cos(NaN) -> NaN
    ::  map into domain
    =.  x  (mod x tau)
    ::  otherwise, use Taylor series
    =/  p   .~~~1
    =/  po  .~~~-2
    =/  i   1
    =/  term  .~~~1
    |-  ^-  @rq
    ?.  (gth (abs term) rtol)
      p
    =/  i2  (add (sun i) (sun i))
    =.  term  (mul (neg term) (div (mul x x) (mul i2 (sub i2 .~~~1))))
    $(i +(i), p (add p term), po p)
  ::    +tan:  @rq -> @rq
  ::
  ::  Returns the tangent of a floating-point atom.
  ::    Examples
  ::      > (tan .~~~1)
  ::      .~~~1.5574077246549022305069793269617903
  ::      > (tan .~~~2)
  ::      .~~~-2.1850398632615189916433278966958165
  ::      > (tan pi)
  ::      .~~~-2.1850398632615189916433278966958165
  ::  Source
  ++  tan
    |=  x=@rq  ^-  @rq
    (div (sin x) (cos x))
  ::    +pow-n:  [@rq @rq] -> @rq
  ::
  ::  Returns the power of a floating-point atom to a signed integer exponent.
  ::    Examples
  ::      > (pow-n .~~~2 .~~~2)
  ::      .~~~4
  ::      > (pow-n .~~~2 .~~~-2)
  ::      .~~~0.25
  ::  Source
  ++  pow-n
    |=  [x=@rq n=@rq]  ^-  @rq
    ?:  =(n .~~~0)  .~~~1
    ?>  &((gth n .~~~0) (isint n))
    =/  p  x
    |-  ^-  @rq
    ?:  (lth n .~~~2)
      p
    $(n (sub n .~~~1), p (mul p x))
  ::    +log:  @rq -> @rq
  ::
  ::  Returns the natural logarithm of a floating-point atom.
  ::    Examples
  ::      > (log .~~~1)
  ::      .~~~0
  ::      > (log .~~~2)
  ::      .~~~0.6931471805599453094170735934298606
  ::      > (~(log rq [%z .~~~1e-5]) .~~~2)
  ::      .~~~0.6931470737597852366942444674497712
  ::      > (log .~~~inf)
  ::      .~~~inf
  ::  Source
  ++  log
    |=  z=@rq  ^-  @rq
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(z 0x7fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000  :: exp(+inf) -> inf
    ?:  =(z 0xffff.0000.0000.0000.0000.0000.0000.0000)  .~~~0.0      :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gte (dis 0x7fff.8000.0000.0000.0000.0000.0000.0000 z) 0)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000  :: exp(NaN) -> NaN
    ::  otherwise, use Taylor series
    =/  p   .~~~0
    =/  po  .~~~-1
    =/  i   .~~~0
    |-  ^-  @rq
    ?:  (lth (abs (sub po p)) rtol)
      (mul (div (mul .~~~2 (sub z .~~~1)) (add z .~~~1)) p)
    =/  term1  (div .~~~1 (add .~~~1 (mul .~~~2 i)))
    =/  term2  (mul (sub z .~~~1) (sub z .~~~1))
    =/  term3  (mul (add z .~~~1) (add z .~~~1))
    =/  term  (mul term1 (pow-n (div term2 term3) i))
    $(i (add i .~~~1), p (add p term), po p)
  ::    +pow:  [@rq @rq] -> @rq
  ::
  ::  Returns the power of a floating-point atom to a floating-point exponent.
  ::    Examples
  ::      > (pow .~~~1 .~~~2)
  ::      .~~~1
  ::      > (pow .~~~2 .~~~2)
  ::      .~~~4
  ::      > (~(pow rq:math [%z .~~~1e-5]) .~~~2 .~~~3.5)
  ::      .~~~11.313703735926135014164384135726204
  ::  Source
  ++  pow
    |=  [x=@rq n=@rq]  ^-  @rq
    ::  fall through on integers (faster)
    ?:  =(n (san (need (toi n))))  (pow-n x (san (need (toi n))))
    (exp (mul n (log x)))
  ::    +sqrt:  @rq -> @rq
  ::
  ::  Returns the square root of a floating-point atom.
  ::  Alias for +sqt.
  ::    Examples
  ::      > (sqrt .~~~1)
  ::      .~~~1
  ::      > (sqrt .~~~2)
  ::      .~~~1.4142135623730950488015335862957159
  ::      > (~(sqrt rq:math [%z .~~~1e-10]) .~~~2)
  ::      .~~~1.4142135623721439870165294373250435
  ::  Source
  ++  sqrt  sqt
  ::    +sqt:  @rq -> @rq
  ::
  ::  Returns the square root of a floating-point atom.
  ::    Examples
  ::      > (sqt .~~~1)
  ::      .~~~1
  ::      > (sqt .~~~2)
  ::      .~~~1.4142135623730950488015335862957159
  ::      > (~(sqt rq:math [%z .~~~1e-10]) .~~~2)
  ::      .~~~1.4142135623721439870165294373250435
  ::  Source
  ++  sqt
    |=  x=@rq  ^-  @rq
    ?>  (sgn x)
    (pow x .~~~0.5)
  ::    +cbrt:  @rq -> @rq
  ::
  ::  Returns the cube root of a floating-point atom.
  ::  Alias for +cbt.
  ::    Examples
  ::      > (cbrt .~~~1)
  ::      .~~~1
  ::      > (cbrt .~~~2)
  ::      .~~~1.2598919398737178526805575821133312
  ::      > (~(cbrt rq:math [%z .~~~1e-10]) .~~~2)
  ::      .~~~1.2598919398731638759238176665172822
  ::  Source
  ++  cbrt  cbt
  ::    +cbt:  @rq -> @rq
  ::
  ::  Returns the cube root of a floating-point atom.
  ::    Examples
  ::      > (cbt .~~~1)
  ::      .~~~1
  ::      > (cbt .~~~2)
  ::      .~~~1.2598919398737178526805575821133312
  ::      > (~(cbt rq:math [%z .~~~1e-10]) .~~~2)
  ::      .~~~1.2598919398731638759238176665172822
  ::  Source
  ++  cbt
    |=  x=@rq  ^-  @rq
    ?>  (sgn x)
    (pow x .~~~0.3333)
  ::    +arg:  @rq -> @rq
  ::
  ::  Returns the argument of a floating-point atom (real argument = absolute
  ::  value).
  ::    Examples
  ::      > (arg .~~~1)
  ::      .~~~1
  ::      > (arg .~~~-1)
  ::      .~~~1
  ::  Source
  ++  arg  abs
  --
::  reference values
++  reference
  |%
  ::  hardcoded string constants for your viewing pleasure
  ::  OEIS A019692
  ++  tau   '6.28318530717958647692528676655900576839433879875021164194988918461563281257241799625606965068423413596428'
  ::  OEIS A000796
  ++  pi     '3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214'
  ::  OEIS A001113
  ++  e      '2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746'
  ::  OEIS A001622
  ++  phi    '1.61803398874989484820458683436563811772030917980576286213544862270526046281890244970720720418939113748475'
  ::  OEIS  A002193
  ++  sqt2  '1.41421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157273'
  ::  OEIS A010503
  ++  invsqt2  '0.70710678118654752440084436210484903928483593768847403658833986899536623923105351942519376716382086'
  ::  OEIS A002162
  ++  log2    '0.69314718055994530941723212145817656807550013436025525412068000949339362196969471560586332699641868754'
  ::  OEIS A002392
  ++  log10   '2.30258509299404568401799145468436420760110148862877297603332790096757260967735248023599726645985502929'
  --
--
