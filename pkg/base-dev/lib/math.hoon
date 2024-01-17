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
  ::  OEIS A019692
  ++  tau  .6.2831855
  ::  OEIS A000796
  ++  pi  .3.1415927
  ::  OEIS A001113
  ++  e  .2.7182817
  ::  OEIS A001622
  ++  phi  .1.618034
  ::  OEIS A002193
  ++  sqt2  .1.4142135
  ::  OEIS A010503
  ++  invsqt2  .70710677
  ::  OEIS A002162
  ++  log2  .0.6931472
  ++  invlog2  .1.442695
  ::  OEIS A002392
  ++  log10  .2.3025851
  ::
  ++  huge  `@rs`0x7f80.0000  ::  3.40282346638528859812e+38
  ++  tiny  `@rs`0x1          ::  1.40129846432481707092e-45
  ::
  ::  Operations
  ::
  ++  sea  sea:^rs
  ++  bit  bit:^rs
  ++  sun  sun:^rs
  ++  san  san:^rs
  ::++  exp  exp:^rs  :: no pass-through because of exp function
  ++  toi  toi:^rs
  ++  drg  drg:^rs
  ++  grd  grd:^rs
  ::
  ::  Comparison
  ::
  ++  lth  lth:^rs
  ++  lte  lte:^rs
  ++  leq  lte:^rs
  ++  equ  equ:^rs
  ++  gte  gte:^rs
  ++  gth  gth:^rs
  ++  geq  gte:^rs
  ++  neq  |=([a=@rs b=@rs] ^-(? !(equ:^rs a b)))
  ++  isclose
    |=  [p=@rs r=@rs]
    (lth (abs (sub p r)) rtol)
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
  ::  use equality rather than isclose here
  ++  isint
    |=  x=@rs  ^-  ?
    (equ x (san (need (toi x))))
  ::
  ::  Algebraic
  ::
  ++  add  add:^rs
  ++  sub  sub:^rs
  ++  mul  mul:^rs
  ++  div  div:^rs
  ++  fma  fma:^rs
  ++  sig  |=(x=@rs =(0 (rsh [0 31] x)))
  ++  sgn  sig
  ++  neg  |=(x=@rs (sub .0 x))
  ++  factorial
    |=  x=@rs  ^-  @rs
    ?>  (gth x .0)
    =/  t=@rs  .1
    ?:  (isclose x .0)
      t
    |-  ^-  @rs
    ?:  (isclose x .1)
      t
    $(x (sub x .1), t (mul t x))
  ++  abs
    |=  x=@rs  ^-  @rs
    ?:((sgn x) x (neg x))
  ::  exponential
  ::
  ++  exp
    |=  x=@rs  ^-  @rs
    ::  filter out non-finite arguments
    ?:  =(x 0x0)  .1
    ::    check infinities
    ?:  =(x 0x7f80.0000)  `@rs`0x7f80.0000  :: exp(+inf) -> inf
    ?:  =(x 0xff80.0000)  .0.0              :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gth (dis 0x7fc0.0000 x) 0)  `@rs`0x7fc0.0000  :: exp(NaN) -> NaN
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
  ::  restricted power for integers only
  ::
  ++  pow-n
    |=  [x=@rs n=@rs]  ^-  @rs
    ?:  =(n .0)  .1
    =/  p  x
    |-  ^-  @rs
    ?:  (lth n .2)
      p
    $(n (sub n .1), p (mul p x))
  ::  natural logarithm, z > 0
  ::
  ++  log
    |=  z=@rs  ^-  @rs
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(z 0x7f80.0000)  `@rs`0x7f80.0000  :: log(+inf) -> inf
    ?:  =(z 0xff80.0000)  `@rs`0x7fc0.0000  :: log(-inf) -> NaN
    ::    check NaN
    ?.  (^gth (dis 0x7fc0.0000 z) 0)  `@rs`0x7fc0.0000  :: exp(NaN) -> NaN
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
  ::  general power, based on logarithms (slower)
  ::  x^n = exp(n ln x)
  ++  pow
    |=  [x=@rs n=@rs]  ^-  @rs
    ::  fall through on integers (faster)
    ?:  =(n (san (need (toi n))))  (pow-n x (san (need (toi n))))
    (exp (mul n (log x)))
  ::  square root
  ++  sqrt  sqt
  ++  sqt
    |=  x=@rs  ^-  @rs
    ?>  (sgn x)
    (pow x .0.5)
  ::  cube root
  ++  cbrt  cbt
  ++  cbt
    |=  x=@rs  ^-  @rs
    ?>  (sgn x)
    (pow x .0.33333333)
  ::  argument (real argument = absolute value)
  ++  arg  abs
  --
::  double precision
++  rd
  ^|
  |_  $:  r=$?(%n %u %d %z)   :: round nearest, up, down, to zero
          rtol=_.~1e-10       :: relative tolerance for precision of operations
      ==
  ::  mathematics constants to single precision
  ::  OEIS A019692
  ++  tau  .6.283185307179586
  ::  OEIS A000796
  ++  pi  .3.141592653589793
  ::  OEIS A001113
  ++  e  .2.718281828459045
  ::  OEIS A001622
  ++  phi  .1.618033988749895
  ::  OEIS A002193
  ++  sqt2  .1.4142135623730951
  ::  OEIS A010503
  ++  invsqt2  .7071067811865476
  ::  OEIS A002162
  ++  log2  .0.6931471805599453
  ++  invlog2  .1.4426950408889634
  ::  OEIS A002392
  ++  log10  .2.302585092994046
  ::
  ++  huge  `@rd`0x7fef.ffff.ffff.ffff  ::  1.79769313486231570815e+308
  ++  tiny  `@rd`0x10.0000.0000.0000    ::  2.22507385850720138309e-308
  ::
  ::  Operations
  ::
  ++  sea  sea:^rd
  ++  bit  bit:^rd
  ++  sun  sun:^rd
  ++  san  san:^rd
  ::++  exp  exp:^rd  :: no pass-through because of exp function
  ++  toi  toi:^rd
  ++  drg  drg:^rd
  ++  grd  grd:^rd
  ::
  ::  Comparison
  ::
  ++  lth  lth:^rd
  ++  lte  lte:^rd
  ++  leq  lte:^rd
  ++  equ  equ:^rd
  ++  gte  gte:^rd
  ++  gth  gth:^rd
  ++  geq  gte:^rd
  ++  neq  |=([a=@rd b=@rd] ^-(? !(equ:^rd a b)))
  ++  isclose
    |=  [p=@rd r=@rd]
    (lth (abs (sub p r)) rtol)
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
  ::  use equality rather than isclose here
  ++  isint
    |=  x=@rd  ^-  ?
    (equ x (san (need (toi x))))
  ::
  ::  Algebraic
  ::
  ++  add  add:^rd
  ++  sub  sub:^rd
  ++  mul  mul:^rd
  ++  div  div:^rd
  ++  fma  fma:^rd
  ++  sig  |=(x=@rd =(0 (rsh [0 63] x)))
  ++  sgn  sig
  ++  neg  |=(x=@rd (sub .~0 x))
  ++  factorial
    |=  x=@rd  ^-  @rd
    ?>  (gth x .~0)
    =/  t=@rd  .~1
    ?:  (isclose x .~0)
      t
    |-  ^-  @rd
    ?:  (isclose x .~1)
      t
    $(x (sub x .~1), t (mul t x))
  ++  abs
    |=  x=@rd  ^-  @rd
    ?:((sgn x) x (neg x))
  ::  exponential
  ::
  ++  exp
    |=  x=@rd  ^-  @rd
    ::  filter out non-finite arguments
    ?:  =(x 0x0)  .~1
    ::    check infinities
    ?:  =(x 0x7ff0.0000.0000.0000)  `@rd`0x7ff0.0000.0000.0000  :: exp(+inf) -> inf
    ?:  =(x 0xfff0.0000.0000.0000)  .~0.0                       :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gth (dis 0x7ff8.0000.0000.0000 x) 0)  `@rd`0x7ff8.0000.0000.0000  :: exp(NaN) -> NaN
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
  ::  restricted power for integers only
  ::
  ++  pow-n
    |=  [x=@rd n=@rd]  ^-  @rd
    ?:  =(n .~0)  .~1
    =/  p  x
    |-  ^-  @rd
    ?:  (lth n .~2)
      p
    $(n (sub n .~1), p (mul p x))
  ::  natural logarithm, z > 0
  ::
  ++  log
    |=  z=@rd  ^-  @rd
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(z 0x7ff0.0000.0000.0000)  `@rd`0x7ff0.0000.0000.0000  :: exp(+inf) -> inf
    ?:  =(z 0xfff0.0000.0000.0000)  .~0.0                       :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gth (dis 0x7ff8.0000.0000.0000 z) 0)  `@rd`0x7ff8.0000.0000.0000  :: exp(NaN) -> NaN
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
  ::  general power, based on logarithms (slower)
  ::  x^n = exp(n ln x)
  ++  pow
    |=  [x=@rd n=@rd]  ^-  @rd
    ::  fall through on integers (faster)
    ?:  =(n (san (need (toi n))))  (pow-n x (san (need (toi n))))
    (exp (mul n (log x)))
  ::  square root
  ++  sqrt  sqt
  ++  sqt
    |=  x=@rd  ^-  @rd
    ?>  (sgn x)
    (pow x .~0.5)
  ::  cube root
  ++  cbrt  cbt
  ++  cbt
    |=  x=@rd  ^-  @rd
    ?>  (sgn x)
    (pow x .~0.3333333333333333)
  ::  argument (real argument = absolute value)
  ++  arg  abs
  --
::  half precision
++  rh
  ^|
  |_  $:  r=$?(%n %u %d %z)   :: round nearest, up, down, to zero
          rtol=_.~~1e-2       :: relative tolerance for precision of operations
      ==
  ::  mathematics constants to half precision
  ::  OEIS A019692
  ++  tau  .6.28
  ::  OEIS A000796
  ++  pi  .3.14
  ::  OEIS A001113
  ++  e  .2.719
  ::  OEIS A001622
  ++  phi  .1.618
  ::  OEIS A002193
  ++  sqt2  .1.414
  ::  OEIS A010503
  ++  invsqt2  .707
  ::  OEIS A002162
  ++  log2  .0.6934
  ++  invlog2  .1.443
  ::  OEIS A002392
  ++  log10  .2.303
  ::
  ++  huge  `@rh`0x7fe0  ::  6.55957e+04
  ++  tiny  `@rh`0x1     ::  6.10352e-05
  ::
  ::  Operations
  ::
  ++  sea  sea:^rh
  ++  bit  bit:^rh
  ++  sun  sun:^rh
  ++  san  san:^rh
  ::++  exp  exp:^rh  :: no pass-through because of exp function
  ++  toi  toi:^rh
  ++  drg  drg:^rh
  ++  grd  grd:^rh
  ::
  ::  Comparison
  ::
  ++  lth  lth:^rh
  ++  lte  lte:^rh
  ++  leq  lte:^rh
  ++  equ  equ:^rh
  ++  gte  gte:^rh
  ++  gth  gth:^rh
  ++  geq  gte:^rh
  ++  neq  |=([a=@rh b=@rh] ^-(? !(equ:^rh a b)))
  ++  isclose
    |=  [p=@rh r=@rh]
    (lth (abs (sub p r)) rtol)
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
  ::  use equality rather than isclose here
  ++  isint
    |=  x=@rh  ^-  ?
    (equ x (san (need (toi x))))
  ::
  ::  Algebraic
  ::
  ++  add  add:^rh
  ++  sub  sub:^rh
  ++  mul  mul:^rh
  ++  div  div:^rh
  ++  fma  fma:^rh
  ++  sig  |=(x=@rh =(0 (rsh [0 15] x)))
  ++  sgn  sig
  ++  neg  |=(x=@rh (sub .~~0 x))
  ++  factorial
    |=  x=@rh  ^-  @rh
    ?>  (gth x .~~0)
    =/  t=@rh  .~~1
    ?:  (isclose x .~~0)
      t
    |-  ^-  @rh
    ?:  (isclose x .~~1)
      t
    $(x (sub x .~~1), t (mul t x))
  ++  abs
    |=  x=@rh  ^-  @rh
    ?:((sgn x) x (neg x))
  ::  exponential
  ::
  ++  exp
    |=  x=@rh  ^-  @rh
    ::  filter out non-finite arguments
    ?:  =(x 0x0)  .~~1
    ::    check infinities
    ?:  =(x 0x7c00)  `@rh`0x7c00  :: exp(+inf) -> inf
    ?:  =(x 0xfc00)  .~~0.0       :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gth (dis 0x7e00 x) 0)  `@rh`0x7e00  :: exp(NaN) -> NaN
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
  ::  restricted power for integers only
  ::
  ++  pow-n
    |=  [x=@rh n=@rh]  ^-  @rh
    ?:  =(n .~~0)  .~~1
    =/  p  x
    |-  ^-  @rh
    ?:  (lth n .~~2)
      p
    $(n (sub n .~~1), p (mul p x))
  ::  natural logarithm, z > 0
  ::
  ++  log
    |=  z=@rh  ^-  @rh
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(z 0x7c00)  `@rh`0x7c00  :: exp(+inf) -> inf
    ?:  =(z 0xfc00)  .~~0.0       :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gth (dis 0x7e00 z) 0)  `@rh`0x7e00  :: exp(NaN) -> NaN
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
  ::  general power, based on logarithms (slower)
  ::  x^n = exp(n ln x)
  ++  pow
    |=  [x=@rh n=@rh]  ^-  @rh
    ::  fall through on integers (faster)
    ?:  =(n (san (need (toi n))))  (pow-n x (san (need (toi n))))
    (exp (mul n (log x)))
  ::  square root
  ++  sqrt  sqt
  ++  sqt
    |=  x=@rh  ^-  @rh
    ?>  (sgn x)
    (pow x .~~0.5)
  ::  cube root
  ++  cbrt  cbt
  ++  cbt
    |=  x=@rh  ^-  @rh
    ?>  (sgn x)
    (pow x .~~0.3333)
  ::  argument (real argument = absolute value)
  ++  arg  abs
  --
::  quad precision
++  rq
  ^|
  |_  $:  r=$?(%n %u %d %z)   :: round nearest, up, down, to zero
          rtol=_.~~~1e-20     :: relative tolerance for precision of operations
      ==
  ::  mathematics constants to quad precision
  ::  OEIS A019692
  ++  tau  .~~~6.2831853071795864769252867665590056
  ::  OEIS A000796
  ++  pi  .~~~3.1415926535897932384626433832795028
  ::  OEIS A001113
  ++  e  .~~~2.7182818284590452353602874713526623
  ::  OEIS A001622
  ++  phi  .~~~1.6180339887498948482045868343656382
  ::  OEIS A002193
  ++  sqt2  .~~~1.414213562373095048801688724209698
  ::  OEIS A010503
  ++  invsqt2  .~~~0.707106781186547524400844362104849
  ::  OEIS A002162
  ++  log2  .~~~0.6931471805599453094172321214581766
  ++  invlog2  .~~~1.442695040888963387004650940070860  :: TODO check
  ::  OEIS A002392
  ++  log10  .~~~2.302585092994045684017991454684364
  ::
  ++  huge  `@rq`0x7ffe.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff  ::  1.18973149535723176508575932662800702e4932
  ++  tiny  `@rq`0x1.0000.0000.0000.0000.0000.0000.0000.0000     ::  3.36210314311209350626267781732175260e-4932
  ::
  ::  Operations
  ::
  ++  sea  sea:^rq
  ++  bit  bit:^rq
  ++  sun  sun:^rq
  ++  san  san:^rq
  ::++  exp  exp:^rq  :: no pass-through because of exp function
  ++  toi  toi:^rq
  ++  drg  drg:^rq
  ++  grd  grd:^rq
  ::
  ::  Comparison
  ::
  ++  lth  lth:^rq
  ++  lte  lte:^rq
  ++  leq  lte:^rq
  ++  equ  equ:^rq
  ++  gte  gte:^rq
  ++  gth  gth:^rq
  ++  geq  gte:^rq
  ++  neq  |=([a=@rq b=@rq] ^-(? !(equ:^rq a b)))
  ++  isclose
    |=  [p=@rq r=@rq]
    (lth (abs (sub p r)) rtol)
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
  ::  use equality rather than isclose here
  ++  isint
    |=  x=@rq  ^-  ?
    (equ x (san (need (toi x))))
  ::
  ::  Algebraic
  ::
  ++  add  add:^rq
  ++  sub  sub:^rq
  ++  mul  mul:^rq
  ++  div  div:^rq
  ++  fma  fma:^rq
  ++  sig  |=(x=@rq =(0 (rsh [0 127] x)))
  ++  sgn  sig
  ++  neg  |=(x=@rq (sub .~~~0 x))
  ++  factorial
    |=  x=@rq  ^-  @rq
    ?>  (gth x .~~~0)
    =/  t=@rq  .~~~1
    ?:  (isclose x .~~~0)
      t
    |-  ^-  @rq
    ?:  (isclose x .~~~1)
      t
    $(x (sub x .~~~1), t (mul t x))
  ++  abs
    |=  x=@rq  ^-  @rq
    ?:((sgn x) x (neg x))
  ::  exponential
  ::
  ++  exp
    |=  x=@rq  ^-  @rq
    ::  filter out non-finite arguments
    ?:  =(x 0x0)  .~~~1
    ::    check infinities
    ?:  =(x 0x7fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000  :: exp(+inf) -> inf
    ?:  =(x 0xffff.0000.0000.0000.0000.0000.0000.0000)  .~~~0.0      :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gth (dis 0x7fff.8000.0000.0000.0000.0000.0000.0000 x) 0)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000  :: exp(NaN) -> NaN
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
  ::  restricted power for integers only
  ::
  ++  pow-n
    |=  [x=@rq n=@rq]  ^-  @rq
    ?:  =(n .~~~0)  .~~~1
    =/  p  x
    |-  ^-  @rq
    ?:  (lth n .~~~2)
      p
    $(n (sub n .~~~1), p (mul p x))
  ::  natural logarithm, z > 0
  ::
  ++  log
    |=  z=@rq  ^-  @rq
    ::  filter out non-finite arguments
    ::    check infinities
    ?:  =(z 0x7fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000  :: exp(+inf) -> inf
    ?:  =(z 0xffff.0000.0000.0000.0000.0000.0000.0000)  .~~~0.0      :: exp(-inf) -> 0
    ::    check NaN
    ?.  (^gth (dis 0x7fff.8000.0000.0000.0000.0000.0000.0000 z) 0)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000  :: exp(NaN) -> NaN
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
  ::  general power, based on logarithms (slower)
  ::  x^n = exp(n ln x)
  ++  pow
    |=  [x=@rq n=@rq]  ^-  @rq
    ::  fall through on integers (faster)
    ?:  =(n (san (need (toi n))))  (pow-n x (san (need (toi n))))
    (exp (mul n (log x)))
  ::  square root
  ++  sqrt  sqt
  ++  sqt
    |=  x=@rq  ^-  @rq
    ?>  (sgn x)
    (pow x .~~~0.5)
  ::  cube root
  ++  cbrt  cbt
  ++  cbt
    |=  x=@rq  ^-  @rq
    ?>  (sgn x)
    (pow x .~~~0.3333)
  ::  argument (real argument = absolute value)
  ++  arg  abs
  --
::  reference values
++  reference-core
  |%
  ::  hardcoded string constants for your viewing pleasure
  ::  OEIS A019692
  ++  tau    .~~~6.28318530717958647692528676655900576839433879875021164194988918461563281257241799625606965068423413596428
  ::  OEIS A000796
  ++  pi     .~~~3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214
  ::  OEIS A001113
  ++  e      .~~~2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746
  ::  OEIS A001622
  ++  phi    .~~~1.61803398874989484820458683436563811772030917980576286213544862270526046281890244970720720418939113748475
  ::  OEIS  A002193
  ++  sqt2  .~~~1.41421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157273
  ::  OEIS A010503
  ++  invsqt2  .~~~0.70710678118654752440084436210484903928483593768847403658833986899536623923105351942519376716382086
  ::  OEIS A002162
  ++  log2    .~~~0.69314718055994530941723212145817656807550013436025525412068000949339362196969471560586332699641868754
  ::  OEIS A002392
  ++  log10   .~~~2.30258509299404568401799145468436420760110148862877297603332790096757260967735248023599726645985502929
  --
--
