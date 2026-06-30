  ::
::::  Mathematical library
::
::  Pure Hoon implementations of (often naive) formally correct algorithms.
::
::  This file's value is the %math engine core (via =< math below), so users
::  still write `/+ math` and `(exp:rd:math ...)` / `~(exp rd:math [%n rtol])`.
::
=<  math
~%  %non  ..part  ~  :: nest non in hex for now
|%
::    $rounding-mode:  the four IEEE modes the @r precision doors accept
+$  rounding-mode  ?(%n %u %d %z)
::    +math:  the math engine core
++  math
  ^|
  =+  [rnd=*rounding-mode]
  ~/  %math
  |%
  ++  rs
    ~/  %rs
    ^|
    |_  $:  r=$?(%n %u %d %z)   :: round nearest, up, down, to zero
            rtol=_.1e-5         :: relative tolerance: scales with |r|
            atol=_.0            :: absolute tolerance floor (default 0)
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
    ++  huge  `@rs`0x7f7f.ffff  ::  3.40282346638528859812e+38
    ::    +tiny:  @rs
    ::
    ::  Returns the smallest representable positive (subnormal) number, 2^-149.
    ::  (Not the smallest NORMAL, which is 2^-126 = .1.1754944e-38.)
    ::    Examples
    ::      > tiny
    ::      .1e-45
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
    ++  sun  ~(sun ^rs r)
    ::    +san:  @sd -> @rs
    ::
    ::  Returns the floating-point atom of a signed integer atom.
    ::    Examples
    ::      > (san --1)
    ::      .1
    ::      > (san -1)
    ::      .-1
    ::  Source
    ++  san  ~(san ^rs r)
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
    ++  toi  ~(toi ^rs r)
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
    ++  drg  ~(drg ^rs r)
    ::    +grd:  dn -> @rs
    ::
    ::  Returns the floating-point atom of a decimal form.
    ::    Examples
    ::      > (grd [%d s=%.y e=--0 a=1])
    ::      .1
    ::      > (grd [%d s=%.y e=-1 a=11])
    ::      .1.1
    ::  Source
    ++  grd  ~(grd ^rs r)
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
    ::    +is-close:  [@rs @rs] -> ?
    ::
    ::  Returns `%.y` if `|p - r| < atol + rtol × |r|`, combining an absolute
    ::  floor `atol` with a relative tolerance `rtol` (both from the +rs door).
    ::    Examples
    ::      > (is-close .1 .2)
    ::      %.n
    ::      > (is-close .1 .1.000001)
    ::      %.y
    ::      > (~(is-close rs [%z .1e-8]) .1 .1.000001)
    ::      %.n
    ::  Source
    ++  is-close
      |=  [p=@rs r=@rs]
      (lth (abs (sub p r)) (add atol (mul rtol (abs r))))
    ::    +all-close:  [@rs (list @rs)] -> ?
    ::
    ::  Returns `%.y` if `|p - r| < atol + rtol × |r|` for all `r` in `q`
    ::  (both tolerances provided by the +rs door).
    ::    Examples
    ::      > (all-close .1 ~[.1 .2])
    ::      %.n
    ::      > (all-close .1 ~[.1 .1.000001])
    ::      %.y
    ::      > (~(all-close rs [%z .1e-8]) .1 ~[.1 .1000001])
    ::      %.n
    ::  Source
    ++  all-close
      |=  [p=@rs q=(list @rs)]
      =/  i  0
      =/  n  (lent q)
      |-  ^-  ?
      ?:  =(n i)
        %.y
      ?.  (is-close p (snag i q))
        %.n
      $(i +(i))
    ::    +is-int:  @rs -> ?
    ::
    ::  Returns whether a floating-point value is an integer (no fractional part).
    ::    Examples
    ::      > (is-int .1)
    ::      %.y
    ::      > (is-int .1.1)
    ::      %.n
    ::  Source
    ++  is-int
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
    ++  add  ~(add ^rs r)
    ::    +sub:  [@rs @rs] -> @rs
    ::
    ::  Returns the difference of two floating-point atoms.
    ::    Examples
    ::      > (sub .1 .2)
    ::      .-1
    ::  Source
    ++  sub  ~(sub ^rs r)
    ::    +mul:  [@rs @rs] -> @rs
    ::
    ::  Returns the product of two floating-point atoms.
    ::    Examples
    ::      > (mul .1 .2)
    ::      .2
    ::      > (mul .2 .3)
    ::      .6
    ::  Source
    ++  mul  ~(mul ^rs r)
    ::    +div:  [@rs @rs] -> @rs
    ::
    ::  Returns the quotient of two floating-point atoms.
    ::    Examples
    ::      > (div .1 .2)
    ::      .0.5
    ::  Source
    ++  div  ~(div ^rs r)
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
      (sub a (mul b (san (need (toi (div a b))))))  ::  a - b * round_r(a/b); round mode is the door's rnd
    ::    +fma:  [@rs @rs @rs] -> @rs
    ::
    ::  Returns the fused multiply-add of three floating-point atoms.
    ::    Examples
    ::      > (fma .1 .2 .3)
    ::      .5
    ::      > (fma .2 .3 .4)
    ::      .10
    ::  Source
    ++  fma  ~(fma ^rs r)
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
    ::  Crashes via `?>` if `x < 0` or if `x` is a positive non-integer (the
    ::  decrement loop passes through zero).
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
      ?:  (lth (abs x) rtol)
        t
      |-  ^-  @rs
      ?:  (lth (abs (sub x .1)) rtol)
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
      ~/  %exp
      |=  x=@rs  ^-  @rs
      ::  Chebyshev: x = k*ln2 + r (Cody-Waite reduction); exp(x) = 2^k * P(r),
      ::  P a degree-6 minimax polynomial faithful to <=1 ULP.  Internals are
      ::  forced to round-nearest-even: a correctly-rounded transcendental does
      ::  not take a rounding-mode axis (and the SoftFloat jet will match this).
      ::  +scale2 is a correctly-rounded ldexp that stays exact across the normal
      ::  range and rounds exactly once into the overflow/subnormal tails.
      =/  pow2  |=(j=@s `@rs`(lsh [0 23] (abs:si (sum:si j --127))))
      =/  scale2
        |=  [p=@rs k=@s]  ^-  @rs
        ?:  (syn:si (dif:si k --128))                 :: k>127: (p*2^127)*2^(k-127)
          (~(mul ^rs %n) (~(mul ^rs %n) p (pow2 --127)) (pow2 (dif:si k --127)))
        ?:  !(syn:si (sum:si k --126))                :: k<-126: (p*2^(k+24))*2^-24
          (~(mul ^rs %n) (~(mul ^rs %n) p (pow2 (sum:si k --24))) (pow2 -24))
        (~(mul ^rs %n) p (pow2 k))
      ?:  !(~(equ ^rs %n) x x)    `@rs`0x7fc0.0000    :: exp(NaN) -> NaN
      ?:  =(x `@rs`0x7f80.0000)   `@rs`0x7f80.0000    :: exp(+inf) -> inf
      ?:  =(x `@rs`0xff80.0000)   `@rs`0x0            :: exp(-inf) -> 0
      =/  log2e  `@rs`0x3fb8.aa3b
      =/  ln2hi  `@rs`0x3f31.7200
      =/  ln2lo  `@rs`0x35bf.be8e
      =/  k=@s   (need (~(toi ^rs %n) (~(mul ^rs %n) x log2e)))
      ?:  (syn:si (dif:si k --129))   `@rs`0x7f80.0000  :: overflow -> inf
      ?:  !(syn:si (sum:si k --150))  `@rs`0x0          :: underflow -> 0
      =/  ka  (~(sun ^rs %n) (abs:si k))
      =/  kf  ?:((syn:si k) ka (~(sub ^rs %n) .0 ka))   :: k as @rs
      =/  r
        %-  ~(sub ^rs %n)
        :-  (~(sub ^rs %n) x (~(mul ^rs %n) kf ln2hi))
        (~(mul ^rs %n) kf ln2lo)
      =/  cs=(list @rs)
        :~  `@rs`0x3f80.0000  `@rs`0x3f80.0000  `@rs`0x3f00.0000
            `@rs`0x3e2a.aa02  `@rs`0x3d2a.aa56  `@rs`0x3c09.37d3
            `@rs`0x3ab6.ba99
        ==
      =/  p  (roll (flop cs) |=([c=@rs acc=@rs] (~(add ^rs %n) (~(mul ^rs %n) acc r) c)))
      (scale2 p k)
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
      ~/  %sin
      |=  x=@rs  ^-  @rs
      ::  Reduce x = q*(pi/2) + (rhi+rlo) with a 3-part pi/2 (f32 needs the bits),
      ::  then fdlibm sin/cos kernels picked by q&3.  Faithful to <=1 ULP for
      ::  |x| <~ 500.  Round-nearest-even internally (the SoftFloat jet matches).
      ?:  !(~(equ ^rs %n) x x)  `@rs`0x7fc0.0000                          :: NaN
      ?:  |(=(x `@rs`0x7f80.0000) =(x `@rs`0xff80.0000))  `@rs`0x7fc0.0000  :: +-inf
      ?:  |(=(x `@rs`0x0) =(x `@rs`0x8000.0000))  x                       :: +-0 -> +-0
      %-  trig-fin:rs-trig
      [%.y `@rs`(dis x 0x7fff.ffff) (rsh [0 31] x)]
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
      ~/  %cos
      |=  x=@rs  ^-  @rs
      ?:  !(~(equ ^rs %n) x x)  `@rs`0x7fc0.0000
      ?:  |(=(x `@rs`0x7f80.0000) =(x `@rs`0xff80.0000))  `@rs`0x7fc0.0000
      %-  trig-fin:rs-trig
      [%.n `@rs`(dis x 0x7fff.ffff) 0]
    ::  +rs-trig: shared sin/cos engine for the @rs door (see +sin / +cos).
    ++  rs-trig
      |%
      ++  sc  ^-((list @rs) :~(`@rs`0xbe2a.aaab `@rs`0x3c08.8889 `@rs`0xb950.0d01 `@rs`0x3638.ef1d `@rs`0xb2d7.322b))
      ++  cc  ^-((list @rs) :~(`@rs`0x3d2a.aaab `@rs`0xbab6.0b61 `@rs`0x37d0.0d01 `@rs`0xb493.f27e `@rs`0x310f.76c7))
      ++  neg  |=(a=@rs ^-(@rs (~(sub ^rs %n) `@rs`0x0 a)))
      ++  ksin
        |=  [xx=@rs yy=@rs]  ^-  @rs
        =/  z   (~(mul ^rs %n) xx xx)
        =/  r   (roll (flop (tail sc)) |=([c=@rs a=@rs] (~(add ^rs %n) (~(mul ^rs %n) a z) c)))
        =/  v   (~(mul ^rs %n) z xx)
        =/  aa  (~(sub ^rs %n) (~(mul ^rs %n) `@rs`0x3f00.0000 yy) (~(mul ^rs %n) v r))
        =/  bb  (~(sub ^rs %n) (~(mul ^rs %n) z aa) yy)
        =/  dd  (~(sub ^rs %n) bb (~(mul ^rs %n) v (head sc)))
        (~(sub ^rs %n) xx dd)
      ++  kcos
        |=  [xx=@rs yy=@rs]  ^-  @rs
        =/  z   (~(mul ^rs %n) xx xx)
        =/  rc  (roll (flop cc) |=([c=@rs a=@rs] (~(add ^rs %n) (~(mul ^rs %n) a z) c)))
        =/  hz  (~(mul ^rs %n) `@rs`0x3f00.0000 z)
        =/  w2  (~(sub ^rs %n) `@rs`0x3f80.0000 hz)
        =/  aa  (~(sub ^rs %n) (~(sub ^rs %n) `@rs`0x3f80.0000 w2) hz)
        =/  bb  (~(sub ^rs %n) (~(mul ^rs %n) (~(mul ^rs %n) z z) rc) (~(mul ^rs %n) xx yy))
        (~(add ^rs %n) w2 (~(add ^rs %n) aa bb))
      ::  +trig-fin: [is-sin? |x| sign-bit] -> sin x (is-sin?) or cos x
      ++  trig-fin
        |=  [s=? ax=@rs sb=@]  ^-  @rs
        =/  q   (need (~(toi ^rs %n) (~(mul ^rs %n) ax `@rs`0x3f22.f983)))
        =/  qf  (~(sun ^rs %n) (abs:si q))
        =/  r1  (~(sub ^rs %n) ax (~(mul ^rs %n) qf `@rs`0x3fc9.0000))
        =/  r2  (~(sub ^rs %n) r1 (~(mul ^rs %n) qf `@rs`0x39fd.a000))
        =/  w   (~(mul ^rs %n) qf `@rs`0x33a2.2169)
        =/  rhi  (~(sub ^rs %n) r2 w)
        =/  rlo  (~(sub ^rs %n) (~(sub ^rs %n) r2 rhi) w)
        =/  m   (dis (abs:si q) 3)
        =/  ks  (ksin rhi rlo)
        =/  kc  (kcos rhi rlo)
        ?:  s
          =/  v  ?:(=(m 0) ks ?:(=(m 1) kc ?:(=(m 2) (neg ks) (neg kc))))
          ?:(=(sb 1) (neg v) v)
        ?:(=(m 0) kc ?:(=(m 1) (neg ks) ?:(=(m 2) (neg kc) ks)))
      --
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
      ~/  %tan
      |=  x=@rs  ^-  @rs
      (div (sin x) (cos x))
    ::  +asin:  @rs -> @rs
    ::
    ::  Returns the inverse sine of a floating-point atom.
    ::    Examples
    ::      > (asin .0)
    ::      .0
    ::      > (asin .1)
    ::      .1.5707964
    ::      > (asin .0.7)
    ::      .0.7753969
    ::  Source
    ++  asin
      ~/  %asin
      ::  fdlibm rational kernel; see +rs-ainv.  Faithful to <=1 ULP; |x|>1 -> NaN.
      |=  x=@rs  ^-  @rs
      (asn:rs-ainv x)
    ::  +acos:  @rs -> @rs
    ::
    ::  Returns the inverse cosine of a floating-point atom.
    ::    Examples
    ::      > (acos .0)
    ::      .1.5707964
    ::      > (acos .1)
    ::      .0
    ::      > (acos .0.7)
    ::      .0.7953982
    ::  Source
    ++  acos
      ~/  %acos
      |=  x=@rs  ^-  @rs
      (acs:rs-ainv x)
    ::  +rs-ainv: shared asin/acos engine for the @rs door (rational P/Q kernel),
    ::  see +asin / +acos.
    ++  rs-ainv
      |%
      ++  rr
        |=  t=@rs  ^-  @rs
        =/  ps=(list @rs)  :~(`@rs`0x3e2a.aa75 `@rs`0xbd2f.13ba `@rs`0xbc0d.d36b)
        =/  p  (~(mul ^rs %n) t (roll (flop ps) |=([c=@rs a=@rs] (~(add ^rs %n) (~(mul ^rs %n) a t) c))))
        =/  q  (~(add ^rs %n) `@rs`0x3f80.0000 (~(mul ^rs %n) t `@rs`0xbf34.e5ae))
        (~(div ^rs %n) p q)
      ++  asn
        |=  x=@rs  ^-  @rs
        ?:  !(~(equ ^rs %n) x x)  `@rs`0x7fc0.0000
        =/  sgn  (rsh [0 31] x)
        =/  ax   `@rs`(dis x 0x7fff.ffff)
        ?:  (~(gth ^rs %n) ax `@rs`0x3f80.0000)  `@rs`0x7fc0.0000
        ?:  =(ax `@rs`0x3f80.0000)
          (~(add ^rs %n) (~(mul ^rs %n) x `@rs`0x3fc9.0fdb) (~(mul ^rs %n) x `@rs`0xb33b.bd2e))
        ?:  (~(lth ^rs %n) ax `@rs`0x3f00.0000)
          ?:  (~(lth ^rs %n) ax `@rs`0x3980.0000)  x
          (~(add ^rs %n) x (~(mul ^rs %n) x (rr (~(mul ^rs %n) x x))))
        =/  w  (~(sub ^rs %n) `@rs`0x3f80.0000 ax)
        =/  t  (~(mul ^rs %n) w `@rs`0x3f00.0000)
        =/  r  (rr t)
        =/  s  (sqt t)
        ?:  (~(gte ^rs %n) ax `@rs`0x3f79.999a)
          =/  res  (~(sub ^rs %n) `@rs`0x3fc9.0fdb (~(sub ^rs %n) (~(mul ^rs %n) `@rs`0x4000.0000 (~(add ^rs %n) s (~(mul ^rs %n) s r))) `@rs`0xb33b.bd2e))
          ?:(=(sgn 1) (~(sub ^rs %n) `@rs`0x0 res) res)
        =/  df  `@rs`(dis s 0xffff.f000)
        =/  c   (~(div ^rs %n) (~(sub ^rs %n) t (~(mul ^rs %n) df df)) (~(add ^rs %n) s df))
        =/  p2  (~(sub ^rs %n) (~(mul ^rs %n) `@rs`0x4000.0000 (~(mul ^rs %n) s r)) (~(sub ^rs %n) `@rs`0xb33b.bd2e (~(mul ^rs %n) `@rs`0x4000.0000 c)))
        =/  q2  (~(sub ^rs %n) `@rs`0x3f49.0fdb (~(mul ^rs %n) `@rs`0x4000.0000 df))
        =/  res  (~(sub ^rs %n) `@rs`0x3f49.0fdb (~(sub ^rs %n) p2 q2))
        ?:(=(sgn 1) (~(sub ^rs %n) `@rs`0x0 res) res)
      ++  acs
        |=  x=@rs  ^-  @rs
        ?:  !(~(equ ^rs %n) x x)  `@rs`0x7fc0.0000
        =/  neg  (rsh [0 31] x)
        =/  ax   `@rs`(dis x 0x7fff.ffff)
        ?:  (~(gth ^rs %n) ax `@rs`0x3f80.0000)  `@rs`0x7fc0.0000
        ?:  =(ax `@rs`0x3f80.0000)
          ?:  =(neg 0)  `@rs`0x0
          (~(add ^rs %n) `@rs`0x4049.0fdb (~(mul ^rs %n) `@rs`0x4000.0000 `@rs`0xb33b.bd2e))
        ?:  (~(lth ^rs %n) ax `@rs`0x3f00.0000)
          ?:  (~(lth ^rs %n) ax `@rs`0x3280.0000)  `@rs`0x3fc9.0fdb
          =/  z  (~(mul ^rs %n) x x)
          =/  r  (rr z)
          (~(sub ^rs %n) `@rs`0x3fc9.0fdb (~(sub ^rs %n) x (~(sub ^rs %n) `@rs`0xb33b.bd2e (~(mul ^rs %n) x r))))
        ?:  =(neg 1)
          =/  z  (~(mul ^rs %n) (~(add ^rs %n) `@rs`0x3f80.0000 x) `@rs`0x3f00.0000)
          =/  s  (sqt z)
          =/  r  (rr z)
          =/  w  (~(sub ^rs %n) (~(mul ^rs %n) r s) `@rs`0xb33b.bd2e)
          (~(sub ^rs %n) `@rs`0x4049.0fdb (~(mul ^rs %n) `@rs`0x4000.0000 (~(add ^rs %n) s w)))
        =/  z  (~(mul ^rs %n) (~(sub ^rs %n) `@rs`0x3f80.0000 x) `@rs`0x3f00.0000)
        =/  s  (sqt z)
        =/  r  (rr z)
        (~(mul ^rs %n) `@rs`0x4000.0000 (~(add ^rs %n) s (~(mul ^rs %n) s r)))
      --
    ::  +atan:  @rs -> @rs
    ::
    ::  Returns the inverse tangent of a floating-point atom.
    ::    Examples
    ::      > (atan .1)
    ::      .0.7853976
    ::      > (atan .2)
    ::      .1.1071494
    ::      > (atan pi)
    ::      .1.2626364
    ::  Source
    ++  atan
      ~/  %atan
      ::  fdlibm breakpoint reduction + minimax poly; odd.  Round-nearest-even
      ::  internally (the SoftFloat jet matches).
      |=  x=@rs  ^-  @rs
      ?:  !(~(equ ^rs %n) x x)      `@rs`0x7fc0.0000   :: NaN
      ?:  =(x `@rs`0x7f80.0000)     `@rs`0x3fc9.0fdb   :: +inf -> pi/2
      ?:  =(x `@rs`0xff80.0000)     `@rs`0xbfc9.0fdb   :: -inf -> -pi/2
      ?:  |(=(x `@rs`0x0) =(x `@rs`0x8000.0000))  x    :: +-0 -> +-0
      =/  neg  (rsh [0 31] x)
      =/  r    (ker:rs-atan `@rs`(dis x 0x7fff.ffff))
      ?:(=(neg 1) (~(sub ^rs %n) `@rs`0x0 r) r)
    ::  +rs-atan: atan kernel for the @rs door (reduction + poly), see +atan.
    ++  rs-atan
      |%
      ++  at
        ^-  (list @rs)
        :~  `@rs`0x3eaa.aaa9  `@rs`0xbe4c.ca98  `@rs`0x3e11.f50d
            `@rs`0xbdda.1247  `@rs`0x3d7c.ac25
        ==
      ++  atred
        |=  ax=@rs  ^-  [xr=@rs hi=@rs lo=@rs dir=?]
        =/  one  `@rs`0x3f80.0000
        =/  two  `@rs`0x4000.0000
        =/  ohf  `@rs`0x3fc0.0000
        ?:  (~(lth ^rs %n) ax `@rs`0x3ee0.0000)
          [ax `@rs`0x0 `@rs`0x0 %.y]
        ?:  (~(lth ^rs %n) ax `@rs`0x3f30.0000)
          :*  (~(div ^rs %n) (~(sub ^rs %n) (~(add ^rs %n) ax ax) one) (~(add ^rs %n) two ax))
              `@rs`0x3eed.6338  `@rs`0x31ac.376a  %.n
          ==
        ?:  (~(lth ^rs %n) ax `@rs`0x3f98.0000)
          :*  (~(div ^rs %n) (~(sub ^rs %n) ax one) (~(add ^rs %n) ax one))
              `@rs`0x3f49.0fdb  `@rs`0xb2bb.bd2e  %.n
          ==
        ?:  (~(lth ^rs %n) ax `@rs`0x401c.0000)
          :*  (~(div ^rs %n) (~(sub ^rs %n) ax ohf) (~(add ^rs %n) one (~(mul ^rs %n) ohf ax)))
              `@rs`0x3f7b.985f  `@rs`0xb2d7.e096  %.n
          ==
        :*  (~(div ^rs %n) `@rs`0xbf80.0000 ax)
            `@rs`0x3fc9.0fdb  `@rs`0xb33b.bd2e  %.n
        ==
      ++  ker
        |=  ax=@rs  ^-  @rs
        =/  q  (atred ax)
        =/  z  (~(mul ^rs %n) xr.q xr.q)
        =/  s  (~(mul ^rs %n) z (roll (flop at) |=([c=@rs a=@rs] (~(add ^rs %n) (~(mul ^rs %n) a z) c))))
        ?:  dir.q  (~(sub ^rs %n) xr.q (~(mul ^rs %n) xr.q s))
        (~(sub ^rs %n) hi.q (~(sub ^rs %n) (~(sub ^rs %n) (~(mul ^rs %n) xr.q s) lo.q) xr.q))
      --
    ::  +atan2:  [@rs @rs] -> @rs
    ::
    ::  Returns the inverse tangent of a floating-point coordinate.
    ::  Returns 0 for NaN inputs (does not propagate NaN).  The case (0,0) returns 0.
    ::    Examples
    ::      > (atan2 .0 .1)
    ::      .0
    ::      > (atan2 .-1 .0)
    ::      .-1.5707964
    ::      > (atan2 .0.5 .-0.5)
    ::      .2.356195
    ::  Source
    ++  atan2
      ~/  %atan2
      |=  [y=@rs x=@rs]  ^-  @rs
      ?:  (gth x .0)
        (atan (div y x))
      ?:  &((lth x .0) (gte y .0))
        (add (atan (div y x)) pi)
      ?:  &((lth x .0) (lth y .0))
        (sub (atan (div y x)) pi)
      ?:  &(=(.0 x) (gth y .0))
        (div pi .2)
      ?:  &(=(.0 x) (lth y .0))
        (mul .-1 (div pi .2))
      .0  ::  undefined
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
      ~/  %pow-n
      |=  [x=@rs n=@rs]  ^-  @rs
      ?:  =(n .0)  .1
      ?>  &((gth n .0) (is-int n))
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
      ~/  %log
      |=  x=@rs  ^-  @rs
      ::  Reduce x = 2^e * m with m in [sqrt(1/2), sqrt(2)); then
      ::  log(x) = e*ln2 + log(1+f), f = m-1, s = f/(2+f),
      ::  log(1+f) = f - s*(f - 2z*P2(z)), z = s*s, P2 the atanh series
      ::  1/3 + z/5 + z^2/7 + ...  Faithful to <=1 ULP; round-nearest-even
      ::  internally (the SoftFloat jet will match this bit-for-bit).
      ?:  !(~(equ ^rs %n) x x)         `@rs`0x7fc0.0000   :: log(NaN) -> NaN
      ?:  =(x `@rs`0x7f80.0000)        `@rs`0x7f80.0000   :: log(+inf) -> inf
      ?:  |(=(x `@rs`0x0) =(x `@rs`0x8000.0000))  `@rs`0xff80.0000  :: log(+-0) -> -inf
      ?:  =(1 (rsh [0 31] x))          `@rs`0x7fc0.0000   :: log(x<0) -> NaN
      =/  sub  =(0 (dis 0xff (rsh [0 23] x)))                  :: subnormal?
      =/  xx   ?:(sub (~(mul ^rs %n) x `@rs`0x4b80.0000) x)    :: *2^24
      =/  ae   ?:(sub -24 --0)
      =/  b    `@`xx
      =/  ef   (dif:si (new:si %.y (dis 0xff (rsh [0 23] b))) --127)
      =/  m    `@rs`(con (dis b 0x7f.ffff) 0x3f80.0000)
      =/  big  (~(gte ^rs %n) m `@rs`0x3fb5.04f3)              :: m >= sqrt(2)
      =?  m    big  (~(mul ^rs %n) m `@rs`0x3f00.0000)         :: m * 0.5
      =?  ef   big  (sum:si ef --1)
      =.  ef   (sum:si ef ae)
      =/  f    (~(sub ^rs %n) m `@rs`0x3f80.0000)
      =/  s    (~(div ^rs %n) f (~(add ^rs %n) m `@rs`0x3f80.0000))
      =/  z    (~(mul ^rs %n) s s)
      =/  cs=(list @rs)
        :~  `@rs`0x3eaa.aaab  `@rs`0x3e4c.cccd  `@rs`0x3e12.4925
            `@rs`0x3de3.8e39  `@rs`0x3dba.2e8c
        ==
      =/  p2  (roll (flop cs) |=([c=@rs acc=@rs] (~(add ^rs %n) (~(mul ^rs %n) acc z) c)))
      =/  r   (~(mul ^rs %n) (~(add ^rs %n) z z) p2)
      =/  l1  (~(sub ^rs %n) f (~(mul ^rs %n) s (~(sub ^rs %n) f r)))
      =/  efa   (~(sun ^rs %n) (abs:si ef))
      =/  ef-f  ?:((syn:si ef) efa (~(sub ^rs %n) .0 efa))     :: e as @rs
      =/  hi  (~(mul ^rs %n) ef-f `@rs`0x3f31.7200)            :: e*ln2hi
      =/  lo  (~(mul ^rs %n) ef-f `@rs`0x35bf.be8e)            :: e*ln2lo
      (~(add ^rs %n) hi (~(add ^rs %n) l1 lo))
    ::    +log-10:  @rs -> @rs
    ::
    ::  Returns the base-10 logarithm of a floating-point atom.
    ::    Examples
    ::      > (log-10 .0.1)
    ::      .-0.999989
    ::      > (log-10 .2)
    ::      .0.30102932
    ::      > (~(log-10 rs [%z .1e-8]) .2)
    ::      .0.3010301
    ::      > (log-10 .inf)
    ::      .inf
    ::  Source
    ++  log-10
      ~/  %log-10
      ::  e*log10(2) + log(m)/ln10, reusing +lr so the integer part is added with
      ::  no division rounding (more accurate than log(x)/ln10).
      |=  x=@rs  ^-  @rs
      ?:  !(~(equ ^rs %n) x x)  `@rs`0x7fc0.0000
      ?:  =(x `@rs`0x7f80.0000)  `@rs`0x7f80.0000
      ?:  |(=(x `@rs`0x0) =(x `@rs`0x8000.0000))  `@rs`0xff80.0000
      ?:  =(1 (rsh [0 31] x))  `@rs`0x7fc0.0000
      =/  el  (lr x)
      (~(add ^rs %n) (~(mul ^rs %n) ef.el `@rs`0x3e9a.209b) (~(mul ^rs %n) lm.el `@rs`0x3ede.5bd9))
    ::    +log-2:  @rs -> @rs
    ::
    ::  Returns the base-2 logarithm of a floating-point atom.
    ::    Examples
    ::      > (log-2 .0.1)
    ::      .-3.321928
    ::      > (log-2 .2)
    ::      .1.5849625
    ::  Source
    ++  log-2
      ~/  %log-2
      ::  e + log(m)/ln2 (integer part exact); see +lr.
      |=  x=@rs  ^-  @rs
      ?:  !(~(equ ^rs %n) x x)  `@rs`0x7fc0.0000
      ?:  =(x `@rs`0x7f80.0000)  `@rs`0x7f80.0000
      ?:  |(=(x `@rs`0x0) =(x `@rs`0x8000.0000))  `@rs`0xff80.0000
      ?:  =(1 (rsh [0 31] x))  `@rs`0x7fc0.0000
      =/  el  (lr x)
      (~(add ^rs %n) ef.el (~(mul ^rs %n) lm.el `@rs`0x3fb8.aa3b))
    ::  +lr: log reduction for finite positive x -> [e (as @rs), log(mantissa)].
    ++  lr
      |=  x=@rs  ^-  [ef=@rs lm=@rs]
      =/  sub  =(0 (dis 0xff (rsh [0 23] x)))
      =/  xx   ?:(sub (~(mul ^rs %n) x `@rs`0x4b80.0000) x)
      =/  ae   ?:(sub -24 --0)
      =/  b    `@`xx
      =/  e    (dif:si (new:si %.y (dis 0xff (rsh [0 23] b))) --127)
      =/  m    `@rs`(con (dis b 0x7f.ffff) 0x3f80.0000)
      =/  big  (~(gte ^rs %n) m `@rs`0x3fb5.04f3)
      =?  m    big  (~(mul ^rs %n) m `@rs`0x3f00.0000)
      =?  e    big  (sum:si e --1)
      =.  e    (sum:si e ae)
      =/  f    (~(sub ^rs %n) m `@rs`0x3f80.0000)
      =/  s    (~(div ^rs %n) f (~(add ^rs %n) m `@rs`0x3f80.0000))
      =/  z    (~(mul ^rs %n) s s)
      =/  cs=(list @rs)
        :~  `@rs`0x3eaa.aaab  `@rs`0x3e4c.cccd  `@rs`0x3e12.4925
            `@rs`0x3de3.8e39  `@rs`0x3dba.2e8c
        ==
      =/  p2  (roll (flop cs) |=([c=@rs a=@rs] (~(add ^rs %n) (~(mul ^rs %n) a z) c)))
      =/  r   (~(mul ^rs %n) (~(add ^rs %n) z z) p2)
      =/  l1  (~(sub ^rs %n) f (~(mul ^rs %n) s (~(sub ^rs %n) f r)))
      =/  efa  (~(sun ^rs %n) (abs:si e))
      =/  ef   ?:((syn:si e) efa (~(sub ^rs %n) .0 efa))
      [ef l1]
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
      ~/  %pow
      |=  [x=@rs n=@rs]  ^-  @rs
      ::  fall through on positive integers (faster)
      ?:  &(=(n (san (need (toi n)))) (gth n .0))  (pow-n x (san (need (toi n))))
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
    ::      .1.4142135
    ::      > (sqt .1e5)
    ::      .316.22775
    ::  Source
    ++  sqt
      ~/  %sqt
      ::  Correctly-rounded: delegate to the stdlib (SoftFloat) f32 square root.
      |=  x=@rs  ^-  @rs
      (sqt:^rs x)
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
      ~/  %cbt
      ::  cbrt(x) = sign(x) * exp(log|x| / 3); defined for all reals (unlike pow).
      |=  x=@rs  ^-  @rs
      ?:  !(~(equ ^rs %n) x x)  x                    :: NaN -> NaN
      ?:  |(=(x `@rs`0x0) =(x `@rs`0x8000.0000))  x  :: +-0 -> +-0
      =/  ax  `@rs`(dis x 0x7fff.ffff)
      =/  r   (exp (~(mul ^rs %n) (log ax) `@rs`0x3eaa.aaab))
      ?:(=(1 (rsh [0 31] x)) (~(sub ^rs %n) `@rs`0x0 r) r)
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
    ::    +round:  [@rs @ud] -> @rs
    ::
    ::  Returns the floating-point atom rounded to a given number of significant
    ::  figures (n=1 = 1 sig fig).
    ::    Examples
    ::      > (round .1 0)
    ::      .1
    ::      > (round .1.11 1)
    ::      .1
    ::      > (round .1.11 2)
    ::      .1.1
    ::      > (round .1.11 3)
    ::      .1.11
    ::  Source
    ++  round
      |=  [x=@rs n=@ud]  ^-  @rs
      ?:  =(.0 x)  .0
      ::  Calculate the order of magnitude.
      =/  oom  (san (need (toi (log-10 (abs x)))))
      ::  Calculate the scaling factor.
      =/  scaling  (pow .10 :(sub (sun n) oom .1))
      ::  Round the mantissa to desired significant digits.
      =/  rnd-mantissa  (round-bankers (mul x scaling))
      ::  Convert back to the original scale.
      (div rnd-mantissa scaling)
    ::    +round-places:  [@rs @ud] -> @rs
    ::
    ::  Returns the floating-point atom rounded to a given number of decimal
    ::  places.  This is exceptionally sensitive to off-by-one FP rounding error.
    ::    Examples
    ::      > (round-places .1 0)
    ::      .1
    ::      > (round-places .1.11 1)
    ::      .1.1
    ::      > (round-places .1.285 2)
    ::      .1.28
    ::      > (round-places .4.12345 3)
    ::      .4.1229997
    ::  Source
    ++  round-places
      |=  [x=@rs n=@ud]  ^-  @rs
      ::  Calculate the scaling factor.
      =/  scaling  (pow .10 (sun n))
      ::  Scale the number.
      =/  scaled  (mul x scaling)
      ::  Round the mantissa to desired significant digits.
      =/  rnd-mantissa  (round-bankers scaled)
      ::  Convert back to the original scale.
      (div rnd-mantissa scaling)
    ::    +round-bankers:  @rs -> @rs
    ::
    ::  Returns the floating-point atom rounded to the nearest integer, with
    ::  ties rounded to the nearest even integer (banker's rounding).  This is
    ::  exactly what +toi does under round-nearest (%n), which ties to even, so we
    ::  force %n regardless of the door's configured mode.
    ::    Examples
    ::      > (round-bankers .1.5)
    ::      .2
    ::      > (round-bankers .2.5)
    ::      .2
    ::      > (round-bankers .1.49)
    ::      .1
    ::  Source
    ++  round-bankers
      |=  x=@rs  ^-  @rs
      (san (need (~(toi ^rs %n) x)))
    --
  ::  double precision
  ++  rd
    ~/  %rd
    ^|
    |_  $:  r=$?(%n %u %d %z)   :: round nearest, up, down, to zero
            rtol=_.~1e-10       :: relative tolerance: scales with |r|
            atol=_.~0           :: absolute tolerance floor (default 0)
        ==
    ::  mathematics constants to double precision
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
    ++  sun  ~(sun ^rd r)
    ::    +san:  @sd -> @rd
    ::
    ::  Returns the floating-point atom of a signed integer atom.
    ::    Examples
    ::      > (san --1)
    ::      .~1
    ::      > (san -1)
    ::      .~-1
    ::  Source
    ++  san  ~(san ^rd r)
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
    ++  toi  ~(toi ^rd r)
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
    ++  drg  ~(drg ^rd r)
    ::    +grd:  dn -> @rd
    ::
    ::  Returns the floating-point atom of a decimal form.
    ::    Examples
    ::      > (grd [%d s=%.y e=--0 a=1])
    ::      .~1
    ::      > (grd [%d s=%.y e=-1 a=11])
    ::      .~1.1
    ::  Source
    ++  grd  ~(grd ^rd r)
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
    ::    +is-close:  [@rd @rd] -> ?
    ::
    ::  Returns `%.y` if `|p - r| < atol + rtol × |r|`, combining an absolute
    ::  floor `atol` with a relative tolerance `rtol` (both from the +rd door).
    ::    Examples
    ::      > (is-close .~1 .~2)
    ::      %.n
    ::      > (is-close .~1 .~1.0000001)
    ::      %.n
    ::      > (~(is-close rd [%z .~1e-3]) .~1 .~1.0000001)
    ::      %.y
    ::  Source
    ++  is-close
      |=  [p=@rd r=@rd]
      (lth (abs (sub p r)) (add atol (mul rtol (abs r))))
    ::    +all-close:  [@rd (list @rd)] -> ?
    ::
    ::  Returns `%.y` if `|p - r| < atol + rtol × |r|` for all `r` in `q`
    ::  (both tolerances provided by the +rd door).
    ::    Examples
    ::      > (all-close .~1 ~[.~1 .~2])
    ::      %.n
    ::      > (all-close .~1 ~[.~1 .~1.0000001])
    ::      %.n
    ::      > (~(all-close rd [%z .~1e-3]) .~1 ~[.~1 .~1.0000001])
    ::      %.y
    ::  Source
    ++  all-close
      |=  [p=@rd q=(list @rd)]
      =/  i  0
      =/  n  (lent q)
      |-  ^-  ?
      ?:  =(n i)
        %.y
      ?.  (is-close p (snag i q))
        %.n
      $(i +(i))
    ::    +is-int:  @rd -> ?
    ::
    ::  Returns whether a floating-point value is an integer (no fractional part).
    ::    Examples
    ::      > (is-int .~1)
    ::      %.y
    ::      > (is-int .~1.1)
    ::      %.n
    ::  Source
    ++  is-int
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
    ++  add  ~(add ^rd r)
    ::    +sub:  [@rd @rd] -> @rd
    ::
    ::  Returns the difference of two floating-point atoms.
    ::    Examples
    ::      > (sub .~1 .~2)
    ::      .~-1
    ::  Source
    ++  sub  ~(sub ^rd r)
    ::    +mul:  [@rd @rd] -> @rd
    ::
    ::  Returns the product of two floating-point atoms.
    ::    Examples
    ::      > (mul .~1 .~2)
    ::      .~2
    ::      > (mul .~2 .~3)
    ::      .~6
    ::  Source
    ++  mul  ~(mul ^rd r)
    ::    +div:  [@rd @rd] -> @rd
    ::
    ::  Returns the quotient of two floating-point atoms.
    ::    Examples
    ::      > (div .~1 .~2)
    ::      .~0.5
    ::  Source
    ++  div  ~(div ^rd r)
    ::    +fma:  [@rd @rd @rd] -> @rd
    ::
    ::  Returns the fused multiply-add of three floating-point atoms.
    ::    Examples
    ::      > (fma .~1 .~2 .~3)
    ::      .~5
    ::      > (fma .~2 .~3 .~4)
    ::      .~10
    ::  Source
    ++  fma  ~(fma ^rd r)
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
    ::  Crashes via `?>` if `x < 0` or if `x` is a positive non-integer (the
    ::  decrement loop passes through zero).
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
      ?:  (lth (abs x) rtol)
        t
      |-  ^-  @rd
      ?:  (lth (abs (sub x .~1)) rtol)
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
    ::      .~inf
    ::  Source
    ++  exp
      ~/  %exp
      |=  x=@rd  ^-  @rd
      ::  Chebyshev: x = k*ln2 + r (Cody-Waite reduction); exp(x) = 2^k * P(r),
      ::  P a degree-11 minimax polynomial faithful to <=1 ULP.  Internals are
      ::  forced to round-nearest-even: a correctly-rounded transcendental does
      ::  not take a rounding-mode axis (and the SoftFloat jet will match this).
      ::  +scale2 is a correctly-rounded ldexp that stays exact across the normal
      ::  range and rounds exactly once into the overflow/subnormal tails.
      =/  pow2  |=(j=@s `@rd`(lsh [0 52] (abs:si (sum:si j --1.023))))
      =/  scale2
        |=  [p=@rd k=@s]  ^-  @rd
        ?:  (syn:si (dif:si k --1.024))               :: k>1023: (p*2^1023)*2^(k-1023)
          (~(mul ^rd %n) (~(mul ^rd %n) p (pow2 --1.023)) (pow2 (dif:si k --1.023)))
        ?:  !(syn:si (sum:si k --1.022))              :: k<-1022: (p*2^(k+54))*2^-54
          (~(mul ^rd %n) (~(mul ^rd %n) p (pow2 (sum:si k --54))) (pow2 -54))
        (~(mul ^rd %n) p (pow2 k))
      ?:  !(~(equ ^rd %n) x x)              `@rd`0x7ff8.0000.0000.0000  :: NaN
      ?:  =(x `@rd`0x7ff0.0000.0000.0000)   `@rd`0x7ff0.0000.0000.0000  :: +inf
      ?:  =(x `@rd`0xfff0.0000.0000.0000)   `@rd`0x0                    :: -inf -> 0
      =/  log2e  `@rd`0x3ff7.1547.652b.82fe
      =/  ln2hi  `@rd`0x3fe6.2e42.fee0.0000
      =/  ln2lo  `@rd`0x3dea.39ef.3579.3c76
      =/  k=@s   (need (~(toi ^rd %n) (~(mul ^rd %n) x log2e)))
      ?:  (syn:si (dif:si k --1.025))   `@rd`0x7ff0.0000.0000.0000  :: overflow -> inf
      ?:  !(syn:si (sum:si k --1.075))  `@rd`0x0                    :: underflow -> 0
      =/  ka  (~(sun ^rd %n) (abs:si k))
      =/  kf  ?:((syn:si k) ka (~(sub ^rd %n) .~0 ka))   :: k as @rd
      =/  r
        %-  ~(sub ^rd %n)
        :-  (~(sub ^rd %n) x (~(mul ^rd %n) kf ln2hi))
        (~(mul ^rd %n) kf ln2lo)
      =/  cs=(list @rd)
        :~  `@rd`0x3ff0.0000.0000.0000  `@rd`0x3ff0.0000.0000.0000
            `@rd`0x3fe0.0000.0000.0011  `@rd`0x3fc5.5555.5555.555a
            `@rd`0x3fa5.5555.5554.f0cf  `@rd`0x3f81.1111.1110.f225
            `@rd`0x3f56.c16c.187f.be02  `@rd`0x3f2a.01a0.1b14.378f
            `@rd`0x3efa.0199.1ac8.730a  `@rd`0x3ec7.1ddf.5749.d126
            `@rd`0x3e92.8b40.57f4.4145  `@rd`0x3e5a.f631.d005.9bec
        ==
      =/  p  (roll (flop cs) |=([c=@rd acc=@rd] (~(add ^rd %n) (~(mul ^rd %n) acc r) c)))
      (scale2 p k)
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
      ~/  %sin
      |=  x=@rd  ^-  @rd
      ::  Reduce x = q*(pi/2) + (rhi+rlo) (2-part pi/2), then fdlibm sin/cos
      ::  kernels picked by q&3.  Faithful to <=1 ULP for |x| <~ 2^22.
      ::  Round-nearest-even internally (the SoftFloat jet matches).
      ?:  !(~(equ ^rd %n) x x)  `@rd`0x7ff8.0000.0000.0000
      ?:  |(=(x `@rd`0x7ff0.0000.0000.0000) =(x `@rd`0xfff0.0000.0000.0000))  `@rd`0x7ff8.0000.0000.0000
      ?:  |(=(x `@rd`0x0) =(x `@rd`0x8000.0000.0000.0000))  x   :: +-0 -> +-0
      %-  trig-fin:rd-trig
      [%.y `@rd`(dis x 0x7fff.ffff.ffff.ffff) (rsh [0 63] x)]
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
      ~/  %cos
      |=  x=@rd  ^-  @rd
      ?:  !(~(equ ^rd %n) x x)  `@rd`0x7ff8.0000.0000.0000
      ?:  |(=(x `@rd`0x7ff0.0000.0000.0000) =(x `@rd`0xfff0.0000.0000.0000))  `@rd`0x7ff8.0000.0000.0000
      %-  trig-fin:rd-trig
      [%.n `@rd`(dis x 0x7fff.ffff.ffff.ffff) 0]
    ::  +rd-trig: shared sin/cos engine for the @rd door (see +sin / +cos).
    ++  rd-trig
      |%
      ++  sc
        ^-  (list @rd)
        :~  `@rd`0xbfc5.5555.5555.5555  `@rd`0x3f81.1111.1111.1111
            `@rd`0xbf2a.01a0.1a01.a01a  `@rd`0x3ec7.1de3.a556.c734
            `@rd`0xbe5a.e645.67f5.44e4  `@rd`0x3de6.1246.13a8.6d09
            `@rd`0xbd6a.e7f3.e733.b81f  `@rd`0x3ce9.52c7.7030.ad4a
        ==
      ++  cc
        ^-  (list @rd)
        :~  `@rd`0x3fa5.5555.5555.5555  `@rd`0xbf56.c16c.16c1.6c17
            `@rd`0x3efa.01a0.1a01.a01a  `@rd`0xbe92.7e4f.b778.9f5c
            `@rd`0x3e21.eed8.eff8.d898  `@rd`0xbda9.3974.a8c0.7c9d
            `@rd`0x3d2a.e7f3.e733.b81f  `@rd`0xbca6.8278.63b9.7d97
        ==
      ++  neg  |=(a=@rd ^-(@rd (~(sub ^rd %n) `@rd`0x0 a)))
      ++  ksin
        |=  [xx=@rd yy=@rd]  ^-  @rd
        =/  z   (~(mul ^rd %n) xx xx)
        =/  r   (roll (flop (tail sc)) |=([c=@rd a=@rd] (~(add ^rd %n) (~(mul ^rd %n) a z) c)))
        =/  v   (~(mul ^rd %n) z xx)
        =/  aa  (~(sub ^rd %n) (~(mul ^rd %n) `@rd`0x3fe0.0000.0000.0000 yy) (~(mul ^rd %n) v r))
        =/  bb  (~(sub ^rd %n) (~(mul ^rd %n) z aa) yy)
        =/  dd  (~(sub ^rd %n) bb (~(mul ^rd %n) v (head sc)))
        (~(sub ^rd %n) xx dd)
      ++  kcos
        |=  [xx=@rd yy=@rd]  ^-  @rd
        =/  z   (~(mul ^rd %n) xx xx)
        =/  rc  (roll (flop cc) |=([c=@rd a=@rd] (~(add ^rd %n) (~(mul ^rd %n) a z) c)))
        =/  hz  (~(mul ^rd %n) `@rd`0x3fe0.0000.0000.0000 z)
        =/  w2  (~(sub ^rd %n) `@rd`0x3ff0.0000.0000.0000 hz)
        =/  aa  (~(sub ^rd %n) (~(sub ^rd %n) `@rd`0x3ff0.0000.0000.0000 w2) hz)
        =/  bb  (~(sub ^rd %n) (~(mul ^rd %n) (~(mul ^rd %n) z z) rc) (~(mul ^rd %n) xx yy))
        (~(add ^rd %n) w2 (~(add ^rd %n) aa bb))
      ::  +trig-fin: [is-sin? |x| sign-bit] -> sin x (is-sin?) or cos x
      ++  trig-fin
        |=  [s=? ax=@rd sb=@]  ^-  @rd
        =/  q   (need (~(toi ^rd %n) (~(mul ^rd %n) ax `@rd`0x3fe4.5f30.6dc9.c883)))
        =/  qf  (~(sun ^rd %n) (abs:si q))
        =/  t   (~(sub ^rd %n) ax (~(mul ^rd %n) qf `@rd`0x3ff9.21fb.5440.0000))
        =/  w   (~(mul ^rd %n) qf `@rd`0x3dd0.b461.1a62.6331)
        =/  rhi  (~(sub ^rd %n) t w)
        =/  rlo  (~(sub ^rd %n) (~(sub ^rd %n) t rhi) w)
        =/  m   (dis (abs:si q) 3)
        =/  ks  (ksin rhi rlo)
        =/  kc  (kcos rhi rlo)
        ?:  s
          =/  v  ?:(=(m 0) ks ?:(=(m 1) kc ?:(=(m 2) (neg ks) (neg kc))))
          ?:(=(sb 1) (neg v) v)
        ?:(=(m 0) kc ?:(=(m 1) (neg ks) ?:(=(m 2) (neg kc) ks)))
      --
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
      ~/  %tan
      ::  Dedicated fdlibm __kernel_tan (faithful <=1 ULP); the sin/cos ratio is
      ::  ~2 ULP.  q*pi/2 reduction, odd q uses the -cot path.  See +rd-tan.
      |=  x=@rd  ^-  @rd
      (main:rd-tan x)
    ::  +rd-tan: dedicated tangent kernel for the @rd door, see +tan.
    ++  rd-tan
      |%
      ++  redq
        |=  ax=@rd  ^-  [q=@s rhi=@rd rlo=@rd]
        =/  q   (need (~(toi ^rd %n) (~(mul ^rd %n) ax `@rd`0x3fe4.5f30.6dc9.c883)))
        =/  qf  (~(sun ^rd %n) (abs:si q))
        =/  t   (~(sub ^rd %n) ax (~(mul ^rd %n) qf `@rd`0x3ff9.21fb.5440.0000))
        =/  w   (~(mul ^rd %n) qf `@rd`0x3dd0.b461.1a62.6331)
        =/  rhi  (~(sub ^rd %n) t w)
        =/  rlo  (~(sub ^rd %n) (~(sub ^rd %n) t rhi) w)
        [q rhi rlo]
      ++  ktan
        |=  [x=@rd y=@rd iy=@s]  ^-  @rd
        =/  hxneg  (rsh [0 63] x)
        =/  big  (~(gte ^rd %n) `@rd`(dis x 0x7fff.ffff.ffff.ffff) `@rd`0x3fe5.9428.0000.0000)
        =/  xa  ?:(=(hxneg 1) (~(sub ^rd %n) `@rd`0x0 x) x)
        =/  ya  ?:(=(hxneg 1) (~(sub ^rd %n) `@rd`0x0 y) y)
        =/  xr
          ?.  big  x
          (~(add ^rd %n) (~(sub ^rd %n) `@rd`0x3fe9.21fb.5444.2d18 xa) (~(sub ^rd %n) `@rd`0x3c81.a626.3314.5c07 ya))
        =/  yr  ?:(big `@rd`0x0 y)
        =/  z   (~(mul ^rd %n) xr xr)
        =/  w   (~(mul ^rd %n) z z)
        =/  rl=(list @rd)
          :~  `@rd`0x3fc1.1111.1110.fe7a  `@rd`0x3f96.64f4.8406.d637
              `@rd`0x3f6d.6d22.c956.0328  `@rd`0x3f43.44d8.f2f2.6501
              `@rd`0x3f14.7e88.a037.92a6  `@rd`0xbef3.75cb.db60.5373
          ==
        =/  vl=(list @rd)
          :~  `@rd`0x3fab.a1ba.1bb3.41fe  `@rd`0x3f82.26e3.e96e.8493
              `@rd`0x3f57.dbc8.fee0.8315  `@rd`0x3f30.26f7.1a8d.1068
              `@rd`0x3f12.b80f.32f0.a7e9  `@rd`0x3efb.2a70.74bf.7ad4
          ==
        =/  rr  (roll (flop rl) |=([c=@rd a=@rd] (~(add ^rd %n) (~(mul ^rd %n) a w) c)))
        =/  vv  (~(mul ^rd %n) z (roll (flop vl) |=([c=@rd a=@rd] (~(add ^rd %n) (~(mul ^rd %n) a w) c))))
        =/  s   (~(mul ^rd %n) z xr)
        =/  r   (~(add ^rd %n) yr (~(mul ^rd %n) z (~(add ^rd %n) (~(mul ^rd %n) s (~(add ^rd %n) rr vv)) yr)))
        =.  r   (~(add ^rd %n) r (~(mul ^rd %n) `@rd`0x3fd5.5555.5555.5563 s))
        =/  w2  (~(add ^rd %n) xr r)
        ?:  big
          =/  fac  ?:(=(hxneg 1) `@rd`0xbff0.0000.0000.0000 `@rd`0x3ff0.0000.0000.0000)
          =/  v    ?:(=(iy --1) `@rd`0x3ff0.0000.0000.0000 `@rd`0xbff0.0000.0000.0000)
          %+  ~(mul ^rd %n)  fac
          %+  ~(sub ^rd %n)  v
          %+  ~(mul ^rd %n)  `@rd`0x4000.0000.0000.0000
          %+  ~(sub ^rd %n)  xr
          (~(sub ^rd %n) (~(div ^rd %n) (~(mul ^rd %n) w2 w2) (~(add ^rd %n) w2 v)) r)
        ?:  =(iy --1)  w2
        =/  zz   `@rd`(dis w2 0xffff.ffff.0000.0000)
        =/  vv2  (~(sub ^rd %n) r (~(sub ^rd %n) zz xr))
        =/  a    (~(div ^rd %n) `@rd`0xbff0.0000.0000.0000 w2)
        =/  tt   `@rd`(dis a 0xffff.ffff.0000.0000)
        =/  ss   (~(add ^rd %n) `@rd`0x3ff0.0000.0000.0000 (~(mul ^rd %n) tt zz))
        (~(add ^rd %n) tt (~(mul ^rd %n) a (~(add ^rd %n) ss (~(mul ^rd %n) tt vv2))))
      ++  main
        |=  x=@rd  ^-  @rd
        ?:  !(~(equ ^rd %n) x x)  `@rd`0x7ff8.0000.0000.0000
        ?:  |(=(x `@rd`0x7ff0.0000.0000.0000) =(x `@rd`0xfff0.0000.0000.0000))  `@rd`0x7ff8.0000.0000.0000
        ?:  |(=(x `@rd`0x0) =(x `@rd`0x8000.0000.0000.0000))  x
        =/  neg  (rsh [0 63] x)
        =/  ax   `@rd`(dis x 0x7fff.ffff.ffff.ffff)
        =/  red  (redq ax)
        =/  iy   ?:(=(0 (dis (abs:si q.red) 1)) --1 -1)
        =/  t    (ktan rhi.red rlo.red iy)
        ?:(=(neg 1) (~(sub ^rd %n) `@rd`0x0 t) t)
      --
    ::  +asin:  @rd -> @rd
    ::
    ::  Returns the inverse sine of a floating-point atom.
    ::    Examples
    ::      > (asin .~0)
    ::      .~0
    ::      > (asin .~1)
    ::      .~1.5707963267948966
    ::      > (asin .~0.7)
    ::      .~0.7753974965943197
    ::
    ++  asin
      ~/  %asin
      ::  fdlibm rational kernel; see +rd-ainv.  Faithful to <=1 ULP; |x|>1 -> NaN.
      |=  x=@rd  ^-  @rd
      (asn:rd-ainv x)
    ::  +acos:  @rd -> @rd
    ::
    ::  Returns the inverse cosine of a floating-point atom.
    ::    Examples
    ::      > (acos .~0)
    ::      .~1.5707963267948966
    ::      > (acos .~1)
    ::      .~0
    ::      > (acos .~0.7)
    ::      .~0.7953988301652518
    ::
    ++  acos
      ~/  %acos
      |=  x=@rd  ^-  @rd
      (acs:rd-ainv x)
    ::  +rd-ainv: shared asin/acos engine for the @rd door (rational P/Q kernel),
    ::  see +asin / +acos.
    ++  rd-ainv
      |%
      ++  rr
        |=  t=@rd  ^-  @rd
        =/  ps=(list @rd)
          :~  `@rd`0x3fc5.5555.5555.5555  `@rd`0xbfd4.d612.03eb.6f7d
              `@rd`0x3fc9.c155.0e88.4455  `@rd`0xbfa4.8228.b568.8f3b
              `@rd`0x3f49.efe0.7501.b288  `@rd`0x3f02.3de1.0dfd.f709
          ==
        =/  qs=(list @rd)
          :~  `@rd`0xc003.3a27.1c8a.2d4b  `@rd`0x4000.2ae5.9c59.8ac8
              `@rd`0xbfe6.066c.1b8d.0159  `@rd`0x3fb3.b8c5.b12e.9282
          ==
        =/  p  (~(mul ^rd %n) t (roll (flop ps) |=([c=@rd a=@rd] (~(add ^rd %n) (~(mul ^rd %n) a t) c))))
        =/  q  (~(add ^rd %n) `@rd`0x3ff0.0000.0000.0000 (~(mul ^rd %n) t (roll (flop qs) |=([c=@rd a=@rd] (~(add ^rd %n) (~(mul ^rd %n) a t) c)))))
        (~(div ^rd %n) p q)
      ++  asn
        |=  x=@rd  ^-  @rd
        ?:  !(~(equ ^rd %n) x x)  `@rd`0x7ff8.0000.0000.0000
        =/  sgn  (rsh [0 63] x)
        =/  ax   `@rd`(dis x 0x7fff.ffff.ffff.ffff)
        ?:  (~(gth ^rd %n) ax `@rd`0x3ff0.0000.0000.0000)  `@rd`0x7ff8.0000.0000.0000
        ?:  =(ax `@rd`0x3ff0.0000.0000.0000)
          (~(add ^rd %n) (~(mul ^rd %n) x `@rd`0x3ff9.21fb.5444.2d18) (~(mul ^rd %n) x `@rd`0x3c91.a626.3314.5c07))
        ?:  (~(lth ^rd %n) ax `@rd`0x3fe0.0000.0000.0000)
          ?:  (~(lth ^rd %n) ax `@rd`0x3e50.0000.0000.0000)  x
          =/  t  (~(mul ^rd %n) x x)
          (~(add ^rd %n) x (~(mul ^rd %n) x (rr t)))
        =/  w  (~(sub ^rd %n) `@rd`0x3ff0.0000.0000.0000 ax)
        =/  t  (~(mul ^rd %n) w `@rd`0x3fe0.0000.0000.0000)
        =/  r  (rr t)
        =/  s  (sqt t)
        ?:  (~(gte ^rd %n) ax `@rd`0x3fef.3333.0000.0000)
          =/  res  (~(sub ^rd %n) `@rd`0x3ff9.21fb.5444.2d18 (~(sub ^rd %n) (~(mul ^rd %n) `@rd`0x4000.0000.0000.0000 (~(add ^rd %n) s (~(mul ^rd %n) s r))) `@rd`0x3c91.a626.3314.5c07))
          ?:(=(sgn 1) (~(sub ^rd %n) `@rd`0x0 res) res)
        =/  df  `@rd`(dis s 0xffff.ffff.0000.0000)
        =/  c   (~(div ^rd %n) (~(sub ^rd %n) t (~(mul ^rd %n) df df)) (~(add ^rd %n) s df))
        =/  p2  (~(sub ^rd %n) (~(mul ^rd %n) `@rd`0x4000.0000.0000.0000 (~(mul ^rd %n) s r)) (~(sub ^rd %n) `@rd`0x3c91.a626.3314.5c07 (~(mul ^rd %n) `@rd`0x4000.0000.0000.0000 c)))
        =/  q2  (~(sub ^rd %n) `@rd`0x3fe9.21fb.5444.2d18 (~(mul ^rd %n) `@rd`0x4000.0000.0000.0000 df))
        =/  res  (~(sub ^rd %n) `@rd`0x3fe9.21fb.5444.2d18 (~(sub ^rd %n) p2 q2))
        ?:(=(sgn 1) (~(sub ^rd %n) `@rd`0x0 res) res)
      ++  acs
        |=  x=@rd  ^-  @rd
        ?:  !(~(equ ^rd %n) x x)  `@rd`0x7ff8.0000.0000.0000
        =/  neg  (rsh [0 63] x)
        =/  ax   `@rd`(dis x 0x7fff.ffff.ffff.ffff)
        ?:  (~(gth ^rd %n) ax `@rd`0x3ff0.0000.0000.0000)  `@rd`0x7ff8.0000.0000.0000
        ?:  =(ax `@rd`0x3ff0.0000.0000.0000)
          ?:  =(neg 0)  `@rd`0x0
          (~(add ^rd %n) `@rd`0x4009.21fb.5444.2d18 (~(mul ^rd %n) `@rd`0x4000.0000.0000.0000 `@rd`0x3c91.a626.3314.5c07))
        ?:  (~(lth ^rd %n) ax `@rd`0x3fe0.0000.0000.0000)
          ?:  (~(lth ^rd %n) ax `@rd`0x3c60.0000.0000.0000)  `@rd`0x3ff9.21fb.5444.2d18
          =/  z  (~(mul ^rd %n) x x)
          =/  r  (rr z)
          (~(sub ^rd %n) `@rd`0x3ff9.21fb.5444.2d18 (~(sub ^rd %n) x (~(sub ^rd %n) `@rd`0x3c91.a626.3314.5c07 (~(mul ^rd %n) x r))))
        ?:  =(neg 1)
          =/  z  (~(mul ^rd %n) (~(add ^rd %n) `@rd`0x3ff0.0000.0000.0000 x) `@rd`0x3fe0.0000.0000.0000)
          =/  s  (sqt z)
          =/  r  (rr z)
          =/  w  (~(sub ^rd %n) (~(mul ^rd %n) r s) `@rd`0x3c91.a626.3314.5c07)
          (~(sub ^rd %n) `@rd`0x4009.21fb.5444.2d18 (~(mul ^rd %n) `@rd`0x4000.0000.0000.0000 (~(add ^rd %n) s w)))
        =/  z   (~(mul ^rd %n) (~(sub ^rd %n) `@rd`0x3ff0.0000.0000.0000 x) `@rd`0x3fe0.0000.0000.0000)
        =/  s   (sqt z)
        =/  df  `@rd`(dis s 0xffff.ffff.0000.0000)
        =/  c   (~(div ^rd %n) (~(sub ^rd %n) z (~(mul ^rd %n) df df)) (~(add ^rd %n) s df))
        =/  r   (rr z)
        =/  w   (~(add ^rd %n) (~(mul ^rd %n) r s) c)
        (~(mul ^rd %n) `@rd`0x4000.0000.0000.0000 (~(add ^rd %n) df w))
      --
    ::  +atan:  @rd -> @rd
    ::
    ::  Returns the inverse tangent of a floating-point atom.
    ::    Examples
    ::      > (atan .~1)
    ::      .~0.7853981633821053
    ::      > (atan .~2)
    ::      .~1.1071487178081938
    ::      > (atan pi)
    ::      .~1.2626272558398273
    ::
    ++  atan
      ~/  %atan
      ::  fdlibm breakpoint reduction + minimax poly; odd.  Round-nearest-even
      ::  internally (the SoftFloat jet matches).
      |=  x=@rd  ^-  @rd
      ?:  !(~(equ ^rd %n) x x)              `@rd`0x7ff8.0000.0000.0000  :: NaN
      ?:  =(x `@rd`0x7ff0.0000.0000.0000)   `@rd`0x3ff9.21fb.5444.2d18  :: +inf -> pi/2
      ?:  =(x `@rd`0xfff0.0000.0000.0000)   `@rd`0xbff9.21fb.5444.2d18  :: -inf -> -pi/2
      ?:  |(=(x `@rd`0x0) =(x `@rd`0x8000.0000.0000.0000))  x           :: +-0 -> +-0
      =/  neg  (rsh [0 63] x)
      =/  r    (ker:rd-atan `@rd`(dis x 0x7fff.ffff.ffff.ffff))
      ?:(=(neg 1) (~(sub ^rd %n) `@rd`0x0 r) r)
    ::  +rd-atan: atan kernel for the @rd door (reduction + poly), see +atan.
    ++  rd-atan
      |%
      ++  at
        ^-  (list @rd)
        :~  `@rd`0x3fd5.5555.5555.550d  `@rd`0xbfc9.9999.9998.ebc4
            `@rd`0x3fc2.4924.9200.83ff  `@rd`0xbfbc.71c6.fe23.1671
            `@rd`0x3fb7.45cd.c54c.206e  `@rd`0xbfb3.b0f2.af74.9a6d
            `@rd`0x3fb1.0d66.a0d0.3d51  `@rd`0xbfad.de2d.52de.fd9a
            `@rd`0x3fa9.7b4b.2476.0deb  `@rd`0xbfa2.b444.2c6a.6c2f
            `@rd`0x3f90.ad3a.e322.da11
        ==
      ++  atred
        |=  ax=@rd  ^-  [xr=@rd hi=@rd lo=@rd dir=?]
        =/  one  `@rd`0x3ff0.0000.0000.0000
        =/  two  `@rd`0x4000.0000.0000.0000
        =/  ohf  `@rd`0x3ff8.0000.0000.0000
        ?:  (~(lth ^rd %n) ax `@rd`0x3fdc.0000.0000.0000)
          [ax `@rd`0x0 `@rd`0x0 %.y]
        ?:  (~(lth ^rd %n) ax `@rd`0x3fe6.0000.0000.0000)
          :*  (~(div ^rd %n) (~(sub ^rd %n) (~(add ^rd %n) ax ax) one) (~(add ^rd %n) two ax))
              `@rd`0x3fdd.ac67.0561.bb4f  `@rd`0x3c7a.2b7f.222f.65e2  %.n
          ==
        ?:  (~(lth ^rd %n) ax `@rd`0x3ff3.0000.0000.0000)
          :*  (~(div ^rd %n) (~(sub ^rd %n) ax one) (~(add ^rd %n) ax one))
              `@rd`0x3fe9.21fb.5444.2d18  `@rd`0x3c81.a626.3314.5c07  %.n
          ==
        ?:  (~(lth ^rd %n) ax `@rd`0x4003.8000.0000.0000)
          :*  (~(div ^rd %n) (~(sub ^rd %n) ax ohf) (~(add ^rd %n) one (~(mul ^rd %n) ohf ax)))
              `@rd`0x3fef.730b.d281.f69b  `@rd`0x3c70.0788.7af0.cbbd  %.n
          ==
        :*  (~(div ^rd %n) `@rd`0xbff0.0000.0000.0000 ax)
            `@rd`0x3ff9.21fb.5444.2d18  `@rd`0x3c91.a626.3314.5c07  %.n
        ==
      ++  ker
        |=  ax=@rd  ^-  @rd
        =/  q  (atred ax)
        =/  z  (~(mul ^rd %n) xr.q xr.q)
        =/  s  (~(mul ^rd %n) z (roll (flop at) |=([c=@rd a=@rd] (~(add ^rd %n) (~(mul ^rd %n) a z) c))))
        ?:  dir.q  (~(sub ^rd %n) xr.q (~(mul ^rd %n) xr.q s))
        (~(sub ^rd %n) hi.q (~(sub ^rd %n) (~(sub ^rd %n) (~(mul ^rd %n) xr.q s) lo.q) xr.q))
      --
    ::  +atan2:  [@rd @rd] -> @rd
    ::
    ::  Returns the inverse tangent of a floating-point coordinate.
    ::  Returns 0 for NaN inputs (does not propagate NaN).  The case (0,0) returns 0.
    ::    Examples
    ::      > (atan2 .~0 .~1)
    ::      .~0
    ::      > (atan2 .~-1 .~0)
    ::      .~-1.5707963267948966
    ::      > (atan2 .~0.5 .~-0.5)
    ::      .~2.3561944902107888
    ::
    ++  atan2
      ~/  %atan2
      |=  [y=@rd x=@rd]  ^-  @rd
      ?:  (gth x .~0)
        (atan (div y x))
      ?:  &((lth x .~0) (gte y .~0))
        (add (atan (div y x)) pi)
      ?:  &((lth x .~0) (lth y .~0))
        (sub (atan (div y x)) pi)
      ?:  &(=(.~0 x) (gth y .~0))
        (div pi .~2)
      ?:  &(=(.~0 x) (lth y .~0))
        (mul .~-1 (div pi .~2))
      .~0  ::  undefined
    ::    +pow-n:  [@rd @rd] -> @rd
    ::
    ::  Returns the power of a floating-point atom to an integer exponent.
    ::    Examples
    ::      > (pow-n .~1 .~2)
    ::      .~1
    ::      > (pow-n .~2 .~2)
    ::      .~4
    ::      > (pow-n .~2 .~3)
    ::      .~8
    ::  Source
    ++  pow-n
      ~/  %pow-n
      |=  [x=@rd n=@rd]  ^-  @rd
      ?:  =(n .~0)  .~1
      ?>  &((gth n .~0) (is-int n))
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
      ~/  %log
      |=  x=@rd  ^-  @rd
      ::  Reduce x = 2^e * m with m in [sqrt(1/2), sqrt(2)); then
      ::  log(x) = e*ln2 + log(1+f), f = m-1, s = f/(2+f),
      ::  log(1+f) = f - s*(f - 2z*P2(z)), z = s*s, P2 the atanh series
      ::  1/3 + z/5 + z^2/7 + ...  Faithful to <=1 ULP; round-nearest-even
      ::  internally (the SoftFloat jet will match this bit-for-bit).
      ?:  !(~(equ ^rd %n) x x)              `@rd`0x7ff8.0000.0000.0000  :: NaN
      ?:  =(x `@rd`0x7ff0.0000.0000.0000)   `@rd`0x7ff0.0000.0000.0000  :: +inf
      ?:  |(=(x `@rd`0x0) =(x `@rd`0x8000.0000.0000.0000))  `@rd`0xfff0.0000.0000.0000  :: +-0 -> -inf
      ?:  =(1 (rsh [0 63] x))               `@rd`0x7ff8.0000.0000.0000  :: x<0 -> NaN
      =/  sub  =(0 (dis 0x7ff (rsh [0 52] x)))                    :: subnormal?
      =/  xx   ?:(sub (~(mul ^rd %n) x `@rd`0x4350.0000.0000.0000) x)  :: *2^54
      =/  ae   ?:(sub -54 --0)
      =/  b    `@`xx
      =/  ef   (dif:si (new:si %.y (dis 0x7ff (rsh [0 52] b))) --1.023)
      =/  m    `@rd`(con (dis b 0xf.ffff.ffff.ffff) 0x3ff0.0000.0000.0000)
      =/  big  (~(gte ^rd %n) m `@rd`0x3ff6.a09e.667f.3bcd)       :: m >= sqrt(2)
      =?  m    big  (~(mul ^rd %n) m `@rd`0x3fe0.0000.0000.0000)  :: m * 0.5
      =?  ef   big  (sum:si ef --1)
      =.  ef   (sum:si ef ae)
      =/  f    (~(sub ^rd %n) m `@rd`0x3ff0.0000.0000.0000)
      =/  s    (~(div ^rd %n) f (~(add ^rd %n) m `@rd`0x3ff0.0000.0000.0000))
      =/  z    (~(mul ^rd %n) s s)
      =/  cs=(list @rd)
        :~  `@rd`0x3fd5.5555.5555.5555  `@rd`0x3fc9.9999.9999.999a
            `@rd`0x3fc2.4924.9249.2492  `@rd`0x3fbc.71c7.1c71.c71c
            `@rd`0x3fb7.45d1.745d.1746  `@rd`0x3fb3.b13b.13b1.3b14
            `@rd`0x3fb1.1111.1111.1111  `@rd`0x3fae.1e1e.1e1e.1e1e
            `@rd`0x3faa.f286.bca1.af28  `@rd`0x3fa8.6186.1861.8618
        ==
      =/  p2  (roll (flop cs) |=([c=@rd acc=@rd] (~(add ^rd %n) (~(mul ^rd %n) acc z) c)))
      =/  r   (~(mul ^rd %n) (~(add ^rd %n) z z) p2)
      =/  l1  (~(sub ^rd %n) f (~(mul ^rd %n) s (~(sub ^rd %n) f r)))
      =/  efa   (~(sun ^rd %n) (abs:si ef))
      =/  ef-f  ?:((syn:si ef) efa (~(sub ^rd %n) .~0 efa))      :: e as @rd
      =/  hi  (~(mul ^rd %n) ef-f `@rd`0x3fe6.2e42.fee0.0000)    :: e*ln2hi
      =/  lo  (~(mul ^rd %n) ef-f `@rd`0x3dea.39ef.3579.3c76)    :: e*ln2lo
      (~(add ^rd %n) hi (~(add ^rd %n) l1 lo))
    ::    +log-10:  @rd -> @rd
    ::
    ::  Returns the base-10 logarithm of a floating-point atom.
    ::    Examples
    ::      > (log-10 .~0.1)
    ::      .~-0.9999999999082912
    ::      > (log-10 .~2)
    ::      .~0.30102999566353394
    ::      > (~(log-10 rd [%z .~1e-8]) .~2)
    ::      .~0.30102999562024696
    ::  Source
    ++  log-10
      ~/  %log-10
      ::  e*log10(2) + log(m)/ln10, reusing +lr so the integer part is added with
      ::  no division rounding (more accurate than log(x)/ln10).
      |=  x=@rd  ^-  @rd
      ?:  !(~(equ ^rd %n) x x)  `@rd`0x7ff8.0000.0000.0000
      ?:  =(x `@rd`0x7ff0.0000.0000.0000)  `@rd`0x7ff0.0000.0000.0000
      ?:  |(=(x `@rd`0x0) =(x `@rd`0x8000.0000.0000.0000))  `@rd`0xfff0.0000.0000.0000
      ?:  =(1 (rsh [0 63] x))  `@rd`0x7ff8.0000.0000.0000
      =/  el  (lr x)
      (~(add ^rd %n) (~(mul ^rd %n) ef.el `@rd`0x3fd3.4413.509f.79ff) (~(mul ^rd %n) lm.el `@rd`0x3fdb.cb7b.1526.e50e))
    ::    +log-2:  @rd -> @rd
    ::
    ::  Returns the base-2 logarithm of a floating-point atom.
    ::    Examples
    ::      > (log-2 .~2)
    ::      .~1
    ::      > (log-2 .~0.1)
    ::      .~-3.321928094887362
    ::  Source
    ++  log-2
      ~/  %log-2
      ::  e + log(m)/ln2 (integer part exact); see +lr.
      |=  x=@rd  ^-  @rd
      ?:  !(~(equ ^rd %n) x x)  `@rd`0x7ff8.0000.0000.0000
      ?:  =(x `@rd`0x7ff0.0000.0000.0000)  `@rd`0x7ff0.0000.0000.0000
      ?:  |(=(x `@rd`0x0) =(x `@rd`0x8000.0000.0000.0000))  `@rd`0xfff0.0000.0000.0000
      ?:  =(1 (rsh [0 63] x))  `@rd`0x7ff8.0000.0000.0000
      =/  el  (lr x)
      (~(add ^rd %n) ef.el (~(mul ^rd %n) lm.el `@rd`0x3ff7.1547.652b.82fe))
    ::  +lr: log reduction for finite positive x -> [e (as @rd), log(mantissa)].
    ++  lr
      |=  x=@rd  ^-  [ef=@rd lm=@rd]
      =/  sub  =(0 (dis 0x7ff (rsh [0 52] x)))
      =/  xx   ?:(sub (~(mul ^rd %n) x `@rd`0x4350.0000.0000.0000) x)
      =/  ae   ?:(sub -54 --0)
      =/  b    `@`xx
      =/  e    (dif:si (new:si %.y (dis 0x7ff (rsh [0 52] b))) --1.023)
      =/  m    `@rd`(con (dis b 0xf.ffff.ffff.ffff) 0x3ff0.0000.0000.0000)
      =/  big  (~(gte ^rd %n) m `@rd`0x3ff6.a09e.667f.3bcd)
      =?  m    big  (~(mul ^rd %n) m `@rd`0x3fe0.0000.0000.0000)
      =?  e    big  (sum:si e --1)
      =.  e    (sum:si e ae)
      =/  f    (~(sub ^rd %n) m `@rd`0x3ff0.0000.0000.0000)
      =/  s    (~(div ^rd %n) f (~(add ^rd %n) m `@rd`0x3ff0.0000.0000.0000))
      =/  z    (~(mul ^rd %n) s s)
      =/  cs=(list @rd)
        :~  `@rd`0x3fd5.5555.5555.5555  `@rd`0x3fc9.9999.9999.999a
            `@rd`0x3fc2.4924.9249.2492  `@rd`0x3fbc.71c7.1c71.c71c
            `@rd`0x3fb7.45d1.745d.1746  `@rd`0x3fb3.b13b.13b1.3b14
            `@rd`0x3fb1.1111.1111.1111  `@rd`0x3fae.1e1e.1e1e.1e1e
            `@rd`0x3faa.f286.bca1.af28  `@rd`0x3fa8.6186.1861.8618
        ==
      =/  p2  (roll (flop cs) |=([c=@rd a=@rd] (~(add ^rd %n) (~(mul ^rd %n) a z) c)))
      =/  r   (~(mul ^rd %n) (~(add ^rd %n) z z) p2)
      =/  l1  (~(sub ^rd %n) f (~(mul ^rd %n) s (~(sub ^rd %n) f r)))
      =/  efa  (~(sun ^rd %n) (abs:si e))
      =/  ef   ?:((syn:si e) efa (~(sub ^rd %n) .~0 efa))
      [ef l1]
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
      ~/  %pow
      |=  [x=@rd n=@rd]  ^-  @rd
      ::  fall through on positive integers (faster)
      ?:  &(=(n (san (need (toi n)))) (gth n .~0))  (pow-n x (san (need (toi n))))
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
    ::      .~1.414213562373095
    ::      > (sqt 1e5)
    ::      .~316.2277660168379
    ::  Source
    ++  sqt
      ~/  %sqt
      ::  Correctly-rounded f64 square root.  The stdlib f64 root is only
      ::  faithful (off by up to 1 ULP), so seed with it and apply one Markstein
      ::  correction (r = x - g*g via fma; g + (0.5/g)*r), which lands on the
      ::  correctly-rounded value -- matching the SoftFloat sqrt jet bit-for-bit.
      |=  x=@rd  ^-  @rd
      ?:  !(~(equ ^rd %n) x x)              `@rd`0x7ff8.0000.0000.0000  :: NaN
      ?:  =(x `@rd`0x7ff0.0000.0000.0000)   `@rd`0x7ff0.0000.0000.0000  :: +inf
      ?:  |(=(x `@rd`0x0) =(x `@rd`0x8000.0000.0000.0000))  x           :: +-0
      ?:  =(1 (rsh [0 63] x))               `@rd`0x7ff8.0000.0000.0000  :: x<0 -> NaN
      =/  g  (sqt:^rd x)                                          :: faithful seed
      =/  h  (~(div ^rd %n) `@rd`0x3fe0.0000.0000.0000 g)         :: 0.5/g
      =/  r  (~(fma ^rd %n) (~(sub ^rd %n) `@rd`0x0 g) g x)       :: x - g*g
      (~(fma ^rd %n) h r g)                                       :: g + h*r
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
      ~/  %cbt
      ::  cbrt(x) = sign(x) * exp(log|x| / 3); defined for all reals (unlike pow).
      |=  x=@rd  ^-  @rd
      ?:  !(~(equ ^rd %n) x x)  x                                :: NaN -> NaN
      ?:  |(=(x `@rd`0x0) =(x `@rd`0x8000.0000.0000.0000))  x     :: +-0 -> +-0
      =/  ax  `@rd`(dis x 0x7fff.ffff.ffff.ffff)
      =/  r   (exp (~(mul ^rd %n) (log ax) `@rd`0x3fd5.5555.5555.5555))
      ?:(=(1 (rsh [0 63] x)) (~(sub ^rd %n) `@rd`0x0 r) r)
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
    ::    +round:  [@rd @ud] -> @rd
    ::
    ::  Returns the floating-point atom rounded to a given number of significant
    ::  figures (n=1 = 1 sig fig).
    ::    Examples
    ::      > (round .~1 0)
    ::      .~1
    ::      > (round .~1.11 1)
    ::      .~1
    ::      > (round .~1.11 2)
    ::      .~1.1
    ::      > (round .~1.11 3)
    ::      .~1.11
    ::  Source
    ++  round
      |=  [x=@rd n=@ud]  ^-  @rd
      ?:  =(.~0 x)  .~0
      ::  Calculate the order of magnitude.
      =/  oom  (san (need (toi (log-10 (abs x)))))
      ::  Calculate the scaling factor.
      =/  scaling  (pow .~10 :(sub (sun n) oom .~1))
      ::  Round the mantissa to desired significant digits.
      =/  rnd-mantissa  (round-bankers (mul x scaling))
      ::  Convert back to the original scale.
      (div rnd-mantissa scaling)
    ::    +round-places:  [@rd @ud] -> @rd
    ::
    ::  Returns the floating-point atom rounded to a given number of decimal
    ::  places.  This is exceptionally sensitive to off-by-one FP rounding error.
    ::    Examples
    ::      > (round-places .~1 0)
    ::      .~1
    ::      > (round-places .~1.11 1)
    ::      .~1.1
    ::      > (round-places .~1.285 2)
    ::      .~1.28
    ::      > (round-places .~4.12345 3)
    ::      .~4.1229997
    ::  Source
    ++  round-places
      |=  [x=@rd n=@ud]  ^-  @rd
      ::  Calculate the scaling factor.
      =/  scaling  (pow .~10 (sun n))
      ::  Scale the number.
      =/  scaled  (mul x scaling)
      ::  Round the mantissa to desired significant digits.
      =/  rnd-mantissa  (round-bankers scaled)
      ::  Convert back to the original scale.
      (div rnd-mantissa scaling)
    ::    +round-bankers:  @rs -> @rs
    ::
    ::  Returns the floating-point atom rounded to the nearest integer, with ties
    ::  rounded to the nearest even integer (banker's rounding).  This is exactly
    ::  what +toi does under round-nearest (%n), which ties to even, so we force
    ::  %n regardless of the door's configured mode.
    ::    Examples
    ::      > (round-bankers .~1.5)
    ::      .~2
    ::      > (round-bankers .~2.5)
    ::      .~2
    ::      > (round-bankers .~1.49)
    ::      .~1
    ::  Source
    ++  round-bankers
      |=  x=@rd  ^-  @rd
      (san (need (~(toi ^rd %n) x)))
    --
  ::  half precision
  ++  rh
    ~/  %rh
    ^|
    |_  $:  r=$?(%n %u %d %z)   :: round nearest, up, down, to zero
            rtol=_.~~1e-2       :: relative tolerance: scales with |r|
            atol=_.~~0          :: absolute tolerance floor (default 0)
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
    ::  Returns the smallest representable positive (subnormal) number, 2^-24.
    ::  (Not the smallest NORMAL, which is 2^-14 = .~~6.10e-05.)
    ::    Examples
    ::      > tiny
    ::      .~~6e-8
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
    ++  sun  ~(sun ^rh r)
    ::    +san:  @sd -> @rh
    ::
    ::  Returns the floating-point atom of a signed integer atom.
    ::    Examples
    ::      > (san --1)
    ::      .~~1
    ::      > (san -1)
    ::      .~~-1
    ::  Source
    ++  san  ~(san ^rh r)
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
    ++  toi  ~(toi ^rh r)
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
    ++  drg  ~(drg ^rh r)
    ::    +grd:  dn -> @rh
    ::
    ::  Returns the floating-point atom of a decimal form.
    ::    Examples
    ::      > (grd [%d s=%.y e=--0 a=1])
    ::      .~~1
    ::      > (grd [%d s=%.y e=-1 a=11])
    ::      .~~1.1
    ::  Source
    ++  grd  ~(grd ^rh r)
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
    ::    +is-close:  [@rh @rh] -> ?
    ::
    ::  Returns `%.y` if `|p - r| < atol + rtol × |r|`, combining an absolute
    ::  floor `atol` with a relative tolerance `rtol` (both from the +rh door).
    ::    Examples
    ::      > (is-close .~~1 .~~2)
    ::      %.n
    ::      > (is-close .~~1 .~~1.1)
    ::      %.n
    ::      > (~(is-close rh [%z .~~1e-3]) .~~1 .~~1.0001)
    ::      %.y
    ::  Source
    ++  is-close
      |=  [p=@rh r=@rh]
      (lth (abs (sub p r)) (add atol (mul rtol (abs r))))
    ::    +all-close:  [@rh (list @rh)] -> ?
    ::
    ::  Returns `%.y` if `|p - r| < atol + rtol × |r|` for all `r` in `q`
    ::  (both tolerances provided by the +rh door).
    ::    Examples
    ::      > (all-close .~~1 ~[.~~1 .~~2])
    ::      %.n
    ::      > (all-close .~~1 ~[.~~1 .~~1.0000001])
    ::      %.n
    ::      > (~(all-close rh [%z .~~1e-3]) .~~1 ~[.~~1 .~~1.0001])
    ::      %.y
    ::  Source
    ++  all-close
      |=  [p=@rh q=(list @rh)]
      =/  i  0
      =/  n  (lent q)
      |-  ^-  ?
      ?:  =(n i)
        %.y
      ?.  (is-close p (snag i q))
        %.n
      $(i +(i))
    ::    +is-int:  @rh -> ?
    ::
    ::  Returns whether a floating-point value is an integer (no fractional part).
    ::    Examples
    ::      > (is-int .~~1)
    ::      %.y
    ::      > (is-int .~~1.1)
    ::      %.n
    ::  Source
    ++  is-int
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
    ++  add  ~(add ^rh r)
    ::    +sub:  [@rh @rh] -> @rh
    ::
    ::  Returns the difference of two floating-point atoms.
    ::    Examples
    ::      > (sub .~~1 .~~2)
    ::      .~~-1
    ::  Source
    ++  sub  ~(sub ^rh r)
    ::    +mul:  [@rh @rh] -> @rh
    ::
    ::  Returns the product of two floating-point atoms.
    ::    Examples
    ::      > (mul .~~1 .~~2)
    ::      .~~2
    ::  Source
    ++  mul  ~(mul ^rh r)
    ::    +div:  [@rh @rh] -> @rh
    ::
    ::  Returns the quotient of two floating-point atoms.
    ::    Examples
    ::      > (div .~~1 .~~2)
    ::      .~~0.5
    ::  Source
    ++  div  ~(div ^rh r)
    ::    +fma:  [@rh @rh @rh] -> @rh
    ::
    ::  Returns the fused multiply-add of three floating-point atoms.
    ::    Examples
    ::      > (fma .~~1 .~~2 .~~3)
    ::      .~~5
    ::      > (fma .~~2 .~~3 .~~4)
    ::      .~~10
    ::  Source
    ++  fma  ~(fma ^rh r)
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
    ::  Crashes via `?>` if `x < 0` or if `x` is a positive non-integer (the
    ::  decrement loop passes through zero).
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
      ?:  (lth (abs x) rtol)
        t
      |-  ^-  @rh
      ?:  (lth (abs (sub x .~~1)) rtol)
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
    ::  +widen-hs: f16 -> f32, exact.  +narrow-sh: f32 -> f16, correctly-rounded
    ::  RNE.  The @rh transcendentals compute in the (more precise) @rs door and
    ::  round the result down -- correctly-rounded for f16, since an f32 result is
    ::  ~2^13 times finer than an f16 ULP.
    ++  widen-hs
      |=  h=@rh  ^-  @
      =/  hh  `@`h
      =/  s   (lsh [0 16] (dis hh 0x8000))
      =/  e   (dis (rsh [0 10] hh) 0x1f)
      =/  m   (dis hh 0x3ff)
      ?:  =(e 0x1f)  (con s (con 0x7f80.0000 (lsh [0 13] m)))
      ?:  =(e 0)
        ?:  =(m 0)  s
        (con s `@`(~(mul rs [r .1e-5 .0]) (~(sun rs [r .1e-5 .0]) m) `@rs`0x3380.0000))
      (con s (con (lsh [0 23] (^add e 112)) (lsh [0 13] m)))
    ::  +rndup: should the magnitude be incremented?  rem=discarded bits,
    ::  half=tie point, lsb=kept low bit, neg=sign.  Honors the door's r.
    ++  rndup
      |=  [rem=@ half=@ lsb=@ neg=?]  ^-  ?
      ?-  r
        %n  ?|((^gth rem half) &(=(rem half) =(1 lsb)))
        %z  %.n
        %u  &(!neg !=(0 rem))
        %d  &(neg !=(0 rem))
      ==
    ++  narrow-sh
      |=  uu=@rs  ^-  @
      =/  u    `@`uu
      =/  s    (dis (rsh [0 16] u) 0x8000)            ::  sign in f16 position
      =/  neg  =(0x8000 s)
      =/  e    (dis (rsh [0 23] u) 0xff)
      =/  m    (dis u 0x7f.ffff)
      ?:  =(e 0xff)     (con s ?:(=(m 0) 0x7c00 0x7e00))   ::  inf / nan
      ?:  (^gte e 143)                                     ::  overflow: inf / max-finite per r
        ?-  r
          %n  (con s 0x7c00)
          %u  (con s ?:(neg 0x7bff 0x7c00))
          %d  (con s ?:(neg 0x7c00 0x7bff))
          %z  (con s 0x7bff)
        ==
      ?:  (^gte e 113)                                     ::  normal
        =/  ne    (^sub e 112)
        =/  mant  (rsh [0 13] m)
        =/  rem   (dis m 0x1fff)
        =/  rup   (rndup rem 0x1000 (dis mant 1) neg)
        (con s (^add (lsh [0 10] ne) (^add mant ?:(rup 1 0))))
      ?:  (^lth e 102)                                     ::  underflow: 0 / min-subnormal per r
        =/  nz  |(!=(0 e) !=(0 m))                          ::  nonzero? (exact 0 never bumps)
        =/  up  ?&(nz ?-(r %n %.n, %z %.n, %u !neg, %d neg))
        (con s ?:(up 1 0))
      =/  shift  (^sub 126 e)                              ::  subnormal
      =/  mf     (con 0x80.0000 m)
      =/  mant   (rsh [0 shift] mf)
      =/  half   (bex (dec shift))
      =/  rem    (dis mf (dec (bex shift)))
      =/  rup    (rndup rem half (dis mant 1) neg)
      (con s (^add mant ?:(rup 1 0)))
    ++  exp
      ~/  %exp
      ::  native f16: Cody-Waite x=k*ln2+r, exp(x)=2^k*P(r), deg-4 minimax (<=1 ULP).
      |=  x=@rh  ^-  @rh
      =/  pow2  |=(j=@s `@rh`(lsh [0 10] (abs:si (sum:si j --15))))
      =/  scale2
        |=  [p=@rh k=@s]  ^-  @rh
        ?:  (syn:si (dif:si k --16))                 :: k>=16: (p*2^15)*2^(k-15)
          (~(mul ^rh %n) (~(mul ^rh %n) p (pow2 --15)) (pow2 (dif:si k --15)))
        ?:  !(syn:si (sum:si k --14))                :: k<-14: (p*2^(k+11))*2^-11
          (~(mul ^rh %n) (~(mul ^rh %n) p (pow2 (sum:si k --11))) (pow2 -11))
        (~(mul ^rh %n) p (pow2 k))
      ?:  !(~(equ ^rh %n) x x)    `@rh`0x7e00        :: exp(NaN) -> NaN
      ?:  =(x `@rh`0x7c00)        `@rh`0x7c00        :: exp(+inf) -> inf
      ?:  =(x `@rh`0xfc00)        `@rh`0x0           :: exp(-inf) -> 0
      =/  log2e  `@rh`0x3dc5
      =/  ln2hi  `@rh`0x3980
      =/  ln2lo  `@rh`0x1dc8
      =/  k=@s   (need (~(toi ^rh %n) (~(mul ^rh %n) x log2e)))
      ?:  (syn:si (dif:si k --17))   `@rh`0x7c00     :: overflow -> inf
      ?:  !(syn:si (sum:si k --24))  `@rh`0x0        :: underflow -> 0
      =/  ka  (~(sun ^rh %n) (abs:si k))
      =/  kf  ?:((syn:si k) ka (~(sub ^rh %n) `@rh`0x0 ka))
      =/  r
        %-  ~(sub ^rh %n)
        :-  (~(sub ^rh %n) x (~(mul ^rh %n) kf ln2hi))
        (~(mul ^rh %n) kf ln2lo)
      =/  cs=(list @rh)
        :~  `@rh`0x3c00  `@rh`0x3c00  `@rh`0x3800  `@rh`0x3160  `@rh`0x295c
        ==
      =/  p  (roll (flop cs) |=([c=@rh acc=@rh] (~(add ^rh %n) (~(mul ^rh %n) acc r) c)))
      (scale2 p k)
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
      ~/  %sin
      ::  native f16: q*pi/2 reduction + fdlibm sin/cos kernels (see +rh-trig).
      |=  x=@rh  ^-  @rh
      ?:  !(~(equ ^rh %n) x x)  `@rh`0x7e00                       :: NaN
      ?:  |(=(x `@rh`0x7c00) =(x `@rh`0xfc00))  `@rh`0x7e00       :: +-inf
      ?:  |(=(x `@rh`0x0) =(x `@rh`0x8000))  x                    :: +-0 -> +-0
      %-  trig-fin:rh-trig
      [%.y `@rh`(dis x 0x7fff) (rsh [0 15] x)]
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
      ~/  %cos
      |=  x=@rh  ^-  @rh
      ?:  !(~(equ ^rh %n) x x)  `@rh`0x7e00
      ?:  |(=(x `@rh`0x7c00) =(x `@rh`0xfc00))  `@rh`0x7e00
      %-  trig-fin:rh-trig
      [%.n `@rh`(dis x 0x7fff) 0]
    ::  +rh-trig: native f16 sin/cos engine (fdlibm kernels, 2-coeff each).
    ++  rh-trig
      |%
      ++  sc  ^-((list @rh) :~(`@rh`0xb155 `@rh`0x2044))
      ++  cc  ^-((list @rh) :~(`@rh`0x2955 `@rh`0x95b0))
      ++  neg  |=(a=@rh ^-(@rh (~(sub ^rh %n) `@rh`0x0 a)))
      ++  ksin
        |=  [xx=@rh yy=@rh]  ^-  @rh
        =/  z   (~(mul ^rh %n) xx xx)
        =/  r   (roll (flop (tail sc)) |=([c=@rh a=@rh] (~(add ^rh %n) (~(mul ^rh %n) a z) c)))
        =/  v   (~(mul ^rh %n) z xx)
        =/  aa  (~(sub ^rh %n) (~(mul ^rh %n) `@rh`0x3800 yy) (~(mul ^rh %n) v r))
        =/  bb  (~(sub ^rh %n) (~(mul ^rh %n) z aa) yy)
        =/  dd  (~(sub ^rh %n) bb (~(mul ^rh %n) v (head sc)))
        (~(sub ^rh %n) xx dd)
      ++  kcos
        |=  [xx=@rh yy=@rh]  ^-  @rh
        =/  z   (~(mul ^rh %n) xx xx)
        =/  rc  (roll (flop cc) |=([c=@rh a=@rh] (~(add ^rh %n) (~(mul ^rh %n) a z) c)))
        =/  hz  (~(mul ^rh %n) `@rh`0x3800 z)
        =/  w2  (~(sub ^rh %n) `@rh`0x3c00 hz)
        =/  aa  (~(sub ^rh %n) (~(sub ^rh %n) `@rh`0x3c00 w2) hz)
        =/  bb  (~(sub ^rh %n) (~(mul ^rh %n) (~(mul ^rh %n) z z) rc) (~(mul ^rh %n) xx yy))
        (~(add ^rh %n) w2 (~(add ^rh %n) aa bb))
      ::  +trig-fin: [is-sin? |x| sign-bit] -> sin x (is-sin?) or cos x
      ++  trig-fin
        |=  [s=? ax=@rh sb=@]  ^-  @rh
        =/  q   (need (~(toi ^rh %n) (~(mul ^rh %n) ax `@rh`0x3918)))
        =/  qf  (~(sun ^rh %n) (abs:si q))
        =/  r1  (~(sub ^rh %n) ax (~(mul ^rh %n) qf `@rh`0x3e00))
        =/  r2  (~(sub ^rh %n) r1 (~(mul ^rh %n) qf `@rh`0x2c80))
        =/  w   (~(mul ^rh %n) qf `@rh`0xfed)
        =/  rhi  (~(sub ^rh %n) r2 w)
        =/  rlo  (~(sub ^rh %n) (~(sub ^rh %n) r2 rhi) w)
        =/  m   (dis (abs:si q) 3)
        =/  ks  (ksin rhi rlo)
        =/  kc  (kcos rhi rlo)
        ?:  s
          =/  v  ?:(=(m 0) ks ?:(=(m 1) kc ?:(=(m 2) (neg ks) (neg kc))))
          ?:(=(sb 1) (neg v) v)
        ?:(=(m 0) kc ?:(=(m 1) (neg ks) ?:(=(m 2) (neg kc) ks)))
      --
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
      ~/  %tan
      ::  native f16: sin/cos ratio (the final div honors the door's r).
      |=  x=@rh  ^-  @rh
      (div (sin x) (cos x))
    ::  +asin:  @rh -> @rh
    ::
    ::  Returns the inverse sine of a floating-point atom.
    ::    Examples
    ::      > (asin .~~0)
    ::      .~~0
    ::      > (asin .~~1)
    ::      .~~1.57
    ::      > (asin .~~0.7)
    ::      .~~0.7773
    ::
    ++  asin
      ~/  %asin
      ::  native f16: fdlibm rational kernel; see +rh-ainv.  |x|>1 -> NaN.
      |=  x=@rh  ^-  @rh
      (asn:rh-ainv x)
    ::  +acos:  @rh -> @rh
    ::
    ::  Returns the inverse cosine of a floating-point atom.
    ::    Examples
    ::      > (acos .~~0)
    ::      .~~1.57
    ::      > (acos .~~1)
    ::      .~~0
    ::      > (acos .~~0.7)
    ::      .~~0.7964
    ::
    ++  acos
      ~/  %acos
      ::  native f16: fdlibm rational kernel; see +rh-ainv.  |x|>1 -> NaN.
      |=  x=@rh  ^-  @rh
      (acs:rh-ainv x)
    ::  +rh-ainv: shared native f16 asin/acos engine (poly R(t)=t*P(t)),
    ::  see +asin / +acos.  Round-nearest-even internally; deg-4 minimax P,
    ::  faithful to ~1 ULP.  PIO2L=0xfed is the low half of pi/2 (positive).
    ++  rh-ainv
      |%
      ++  rr
        |=  t=@rh  ^-  @rh
        =/  ps=(list @rh)
          ^-((list @rh) :~(`@rh`0x3155 `@rh`0x2cea `@rh`0x2729 `@rh`0x2ccc))
        %+  ~(mul ^rh %n)  t
        (roll (flop ps) |=([c=@rh a=@rh] (~(add ^rh %n) (~(mul ^rh %n) a t) c)))
      ++  asn
        |=  x=@rh  ^-  @rh
        ?:  !(~(equ ^rh %n) x x)  `@rh`0x7e00
        =/  sgn  (rsh [0 15] x)
        =/  ax   `@rh`(dis x 0x7fff)
        ?:  (~(gth ^rh %n) ax `@rh`0x3c00)  `@rh`0x7e00     :: |x|>1 -> NaN
        ?:  =(ax `@rh`0x3c00)                               :: |x|==1 -> +-pi/2
          (~(add ^rh %n) (~(mul ^rh %n) x `@rh`0x3e48) (~(mul ^rh %n) x `@rh`0xfed))
        ?:  (~(lth ^rh %n) ax `@rh`0x3800)                  :: |x|<0.5
          ?:  (~(lth ^rh %n) ax `@rh`0xc00)  x              :: |x|<2^-12 -> x
          =/  t  (~(mul ^rh %n) x x)
          (~(add ^rh %n) x (~(mul ^rh %n) x (rr t)))
        =/  w  (~(sub ^rh %n) `@rh`0x3c00 ax)               :: 0.5<=|x|<1
        =/  t  (~(mul ^rh %n) w `@rh`0x3800)
        =/  r  (rr t)
        =/  s  (~(sqt ^rh %n) t)
        ?:  (~(gte ^rh %n) ax `@rh`0x3bcd)                  :: |x|>=0.975
          =/  res
            (~(sub ^rh %n) `@rh`0x3e48 (~(mul ^rh %n) `@rh`0x4000 (~(add ^rh %n) s (~(mul ^rh %n) s r))))
          ?:(=(sgn 1) (~(sub ^rh %n) `@rh`0x0 res) res)
        =/  df  `@rh`(dis s 0xfff0)
        =/  c   (~(div ^rh %n) (~(sub ^rh %n) t (~(mul ^rh %n) df df)) (~(add ^rh %n) s df))
        =/  p2  (~(sub ^rh %n) (~(mul ^rh %n) `@rh`0x4000 (~(mul ^rh %n) s r)) (~(sub ^rh %n) `@rh`0xfed (~(mul ^rh %n) `@rh`0x4000 c)))
        =/  q2  (~(sub ^rh %n) `@rh`0x3a48 (~(mul ^rh %n) `@rh`0x4000 df))
        =/  res  (~(sub ^rh %n) `@rh`0x3a48 (~(sub ^rh %n) p2 q2))
        ?:(=(sgn 1) (~(sub ^rh %n) `@rh`0x0 res) res)
      ++  acs
        |=  x=@rh  ^-  @rh
        ?:  !(~(equ ^rh %n) x x)  `@rh`0x7e00
        =/  neg  (rsh [0 15] x)
        =/  ax   `@rh`(dis x 0x7fff)
        ?:  (~(gth ^rh %n) ax `@rh`0x3c00)  `@rh`0x7e00     :: |x|>1 -> NaN
        ?:  =(ax `@rh`0x3c00)                               :: |x|==1
          ?:  =(neg 0)  `@rh`0x0
          (~(add ^rh %n) `@rh`0x4248 (~(mul ^rh %n) `@rh`0x4000 `@rh`0xfed))
        ?:  (~(lth ^rh %n) ax `@rh`0x3800)                  :: |x|<0.5
          =/  z  (~(mul ^rh %n) x x)
          =/  r  (rr z)
          (~(sub ^rh %n) `@rh`0x3e48 (~(sub ^rh %n) x (~(sub ^rh %n) `@rh`0xfed (~(mul ^rh %n) x r))))
        ?:  =(neg 1)                                        :: -1<x<=-0.5
          =/  z  (~(mul ^rh %n) (~(add ^rh %n) `@rh`0x3c00 x) `@rh`0x3800)
          =/  s  (~(sqt ^rh %n) z)
          =/  r  (rr z)
          =/  w  (~(sub ^rh %n) (~(mul ^rh %n) r s) `@rh`0xfed)
          (~(sub ^rh %n) `@rh`0x4248 (~(mul ^rh %n) `@rh`0x4000 (~(add ^rh %n) s w)))
        =/  z  (~(mul ^rh %n) (~(sub ^rh %n) `@rh`0x3c00 x) `@rh`0x3800)  :: 0.5<=x<1
        =/  s  (~(sqt ^rh %n) z)
        =/  r  (rr z)
        (~(mul ^rh %n) `@rh`0x4000 (~(add ^rh %n) s (~(mul ^rh %n) s r)))
      --
    ::  +atan:  @rh -> @rh
    ::
    ::  Returns the inverse tangent of a floating-point atom.
    ::    Examples
    ::      > (atan .~~1)
    ::      .~~0.7866
    ::      > (atan .~~2)
    ::      .~~1.111
    ::      > (atan pi)
    ::      .~~1.281
    ::
    ++  atan
      ~/  %atan
      ::  native f16: fdlibm breakpoint reduction + deg-2 atan minimax.
      |=  x=@rh  ^-  @rh
      ?:  !(~(equ ^rh %n) x x)  `@rh`0x7e00        :: NaN
      ?:  =(x `@rh`0x7c00)      `@rh`0x3e48        :: +inf -> pi/2
      ?:  =(x `@rh`0xfc00)      `@rh`0xbe48        :: -inf -> -pi/2
      ?:  |(=(x `@rh`0x0) =(x `@rh`0x8000))  x     :: +-0 -> +-0
      =/  neg  (rsh [0 15] x)
      =/  r    (ker:rh-atan `@rh`(dis x 0x7fff))
      ?:(=(neg 1) (~(sub ^rh %n) `@rh`0x0 r) r)
    ::  +rh-atan: native f16 atan engine (breakpoint reduction + poly).
    ++  rh-atan
      |%
      ++  at  ^-((list @rh) :~(`@rh`0x3555 `@rh`0xb266 `@rh`0x3092))   :: 1/3,-1/5,1/7
      ++  atred
        |=  ax=@rh  ^-  [xr=@rh hi=@rh lo=@rh dir=?]
        =/  one  `@rh`0x3c00
        =/  two  `@rh`0x4000
        =/  ohf  `@rh`0x3e00
        ?:  (~(lth ^rh %n) ax `@rh`0x3700)              :: < 7/16
          [ax `@rh`0x0 `@rh`0x0 %.y]
        ?:  (~(lth ^rh %n) ax `@rh`0x3980)              :: < 11/16
          :*  (~(div ^rh %n) (~(sub ^rh %n) (~(add ^rh %n) ax ax) one) (~(add ^rh %n) two ax))
              `@rh`0x376b  `@rh`0x19c  %.n
          ==
        ?:  (~(lth ^rh %n) ax `@rh`0x3cc0)              :: < 19/16
          :*  (~(div ^rh %n) (~(sub ^rh %n) ax one) (~(add ^rh %n) ax one))
              `@rh`0x3a48  `@rh`0xbed  %.n
          ==
        ?:  (~(lth ^rh %n) ax `@rh`0x40e0)              :: < 39/16
          :*  (~(div ^rh %n) (~(sub ^rh %n) ax ohf) (~(add ^rh %n) one (~(mul ^rh %n) ohf ax)))
              `@rh`0x3bdd  `@rh`0x87a1  %.n
          ==
        :*  (~(div ^rh %n) `@rh`0xbc00 ax)              :: -1/ax
            `@rh`0x3e48  `@rh`0xfed  %.n
        ==
      ++  ker
        |=  ax=@rh  ^-  @rh
        =/  q  (atred ax)
        =/  z  (~(mul ^rh %n) xr.q xr.q)
        =/  s  (~(mul ^rh %n) z (roll (flop at) |=([c=@rh a=@rh] (~(add ^rh %n) (~(mul ^rh %n) a z) c))))
        ?:  dir.q  (~(sub ^rh %n) xr.q (~(mul ^rh %n) xr.q s))
        (~(sub ^rh %n) hi.q (~(sub ^rh %n) (~(sub ^rh %n) (~(mul ^rh %n) xr.q s) lo.q) xr.q))
      --
    ::  +atan2:  [@rh @rh] -> @rh
    ::
    ::  Returns the inverse tangent of a floating-point coordinate.
    ::  Returns 0 for NaN inputs (does not propagate NaN).  The case (0,0) returns 0.
    ::    Examples
    ::      > (atan2 .~~0 .~~1)
    ::      .~~0
    ::      > (atan2 .~~-1 .~~0)
    ::      .~~-1.57
    ::      > (atan2 .~~0.5 .~~-0.5)
    ::      .~~2.354
    ::
    ++  atan2
      ~/  %atan2
      ::  native f16: quadrant dispatch over the native +atan (composite ops
      ::  honor the door's r).
      |=  [y=@rh x=@rh]  ^-  @rh
      ?:  (gth x .~~0)
        (atan (div y x))
      ?:  &((lth x .~~0) (gte y .~~0))
        (add (atan (div y x)) pi)
      ?:  &((lth x .~~0) (lth y .~~0))
        (sub (atan (div y x)) pi)
      ?:  &(=(.~~0 x) (gth y .~~0))
        (div pi .~~2)
      ?:  &(=(.~~0 x) (lth y .~~0))
        (mul .~~-1 (div pi .~~2))
      .~~0  ::  undefined
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
      ~/  %pow-n
      ::  native @rh repeated multiply (not via @rs); positive integer n only.
      |=  [x=@rh n=@rh]  ^-  @rh
      ?:  =(n `@rh`0x0)  `@rh`0x3c00
      ?>  &((gth n `@rh`0x0) (is-int n))
      =/  p  x
      |-  ^-  @rh
      ?:  (lth n `@rh`0x4000)
        p
      $(n (sub n `@rh`0x3c00), p (mul p x))
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
      ~/  %log
      ::  native f16: x=2^e*m reduction (m in [sqrt(1/2),sqrt(2))) + deg-1 atanh.
      |=  x=@rh  ^-  @rh
      ?:  !(~(equ ^rh %n) x x)         `@rh`0x7e00        :: log(NaN) -> NaN
      ?:  =(x `@rh`0x7c00)             `@rh`0x7c00        :: log(+inf) -> inf
      ?:  |(=(x `@rh`0x0) =(x `@rh`0x8000))  `@rh`0xfc00  :: log(+-0) -> -inf
      ?:  =(1 (rsh [0 15] x))          `@rh`0x7e00        :: log(x<0) -> NaN
      =/  sub  =(0 (dis 0x1f (rsh [0 10] x)))                  :: subnormal?
      =/  xx   ?:(sub (~(mul ^rh %n) x `@rh`0x6400) x)         :: *2^10
      =/  ae   ?:(sub -10 --0)
      =/  b    `@`xx
      =/  ef   (dif:si (new:si %.y (dis 0x1f (rsh [0 10] b))) --15)
      =/  m    `@rh`(con (dis b 0x3ff) 0x3c00)
      =/  big  (~(gte ^rh %n) m `@rh`0x3da8)                   :: m >= sqrt(2)
      =?  m    big  (~(mul ^rh %n) m `@rh`0x3800)              :: m * 0.5
      =?  ef   big  (sum:si ef --1)
      =.  ef   (sum:si ef ae)
      =/  f    (~(sub ^rh %n) m `@rh`0x3c00)
      =/  s    (~(div ^rh %n) f (~(add ^rh %n) m `@rh`0x3c00))
      =/  z    (~(mul ^rh %n) s s)
      =/  cs=(list @rh)  :~(`@rh`0x3555 `@rh`0x3266)
      =/  p2  (roll (flop cs) |=([c=@rh acc=@rh] (~(add ^rh %n) (~(mul ^rh %n) acc z) c)))
      =/  r   (~(mul ^rh %n) (~(add ^rh %n) z z) p2)
      =/  l1  (~(sub ^rh %n) f (~(mul ^rh %n) s (~(sub ^rh %n) f r)))
      =/  efa   (~(sun ^rh %n) (abs:si ef))
      =/  ef-f  ?:((syn:si ef) efa (~(sub ^rh %n) `@rh`0x0 efa))
      =/  hi  (~(mul ^rh %n) ef-f `@rh`0x3980)                 :: e*ln2hi
      =/  lo  (~(mul ^rh %n) ef-f `@rh`0x1dc8)                 :: e*ln2lo
      (~(add ^rh %n) hi (~(add ^rh %n) l1 lo))
    ::    +log-10:  @rh -> @rh
    ::
    ::  Returns the base-10 logarithm of a floating-point atom.
    ::    Examples
    ::      > (~(log-10 rh:math [%n .~~1e-2]) .~~10)
    ::      .~~1
    ::  Source
    ++  log-10
      ~/  %log-10
      ::  native f16: e*log10(2) + log(m)/ln10, reusing +lr (integer part added
      ::  with no division rounding).
      |=  x=@rh  ^-  @rh
      ?:  !(~(equ ^rh %n) x x)  `@rh`0x7e00
      ?:  =(x `@rh`0x7c00)  `@rh`0x7c00
      ?:  |(=(x `@rh`0x0) =(x `@rh`0x8000))  `@rh`0xfc00
      ?:  =(1 (rsh [0 15] x))  `@rh`0x7e00
      =/  el  (lr x)
      (~(add ^rh %n) (~(mul ^rh %n) ef.el `@rh`0x34d1) (~(mul ^rh %n) lm.el `@rh`0x36f3))
    ::    +log-2:  @rh -> @rh
    ::
    ::  Returns the base-2 logarithm of a floating-point atom.
    ::    Examples
    ::      > (~(log-2 rh:math [%n .~~1e-2]) .~~2)
    ::      .~~1
    ::  Source
    ++  log-2
      ~/  %log-2
      ::  native f16: e + log(m)/ln2 (integer part exact); see +lr.
      |=  x=@rh  ^-  @rh
      ?:  !(~(equ ^rh %n) x x)  `@rh`0x7e00
      ?:  =(x `@rh`0x7c00)  `@rh`0x7c00
      ?:  |(=(x `@rh`0x0) =(x `@rh`0x8000))  `@rh`0xfc00
      ?:  =(1 (rsh [0 15] x))  `@rh`0x7e00
      =/  el  (lr x)
      (~(add ^rh %n) ef.el (~(mul ^rh %n) lm.el `@rh`0x3dc5))
    ::  +lr: native f16 log reduction for finite positive x -> [e (as @rh),
    ::  log(mantissa)].  Mirrors the reduction inside +log; see +log-2 / +log-10.
    ++  lr
      |=  x=@rh  ^-  [ef=@rh lm=@rh]
      =/  sub  =(0 (dis 0x1f (rsh [0 10] x)))
      =/  xx   ?:(sub (~(mul ^rh %n) x `@rh`0x6400) x)        :: *2^10
      =/  ae   ?:(sub -10 --0)
      =/  b    `@`xx
      =/  e    (dif:si (new:si %.y (dis 0x1f (rsh [0 10] b))) --15)
      =/  m    `@rh`(con (dis b 0x3ff) 0x3c00)
      =/  big  (~(gte ^rh %n) m `@rh`0x3da8)                  :: m >= sqrt(2)
      =?  m    big  (~(mul ^rh %n) m `@rh`0x3800)
      =?  e    big  (sum:si e --1)
      =.  e    (sum:si e ae)
      =/  f    (~(sub ^rh %n) m `@rh`0x3c00)
      =/  s    (~(div ^rh %n) f (~(add ^rh %n) m `@rh`0x3c00))
      =/  z    (~(mul ^rh %n) s s)
      =/  cs=(list @rh)  :~(`@rh`0x3555 `@rh`0x3266)
      =/  p2  (roll (flop cs) |=([c=@rh a=@rh] (~(add ^rh %n) (~(mul ^rh %n) a z) c)))
      =/  r   (~(mul ^rh %n) (~(add ^rh %n) z z) p2)
      =/  l1  (~(sub ^rh %n) f (~(mul ^rh %n) s (~(sub ^rh %n) f r)))
      =/  efa  (~(sun ^rh %n) (abs:si e))
      =/  ef   ?:((syn:si e) efa (~(sub ^rh %n) `@rh`0x0 efa))
      [ef l1]
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
      ~/  %pow
      ::  native f16: positive-integer exponents via +pow-n; else exp(n*log x)
      ::  (the n*log x multiply honors the door's r).
      |=  [x=@rh n=@rh]  ^-  @rh
      ?:  &(=(n (san (need (toi n)))) (gth n .~~0))  (pow-n x (san (need (toi n))))
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
    ::      .~~1.414
    ::      > (sqt .~~1e3)
    ::      .~~31.61
    ::  Source
    ++  sqt
      ~/  %sqt
      ::  native f16: correctly-rounded stdlib (SoftFloat) f16 square root.
      |=  x=@rh  ^-  @rh
      (sqt:^rh x)
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
      ~/  %cbt
      ::  native f16: cbrt(x) = sign(x) * exp(log|x| / 3); all reals.
      |=  x=@rh  ^-  @rh
      ?:  !(~(equ ^rh %n) x x)  x                            :: NaN -> NaN
      ?:  |(=(x `@rh`0x0) =(x `@rh`0x8000))  x               :: +-0 -> +-0
      =/  ax  `@rh`(dis x 0x7fff)
      =/  r   (exp (~(mul ^rh %n) (log ax) `@rh`0x3555))
      ?:(=(1 (rsh [0 15] x)) (~(sub ^rh %n) `@rh`0x0 r) r)
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
    ~/  %rq
    ^|
    |_  $:  r=$?(%n %u %d %z)   :: round nearest, up, down, to zero
            rtol=_.~~~1e-20     :: relative tolerance: scales with |r|
            atol=_.~~~0         :: absolute tolerance floor (default 0)
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
    ++  invlog2  .~~~1.442695040888963387004650940070860
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
    ++  sun  ~(sun ^rq r)
    ::    +san:  @sd -> @rq
    ::
    ::  Returns the floating-point atom of a signed integer atom.
    ::    Examples
    ::      > (san --1)
    ::      .~~~1
    ::      > (san -1)
    ::      .~~~-1
    ::  Source
    ++  san  ~(san ^rq r)
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
    ++  toi  ~(toi ^rq r)
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
    ++  drg  ~(drg ^rq r)
    ::    +grd:  dn -> @rq
    ::
    ::  Returns the floating-point atom of a decimal form.
    ::    Examples
    ::      > (grd [%d s=%.y e=--0 a=1])
    ::      .~~~1
    ::      > (grd [%d s=%.y e=-1 a=11])
    ::      .~~~1.1
    ::  Source
    ++  grd  ~(grd ^rq r)
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
    ::    +is-close:  [@rq @rq] -> ?
    ::
    ::  Returns `%.y` if `|p - r| < atol + rtol × |r|`, combining an absolute
    ::  floor `atol` with a relative tolerance `rtol` (both from the +rq door).
    ::    Examples
    ::      > (is-close .~~~1 .~~~2)
    ::      %.n
    ::      > (is-close .~~~1 .~~~1.0000001)
    ::      %.n
    ::      > (~(is-close rq [%z .~~~1e-3]) .~~~1 .~~~1.0001)
    ::      %.y
    ::      > (~(is-close rq [%z .~~~1e-30]) .~~~1 .~~~1.0001)
    ::      %.n
    ::  Source
    ++  is-close
      |=  [p=@rq r=@rq]
      (lth (abs (sub p r)) (add atol (mul rtol (abs r))))
    ::    +all-close:  [@rq (list @rq)] -> ?
    ::
    ::  Returns `%.y` if `|p - r| < atol + rtol × |r|` for all `r` in `q`
    ::  (both tolerances provided by the +rq door).
    ::    Examples
    ::      > (all-close .~~~1 ~[.~~~1 .~~~2])
    ::      %.n
    ::      > (all-close .~~~1 ~[.~~~1 .~~~1.0000001])
    ::      %.n
    ::      > (~(all-close rq [%z .~~~1e-3]) .~~~1 ~[.~~~1 .~~~1.0001])
    ::      %.y
    ::  Source
    ++  all-close
      |=  [p=@rq q=(list @rq)]
      =/  i  0
      =/  n  (lent q)
      |-  ^-  ?
      ?:  =(n i)
        %.y
      ?.  (is-close p (snag i q))
        %.n
      $(i +(i))
    ::    +is-int:  @rq -> ?
    ::
    ::  Returns whether a floating-point value is an integer (no fractional part).
    ::    Examples
    ::      > (is-int .~~~1)
    ::      %.y
    ::      > (is-int .~~~1.1)
    ::      %.n
    ::  Source
    ++  is-int
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
    ++  add  ~(add ^rq r)
    ::    +sub:  [@rq @rq] -> @rq
    ::
    ::  Returns the difference of two floating-point atoms.
    ::    Examples
    ::      > (sub .~~~1 .~~~2)
    ::      .~~~-1
    ::  Source
    ++  sub  ~(sub ^rq r)
    ::    +mul:  [@rq @rq] -> @rq
    ::
    ::  Returns the product of two floating-point atoms.
    ::    Examples
    ::      > (mul .~~~1 .~~~2)
    ::      .~~~2
    ::  Source
    ++  mul  ~(mul ^rq r)
    ::    +div:  [@rq @rq] -> @rq
    ::
    ::  Returns the quotient of two floating-point atoms.
    ::    Examples
    ::      > (div .~~~1 .~~~2)
    ::      .~~~0.5
    ::  Source
    ++  div  ~(div ^rq r)
    ::    +fma:  [@rq @rq @rq] -> @rq
    ::
    ::  Returns the fused multiply-add of three floating-point atoms.
    ::    Examples
    ::      > (fma .~~~1 .~~~2 .~~~3)
    ::      .~~~5
    ::      > (fma .~~~2 .~~~3 .~~~4)
    ::      .~~~10
    ::  Source
    ++  fma  ~(fma ^rq r)
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
    ::  Crashes via `?>` if `x < 0` or if `x` is a positive non-integer (the
    ::  decrement loop passes through zero).
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
      ?:  (lth (abs x) rtol)
        t
      |-  ^-  @rq
      ?:  (lth (abs (sub x .~~~1)) rtol)
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
      ~/  %exp
      ::  Cody-Waite reduction + fdlibm rational reconstruction (deg-10 even
      ::  minimax P in t=r^2): exp(r) = 1 - ((lo - r*c/(2-c)) - hi), c = r-t*P(t).
      ::  Faithful to ~0.87 ULP (the compensated reconstruction keeps the leading
      ::  1+r out of the rounded sum, unlike a flat Horner).  Round-nearest-even
      ::  internally (matches the SoftFloat jet, see tools/rq_check.c).
      |=  x=@rq  ^-  @rq
      =/  pow2  |=(j=@s `@rq`(lsh [0 112] (abs:si (sum:si j --16.383))))
      =/  scale2
        |=  [p=@rq k=@s]  ^-  @rq
        ?:  (syn:si (dif:si k --16.384))
          (~(mul ^rq %n) (~(mul ^rq %n) p (pow2 --16.383)) (pow2 (dif:si k --16.383)))
        ?:  !(syn:si (sum:si k --16.382))
          (~(mul ^rq %n) (~(mul ^rq %n) p (pow2 (sum:si k --112))) (pow2 -112))
        (~(mul ^rq %n) p (pow2 k))
      ?:  !(~(equ ^rq %n) x x)    `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      ?:  =(x `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000
      ?:  =(x `@rq`0xffff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x0
      =/  log2e  `@rq`0x3fff.7154.7652.b82f.e177.7d0f.fda0.d23a
      =/  ln2hi  `@rq`0x3ffe.62e4.2fef.a39e.f357.93c8.0000.0000
      =/  ln2lo  `@rq`0xbfad.319f.f034.2542.fc32.f366.359d.274a
      =/  k=@s   (need (~(toi ^rq %n) (~(mul ^rq %n) x log2e)))
      ?:  (syn:si (dif:si k --16.385))    `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000
      ?:  !(syn:si (sum:si k --16.494))  `@rq`0x0
      =/  ka  (~(sun ^rq %n) (abs:si k))
      =/  kf  ?:((syn:si k) ka (~(sub ^rq %n) `@rq`0x0 ka))
      =/  hi   (~(sub ^rq %n) x (~(mul ^rq %n) kf ln2hi))     :: high part of r
      =/  lo   (~(mul ^rq %n) kf ln2lo)                       :: low correction
      =/  r    (~(sub ^rq %n) hi lo)                          :: reduced argument
      =/  t    (~(mul ^rq %n) r r)
      =/  ps=(list @rq)                                       :: even minimax P(t)
        :~  `@rq`0x3ffc.5555.5555.5555.5555.5555.5555.5555  `@rq`0xbff6.6c16.c16c.16c1.6c16.c16c.16c0.9e83
            `@rq`0x3ff1.1566.abc0.1156.6abc.0115.453d.96dd  `@rq`0xbfeb.bbd7.7933.4ef0.aac6.63e4.a6d6.5cca
            `@rq`0x3fe6.66a8.f2bf.70eb.da06.1159.86f5.07fb  `@rq`0xbfe1.2280.5d64.4267.43eb.0e28.8c2e.45a8
            `@rq`0x3fdb.d6db.2c4e.0507.12be.0476.b628.552f  `@rq`0xbfd6.7da4.e1ef.b419.eb83.8f5d.a821.635a
            `@rq`0x3fd1.3558.67f7.df64.dc61.daec.bfc0.d781  `@rq`0xbfcb.f56e.4264.f8ad.54bb.7852.bc52.bd9a
            `@rq`0x3fc6.8fc1.3579.bfe0.8221.6227.0789.ca71
        ==
      =/  pp  (roll (flop ps) |=([a=@rq acc=@rq] (~(add ^rq %n) (~(mul ^rq %n) acc t) a)))
      =/  cr  (~(sub ^rq %n) r (~(mul ^rq %n) t pp))          :: c = r - t*P(t)
      =/  rc  (~(div ^rq %n) (~(mul ^rq %n) r cr) (~(sub ^rq %n) `@rq`0x4000.0000.0000.0000.0000.0000.0000.0000 cr))
      =/  y   (~(sub ^rq %n) `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000 (~(sub ^rq %n) (~(sub ^rq %n) lo rc) hi))
      (scale2 y k)
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
      ~/  %sin
      ::  q*pi/2 reduction + fdlibm kernels (f128); see +rq-trig.
      |=  x=@rq  ^-  @rq
      ?:  !(~(equ ^rq %n) x x)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      ?:  |(=(x `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000) =(x `@rq`0xffff.0000.0000.0000.0000.0000.0000.0000))  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      ?:  |(=(x `@rq`0x0) =(x `@rq`0x8000.0000.0000.0000.0000.0000.0000.0000))  x
      %-  trig-fin:rq-trig
      [%.y `@rq`(dis x 0x7fff.ffff.ffff.ffff.ffff.ffff.ffff.ffff) (rsh [0 127] x)]
    ++  rq-trig
      |%
      ++  sc
        ^-  (list @rq)
        :~  `@rq`0xbffc.5555.5555.5555.5555.5555.5555.5555
            `@rq`0x3ff8.1111.1111.1111.1111.1111.1111.1111
            `@rq`0xbff2.a01a.01a0.1a01.a01a.01a0.1a01.a01a
            `@rq`0x3fec.71de.3a55.6c73.38fa.ac1c.88e5.0017
            `@rq`0xbfe5.ae64.567f.544e.38fe.747e.4b83.7dc7
            `@rq`0x3fde.6124.613a.86d0.97ca.3833.1d23.af68
            `@rq`0xbfd6.ae7f.3e73.3b81.f11d.8656.b0ee.8cb0
            `@rq`0x3fce.952c.7703.0ad4.a6b2.6051.9777.1b00
            `@rq`0xbfc6.2f49.b468.1415.724c.a1ec.3b7b.9675
            `@rq`0x3fbd.71b8.ef6d.cf57.18be.f146.fcee.6e45
            `@rq`0xbfb4.761b.4131.6381.9d97.b870.4dd7.f628
            `@rq`0x3fab.3f3c.cdd1.65fa.8d4e.44a4.1977.6f11
            `@rq`0xbfa1.d1ab.1c2d.ccea.320a.9a18.f15d.4277
            `@rq`0x3f98.259f.98b4.358a.d7ab.e30e.7766.f129
            `@rq`0xbf8e.434d.2e78.3f5b.c42e.1ee4.6fa6.bfc4
            `@rq`0x3f84.3981.254d.d0d5.1b53.82cd.ffa9.7422
        ==
      ++  cc
        ^-  (list @rq)
        :~  `@rq`0x3ffa.5555.5555.5555.5555.5555.5555.5555
            `@rq`0xbff5.6c16.c16c.16c1.6c16.c16c.16c1.6c17
            `@rq`0x3fef.a01a.01a0.1a01.a01a.01a0.1a01.a01a
            `@rq`0xbfe9.27e4.fb77.89f5.c72e.f016.d3ea.6679
            `@rq`0x3fe2.1eed.8eff.8d89.7b54.4da9.87ac.fe85
            `@rq`0xbfda.9397.4a8c.07c9.d20b.adf1.45df.a3e5
            `@rq`0x3fd2.ae7f.3e73.3b81.f11d.8656.b0ee.8cb0
            `@rq`0xbfca.6827.863b.97d9.77bb.0048.86a2.c2ab
            `@rq`0x3fc1.e542.ba40.2022.507a.9cad.2bf8.f0bb
            `@rq`0xbfb9.0ce3.96db.7f85.2945.0c90.b7f3.38ec
            `@rq`0x3faf.f2cf.0197.2f57.7cca.4b40.67ca.9d8a
            `@rq`0xbfa6.88e8.5fc6.a4e5.9a38.f205.0ba6.b015
            `@rq`0x3f9d.0a18.a263.5085.d373.c5c5.1c35.4a8d
            `@rq`0xbf93.3932.c504.7d60.e60c.aded.4c29.89c5
            `@rq`0x3f89.434d.2e78.3f5b.c42e.1ee4.6fa6.bfc4
            `@rq`0xbf7f.2710.231c.0fd7.a13f.8a2b.4af9.d6b7
        ==
      ++  neg  |=(a=@rq ^-(@rq (~(sub ^rq %n) `@rq`0x0 a)))
      ++  ksin
        |=  [xx=@rq yy=@rq]  ^-  @rq
        =/  z   (~(mul ^rq %n) xx xx)
        =/  r   (roll (flop (tail sc)) |=([c=@rq a=@rq] (~(add ^rq %n) (~(mul ^rq %n) a z) c)))
        =/  v   (~(mul ^rq %n) z xx)
        =/  aa  (~(sub ^rq %n) (~(mul ^rq %n) `@rq`0x3ffe.0000.0000.0000.0000.0000.0000.0000 yy) (~(mul ^rq %n) v r))
        =/  bb  (~(sub ^rq %n) (~(mul ^rq %n) z aa) yy)
        =/  dd  (~(sub ^rq %n) bb (~(mul ^rq %n) v (head sc)))
        (~(sub ^rq %n) xx dd)
      ++  kcos
        |=  [xx=@rq yy=@rq]  ^-  @rq
        =/  z   (~(mul ^rq %n) xx xx)
        =/  rc  (roll (flop cc) |=([c=@rq a=@rq] (~(add ^rq %n) (~(mul ^rq %n) a z) c)))
        =/  hz  (~(mul ^rq %n) `@rq`0x3ffe.0000.0000.0000.0000.0000.0000.0000 z)
        =/  w2  (~(sub ^rq %n) `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000 hz)
        =/  aa  (~(sub ^rq %n) (~(sub ^rq %n) `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000 w2) hz)
        =/  bb  (~(sub ^rq %n) (~(mul ^rq %n) (~(mul ^rq %n) z z) rc) (~(mul ^rq %n) xx yy))
        (~(add ^rq %n) w2 (~(add ^rq %n) aa bb))
      ++  trig-fin
        |=  [s=? ax=@rq sb=@]  ^-  @rq
        =/  q   (need (~(toi ^rq %n) (~(mul ^rq %n) ax `@rq`0x3ffe.45f3.06dc.9c88.2a53.f84e.afa3.ea6a)))
        =/  qf  (~(sun ^rq %n) (abs:si q))
        =/  t   (~(sub ^rq %n) ax (~(mul ^rq %n) qf `@rq`0x3fff.921f.b544.42d1.8460.0000.0000.0000))
        =/  w   (~(mul ^rq %n) qf `@rq`0x3fc2.3131.98a2.e037.0734.4a40.9382.229a)
        =/  rhi  (~(sub ^rq %n) t w)
        =/  rlo  (~(sub ^rq %n) (~(sub ^rq %n) t rhi) w)
        =/  m   (dis (abs:si q) 3)
        =/  ks  (ksin rhi rlo)
        =/  kc  (kcos rhi rlo)
        ?:  s
          =/  v  ?:(=(m 0) ks ?:(=(m 1) kc ?:(=(m 2) (neg ks) (neg kc))))
          ?:(=(sb 1) (neg v) v)
        ?:(=(m 0) kc ?:(=(m 1) (neg ks) ?:(=(m 2) (neg kc) ks)))
      --
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
      ~/  %cos
      |=  x=@rq  ^-  @rq
      ?:  !(~(equ ^rq %n) x x)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      ?:  |(=(x `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000) =(x `@rq`0xffff.0000.0000.0000.0000.0000.0000.0000))  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      %-  trig-fin:rq-trig
      [%.n `@rq`(dis x 0x7fff.ffff.ffff.ffff.ffff.ffff.ffff.ffff) 0]
    ::    +tan:  @rq -> @rq
    ::
    ::  Returns the tangent of a floating-point atom.
    ::    Examples
    ::      > (tan .~~~1)
    ::      .~~~1.5574077246549022305069793269617903
    ::      > (tan .~~~2)
    ::      .~~~-2.1850398632615189916433278966958165
    ::      > (tan pi)
    ::      .~~~0  :: TODO: verify on ship
    ::  Source
    ++  tan
      ~/  %tan
      |=  x=@rq  ^-  @rq
      (div (sin x) (cos x))
    ::  +asin:  @rq -> @rq
    ::
    ::  Returns the inverse sine of a floating-point atom.
    ::    Examples
    ::      > (asin .~~~0)
    ::      .~~~0
    ::      > (asin .~~~1)
    ::      .~~~1.5707963267948966192313216916397514
    ::      > (asin .~~~0.7)
    ::      .~~~0.7753974966107530637394463388579305
    ::
    ++  asin
      ~/  %asin
      ::  fdlibm rational-form kernel (poly R, deg-30) + sqrt head/tail; f128.
      |=  x=@rq  ^-  @rq
      (asn:rq-ainv x)
    ++  acos
      ~/  %acos
      |=  x=@rq  ^-  @rq
      (acs:rq-ainv x)
    ++  rq-ainv
      |%
      ++  rr
        |=  t=@rq  ^-  @rq
        =/  cs=(list @rq)
          :~  `@rq`0x3f80.8991.2e54.d43f.83f4.00d5.0d55.a7e8  `@rq`0x3ffc.5555.5555.5555.5555.5555.5555.5552
              `@rq`0x3ffb.3333.3333.3333.3333.3333.3333.5009  `@rq`0x3ffa.6db6.db6d.b6db.6db6.db6d.b668.951b
              `@rq`0x3ff9.f1c7.1c71.c71c.71c7.1c72.bac1.ec0e  `@rq`0x3ff9.6e8b.a2e8.ba2e.8ba2.e81a.a31a.41d8
              `@rq`0x3ff9.1c4e.c4ec.4ec4.ec4f.0b6f.e680.df37  `@rq`0x3ff8.c999.9999.9999.996c.f372.753e.7c99
              `@rq`0x3ff8.7a87.8787.8787.9217.7cc3.2753.53f7  `@rq`0x3ff8.3fde.50d7.9433.f7f1.afe5.cbea.c090
              `@rq`0x3ff8.12ef.3cf3.cf83.f127.037c.33f0.fb5b  `@rq`0x3ff7.df3b.d37a.5ede.5c11.4e9d.a47d.edfd
              `@rq`0x3ff7.a686.3d72.3133.909e.0729.7f5e.2958  `@rq`0x3ff7.782d.d9f3.ff64.52ce.6cb8.56ef.901f
              `@rq`0x3ff7.51ba.328f.884c.2fdb.62f4.1c70.9bd1  `@rq`0x3ff7.3168.1fe6.e02d.b0d4.9614.21ef.a7ec
              `@rq`0x3ff7.15ef.e556.e52a.3408.e80b.5b3f.8641  `@rq`0x3ff6.fc96.253e.cb71.1812.7122.68a4.5b42
              `@rq`0x3ff6.d4a8.2428.408a.ebe9.bc6e.47ec.09af  `@rq`0x3ff6.aa37.7fe9.13f6.c0a9.facb.9451.1de4
              `@rq`0x3ff6.b48c.a21a.48d1.8473.7463.fe86.56d8  `@rq`0x3ff5.7e9a.a4b5.b4c4.e391.3451.8885.e78f
              `@rq`0x3ff8.1906.4c51.85fa.a0d0.3ab9.c514.26b2  `@rq`0xbff9.300f.0da2.da1e.1c08.5f96.2a89.aacc
              `@rq`0x3ffb.0643.398c.dbcb.97d2.5a1b.e10b.6c8a  `@rq`0xbffc.27ed.3dd5.cd82.528e.0d54.bf4f.5e05
              `@rq`0x3ffd.1d64.319b.e957.ad88.0c8c.d533.b68b  `@rq`0xbffd.9731.e485.678b.81c3.902a.2c54.acc7
              `@rq`0x3ffd.ae10.872f.69b7.a99a.a13d.6e9c.d204  `@rq`0xbffd.228f.c652.7609.25c1.64c7.f610.91fa
              `@rq`0x3ffb.9aa4.ca63.cbd7.2c15.b8ad.9b23.77ce
          ==
        (roll (flop cs) |=([c=@rq a=@rq] (~(add ^rq %n) (~(mul ^rq %n) a t) c)))
      ++  asn
        |=  x=@rq  ^-  @rq
        ?:  !(~(equ ^rq %n) x x)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
        =/  sgn  (rsh [0 127] x)
        =/  ax   `@rq`(dis x 0x7fff.ffff.ffff.ffff.ffff.ffff.ffff.ffff)
        ?:  (~(gth ^rq %n) ax `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
        ?:  =(ax `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000)  (~(add ^rq %n) (~(mul ^rq %n) x `@rq`0x3fff.921f.b544.42d1.8469.898c.c517.01b8) (~(mul ^rq %n) x `@rq`0x3f8c.cd12.9024.e088.a67c.c740.20bb.ea64))
        ?:  (~(lth ^rq %n) ax `@rq`0x3ffe.0000.0000.0000.0000.0000.0000.0000)
          ?:  (~(lth ^rq %n) ax `@rq`0x3fc6.0000.0000.0000.0000.0000.0000.0000)  x
          (~(add ^rq %n) x (~(mul ^rq %n) x (rr (~(mul ^rq %n) x x))))
        =/  w  (~(sub ^rq %n) `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000 ax)
        =/  t  (~(mul ^rq %n) w `@rq`0x3ffe.0000.0000.0000.0000.0000.0000.0000)
        =/  r  (rr t)
        =/  s  (sqt t)
        ?:  (~(gte ^rq %n) ax `@rq`0x3ffe.f333.3333.3333.3333.3333.3333.3333)
          =/  res  (~(sub ^rq %n) `@rq`0x3fff.921f.b544.42d1.8469.898c.c517.01b8 (~(sub ^rq %n) (~(mul ^rq %n) `@rq`0x4000.0000.0000.0000.0000.0000.0000.0000 (~(add ^rq %n) s (~(mul ^rq %n) s r))) `@rq`0x3f8c.cd12.9024.e088.a67c.c740.20bb.ea64))
          ?:(=(sgn 1) (~(sub ^rq %n) `@rq`0x0 res) res)
        =/  df  `@rq`(dis s 0xffff.ffff.ffff.ffff.ff00.0000.0000.0000)
        =/  c   (~(div ^rq %n) (~(sub ^rq %n) t (~(mul ^rq %n) df df)) (~(add ^rq %n) s df))
        =/  p2  (~(sub ^rq %n) (~(mul ^rq %n) `@rq`0x4000.0000.0000.0000.0000.0000.0000.0000 (~(mul ^rq %n) s r)) (~(sub ^rq %n) `@rq`0x3f8c.cd12.9024.e088.a67c.c740.20bb.ea64 (~(mul ^rq %n) `@rq`0x4000.0000.0000.0000.0000.0000.0000.0000 c)))
        =/  q2  (~(sub ^rq %n) `@rq`0x3ffe.921f.b544.42d1.8469.898c.c517.01b8 (~(mul ^rq %n) `@rq`0x4000.0000.0000.0000.0000.0000.0000.0000 df))
        =/  res  (~(sub ^rq %n) `@rq`0x3ffe.921f.b544.42d1.8469.898c.c517.01b8 (~(sub ^rq %n) p2 q2))
        ?:(=(sgn 1) (~(sub ^rq %n) `@rq`0x0 res) res)
      ++  acs
        |=  x=@rq  ^-  @rq
        ?:  !(~(equ ^rq %n) x x)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
        =/  neg  (rsh [0 127] x)
        =/  ax   `@rq`(dis x 0x7fff.ffff.ffff.ffff.ffff.ffff.ffff.ffff)
        ?:  (~(gth ^rq %n) ax `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
        ?:  =(ax `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000)
          ?:  =(neg 0)  `@rq`0x0
          (~(add ^rq %n) `@rq`0x4000.921f.b544.42d1.8469.898c.c517.01b8 (~(mul ^rq %n) `@rq`0x4000.0000.0000.0000.0000.0000.0000.0000 `@rq`0x3f8c.cd12.9024.e088.a67c.c740.20bb.ea64))
        ?:  (~(lth ^rq %n) ax `@rq`0x3ffe.0000.0000.0000.0000.0000.0000.0000)
          ?:  (~(lth ^rq %n) ax `@rq`0x3f87.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x3fff.921f.b544.42d1.8469.898c.c517.01b8
          =/  z  (~(mul ^rq %n) x x)
          =/  r  (rr z)
          (~(sub ^rq %n) `@rq`0x3fff.921f.b544.42d1.8469.898c.c517.01b8 (~(sub ^rq %n) x (~(sub ^rq %n) `@rq`0x3f8c.cd12.9024.e088.a67c.c740.20bb.ea64 (~(mul ^rq %n) x r))))
        ?:  =(neg 1)
          =/  z  (~(mul ^rq %n) (~(add ^rq %n) `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000 x) `@rq`0x3ffe.0000.0000.0000.0000.0000.0000.0000)
          =/  s  (sqt z)
          =/  r  (rr z)
          =/  w  (~(sub ^rq %n) (~(mul ^rq %n) r s) `@rq`0x3f8c.cd12.9024.e088.a67c.c740.20bb.ea64)
          (~(sub ^rq %n) `@rq`0x4000.921f.b544.42d1.8469.898c.c517.01b8 (~(mul ^rq %n) `@rq`0x4000.0000.0000.0000.0000.0000.0000.0000 (~(add ^rq %n) s w)))
        =/  z   (~(mul ^rq %n) (~(sub ^rq %n) `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000 x) `@rq`0x3ffe.0000.0000.0000.0000.0000.0000.0000)
        =/  s   (sqt z)
        =/  df  `@rq`(dis s 0xffff.ffff.ffff.ffff.ff00.0000.0000.0000)
        =/  c   (~(div ^rq %n) (~(sub ^rq %n) z (~(mul ^rq %n) df df)) (~(add ^rq %n) s df))
        =/  r   (rr z)
        =/  w   (~(add ^rq %n) (~(mul ^rq %n) r s) c)
        (~(mul ^rq %n) `@rq`0x4000.0000.0000.0000.0000.0000.0000.0000 (~(add ^rq %n) df w))
      --
    ::  +acos:  @rq -> @rq
    ::
    ::  Returns the inverse cosine of a floating-point atom.
    ::    Examples
    ::      > (acos .~~~0)
    ::      .~~~1.5707963267948966192313216916397514
    ::      > (acos .~~~1)
    ::      .~~~0
    ::      > (acos .~~~0.7)
    ::      .~~~0.7953988301841435554899943710156033
    ::
    ::  +atan:  @rq -> @rq
    ::
    ::  Returns the inverse tangent of a floating-point atom.
    ::    Examples
    ::      > (atan .~~~1)
    ::      .~~~0.7853981633974483096146231179876219
    ::      > (atan .~~~2)
    ::      .~~~1.1071487177940905030161167763325275
    ::      > (atan pi)
    ::      .~~~1.2626272556789116834540013074115034
    ::
    ++  atan
      ~/  %atan
      ::  fdlibm breakpoint reduction + degree-30 minimax (f128); odd.
      |=  x=@rq  ^-  @rq
      ?:  !(~(equ ^rq %n) x x)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      ?:  =(x `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x3fff.921f.b544.42d1.8469.898c.c517.01b8
      ?:  =(x `@rq`0xffff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0xbfff.921f.b544.42d1.8469.898c.c517.01b8
      ?:  |(=(x `@rq`0x0) =(x `@rq`0x8000.0000.0000.0000.0000.0000.0000.0000))  x
      =/  neg  (rsh [0 127] x)
      =/  r    (ker:rq-atan `@rq`(dis x 0x7fff.ffff.ffff.ffff.ffff.ffff.ffff.ffff))
      ?:(=(neg 1) (~(sub ^rq %n) `@rq`0x0 r) r)
    ++  rq-atan
      |%
      ++  at
        ^-  (list @rq)
        :~  `@rq`0x3ffd.5555.5555.5555.5555.5555.5555.5555  `@rq`0xbffc.9999.9999.9999.9999.9999.9999.999a
            `@rq`0x3ffc.2492.4924.9249.2492.4924.9249.2492  `@rq`0xbffb.c71c.71c7.1c71.c71c.71c7.1c71.c705
            `@rq`0x3ffb.745d.1745.d174.5d17.45d1.745c.f720  `@rq`0xbffb.3b13.b13b.13b1.3b13.b13b.1395.a0f6
            `@rq`0x3ffb.1111.1111.1111.1111.1111.010e.24e1  `@rq`0xbffa.e1e1.e1e1.e1e1.e1e1.e1d4.8fd7.bd0f
            `@rq`0x3ffa.af28.6bca.1af2.86bc.9d8a.1266.1ce3  `@rq`0xbffa.8618.6186.1861.8617.62af.171f.46fb
            `@rq`0x3ffa.642c.8590.b216.4297.f77f.1796.654a  `@rq`0xbffa.47ae.147a.e147.a6ae.eb97.4d91.c763
            `@rq`0x3ffa.2f68.4bda.12f5.982c.8384.0df4.8c76  `@rq`0xbffa.1a7b.9611.a7a0.f241.3484.8b6f.9bc3
            `@rq`0x3ffa.0842.1084.1eed.46f1.272e.8718.edfe  `@rq`0xbff9.f07c.1f07.73e1.dac4.76af.1946.ed1a
            `@rq`0x3ff9.d41d.41cf.56a0.771b.4773.d1fd.bc46  `@rq`0xbff9.bacf.910c.a5ea.d9a2.c9f0.ffa2.8317
            `@rq`0x3ff9.a41a.3ed6.e709.5da3.c3e4.8cd5.5593  `@rq`0xbff9.8f9b.fe02.ad67.4d8a.c158.72fb.b51c
            `@rq`0x3ff9.7d05.170d.1702.069f.17b9.5f6e.54f4  `@rq`0xbff9.6c10.bc42.d041.6ea0.5add.542a.f078
            `@rq`0x3ff9.5c74.e7f6.f412.eab1.21f2.0635.a41a  `@rq`0xbff9.4dac.2e21.aa0e.283b.7552.8258.306b
            `@rq`0x3ff9.3e57.3faa.c561.db9d.4b05.d70c.99cf  `@rq`0xbff9.2af3.2cae.28f7.4f59.6690.8860.d11a
            `@rq`0x3ff9.0c8b.03c5.5304.dec0.acdf.4b35.6e20  `@rq`0xbff8.b4ed.3a33.49ac.f7ad.9702.76c5.cf2b
            `@rq`0x3ff8.261c.1a9e.da3a.d5dd.688f.198a.e3cb  `@rq`0xbff7.1a7a.c449.b285.876f.4b57.d627.e71e
            `@rq`0x3ff5.1a4e.a418.ebe8.1381.31c1.5128.032a
        ==
      ++  atred
        |=  ax=@rq  ^-  [xr=@rq hi=@rq lo=@rq dir=?]
        ?:  (~(lth ^rq %n) ax `@rq`0x3ffd.c000.0000.0000.0000.0000.0000.0000)  [ax `@rq`0x0 `@rq`0x0 %.y]
        ?:  (~(lth ^rq %n) ax `@rq`0x3ffe.6000.0000.0000.0000.0000.0000.0000)
          :*  (~(div ^rq %n) (~(sub ^rq %n) (~(add ^rq %n) ax ax) `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000) (~(add ^rq %n) `@rq`0x4000.0000.0000.0000.0000.0000.0000.0000 ax))
              `@rq`0x3ffd.dac6.7056.1bb4.f68a.dfc8.8bd9.7875  `@rq`0x3f89.a06d.c282.b0e4.c39b.e01c.59e2.dcdd  %.n  ==
        ?:  (~(lth ^rq %n) ax `@rq`0x3fff.3000.0000.0000.0000.0000.0000.0000)
          :*  (~(div ^rq %n) (~(sub ^rq %n) ax `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000) (~(add ^rq %n) ax `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000))
              `@rq`0x3ffe.921f.b544.42d1.8469.898c.c517.01b8  `@rq`0x3f8b.cd12.9024.e088.a67c.c740.20bb.ea64  %.n  ==
        ?:  (~(lth ^rq %n) ax `@rq`0x4000.3800.0000.0000.0000.0000.0000.0000)
          :*  (~(div ^rq %n) (~(sub ^rq %n) ax `@rq`0x3fff.8000.0000.0000.0000.0000.0000.0000) (~(add ^rq %n) `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000 (~(mul ^rq %n) `@rq`0x3fff.8000.0000.0000.0000.0000.0000.0000 ax)))
              `@rq`0x3ffe.f730.bd28.1f69.b200.f10f.5e19.7794  `@rq`0xbf8b.ebe5.66c9.9ada.9f23.1bcc.ae27.916c  %.n  ==
        :*  (~(div ^rq %n) `@rq`0xbfff.0000.0000.0000.0000.0000.0000.0000 ax)  `@rq`0x3fff.921f.b544.42d1.8469.898c.c517.01b8  `@rq`0x3f8c.cd12.9024.e088.a67c.c740.20bb.ea64  %.n  ==
      ++  ker
        |=  ax=@rq  ^-  @rq
        =/  q   (atred ax)
        =/  z   (~(mul ^rq %n) xr.q xr.q)
        =/  s   (~(mul ^rq %n) z (roll (flop at) |=([c=@rq a=@rq] (~(add ^rq %n) (~(mul ^rq %n) a z) c))))
        ?:  dir.q  (~(sub ^rq %n) xr.q (~(mul ^rq %n) xr.q s))
        (~(sub ^rq %n) hi.q (~(sub ^rq %n) (~(sub ^rq %n) (~(mul ^rq %n) xr.q s) lo.q) xr.q))
      --
    ::  +atan2:  [@rq @rq] -> @rq
    ::
    ::  Returns the inverse tangent of a floating-point coordinate.
    ::  Returns 0 for NaN inputs (does not propagate NaN).  The case (0,0) returns 0.
    ::    Examples
    ::      > (atan2 .~~~0 .~~~1)
    ::      .~~~0
    ::      > (atan2 .~~~-1 .~~~0)
    ::      .~~~-1.5707963267948966192313216916397514
    ::      > (atan2 .~~~0.5 .~~~-0.5)
    ::      .~~~2.3561944901923449288480202652918806
    ::
    ++  atan2
      ~/  %atan2
      |=  [y=@rq x=@rq]  ^-  @rq
      ?:  (gth x `@rq`0x0)  (atan (div y x))
      ?:  &((lth x `@rq`0x0) (gte y `@rq`0x0))  (add (atan (div y x)) `@rq`0x4000.921f.b544.42d1.8469.898c.c517.01b8)
      ?:  &((lth x `@rq`0x0) (lth y `@rq`0x0))  (sub (atan (div y x)) `@rq`0x4000.921f.b544.42d1.8469.898c.c517.01b8)
      ?:  &(=(`@rq`0x0 x) (gth y `@rq`0x0))  `@rq`0x3fff.921f.b544.42d1.8469.898c.c517.01b8
      ?:  &(=(`@rq`0x0 x) (lth y `@rq`0x0))  `@rq`0xbfff.921f.b544.42d1.8469.898c.c517.01b8
      `@rq`0x0

    ::    +pow-n:  [@rq @rq] -> @rq
    ::
    ::  Returns the power of a floating-point atom to a positive integer exponent.
    ::    Examples
    ::      > (pow-n .~~~2 .~~~2)
    ::      .~~~4
    ::      > (pow-n .~~~2 .~~~-2)
    ::      .~~~0.25
    ::  Source
    ++  pow-n
      ~/  %pow-n
      |=  [x=@rq n=@rq]  ^-  @rq
      ?:  =(n `@rq`0x0)  `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000
      ?>  &((gth n `@rq`0x0) (is-int n))
      =/  p  x
      |-  ^-  @rq
      ?:  (lth n `@rq`0x4000.0000.0000.0000.0000.0000.0000.0000)
        p
      $(n (sub n `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000), p (mul p x))
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
      ~/  %log
      ::  x = 2^e * m reduction + atanh series (fdlibm f - s*(f-R)); f128.
      ::  Round-nearest-even internally (matches tools/rq_check.c).
      |=  x=@rq  ^-  @rq
      ?:  !(~(equ ^rq %n) x x)    `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      ?:  =(x `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000
      ?:  |(=(x `@rq`0x0) =(x `@rq`0x8000.0000.0000.0000.0000.0000.0000.0000))  `@rq`0xffff.0000.0000.0000.0000.0000.0000.0000
      ?:  =(1 (rsh [0 127] x))  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      =/  sub  =(0 (dis 0x7fff (rsh [0 112] x)))
      =/  xx   ?:(sub (~(mul ^rq %n) x `@rq`0x4077.0000.0000.0000.0000.0000.0000.0000) x)
      =/  ae   ?:(sub -120 --0)
      =/  b    `@`xx
      =/  e    (dif:si (new:si %.y (dis 0x7fff (rsh [0 112] b))) --16.383)
      =/  m    `@rq`(con (dis b 0xffff.ffff.ffff.ffff.ffff.ffff.ffff) 0x3fff.0000.0000.0000.0000.0000.0000.0000)
      =/  big  (~(gte ^rq %n) m `@rq`0x3fff.6a09.e667.f3bc.c908.b2fb.1366.ea95)
      =?  m    big  (~(mul ^rq %n) m `@rq`0x3ffe.0000.0000.0000.0000.0000.0000.0000)
      =?  e    big  (sum:si e --1)
      =.  e    (sum:si e ae)
      =/  f    (~(sub ^rq %n) m `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000)
      =/  s    (~(div ^rq %n) f (~(add ^rq %n) m `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000))
      =/  z    (~(mul ^rq %n) s s)
      =/  cs=(list @rq)
        :~  `@rq`0x3ffd.5555.5555.5555.5555.5555.5555.5555  `@rq`0x3ffc.9999.9999.9999.9999.9999.9999.999a
            `@rq`0x3ffc.2492.4924.9249.2492.4924.9249.2492  `@rq`0x3ffb.c71c.71c7.1c71.c71c.71c7.1c71.c71c
            `@rq`0x3ffb.745d.1745.d174.5d17.45d1.745d.1746  `@rq`0x3ffb.3b13.b13b.13b1.3b13.b13b.13b1.3b14
            `@rq`0x3ffb.1111.1111.1111.1111.1111.1111.1111  `@rq`0x3ffa.e1e1.e1e1.e1e1.e1e1.e1e1.e1e1.e1e2
            `@rq`0x3ffa.af28.6bca.1af2.86bc.a1af.286b.ca1b  `@rq`0x3ffa.8618.6186.1861.8618.6186.1861.8618
            `@rq`0x3ffa.642c.8590.b216.42c8.590b.2164.2c86  `@rq`0x3ffa.47ae.147a.e147.ae14.7ae1.47ae.147b
            `@rq`0x3ffa.2f68.4bda.12f6.84bd.a12f.684b.da13  `@rq`0x3ffa.1a7b.9611.a7b9.611a.7b96.11a7.b961
            `@rq`0x3ffa.0842.1084.2108.4210.8421.0842.1084  `@rq`0x3ff9.f07c.1f07.c1f0.7c1f.07c1.f07c.1f08
            `@rq`0x3ff9.d41d.41d4.1d41.d41d.41d4.1d41.d41d  `@rq`0x3ff9.bacf.914c.1bac.f914.c1ba.cf91.4c1c
            `@rq`0x3ff9.a41a.41a4.1a41.a41a.41a4.1a41.a41a  `@rq`0x3ff9.8f9c.18f9.c18f.9c18.f9c1.8f9c.18fa
            `@rq`0x3ff9.7d05.f417.d05f.417d.05f4.17d0.5f41  `@rq`0x3ff9.6c16.c16c.16c1.6c16.c16c.16c1.6c17
            `@rq`0x3ff9.5c98.82b9.3105.7262.0ae4.c415.c988
        ==
      =/  p2  (roll (flop cs) |=([c=@rq acc=@rq] (~(add ^rq %n) (~(mul ^rq %n) acc z) c)))
      =/  r   (~(mul ^rq %n) (~(add ^rq %n) z z) p2)
      =/  l1  (~(sub ^rq %n) f (~(mul ^rq %n) s (~(sub ^rq %n) f r)))
      =/  efa   (~(sun ^rq %n) (abs:si e))
      =/  ef    ?:((syn:si e) efa (~(sub ^rq %n) `@rq`0x0 efa))
      =/  hi  (~(mul ^rq %n) ef `@rq`0x3ffe.62e4.2fef.a39e.f357.93c8.0000.0000)
      =/  lo  (~(mul ^rq %n) ef `@rq`0xbfad.319f.f034.2542.fc32.f366.359d.274a)
      (~(add ^rq %n) hi (~(add ^rq %n) l1 lo))
    ::    +log-10:  @rq -> @rq
    ::
    ::  Returns the base-10 logarithm of a floating-point atom.
    ::    Examples
    ::      > (~(log-10 rq:math [%n .~~~1e-20]) .~~~10)
    ::      .~~~1
    ::  Source
    ++  log-10
      ~/  %log-10
      |=  x=@rq  ^-  @rq
      ?:  !(~(equ ^rq %n) x x)    `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      ?:  =(x `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000
      ?:  |(=(x `@rq`0x0) =(x `@rq`0x8000.0000.0000.0000.0000.0000.0000.0000))  `@rq`0xffff.0000.0000.0000.0000.0000.0000.0000
      ?:  =(1 (rsh [0 127] x))  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      =/  el  (lr x)
      (~(add ^rq %n) (~(mul ^rq %n) ef.el `@rq`0x3ffd.3441.3509.f79f.ef31.1f12.b358.16f9) (~(mul ^rq %n) lm.el `@rq`0x3ffd.bcb7.b152.6e50.e32a.6ab7.555f.5a68))
    ::    +log-2:  @rq -> @rq
    ::
    ::  Returns the base-2 logarithm of a floating-point atom.
    ::    Examples
    ::      > (~(log-2 rq:math [%n .~~~1e-20]) .~~~2)
    ::      .~~~1
    ::  Source
    ::  +lr: log reduction for finite positive @rq x -> [e (as @rq), log(mantissa)].
    ++  lr
      |=  x=@rq  ^-  [ef=@rq lm=@rq]
      =/  sub  =(0 (dis 0x7fff (rsh [0 112] x)))
      =/  xx   ?:(sub (~(mul ^rq %n) x `@rq`0x4077.0000.0000.0000.0000.0000.0000.0000) x)
      =/  ae   ?:(sub -120 --0)
      =/  b    `@`xx
      =/  e    (dif:si (new:si %.y (dis 0x7fff (rsh [0 112] b))) --16.383)
      =/  m    `@rq`(con (dis b 0xffff.ffff.ffff.ffff.ffff.ffff.ffff) 0x3fff.0000.0000.0000.0000.0000.0000.0000)
      =/  big  (~(gte ^rq %n) m `@rq`0x3fff.6a09.e667.f3bc.c908.b2fb.1366.ea95)
      =?  m    big  (~(mul ^rq %n) m `@rq`0x3ffe.0000.0000.0000.0000.0000.0000.0000)
      =?  e    big  (sum:si e --1)
      =.  e    (sum:si e ae)
      =/  f    (~(sub ^rq %n) m `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000)
      =/  s    (~(div ^rq %n) f (~(add ^rq %n) m `@rq`0x3fff.0000.0000.0000.0000.0000.0000.0000))
      =/  z    (~(mul ^rq %n) s s)
      =/  cs=(list @rq)
        :~  `@rq`0x3ffd.5555.5555.5555.5555.5555.5555.5555  `@rq`0x3ffc.9999.9999.9999.9999.9999.9999.999a
            `@rq`0x3ffc.2492.4924.9249.2492.4924.9249.2492  `@rq`0x3ffb.c71c.71c7.1c71.c71c.71c7.1c71.c71c
            `@rq`0x3ffb.745d.1745.d174.5d17.45d1.745d.1746  `@rq`0x3ffb.3b13.b13b.13b1.3b13.b13b.13b1.3b14
            `@rq`0x3ffb.1111.1111.1111.1111.1111.1111.1111  `@rq`0x3ffa.e1e1.e1e1.e1e1.e1e1.e1e1.e1e1.e1e2
            `@rq`0x3ffa.af28.6bca.1af2.86bc.a1af.286b.ca1b  `@rq`0x3ffa.8618.6186.1861.8618.6186.1861.8618
            `@rq`0x3ffa.642c.8590.b216.42c8.590b.2164.2c86  `@rq`0x3ffa.47ae.147a.e147.ae14.7ae1.47ae.147b
            `@rq`0x3ffa.2f68.4bda.12f6.84bd.a12f.684b.da13  `@rq`0x3ffa.1a7b.9611.a7b9.611a.7b96.11a7.b961
            `@rq`0x3ffa.0842.1084.2108.4210.8421.0842.1084  `@rq`0x3ff9.f07c.1f07.c1f0.7c1f.07c1.f07c.1f08
            `@rq`0x3ff9.d41d.41d4.1d41.d41d.41d4.1d41.d41d  `@rq`0x3ff9.bacf.914c.1bac.f914.c1ba.cf91.4c1c
            `@rq`0x3ff9.a41a.41a4.1a41.a41a.41a4.1a41.a41a  `@rq`0x3ff9.8f9c.18f9.c18f.9c18.f9c1.8f9c.18fa
            `@rq`0x3ff9.7d05.f417.d05f.417d.05f4.17d0.5f41  `@rq`0x3ff9.6c16.c16c.16c1.6c16.c16c.16c1.6c17
            `@rq`0x3ff9.5c98.82b9.3105.7262.0ae4.c415.c988
        ==
      =/  p2  (roll (flop cs) |=([c=@rq acc=@rq] (~(add ^rq %n) (~(mul ^rq %n) acc z) c)))
      =/  r   (~(mul ^rq %n) (~(add ^rq %n) z z) p2)
      =/  l1  (~(sub ^rq %n) f (~(mul ^rq %n) s (~(sub ^rq %n) f r)))
      =/  efa  (~(sun ^rq %n) (abs:si e))
      [?:((syn:si e) efa (~(sub ^rq %n) `@rq`0x0 efa)) l1]
    ++  log-2
      ~/  %log-2
      |=  x=@rq  ^-  @rq
      ?:  !(~(equ ^rq %n) x x)    `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      ?:  =(x `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000
      ?:  |(=(x `@rq`0x0) =(x `@rq`0x8000.0000.0000.0000.0000.0000.0000.0000))  `@rq`0xffff.0000.0000.0000.0000.0000.0000.0000
      ?:  =(1 (rsh [0 127] x))  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      =/  el  (lr x)
      (~(add ^rq %n) ef.el (~(mul ^rq %n) lm.el `@rq`0x3fff.7154.7652.b82f.e177.7d0f.fda0.d23a))
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
      ~/  %pow
      |=  [x=@rq n=@rq]  ^-  @rq
      ?:  &(=(n (san (need (toi n)))) (gth n `@rq`0x0))
        (pow-n x n)
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
    ::      .~~~1.414213562373095048801688724209698
    ::      > (sqt .~~~1e5)
    ::      .~~~316.2277660168379331998893544432718
    ::  Source
    ++  sqt
      ~/  %sqt
      ::  correctly-rounded f128 sqrt: stdlib seed + one Markstein FMA (matches
      ::  the SoftFloat f128_sqrt jet, tools/rq_check.c).
      |=  x=@rq  ^-  @rq
      ?:  !(~(equ ^rq %n) x x)  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      ?:  =(x `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000)  `@rq`0x7fff.0000.0000.0000.0000.0000.0000.0000
      ?:  |(=(x `@rq`0x0) =(x `@rq`0x8000.0000.0000.0000.0000.0000.0000.0000))  x
      ?:  =(1 (rsh [0 127] x))  `@rq`0x7fff.8000.0000.0000.0000.0000.0000.0000
      =/  g  (sqt:^rq x)
      =/  h  (~(div ^rq %n) `@rq`0x3ffe.0000.0000.0000.0000.0000.0000.0000 g)
      =/  r  (~(fma ^rq %n) (~(sub ^rq %n) `@rq`0x0 g) g x)
      (~(fma ^rq %n) h r g)
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
      ~/  %cbt
      |=  x=@rq  ^-  @rq
      ?:  !(~(equ ^rq %n) x x)  x
      ?:  |(=(x `@rq`0x0) =(x `@rq`0x8000.0000.0000.0000.0000.0000.0000.0000))  x
      =/  ax  `@rq`(dis x 0x7fff.ffff.ffff.ffff.ffff.ffff.ffff.ffff)
      =/  r   (exp (~(mul ^rq %n) (log ax) `@rq`0x3ffd.5555.5555.5555.5555.5555.5555.5555))
      ?:(=(1 (rsh [0 127] x)) (~(sub ^rq %n) `@rq`0x0 r) r)

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
--
