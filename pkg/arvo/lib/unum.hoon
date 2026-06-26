/+  twoc   :: two's-complement helpers (sign, comparison) shared with numerics
  ::
::::  Universal numbers (unums), Type III: posits, quires, valids
::
::  Per the 2022 Posit Standard (Posit Working Group, John Gustafson chair):
::  https://posithub.org/docs/posit_standard-2.pdf
::
::  We represent unums using Hoon auras at four bitwidths:
::  - @rpb @rph @rps @rpd @rpq  :: posits  (byte/half/single/double/quad)
::  - @rqb @rqh @rqs            :: quires
::  - @rvb @rvh @rvs            :: valids  (not yet implemented)
::
::  STANDARD, NOT LEGACY.  The 2022 standard fixes the exponent size at
::  es = 2 for every width (the exponent field is a 2-bit unsigned integer,
::  0..3), so useed = 2^2^es = 16.  This differs from the 2017 draft (and
::  SoftPosit's fast p8/p16 types) which scaled es with width.  Only posit32
::  coincides between the two.  We target the standard.
::
::  A posit of precision n has, most-significant first:
::  - sign     S  (1 bit)              s in {0,1}; implicit value 1-3s
::  - regime   R  (k+1 bits)           run of R0 bits, terminated by ~R0;
::                                     r = -k if R0=0, else r = k-1
::  - exponent E  (2 bits, truncable)  e in {0,1,2,3}
::  - fraction F  (rest, truncable)    f = F / 2^m, 0 <= f < 1
::
::  value:  p = ((1-3s) + f) * 2^((1-2s)*(4r + e + s))
::
::  Exceptions: all bits but S zero -> S=0 is posit 0, S=1 is NaR.
::
::  NB: this core defines posit add/sub/mul/div, which shadow the stdlib
::  gates of the same name.  Internal integer arithmetic therefore uses the
::  ^-prefixed forms (^add/^sub/^mul/^div) to reach the standard library.
::
|%
::  G-layer (decoded) representation of a posit, mirroring the stdlib float +$fn
::  (`[%f s=? e=@s a=@u]`):  value = (sign) a * 2^e, with a an integer
::  significand (the hidden 1 included) and e a signed binary exponent.
::
::  s=%.y is non-negative (the +$si convention), so value is
::  ?:(s +1 -1) * a * 2^e.
::
+$  up
  $%  [%p s=? e=@s a=@u]   :: real posit
      [%z ~]              :: zero
      [%n ~]              :: Not a Real (NaR)
  ==
::  Type III Unum Quire: a fixed-point exact accumulator of 16n bits (§3.4):
::  sign (1) . carry guard (31) . integer (8n-16) . fraction (8n-16).
::
+$  uq
  $%  $:  %q
          s=?      :: sign
          c=@      :: carry guard, 31 bits
          i=@      :: integer part, 8n-16 bits
          f=@      :: fractional part, 8n-16 bits
      ==
      [%n ~]       :: NaR
  ==
::  Generic posit core, parameterized by bloq (log2 of the bitwidth).
::
::  Specialize with %*: posit8 is bloq=3 (n=8), posit16 bloq=4, posit32 bloq=5.
::
++  pp
  |_  =bloq
  ++  n  (bex bloq)
  ++  es  2
  ++  useed  16
  ++  msk  (dec (bex n))
  ++  zero    `@`0
  ++  nar     (bex (dec n))
  ++  maxpos  (dec (bex (dec n)))
  ++  minpos  `@`1
  ++  huge  maxpos
  ++  tiny  minpos
  ++  one  (bit [%p %.y --0 1])
  ::  Mathematical constants (Q2.52 fixed-point hex of the value, rounded by
  ::  +bit at each width).  Cross-checked vs SoftPosit (pX2) and a reference.
  ++  pi       (bit [%p %.y -52 0x32.43f6.a888.5a31])   ::  3.14159265358979
  ++  tau      (bit [%p %.y -52 0x64.87ed.5110.b461])   ::  6.28318530717959
  ++  e        (bit [%p %.y -52 0x2b.7e15.1628.aed3])   ::  2.71828182845905
  ++  phi      (bit [%p %.y -52 0x19.e377.9b97.f4a8])   ::  1.61803398874989
  ++  sqt2     (bit [%p %.y -52 0x16.a09e.667f.3bcd])   ::  1.41421356237310
  ++  invsqt2  (bit [%p %.y -52 0xb.504f.333f.9de6])    ::  0.70710678118655
  ++  log2     (bit [%p %.y -52 0xb.1721.7f7d.1cf8])    ::  0.69314718055995
  ++  invlog2  (bit [%p %.y -52 0x17.1547.652b.82fe])   ::  1.44269504088896
  ++  log10    (bit [%p %.y -52 0x24.d763.776a.aa2b])   ::  2.30258509299405
  ::    +sea:  @ -> $up   (decode an n-bit posit into its g-layer form)
  ++  sea
    |=  p=@
    ^-  up
    =.  p  (dis msk p)
    ?:  =(0 p)    [%z ~]
    ?:  =(nar p)  [%n ~]
    =/  neg  =(1 (cut 0 [(dec n) 1] p))
    =/  mag  ?:(neg (^sub (bex n) p) p)
    =/  pw   (dec n)
    =/  r0   (cut 0 [(dec pw) 1] mag)
    =/  k=@  1
    |-  ^-  up
    ?:  =(k pw)
      =/  r  ?:(=(1 r0) (sun:si (dec k)) (dif:si --0 (sun:si k)))
      [%p !neg (pro:si r --4) 1]
    =/  nb  (cut 0 [(^sub (dec pw) k) 1] mag)
    ?:  =(nb r0)
      $(k +(k))
    =/  r       ?:(=(1 r0) (sun:si (dec k)) (dif:si --0 (sun:si k)))
    =/  remwid  (^sub pw +(k))
    =/  rem     (dis (dec (bex remwid)) mag)
    =/  elo  ?:  (^gte remwid 2)
               (rsh [0 (^sub remwid 2)] rem)
             ?:(=(1 remwid) (^mul 2 rem) 0)
    =/  fw    ?:((^gte remwid 2) (^sub remwid 2) 0)
    =/  frac  (dis (dec (bex fw)) rem)
    =/  x  (sum:si (pro:si r --4) (sun:si elo))
    =/  a  (^add (bex fw) frac)
    [%p !neg (dif:si x (sun:si fw)) a]
  ::    +bit:  $up -> @   (encode, round-to-nearest-even, saturating)
  ++  bit
    |=  =up
    ^-  @
    ?:  ?=(%z -.up)  zero
    ?:  ?=(%n -.up)  nar
    ?>  ?=(%p -.up)
    ?:  =(0 a.up)  zero
    =/  neg   !s.up
    =/  lead  (dec (met 0 a.up))
    =/  x     (sum:si e.up (sun:si lead))
    =/  frac  (dis (dec (bex lead)) a.up)
    =/  rel
      =/  ax  (abs:si x)
      ?:  (syn:si x)
        [(sun:si (^div ax 4)) (mod ax 4)]
      =/  q  (^div ax 4)
      =/  m  (mod ax 4)
      ?:  =(0 m)
        [(dif:si --0 (sun:si q)) 0]
      [(dif:si --0 (sun:si +(q))) (^sub 4 m)]
    =/  r=@s   -.rel
    =/  elo=@  +.rel
    ?:  (gte-s r (sun:si (^sub n 2)))    (smag neg maxpos)
    ?:  (lte-s r (dif:si --0 (sun:si (dec n))))  (smag neg minpos)
    =/  rmag  (abs:si r)
    =/  rr
      ?:  (syn:si r)
        [(lsh [0 1] (dec (bex +(rmag)))) (^add rmag 2)]
      [1 (^add rmag 1)]
    =/  regval=@  -.rr
    =/  regwid=@  +.rr
    =/  totw  (^add (^add regwid 2) lead)
    =/  pay   (con (lsh [0 (^add 2 lead)] regval) (con (lsh [0 lead] elo) frac))
    =/  pw    (dec n)
    ?:  (^lte totw pw)
      (smag neg (lsh [0 (^sub pw totw)] pay))
    =/  sh      (^sub totw pw)
    =/  keep    (rsh [0 sh] pay)
    =/  guard   (cut 0 [(dec sh) 1] pay)
    =/  sticky  ?:(=(0 (dis (dec (bex (dec sh))) pay)) 0 1)
    =/  lsbit   (dis 1 keep)
    =/  roundup  &(=(1 guard) |(=(1 sticky) =(1 lsbit)))
    =/  mag      ?:(roundup +(keep) keep)
    =?  mag  (^gth mag maxpos)  maxpos
    (smag neg mag)
  ++  smag
    |=  [neg=? mag=@]
    ^-  @
    ?.  neg  mag
    (dis msk (^sub (bex n) mag))
  ++  gte-s  |=([a=@s b=@s] ^-(? !=(-1 (cmp:si a b))))
  ++  lte-s  |=([a=@s b=@s] ^-(? !=(--1 (cmp:si a b))))
  ::  Comparisons (= two's-complement integer ordering of the raw bits, sec 5.3).
  ++  gth  |=([a=@ b=@] ^-(? (~(gth twoc:twoc bloq) a b)))
  ++  lth  |=([a=@ b=@] ^-(? (~(lth twoc:twoc bloq) a b)))
  ++  gte  |=([a=@ b=@] ^-(? (~(gte twoc:twoc bloq) a b)))
  ++  lte  |=([a=@ b=@] ^-(? (~(lte twoc:twoc bloq) a b)))
  ++  equ  |=([a=@ b=@] ^-(? =(a b)))
  ++  neq  |=([a=@ b=@] ^-(? !=(a b)))
  ++  neg  |=(a=@ ^-(@ (dis msk (^sub (bex n) a))))
  ++  abs  |=(a=@ ^-(@ ?:(=(1 (~(msb twoc:twoc bloq) a)) (neg a) a)))
  ++  sgn
    |=  a=@
    ^-  @
    ?:  =(zero a)  zero
    ?:  =(nar a)   nar
    ?:  =(1 (~(msb twoc:twoc bloq) a))  (neg one)
    one
  ::  Arithmetic (sec 5.4); exact g-layer combine, single round via +bit.
  ++  mul
    |=  [a=@ b=@]
    ^-  @
    =/  ua  (sea a)
    =/  ub  (sea b)
    ?:  |(?=(%n -.ua) ?=(%n -.ub))  nar
    ?:  |(?=(%z -.ua) ?=(%z -.ub))  zero
    ?>  ?=(%p -.ua)
    ?>  ?=(%p -.ub)
    (bit [%p =(s.ua s.ub) (sum:si e.ua e.ub) (^mul a.ua a.ub)])
  ++  add
    |=  [a=@ b=@]
    ^-  @
    =/  ua  (sea a)
    =/  ub  (sea b)
    ?:  |(?=(%n -.ua) ?=(%n -.ub))  nar
    ?:  ?=(%z -.ua)  b
    ?:  ?=(%z -.ub)  a
    ?>  ?=(%p -.ua)
    ?>  ?=(%p -.ub)
    =/  emin  ?:(=(-1 (cmp:si e.ua e.ub)) e.ua e.ub)
    =/  s1  (lsh [0 (abs:si (dif:si e.ua emin))] a.ua)
    =/  s2  (lsh [0 (abs:si (dif:si e.ub emin))] a.ub)
    ?:  =(s.ua s.ub)
      (bit [%p s.ua emin (^add s1 s2)])
    ?:  (^gth s1 s2)
      (bit [%p s.ua emin (^sub s1 s2)])
    ?:  (^gth s2 s1)
      (bit [%p s.ub emin (^sub s2 s1)])
    zero
  ++  sub  |=([a=@ b=@] ^-(@ (add a (neg b))))
  ++  div
    |=  [a=@ b=@]
    ^-  @
    =/  ua  (sea a)
    =/  ub  (sea b)
    ?:  |(?=(%n -.ua) ?=(%n -.ub))  nar
    ?:  ?=(%z -.ub)  nar
    ?:  ?=(%z -.ua)  zero
    ?>  ?=(%p -.ua)
    ?>  ?=(%p -.ub)
    =/  g    (^add n n)
    =/  num  (lsh [0 g] a.ua)
    =/  q    (^div num a.ub)
    =/  qs   ?:(=(0 (mod num a.ub)) q (con q 1))
    =/  eo   (dif:si (dif:si e.ua e.ub) (sun:si g))
    (bit [%p =(s.ua s.ub) eo qs])
  ::  Elementary and rounding ops.
  ++  isqt
    |=  x=@
    ^-  @
    ?:  =(0 x)  0
    =/  r  (bex (^div (^add (met 0 x) 1) 2))
    |-  ^-  @
    =/  nr  (^div (^add r (^div x r)) 2)
    ?:  (^gte nr r)
      |-  ^-  @
      ?:  (^gth (^mul r r) x)  $(r (dec r))
      r
    $(r nr)
  ++  sqt
    |=  p=@
    ^-  @
    =/  u  (sea p)
    ?:  ?=(%n -.u)  nar
    ?:  ?=(%z -.u)  zero
    ?>  ?=(%p -.u)
    ?.  s.u  nar
    =/  odd  =(1 (dis 1 (abs:si e.u)))
    =/  aa   ?:(odd (lsh [0 1] a.u) a.u)
    =/  ee   ?:(odd (dif:si e.u --1) e.u)
    =/  g    (^add n n)
    =/  m    (lsh [0 (^mul 2 g)] aa)
    =/  s    (isqt m)
    =/  sx   ?:(=(m (^mul s s)) s (con s 1))
    (bit [%p %.y (dif:si (fra:si ee --2) (sun:si g)) sx])
  ++  round
    |=  mode=?(%near %down %up)
    |=  p=@
    ^-  @
    =/  u  (sea p)
    ?.  ?=(%p -.u)  p
    ?:  (gte-s e.u --0)  p
    =/  sh    (abs:si e.u)
    =/  hi    (rsh [0 sh] a.u)
    =/  rem   (dis (dec (bex sh)) a.u)
    =/  half  (bex (dec sh))
    =?  hi  &(?=(%near mode) |((^gth rem half) &(=(rem half) =(1 (dis 1 hi)))))
      +(hi)
    =?  hi  &(?=(%down mode) !s.u !=(0 rem))  +(hi)
    =?  hi  &(?=(%up mode) s.u !=(0 rem))     +(hi)
    ?:  =(0 hi)  zero
    (bit [%p s.u --0 hi])
  ++  rnd  (round %near)
  ++  flr  (round %down)
  ++  cel  (round %up)
  ++  sun  |=(v=@ ^-(@ ?:(=(0 v) zero (bit [%p %.y --0 v]))))
  ++  san
    |=  v=@s
    ^-  @
    =+  [sn mg]=(old:si v)
    ?:  =(0 mg)  zero
    (bit [%p sn --0 mg])
  ++  toi
    |=  p=@
    ^-  (unit @s)
    =/  u  (sea p)
    ?:  ?=(%n -.u)  ~
    =/  r   (rnd p)
    =/  ur  (sea r)
    ?:  ?=(%z -.ur)  `--0
    ?>  ?=(%p -.ur)
    =/  sh   (abs:si e.ur)
    =/  mag  ?:((gte-s e.ur --0) (lsh [0 sh] a.ur) (rsh [0 sh] a.ur))
    `(new:si s.ur mag)
  ++  fma
    |=  [a=@ b=@ c=@]
    ^-  @
    =/  ua  (sea a)
    =/  ub  (sea b)
    =/  uc  (sea c)
    ?:  |(?=(%n -.ua) ?=(%n -.ub) ?=(%n -.uc))  nar
    ?:  |(?=(%z -.ua) ?=(%z -.ub))  c
    ?>  ?=(%p -.ua)
    ?>  ?=(%p -.ub)
    =/  ps  =(s.ua s.ub)
    =/  pe  (sum:si e.ua e.ub)
    =/  pa  (^mul a.ua a.ub)
    ?:  ?=(%z -.uc)  (bit [%p ps pe pa])
    ?>  ?=(%p -.uc)
    =/  emin  ?:(=(-1 (cmp:si pe e.uc)) pe e.uc)
    =/  s1  (lsh [0 (abs:si (dif:si pe emin))] pa)
    =/  s2  (lsh [0 (abs:si (dif:si e.uc emin))] a.uc)
    ?:  =(ps s.uc)    (bit [%p ps emin (^add s1 s2)])
    ?:  (^gth s1 s2)  (bit [%p ps emin (^sub s1 s2)])
    ?:  (^gth s2 s1)  (bit [%p s.uc emin (^sub s2 s1)])
    zero
  ::
  ::  Transcendental / elementary functions, in the naive, reproducible
  ::  Taylor-series style of /lib/math: fixed term counts, posit arithmetic
  ::  throughout (the loop counters use ^-escaped stdlib ops).  NOT correctly
  ::  rounded and accurate only near 0 (no range reduction), matching libmath;
  ::  the running sums could later move to the quire for exact accumulation.
  ::
  ::    +exp:  @ -> @   (e^x = sum x^k/k!)
  ++  exp
    |=  x=@
    ^-  @
    =/  sum   one
    =/  term  one
    =/  nn=@  1
    |-
    ?:  (^gth nn 20)  sum
    =.  term  (mul term (div x (sun nn)))
    =.  sum   (add sum term)
    $(nn +(nn))
  ::    +sin:  @ -> @   (sum (-1)^k x^(2k+1)/(2k+1)!)
  ++  sin
    |=  x=@
    ^-  @
    =/  term  x
    =/  sum   x
    =/  nn=@  1
    |-
    ?:  (^gth nn 20)  sum
    =/  k  (^mul 2 nn)
    =.  term  (neg (mul term (div (mul x x) (mul (sun k) (sun +(k))))))
    =.  sum   (add sum term)
    $(nn +(nn))
  ::    +cos:  @ -> @   (sum (-1)^k x^2k/(2k)!)
  ++  cos
    |=  x=@
    ^-  @
    =/  term  one
    =/  sum   one
    =/  nn=@  1
    |-
    ?:  (^gth nn 20)  sum
    =/  k  (^mul 2 nn)
    =.  term  (neg (mul term (div (mul x x) (mul (sun (dec k)) (sun k)))))
    =.  sum   (add sum term)
    $(nn +(nn))
  ::    +tan:  @ -> @
  ++  tan  |=(x=@ ^-(@ (div (sin x) (cos x))))
  ::    +pow-n:  @ -> @u -> @   (integer power by repeated multiplication)
  ++  pow-n
    |=  [x=@ p=@u]
    ^-  @
    ?:  =(nar x)  nar            :: NaR propagates even when p=0
    =/  res  one
    |-
    ?:  =(0 p)  res
    $(p (dec p), res (mul res x))
  ::    +log:  @ -> @   (ln, via 2*atanh((x-1)/(x+1)))
  ::  Domain is x > 0; x <= 0 (which includes NaR, the most-negative
  ::  bit pattern, and posit zero) returns NaR rather than a divergent
  ::  series result.  Like the rest of /lib/math, accurate only near 1.
  ++  log
    |=  x=@
    ^-  @
    ?:  (lte x zero)  nar
    =/  y     (div (sub x one) (add x one))
    =/  y2    (mul y y)
    =/  sum   y
    =/  term  y
    =/  nn=@  1
    |-
    ?:  (^gth nn 30)  (mul (sun 2) sum)
    =.  term  (mul term y2)
    =/  coef  (div one (sun +((^mul 2 nn))))
    =.  sum   (add sum (mul coef term))
    $(nn +(nn))
  ::    +log-2 / +log-10:  base-2 / base-10 logarithm
  ++  log-2   |=(x=@ ^-(@ (div (log x) log2)))
  ++  log-10  |=(x=@ ^-(@ (div (log x) log10)))
  ::    +pow:  @ -> @ -> @   (x^y = exp(y * log x))
  ++  pow  |=([x=@ y=@] ^-(@ (exp (mul y (log x)))))
  ::    +factorial:  @ -> @   (x! by repeated multiplication)
  ::  Domain x >= 0 (NaR otherwise, and NaR propagates); for integer x this is
  ::  exact up to the precision, halting once x <= 1.  Mirrors /lib/math.
  ++  factorial
    |=  x=@
    ^-  @
    ?:  =(nar x)  nar
    ?:  (lth x zero)  nar
    =/  t  one
    |-  ^-  @
    ?:  (lte x one)  t
    $(x (sub x one), t (mul t x))
  ::    +cbrt:  @ -> @   (cube root, x^(1/3) = exp(log x / 3))
  ::  Domain x > 0 (NaR for x < 0, like the rest of the exp/log-based ops);
  ::  cbrt(0) = 0.  Mirrors /lib/math's `cbt = (pow x .0.33...)`.
  ++  cbrt
    |=  x=@
    ^-  @
    ?:  =(nar x)  nar
    ?:  =(zero x)  zero
    ?:  (lth x zero)  nar
    (pow x (div one (sun 3)))
  ::    +atan:  @ -> @   (inverse tangent, Gauss/AGM iteration)
  ::  arctan(x) = x / ((1+x^2)^0.5 * b), with [a b] driven to the AGM of
  ::  (1+x^2)^-0.5 and 1.  Fixed iteration count (AGM converges quadratically).
  ::  Odd in x, so negative arguments are handled by the carried sign.
  ++  atan
    |=  x=@
    ^-  @
    ?:  =(nar x)  nar
    =/  x2  (add one (mul x x))
    =/  rt  (sqt x2)
    =/  a   (div one rt)
    =/  b   one
    =/  nn=@  0
    |-  ^-  @
    ?:  (^gth nn 40)
      (div x (mul rt b))
    =/  ai  (mul (div one (sun 2)) (add a b))
    =/  bi  (sqt (mul ai b))
    $(a ai, b bi, nn +(nn))
  ::    +asin:  @ -> @   (inverse sine)
  ::  arcsin(x) = atan(x / sqrt(1 - x^2)) for |x| < 1; +-pi/2 at x = +-1;
  ::  NaR outside [-1, 1].  Mirrors /lib/math.
  ++  asin
    |=  x=@
    ^-  @
    ?:  =(nar x)  nar
    ?:  (lth (abs x) one)
      (atan (div x (sqt (sub one (mul x x)))))
    ?:  (equ x one)        (mul pi (div one (sun 2)))
    ?:  (equ x (neg one))  (neg (mul pi (div one (sun 2))))
    nar
  ::    +acos:  @ -> @   (inverse cosine)
  ::  arccos(x) = atan(sqrt(1 - x^2) / x) for 0 < |x| < 1 (pi/2 at 0); 0 at
  ::  x = 1, pi at x = -1; NaR outside [-1, 1].  Mirrors /lib/math.
  ++  acos
    |=  x=@
    ^-  @
    ?:  =(nar x)  nar
    ?:  (lth (abs x) one)
      ?:  (equ x zero)  (mul pi (div one (sun 2)))
      (atan (div (sqt (sub one (mul x x))) x))
    ?:  (equ x one)        zero
    ?:  (equ x (neg one))  pi
    nar
  ::    +is-close:  @ -> @ -> @ -> ?   (|a - b| <= tol)
  ++  is-close  |=([a=@ b=@ tol=@] ^-(? (lte (abs (sub a b)) tol)))
  ::
  ::  Quire (sec 3.4 / 5.11): a 16n-bit fixed-point exact accumulator, held
  ::  as a raw two's-complement atom.  Sums of products accumulate exactly and
  ::  round only once, via +q-to-p -- the basis of the fused dot product +fdp,
  ::  which is why posits beat floats for linear algebra.  q-NaR is the most
  ::  negative pattern.  Verified against SoftPosit qX2 over random vectors.
  ::
  ++  qbits   (^mul 16 n)
  ++  qscale  (^sub (^mul 8 n) 16)
  ++  qmod    (bex qbits)
  ++  q-nar   (bex (dec qbits))
  ++  q-zero  `@`0
  ++  p-to-q
    |=  p=@
    ^-  @
    =/  u  (sea p)
    ?:  ?=(%n -.u)  q-nar
    ?:  ?=(%z -.u)  q-zero
    ?>  ?=(%p -.u)
    =/  m  (lsh [0 (abs:si (sum:si e.u (sun:si qscale)))] a.u)
    ?:(s.u m (^sub qmod m))
  ++  q-to-p
    |=  q=@
    ^-  @
    =.  q  (dis (dec qmod) q)
    ?:  =(q-nar q)  nar
    =/  neg  =(1 (cut 0 [(dec qbits) 1] q))
    =/  acc  ?:(neg (^sub qmod q) q)
    ?:  =(0 acc)  zero
    (bit [%p !neg (dif:si --0 (sun:si qscale)) acc])
  ++  q-mul-add
    |=  [q=@ a=@ b=@]
    ^-  @
    ?:  =(q-nar q)  q-nar
    =/  ua  (sea a)
    =/  ub  (sea b)
    ?:  |(?=(%n -.ua) ?=(%n -.ub))  q-nar
    ?:  |(?=(%z -.ua) ?=(%z -.ub))  q
    ?>  ?=(%p -.ua)
    ?>  ?=(%p -.ub)
    =/  m   (lsh [0 (abs:si :(sum:si e.ua e.ub (sun:si qscale)))] (^mul a.ua a.ub))
    =/  qc  ?:(=(s.ua s.ub) m (^sub qmod m))
    (mod (^add q qc) qmod)
  ++  q-mul-sub  |=([q=@ a=@ b=@] ^-(@ (q-mul-add q a (neg b))))
  ++  q-add-p  |=([q=@ p=@] ^-(@ (q-mul-add q p one)))
  ++  q-sub-p  |=([q=@ p=@] ^-(@ (q-mul-add q (neg p) one)))
  ++  q-negate  |=(q=@ ^-(@ ?:(=(q-nar q) q-nar (mod (^sub qmod q) qmod))))
  ++  q-add-q
    |=  [x=@ y=@]
    ^-  @
    ?:  |(=(q-nar x) =(q-nar y))  q-nar
    (mod (^add x y) qmod)
  ++  q-sub-q  |=([x=@ y=@] ^-(@ (q-add-q x (q-negate y))))
  ::    +fdp:  (list @) -> (list @) -> @  (fused dot product, single rounding)
  ++  fdp
    |=  [av=(list @) bv=(list @)]
    ^-  @
    =|  q=@
    |-  ^-  @
    ?~  av  (q-to-p q)
    ?~  bv  (q-to-p q)
    $(q (q-mul-add q i.av i.bv), av t.av, bv t.bv)
  ::
  ::  IEEE-754 interop (sec 6.5).  Conversion is by VALUE, so a posit of ANY
  ::  width converts to/from a float of ANY width -- the full matrix.  Posits
  ::  pack more accuracy per bit (posit16 ~ float32, posit32 ~ float64), so
  ::  same-width is NOT the meaningful correspondence.  Posit zero <-> float
  ::  +0; posit NaR <-> float NaN; float +-inf/NaN -> NaR.  +up-to-fn/+fn-to-up
  ::  bridge the (identical) posit g-layer +$up and the stdlib float +$fn.
  ::
  ++  up-to-fn
    |=  u=up
    ^-  fn
    ?:  ?=(%n -.u)  [%n ~]
    ?:  ?=(%z -.u)  [%f %.y --0 0]
    [%f s.u e.u a.u]
  ++  fn-to-up
    |=  f=fn
    ^-  up
    ?.  ?=(%f -.f)  [%n ~]                 :: NaN / +-inf -> NaR
    ?:  =(0 a.f)  [%z ~]
    [%p s.f e.f a.f]
  ::    +to-rh/rs/rd/rq:  this-width posit -> half/single/double/quad float
  ++  to-rh  |=(p=@ ^-(@rh (bit:rh (up-to-fn (sea p)))))
  ++  to-rs  |=(p=@ ^-(@rs (bit:rs (up-to-fn (sea p)))))
  ++  to-rd  |=(p=@ ^-(@rd (bit:rd (up-to-fn (sea p)))))
  ++  to-rq  |=(p=@ ^-(@rq (bit:rq (up-to-fn (sea p)))))
  ::    +from-rh/rs/rd/rq:  half/single/double/quad float -> this-width posit
  ++  from-rh  |=(r=@rh ^-(@ (bit (fn-to-up (sea:rh r)))))
  ++  from-rs  |=(r=@rs ^-(@ (bit (fn-to-up (sea:rs r)))))
  ++  from-rd  |=(r=@rd ^-(@ (bit (fn-to-up (sea:rd r)))))
  ++  from-rq  |=(r=@rq ^-(@ (bit (fn-to-up (sea:rq r)))))
  --
::  posit8   ("byte"),   posit<8,2>
++  rpb  %*(. pp bloq 3)
::  posit16  ("half"),   posit<16,2>
++  rph  %*(. pp bloq 4)
::  posit32  ("single"), posit<32,2>
++  rps  %*(. pp bloq 5)
::  posit64  ("double"), posit<64,2>
++  rpd  %*(. pp bloq 6)
::  posit128 ("quad"),   posit<128,2>
++  rpq  %*(. pp bloq 7)
--
