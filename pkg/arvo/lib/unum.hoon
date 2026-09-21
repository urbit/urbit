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
::  Aura note: `@rq` is the Hoon stdlib aura for IEEE 754 binary128 (quad float).
::  `@rpq` (posit-128) is a different format that coincidentally shares the `q`
::  suffix.  The quire auras `@rqb`/`@rqh`/`@rqs` are likewise distinct from both.
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
~%  %non  ..part  ~  :: nest non in hex for now (jet chapter, see /lib/math)
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
  ~/  %unum
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
  ::  NaR (the most-negative bit pattern, e.g. `0x80` for posit8) compares less
  ::  than every real posit, including negative ones (2022 Posit Standard §5.3).
  ++  gth
    ~/  %gth
    |=([a=@ b=@] ^-(? (~(gth twoc:twoc bloq) a b)))
  ++  lth
    ~/  %lth
    |=([a=@ b=@] ^-(? (~(lth twoc:twoc bloq) a b)))
  ++  gte
    ~/  %gte
    |=([a=@ b=@] ^-(? (~(gte twoc:twoc bloq) a b)))
  ++  lte
    ~/  %lte
    |=([a=@ b=@] ^-(? (~(lte twoc:twoc bloq) a b)))
  ++  equ
    ~/  %equ
    |=([a=@ b=@] ^-(? =(a b)))
  ++  neq
    ~/  %neq
    |=([a=@ b=@] ^-(? !=(a b)))
  ++  neg
    ~/  %neg
    |=(a=@ ^-(@ (dis msk (^sub (bex n) a))))
  ++  abs
    ~/  %abs
    |=(a=@ ^-(@ ?:(=(1 (~(msb twoc:twoc bloq) a)) (neg a) a)))
  ++  sgn
    ~/  %sgn
    |=  a=@
    ^-  @
    ?:  =(zero a)  zero
    ?:  =(nar a)   nar
    ?:  =(1 (~(msb twoc:twoc bloq) a))  (neg one)
    one
  ::  Arithmetic (sec 5.4); exact g-layer combine, single round via +bit.
  ++  mul
    ~/  %mul
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
    ~/  %add
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
  ++  sub
    ~/  %sub
    |=([a=@ b=@] ^-(@ (add a (neg b))))
  ++  div
    ~/  %div
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
    ~/  %sqt
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
  ::  rnd/flr/cel: eta-expanded to unary gates wrapping +round so they jet.
  ++  rnd
    ~/  %rnd
    |=(p=@ ((round %near) p))
  ++  flr
    ~/  %flr
    |=(p=@ ((round %down) p))
  ++  cel
    ~/  %cel
    |=(p=@ ((round %up) p))
  ++  sun
    ~/  %sun
    |=(v=@ ^-(@ ?:(=(0 v) zero (bit [%p %.y --0 v]))))
  ++  san
    ~/  %san
    |=  v=@s
    ^-  @
    =+  [sn mg]=(old:si v)
    ?:  =(0 mg)  zero
    (bit [%p sn --0 mg])
  ++  toi
    ~/  %toi
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
    ~/  %fma
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
  ::  Transcendental / elementary functions.  Being migrated (numerics
  ::  NEXT-STEPS.md item #4) from naive fixed-term-count Taylor/AGM series
  ::  (posit arithmetic throughout, correctly rounded only near 0, no range
  ::  reduction) to Chebyshev-minimax kernels with exact range reduction,
  ::  mirroring /lib/math -- adapted for posits: since Hoon `@` atoms are
  ::  arbitrary-precision (unlike IEEE's fixed-width hardware registers),
  ::  reduction needs no hi/lo constant splitting; work in the exact g-layer
  ::  (+sea-decoded), single round via +bit at the end, same as +add/+mul/
  ::  +fma already do internally.  +gmul/+gadd/+gneg/+gsub below are that
  ::  exact (never-rounded) g-layer combine, shared by every migrated fn.
  ::  Not-yet-migrated arms keep the old naive-series comment inline.
  ::
  ::  SCOPE CAVEAT: these arms live in the generic, `bloq`-parameterized +pp
  ::  core, so they run at EVERY instantiated width, including +rpd/+rpq
  ::  (posit64/128) -- there is no width guard falling back to the old naive
  ::  series there.  Correctness/faithful-rounding was only VERIFIED (against
  ::  unum_cheb_check.py + mpmath) at posit8/16/32; the WBITS=128 constant
  ::  precision was sized for posit32's worst case and has NOT been checked
  ::  sufficient at posit64/128 (posit128's own significand can itself
  ::  approach ~124 bits, leaving little margin).  Treat p64/p128 output from
  ::  these arms as unverified, not merely "still naive," until someone
  ::  extends the oracle sweep to those widths.
  ::
  ::    +gmul:  up -> up -> up   (exact multiply, no rounding)
  ++  gmul
    |=  [x=up y=up]
    ^-  up
    ?>  &(?=(%p -.x) ?=(%p -.y))
    [%p =(s.x s.y) (sum:si e.x e.y) (^mul a.x a.y)]
  ::    +gadd:  up -> up -> up   (exact add, no rounding)
  ++  gadd
    |=  [x=up y=up]
    ^-  up
    ?>  &(?=(%p -.x) ?=(%p -.y))
    ?:  =(0 a.x)  y
    ?:  =(0 a.y)  x
    =/  emin  ?:(=(-1 (cmp:si e.x e.y)) e.x e.y)
    =/  s1  (lsh [0 (abs:si (dif:si e.x emin))] a.x)
    =/  s2  (lsh [0 (abs:si (dif:si e.y emin))] a.y)
    ?:  =(s.x s.y)    [%p s.x emin (^add s1 s2)]
    ?:  (^gth s1 s2)  [%p s.x emin (^sub s1 s2)]
    ?:  (^gth s2 s1)  [%p s.y emin (^sub s2 s1)]
    [%p %.y --0 0]
  ::    +gneg:  up -> up   (exact negate)
  ++  gneg
    |=  x=up
    ^-  up
    ?>  ?=(%p -.x)
    [%p !s.x e.x a.x]
  ::    +gsub:  up -> up -> up   (exact subtract, no rounding)
  ++  gsub
    |=  [x=up y=up]
    ^-  up
    (gadd x (gneg y))
  ::    +gdiv:  up -> up -> @ -> up   (x/y truncated to g bits of EXTRA
  ::    precision beyond x's own -- not exact, g is a target-width margin).
  ::    shift is g+(bit-width of y), NOT bare g: +gmul/+gadd never normalize
  ::    (exact combine, no truncation), so a Horner-accumulated x/y pair
  ::    (e.g. tan's sin/cos) can have huge, unrelated raw bit-lengths -- a
  ::    bare (x<<g) would silently produce a zero-bit quotient.
  ++  gdiv
    |=  [x=up y=up g=@]
    ^-  up
    ?>  &(?=(%p -.x) ?=(%p -.y))
    ?:  =(0 a.x)  [%p %.y --0 0]
    =/  shift  (^add g (met 0 a.y))
    [%p =(s.x s.y) (dif:si (dif:si e.x e.y) (sun:si shift)) (^div (lsh [0 shift] a.x) a.y)]
  ::  Wide (WBITS=128) fixed-point constants shared by the Chebyshev-basis
  ::  transcendentals below (+exp/+lr/+log-2/+log-10) -- see
  ::  libmath/tools/unum_cheb_check.py for how these are generated/verified.
  ::    +gpoly:  (list up) -> up -> up   (Horner over `cs`, highest degree
  ::    first, at `z`)
  ++  gpoly
    |=  [cs=(list up) z=up]
    ^-  up
    ?>  ?=(^ cs)
    =/  acc  i.cs
    =/  rest  t.cs
    |-  ^-  up
    ?~  rest  acc
    $(acc (gadd (gmul acc z) i.rest), rest t.rest)
  ++  ln2-wide        [%p %.y -128 0xb172.17f7.d1cf.79ab.c9e3.b398.03f2.f6af]
  ++  invln2-wide     [%p %.y -128 0x1.7154.7652.b82f.e177.7d0f.fda0.d23a.7d12]
  ++  log10-two-wide  [%p %.y -128 0x4d10.4d42.7de7.fbcc.47c4.acd6.05be.48bc]
  ++  invln10-wide    [%p %.y -128 0x6f2d.ec54.9b94.38ca.9aad.d557.d699.ee19]
  ::  pi/2 needs MORE precision (200 bits, not 128) than the others: trig
  ::  reduction error scales with the argument (q*pi2's error grows with q),
  ::  unlike exp/log where WBITS=128 is a flat, argument-independent margin.
  ++  pi2-wide     [%p %.y -200 0x192.1fb5.4442.d184.6989.8cc5.1701.b839.a252.049c.1114.cf98.e804]
  ++  invpi2-wide  [%p %.y -200 0xa2.f983.6e4e.4415.29fc.2757.d1f5.34dd.c0db.6295.993c.4390.41fe]
  ::    +g-round:  up (%p/%z) -> @s   (nearest int, ties away from 0)
  ++  g-round
    |=  x=up
    ^-  @s
    ?:  ?=(%z -.x)  --0
    ?>  ?=(%p -.x)
    =/  mag=@
      ?:  (gte-s e.x --0)
        (lsh [0 (abs:si e.x)] a.x)
      =/  sh  (abs:si e.x)
      =/  q   (rsh [0 sh] a.x)
      =/  rm  (dis (dec (bex sh)) a.x)
      ?:((^gte rm (bex (dec sh))) +(q) q)
    ?:(s.x (sun:si mag) (dif:si --0 (sun:si mag)))
  ::    +exp:  @ -> @   (e^x = 2^k * poly(r), x = k*ln2 + r; Chebyshev-minimax
  ::    degree-7 kernel, correctly rounded at posit8/16/32 -- see
  ::    libmath/tools/unum_cheb_check.py.  +bit's own maxpos/minpos
  ::    saturation gives overflow/underflow for free, no separate guard chain.
  ++  exp
    ~/  %exp
    |=  x=@
    ^-  @
    =/  u  (sea x)
    ?:  ?=(%n -.u)  nar
    ?:  ?=(%z -.u)  one
    ?>  ?=(%p -.u)
    =/  cs=(list up)                         ::  c7..c0, highest degree first
      :~  [%p %.y -128 0xd.0bef.9761.5e30.36b3.f095.cd13.a64d]
          [%p %.y -128 0x5b.69d4.c616.b405.aa1f.fc64.4cd9.fc9e]
          [%p %.y -128 0x222.214c.445d.4691.bf8c.9a8a.f541.fc45]
          [%p %.y -128 0xaaa.a325.0783.cf22.e306.b08b.d797.037e]
          [%p %.y -128 0x2aaa.aaaf.ce24.bb58.3aef.94f8.c614.beae]
          [%p %.y -128 0x8000.002e.44f2.6dbc.7227.1943.2ee4.604c]
          [%p %.y -128 0xffff.ffff.fb0f.e844.2a05.c50f.9cfa.ea45]
          [%p %.y -128 0xffff.ffff.d389.a50d.1cdb.82b4.c34e.2f1d]
      ==
    =/  k=@s  (g-round (gmul u invln2-wide))
    =/  kup   [%p (syn:si k) --0 (abs:si k)]
    =/  r     (gsub u (gmul kup ln2-wide))
    =/  p     (gpoly cs r)
    ?>  ?=(%p -.p)
    (bit [%p s.p (sum:si e.p k) a.p])
  ::    +sc:  ax:up (%p, x>=0) -> [sn=up cs=up]   shared quarter-turn reduction
  ::    for +sin/+cos/+tan.  Reduces |x| only (q=round(ax*2/pi) is then always
  ::    >=0 -- no negative-mod bookkeeping); sin's oddness/cos's evenness are
  ::    handled by the caller re-applying the original sign (or not).  Kernels
  ::    are the EXACT Taylor series in z=r*r (r in [-pi/4,pi/4]).  TRIG_WBITS
  ::    (200, wider than the 128 used elsewhere) is needed because reduction
  ::    error scales with the ARGUMENT (q*pi2's error grows with q) -- see
  ::    unum_cheb_check.py; with 200 bits this is correctly rounded across
  ::    posit32's entire dynamic range (verified to ~maxpos, unlike /lib/math
  ::    which documents a bounded "faithful" range on hardware floats).
  ++  sc
    |=  ax=up
    ^-  [sn=up cs=up]
    ?>  ?=(%p -.ax)
    ?>  s.ax                                 ::  precondition: ax>=0 (internal helper, not called directly)
    =/  q=@s  (g-round (gmul ax invpi2-wide))
    =/  qn=@  (abs:si q)
    =/  r     (gsub ax (gmul [%p %.y --0 qn] pi2-wide))
    =/  z     (gmul r r)
    =/  one-g  [%p %.y --0 1]
    =/  sin-cs=(list up)                   ::  k=6..1: (-1)^k/(2k+1)!, highest degree first
      :~  [%p %.y -128 0xb092.309d.4368.4be5.1c19.8e92]
          [%p %.n -128 0x6b.9915.9fd5.138e.3f9d.1f92.e0df]
          [%p %.y -128 0x2e3b.c74a.ad8e.671f.5583.911c.a003]
          [%p %.n -128 0xd.00d0.0d00.d00d.00d0.0d00.d00d.00d0]
          [%p %.y -128 0x222.2222.2222.2222.2222.2222.2222.2222]
          [%p %.n -128 0x2aaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaab]
      ==
    =/  cos-cs=(list up)                   ::  k=6..1: (-1)^k/(2k)!, highest degree first
      :~  [%p %.y -128 0x8.f76c.77fc.6c4b.daa2.6d4c.3d68]
          [%p %.n -128 0x49f.93ed.de27.d71c.bbc0.5b4f.a99a]
          [%p %.y -128 0x1.a01a.01a0.1a01.a01a.01a0.1a01.a01a]
          [%p %.n -128 0x5b.05b0.5b05.b05b.05b0.5b05.b05b.05b0]
          [%p %.y -128 0xaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaab]
          [%p %.n -128 0x8000.0000.0000.0000.0000.0000.0000.0000]
      ==
    =/  sink  (gmul r (gadd one-g (gmul z (gpoly sin-cs z))))
    =/  cosk  (gadd one-g (gmul z (gpoly cos-cs z)))
    =/  m  (mod qn 4)
    ?:  =(m 0)  [sink cosk]
    ?:  =(m 1)  [cosk (gneg sink)]
    ?:  =(m 2)  [(gneg sink) (gneg cosk)]
    [(gneg cosk) sink]
  ::    +sin:  @ -> @   (odd fn: reduce |x| via +sc, reapply x's sign)
  ++  sin
    ~/  %sin
    |=  x=@
    ^-  @
    =/  u  (sea x)
    ?:  ?=(%n -.u)  nar
    ?:  ?=(%z -.u)  zero
    ?>  ?=(%p -.u)
    =/  sn  sn:(sc [%p %.y e.u a.u])
    ?>  ?=(%p -.sn)
    =/  final-s  ?:(s.u s.sn !s.sn)
    (bit [%p final-s e.sn a.sn])
  ::    +cos:  @ -> @   (even fn: reduce |x| via +sc, sign unaffected)
  ++  cos
    ~/  %cos
    |=  x=@
    ^-  @
    =/  u  (sea x)
    ?:  ?=(%n -.u)  nar
    ?:  ?=(%z -.u)  one
    ?>  ?=(%p -.u)
    (bit cs:(sc [%p %.y e.u a.u]))
  ::    +tan:  @ -> @   (sin(ax)/cos(ax) via +gdiv on the UNROUNDED kernel
  ::    outputs -- one final rounding, more accurate than dividing two
  ::    already-rounded posits; odd fn, same sign handling as +sin)
  ++  tan
    ~/  %tan
    |=  x=@
    ^-  @
    =/  u  (sea x)
    ?:  ?=(%n -.u)  nar
    ?:  ?=(%z -.u)  zero
    ?>  ?=(%p -.u)
    =/  scr  (sc [%p %.y e.u a.u])
    =/  cn   cs.scr
    ?>  ?=(%p -.cn)
    ?:  =(0 a.cn)  nar                     :: cos(ax) exactly 0 (measure-zero, guard anyway)
    =/  raw  (gdiv sn.scr cs.scr 160)
    ?>  ?=(%p -.raw)
    =/  final-s  ?:(s.u s.raw !s.raw)
    (bit [%p final-s e.raw a.raw])
  ::    +pow-n:  @ -> @u -> @   (integer power by repeated multiplication)
  ++  pow-n
    ~/  %pow-n
    |=  [x=@ p=@u]
    ^-  @
    ?:  =(nar x)  nar            :: NaR propagates even when p=0
    =/  res  one
    |-
    ?:  =(0 p)  res
    $(p (dec p), res (mul res x))
  ::    +lr:  up (x>0) -> [e=@s lm=up]   shared mantissa/exponent reduction for
  ::    +log/+log-2/+log-10 (x = m*2^e, m in [1,2), free from +sea's own [a e]
  ::    split -- no subnormal pre-scale needed, unlike /lib/math).  `lm` is
  ::    log(m), NOT the mantissa m itself -- via log(m)=2*atanh(s),
  ::    s=f/(m+1), f=m-1 -- the EXACT atanh Taylor series (no minimax fit:
  ::    z=s*s<1/9 here converges fast on its own), one interior truncating
  ::    divide (+gdiv) for `s`.  Degree 16 is the smallest that's correctly
  ::    rounded at p8/16/32 -- see unum_cheb_check.py; lower degrees look fine
  ::    near x~1 but blow up (100k+ ULP) near cancellation points where E*ln2
  ::    and log(m) nearly cancel (x close to a power of 2).
  ++  lr
    |=  g=up
    ^-  [e=@s lm=up]
    ?>  ?=(%p -.g)
    =/  lead  (dec (met 0 a.g))
    =/  mup   [%p %.y (dif:si --0 (sun:si lead)) a.g]
    =/  bige  (sum:si (sun:si lead) e.g)
    =/  one-g  [%p %.y --0 1]
    =/  two-g  [%p %.y --1 1]
    =/  f     (gsub mup one-g)
    =/  s     (gdiv f (gadd mup one-g) 160)
    =/  z     (gmul s s)
    =/  cs=(list up)                       ::  c_16..c_1 = 1/33..1/3, highest degree first
      :~  [%p %.y -128 0x7c1.f07c.1f07.c1f0.7c1f.07c1.f07c.1f08]
          [%p %.y -128 0x842.1084.2108.4210.8421.0842.1084.2108]
          [%p %.y -128 0x8d3.dcb0.8d3d.cb08.d3dc.b08d.3dcb.08d4]
          [%p %.y -128 0x97b.425e.d097.b425.ed09.7b42.5ed0.97b4]
          [%p %.y -128 0xa3d.70a3.d70a.3d70.a3d7.0a3d.70a3.d70a]
          [%p %.y -128 0xb21.642c.8590.b216.42c8.590b.2164.2c86]
          [%p %.y -128 0xc30.c30c.30c3.0c30.c30c.30c3.0c30.c30c]
          [%p %.y -128 0xd79.435e.50d7.9435.e50d.7943.5e50.d794]
          [%p %.y -128 0xf0f.0f0f.0f0f.0f0f.0f0f.0f0f.0f0f.0f0f]
          [%p %.y -128 0x1111.1111.1111.1111.1111.1111.1111.1111]
          [%p %.y -128 0x13b1.3b13.b13b.13b1.3b13.b13b.13b1.3b14]
          [%p %.y -128 0x1745.d174.5d17.45d1.745d.1745.d174.5d17]
          [%p %.y -128 0x1c71.c71c.71c7.1c71.c71c.71c7.1c71.c71c]
          [%p %.y -128 0x2492.4924.9249.2492.4924.9249.2492.4925]
          [%p %.y -128 0x3333.3333.3333.3333.3333.3333.3333.3333]
          [%p %.y -128 0x5555.5555.5555.5555.5555.5555.5555.5555]
      ==
    =/  poly  (gpoly cs z)
    =/  logm  (gmul (gmul two-g s) (gadd one-g (gmul z poly)))
    [bige logm]
  ::    +log:  @ -> @   (ln x = E*ln2 + log(m), x = m*2^E via +lr).  Domain is
  ::    x > 0; x <= 0 (NaR, the most-negative bit pattern, and posit zero)
  ::    returns NaR rather than a divergent/undefined series result.
  ++  log
    ~/  %log
    |=  x=@
    ^-  @
    =/  u  (sea x)
    ?:  ?=(%n -.u)  nar
    ?:  ?=(%z -.u)  nar
    ?>  ?=(%p -.u)
    ?.  s.u  nar
    =/  em    (lr u)
    =/  eup   [%p (syn:si e.em) --0 (abs:si e.em)]
    (bit (gadd (gmul eup ln2-wide) lm.em))
  ::    +log-2 / +log-10:  base-2 / base-10 logarithm, via +lr directly (E +
  ::    log(m)/ln(b)) rather than dividing +log's result by a posit-rounded
  ::    log2/log10 constant -- avoids a second rounding step, correctly
  ::    rounded at posit8/16/32 (see libmath/tools/unum_cheb_check.py).
  ++  log-2
    ~/  %log-2
    |=  x=@
    ^-  @
    =/  u  (sea x)
    ?:  ?=(%n -.u)  nar
    ?:  ?=(%z -.u)  nar
    ?>  ?=(%p -.u)
    ?.  s.u  nar
    =/  em   (lr u)
    =/  eup  [%p (syn:si e.em) --0 (abs:si e.em)]
    (bit (gadd eup (gmul lm.em invln2-wide)))
  ++  log-10
    ~/  %log-10
    |=  x=@
    ^-  @
    =/  u  (sea x)
    ?:  ?=(%n -.u)  nar
    ?:  ?=(%z -.u)  nar
    ?>  ?=(%p -.u)
    ?.  s.u  nar
    =/  em   (lr u)
    =/  eup  [%p (syn:si e.em) --0 (abs:si e.em)]
    (bit (gadd (gmul eup log10-two-wide) (gmul lm.em invln10-wide)))
  ::    +pow:  @ -> @ -> @   (x^y = exp(y * log x))
  ++  pow
    ~/  %pow
    |=([x=@ y=@] ^-(@ (exp (mul y (log x)))))
  ::    +factorial:  @ -> @   (x! by repeated multiplication)
  ::  Domain x >= 0 (NaR otherwise, and NaR propagates); for integer x this is
  ::  exact up to the precision, halting once x <= 1.  Mirrors /lib/math.
  ++  factorial
    ~/  %factorial
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
    ~/  %cbrt
    |=  x=@
    ^-  @
    ?:  =(nar x)  nar
    ?:  =(zero x)  zero
    ?:  (lth x zero)  nar
    (pow x (div one (sun 3)))
  ::    +glt:  up -> up -> ?   (exact g-layer x<y compare, shared reduction helper)
  ++  glt
    |=  [x=up y=up]
    ^-  ?
    =/  d  (gsub x y)
    ?>  ?=(%p -.d)
    ?:  =(0 a.d)  %.n
    !s.d
  ::    +atan-core:  up (ax, %p, x>=0) -> up   (unrounded atan(ax); shared by
  ::    +atan/+asin/+acos).  fdlibm breakpoint reduction (7/16, 11/16, 19/16,
  ::    39/16 -- exact dyadic thresholds, no quantization needed) picks a
  ::    fixed breakpoint angle bp in {0, atan(1/2), pi/4, atan(3/2), pi/2} via
  ::    the tan-subtraction identity xr=(ax-bp_val)/(1+ax*bp_val), so
  ::    atan(ax)=bp+atan(xr) with |xr| always small; atan(xr) via the EXACT
  ::    odd Taylor series in z=xr*xr (degree 13 is the smallest correctly-
  ::    rounded at p8/16/32 -- see libmath/tools/unum_cheb_check.py).
  ++  atan-core
    |=  ax=up
    ^-  up
    ?>  ?=(%p -.ax)
    ?>  s.ax                                 ::  precondition: ax>=0 (internal helper, not called directly)
    =/  one-g        [%p %.y --0 1]
    =/  half-g       [%p %.y -1 1]
    =/  threehalf-g  [%p %.y -1 3]
    =/  b1  [%p %.y -4 7]
    =/  b2  [%p %.y -4 11]
    =/  b3  [%p %.y -4 19]
    =/  b4  [%p %.y -4 39]
    =/  bp-half       [%p %.y -128 0x76b1.9c15.86ed.3da2.b7f2.22f6.5e1d.4682]
    =/  bp-pi4        [%p %.y -128 0xc90f.daa2.2168.c234.c4c6.628b.80dc.1cd1]
    =/  bp-threehalf  [%p %.y -128 0xfb98.5e94.0fb4.d900.7887.af0c.bbc9.e142]
    =/  bp-pi2        [%p %.y -128 0x1.921f.b544.42d1.8469.898c.c517.01b8.39a2]
    =/  cs=(list up)                       ::  k=13..1, highest degree first
      :~  [%p %.n -128 0x97b.425e.d097.b425.ed09.7b42.5ed0.97b4]
          [%p %.y -128 0xa3d.70a3.d70a.3d70.a3d7.0a3d.70a3.d70a]
          [%p %.n -128 0xb21.642c.8590.b216.42c8.590b.2164.2c86]
          [%p %.y -128 0xc30.c30c.30c3.0c30.c30c.30c3.0c30.c30c]
          [%p %.n -128 0xd79.435e.50d7.9435.e50d.7943.5e50.d794]
          [%p %.y -128 0xf0f.0f0f.0f0f.0f0f.0f0f.0f0f.0f0f.0f0f]
          [%p %.n -128 0x1111.1111.1111.1111.1111.1111.1111.1111]
          [%p %.y -128 0x13b1.3b13.b13b.13b1.3b13.b13b.13b1.3b14]
          [%p %.n -128 0x1745.d174.5d17.45d1.745d.1745.d174.5d17]
          [%p %.y -128 0x1c71.c71c.71c7.1c71.c71c.71c7.1c71.c71c]
          [%p %.n -128 0x2492.4924.9249.2492.4924.9249.2492.4925]
          [%p %.y -128 0x3333.3333.3333.3333.3333.3333.3333.3333]
          [%p %.n -128 0x5555.5555.5555.5555.5555.5555.5555.5555]
      ==
    =/  xrbp
      ?:  (glt ax b1)
        [ax [%p %.y --0 0]]
      ?:  (glt ax b2)
        [(gdiv (gsub ax half-g) (gadd one-g (gmul ax half-g)) 160) bp-half]
      ?:  (glt ax b3)
        [(gdiv (gsub ax one-g) (gadd ax one-g) 160) bp-pi4]
      ?:  (glt ax b4)
        [(gdiv (gsub ax threehalf-g) (gadd one-g (gmul ax threehalf-g)) 160) bp-threehalf]
      [(gneg (gdiv one-g ax 160)) bp-pi2]
    =/  xr  -.xrbp
    =/  bp  +.xrbp
    =/  z   (gmul xr xr)
    =/  series  (gadd one-g (gmul z (gpoly cs z)))
    (gadd bp (gmul xr series))
  ::    +atan:  @ -> @   (inverse tangent, odd fn: reduce |x| via
  ::    +atan-core, reapply x's sign)
  ++  atan
    ~/  %atan
    |=  x=@
    ^-  @
    =/  u  (sea x)
    ?:  ?=(%n -.u)  nar
    ?:  ?=(%z -.u)  zero
    ?>  ?=(%p -.u)
    =/  raw  (atan-core [%p %.y e.u a.u])
    ?>  ?=(%p -.raw)
    =/  final-s  ?:(s.u s.raw !s.raw)
    (bit [%p final-s e.raw a.raw])
  ::    +asin:  @ -> @   (inverse sine)
  ::  arcsin(x) = atan(x / sqrt(1 - x^2)) for |x| < 1; +-pi/2 at x = +-1;
  ::  NaR outside [-1, 1].  Composes existing correctly-rounded +sqt/+div with
  ::  the new +atan -- faithful, not correctly rounded (exhaustively measured
  ::  worst case: 3 ULP at posit32, near |x|~1; acos's worst case is worse, up
  ::  to 7 ULP at posit16 -- see its own comment below), same order as the old
  ::  naive AGM path, rather than a dedicated rational kernel; see
  ::  unum_cheb_check.py.
  ++  asin
    ~/  %asin
    |=  x=@
    ^-  @
    ?:  =(nar x)  nar
    ?:  (lth (abs x) one)
      (atan (div x (sqt (sub one (mul x x)))))
    ?:  (equ x one)        (mul pi (div one (sun 2)))
    ?:  (equ x (neg one))  (neg (mul pi (div one (sun 2))))
    nar
  ::    +acos:  @ -> @   (inverse cosine)
  ::  arccos(x) = atan(sqrt(1 - x^2) / x) for x > 0; pi - atan(sqrt(1-x^2)/|x|)
  ::  for x < 0 (pi/2 at 0); 0 at x = 1, pi at x = -1; NaR outside [-1, 1].
  ::  FIX vs. the old naive version: that one called `(atan (div (sqt ...) x))`
  ::  unconditionally, which is WRONG for x<0 (returns a negative angle in
  ::  (-pi/2,0) instead of the correct (pi/2,pi] -- e.g. acos(-0.5) should be
  ::  120 degrees, not -60) -- a pre-existing bug, not something the Chebyshev
  ::  rewrite introduced; caught while re-deriving this arm's correctness.
  ::  Like +asin, faithful not correctly rounded: exhaustively measured worst
  ::  case is 7 ULP at posit16 (near |x|~0.99), 4 ULP at posit32, 1 ULP at
  ::  posit8 -- see unum_cheb_check.py.
  ++  acos
    ~/  %acos
    |=  x=@
    ^-  @
    ?:  =(nar x)  nar
    ?:  (lth (abs x) one)
      ?:  (equ x zero)  (mul pi (div one (sun 2)))
      ?:  (lth x zero)
        (sub pi (atan (div (sqt (sub one (mul x x))) (neg x))))
      (atan (div (sqt (sub one (mul x x))) x))
    ?:  (equ x one)        zero
    ?:  (equ x (neg one))  pi
    nar
  ::    +is-close:  @ -> @ -> @ -> ?   (|a - b| <= tol)
  ++  is-close
    ~/  %is-close
    |=([a=@ b=@ tol=@] ^-(? (lte (abs (sub a b)) tol)))
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
    ~/  %p-to-q
    |=  p=@
    ^-  @
    =/  u  (sea p)
    ?:  ?=(%n -.u)  q-nar
    ?:  ?=(%z -.u)  q-zero
    ?>  ?=(%p -.u)
    =/  m  (lsh [0 (abs:si (sum:si e.u (sun:si qscale)))] a.u)
    ?:(s.u m (^sub qmod m))
  ++  q-to-p
    ~/  %q-to-p
    |=  q=@
    ^-  @
    =.  q  (dis (dec qmod) q)
    ?:  =(q-nar q)  nar
    =/  neg  =(1 (cut 0 [(dec qbits) 1] q))
    =/  acc  ?:(neg (^sub qmod q) q)
    ?:  =(0 acc)  zero
    (bit [%p !neg (dif:si --0 (sun:si qscale)) acc])
  ::    +q-mul-add:  [quire posit posit] -> quire
  ::
  ::  Exact fused multiply-accumulate into the quire: returns `q + a*b` in the
  ::  quire's `16n`-bit fixed-point representation.  No rounding; rounding only
  ::  occurs when `++q-to-p` converts back to a posit.
  ++  q-mul-add
    ~/  %q-mul-add
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
  ::    +q-mul-sub:  [quire posit posit] -> quire
  ::
  ::  Exact fused multiply-subtract: returns `q - a*b`.  No rounding.
  ++  q-mul-sub
    ~/  %q-mul-sub
    |=([q=@ a=@ b=@] ^-(@ (q-mul-add q a (neg b))))
  ::    +q-add-p:  [quire posit] -> quire
  ::
  ::  Exact posit-to-quire addition: returns `q + p`.  No rounding.
  ++  q-add-p
    ~/  %q-add-p
    |=([q=@ p=@] ^-(@ (q-mul-add q p one)))
  ::    +q-sub-p:  [quire posit] -> quire
  ::
  ::  Exact posit-to-quire subtraction: returns `q - p`.  No rounding.
  ++  q-sub-p
    ~/  %q-sub-p
    |=([q=@ p=@] ^-(@ (q-mul-add q (neg p) one)))
  ::    +q-negate:  quire -> quire
  ::
  ::  Exact quire negation.  No rounding.
  ++  q-negate
    ~/  %q-negate
    |=(q=@ ^-(@ ?:(=(q-nar q) q-nar (mod (^sub qmod q) qmod))))
  ::    +q-add-q:  [quire quire] -> quire
  ::
  ::  Exact quire-to-quire addition.  No rounding.
  ++  q-add-q
    ~/  %q-add-q
    |=  [x=@ y=@]
    ^-  @
    ?:  |(=(q-nar x) =(q-nar y))  q-nar
    (mod (^add x y) qmod)
  ::    +q-sub-q:  [quire quire] -> quire
  ::
  ::  Exact quire-to-quire subtraction.  No rounding.
  ++  q-sub-q
    ~/  %q-sub-q
    |=([x=@ y=@] ^-(@ (q-add-q x (q-negate y))))
  ::    +fdp:  (list @) -> (list @) -> @  (fused dot product, single rounding)
  ::
  ::  Accumulates av[i]*bv[i] into the quire exactly, then rounds once via
  ::  +q-to-p.  If the two lists differ in length, iteration stops when the
  ::  shorter list is exhausted -- the tail of the longer list is silently
  ::  ignored.  Callers are responsible for ensuring equal-length inputs for
  ::  a full dot product.
  ++  fdp
    ~/  %fdp
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
  ++  to-rh
    ~/  %to-rh
    |=(p=@ ^-(@rh (bit:rh (up-to-fn (sea p)))))
  ++  to-rs
    ~/  %to-rs
    |=(p=@ ^-(@rs (bit:rs (up-to-fn (sea p)))))
  ++  to-rd
    ~/  %to-rd
    |=(p=@ ^-(@rd (bit:rd (up-to-fn (sea p)))))
  ++  to-rq
    ~/  %to-rq
    |=(p=@ ^-(@rq (bit:rq (up-to-fn (sea p)))))
  ::    +from-rh/rs/rd/rq:  half/single/double/quad float -> this-width posit
  ++  from-rh
    ~/  %from-rh
    |=(r=@rh ^-(@ (bit (fn-to-up (sea:rh r)))))
  ++  from-rs
    ~/  %from-rs
    |=(r=@rs ^-(@ (bit (fn-to-up (sea:rs r)))))
  ++  from-rd
    ~/  %from-rd
    |=(r=@rd ^-(@ (bit (fn-to-up (sea:rd r)))))
  ++  from-rq
    ~/  %from-rq
    |=(r=@rq ^-(@ (bit (fn-to-up (sea:rq r)))))
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
