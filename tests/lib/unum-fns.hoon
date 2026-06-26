  ::  /tests/lib/unum-fns
::::
::    Posits (2022 Posit Standard, es=2) -- elementary functions, rounding,
::    integer/IEEE conversions, the quire, and transcendentals.  Split from a
::    single oversized file so each compiles quickly.
::
::  Note: the from-rh/rs/rd/rq gates are typed on the float aura, so their
::  arguments must be cast (`@rs`0x..) -- a bare 0x.. literal is @ux and will
::  not nest.  The to-* gates take a bare @, so their literals need no cast.
::
/+  *test,
    unum
^|
|%
++  test-sqt-rpb  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x48)  !>((sqt:rpb:unum 0x50))
    %+  expect-eq  !>(`@`0x43)  !>((sqt:rpb:unum 0x48))
    %+  expect-eq  !>(`@`0x80)  !>((sqt:rpb:unum 0xc0))
    %+  expect-eq  !>(`@`0x0)   !>((sqt:rpb:unum 0x0))
    %+  expect-eq  !>(`@`0x80)  !>((sqt:rpb:unum 0x80))
  ==
::
++  test-round-rpb  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x40)  !>((rnd:rpb:unum 0x42))
    %+  expect-eq  !>(`@`0x48)  !>((rnd:rpb:unum 0x4a))
    %+  expect-eq  !>(`@`0x0)   !>((rnd:rpb:unum 0x38))
    %+  expect-eq  !>(`@`0x50)  !>((rnd:rpb:unum 0x4e))
    %+  expect-eq  !>(`@`0x40)  !>((flr:rpb:unum 0x42))
    %+  expect-eq  !>(`@`0x48)  !>((cel:rpb:unum 0x42))
    %+  expect-eq  !>(`@`0xb8)  !>((flr:rpb:unum 0xbe))
    %+  expect-eq  !>(`@`0xc0)  !>((cel:rpb:unum 0xbe))
  ==
::
++  test-convert-rpb  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x4c)  !>((sun:rpb:unum 3))
    %+  expect-eq  !>(`@`0x0)   !>((sun:rpb:unum 0))
    %+  expect-eq  !>(`@`0x4c)  !>((san:rpb:unum --3))
    %+  expect-eq  !>(`@`0xb4)  !>((san:rpb:unum -3))
    %+  expect-eq  !>(`(unit @s)`[~ --1])  !>((toi:rpb:unum 0x42))
    %+  expect-eq  !>(`(unit @s)`[~ --4])  !>((toi:rpb:unum 0x4e))
    %+  expect-eq  !>(`(unit @s)`~)        !>((toi:rpb:unum 0x80))
    %+  expect-eq  !>(`(unit @s)`[~ --0])  !>((toi:rpb:unum 0x0))
  ==
::
++  test-fma-rpb  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x56)  !>((fma:rpb:unum 0x48 0x4c 0x40))
    %+  expect-eq  !>(`@`0x80)  !>((fma:rpb:unum 0x48 0x80 0x40))
  ==
::
++  test-quire-rpb  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x42)  !>((q-to-p:rpb:unum (p-to-q:rpb:unum 0x42)))
    %+  expect-eq  !>(`@`0x38)  !>((q-to-p:rpb:unum (p-to-q:rpb:unum 0x38)))
    %+  expect-eq  !>(`@`0x4c)
      !>((q-to-p:rpb:unum (q-add-p:rpb:unum (p-to-q:rpb:unum 0x40) 0x48)))
    %+  expect-eq  !>(`@`0x54)
      !>((q-to-p:rpb:unum (q-mul-add:rpb:unum q-zero:rpb:unum 0x48 0x4c)))
  ==
::
++  test-fdp-rpb  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x52)  !>((fdp:rpb:unum ~[0x40 0x48] ~[0x4c 0x40]))
    %+  expect-eq  !>(`@`0x5d)  !>((fdp:rpb:unum ~[0x48 0x4c] ~[0x48 0x4c]))
    %+  expect-eq  !>(`@`0x40)
      !>((fdp:rpb:unum ~[0x7f 0x40 0x81] ~[0x40 0x40 0x40]))
  ==
::
::  posit32 <-> single (value-based; posit -2.0 = 0xb800.0000, NOT the
::  single -2.0 = 0xc000.0000 -- the two formats differ at the same width).
++  test-ieee-rps  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x3f80.0000)  !>((to-rs:rps:unum 0x4000.0000))
    %+  expect-eq  !>(`@`0xc000.0000)  !>((to-rs:rps:unum 0xb800.0000))
    %+  expect-eq  !>(`@`0x3f00.0000)  !>((to-rs:rps:unum 0x3800.0000))
    %+  expect-eq  !>(`@`0x4000.0000)  !>((from-rs:rps:unum `@rs`0x3f80.0000))
    %+  expect-eq  !>(`@`0xb800.0000)  !>((from-rs:rps:unum `@rs`0xc000.0000))
    %+  expect-eq  !>(`@`0x3800.0000)  !>((from-rs:rps:unum `@rs`0x3f00.0000))
  ==
::
++  test-ieee-rph  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x3c00)  !>((to-rh:rph:unum 0x4000))
    %+  expect-eq  !>(`@`0xc000)  !>((to-rh:rph:unum 0xb800))
    %+  expect-eq  !>(`@`0x4000)  !>((from-rh:rph:unum `@rh`0x3c00))
    %+  expect-eq  !>(`@`0xb800)  !>((from-rh:rph:unum `@rh`0xc000))
  ==
::
::  The matrix: ANY posit width <-> ANY float width (value-based).  posits
::  pack more accuracy per bit, so cross-width is the useful correspondence.
::
++  test-ieee-matrix  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x3f80.0000)  !>((to-rs:rph:unum 0x4000))
    %+  expect-eq  !>(`@`0x3ff0.0000.0000.0000)  !>((to-rd:rps:unum 0x4000.0000))
    %+  expect-eq  !>(`@`0x3f80.0000)  !>((to-rs:rpb:unum 0x40))
    %+  expect-eq  !>(`@`0x4000)  !>((from-rs:rph:unum `@rs`0x3f80.0000))
    %+  expect-eq  !>(`@`0x4000.0000)  !>((from-rd:rps:unum `@rd`0x3ff0.0000.0000.0000))
  ==
::
::  Transcendentals (naive Taylor series, /lib/math style).  These posit8
::  values are the series outputs, which match the SoftPosit correctly-rounded
::  result (verified offline by a Python port of the same series).  pow-n is
::  exact integer power.
::
++  test-transcendental-rpb  ^-  tang
  =/  u  rpb:unum
  ;:  weld
    %+  expect-eq  !>(`@`0x40)  !>((exp:u 0x0))
    %+  expect-eq  !>(`@`0x4b)  !>((exp:u 0x40))
    %+  expect-eq  !>(`@`0x0)   !>((sin:u 0x0))
    %+  expect-eq  !>(`@`0x3d)  !>((sin:u 0x40))
    %+  expect-eq  !>(`@`0x40)  !>((cos:u 0x0))
    %+  expect-eq  !>(`@`0x39)  !>((cos:u 0x40))
    %+  expect-eq  !>(`@`0x44)  !>((tan:u 0x40))
    %+  expect-eq  !>(`@`0x0)   !>((log:u 0x40))
    %+  expect-eq  !>(`@`0x3b)  !>((log:u (sun:u 2)))
    %+  expect-eq  !>(`@`0x58)  !>((pow-n:u (sun:u 2) 3))
    %+  expect-eq  !>(`@`0x40)  !>((pow:u (sun:u 2) 0x0))
  ==
::
::  Domain guards: log/pow of out-of-domain inputs return NaR (not a divergent
::  series result), and NaR propagates through pow-n even when the exponent is 0.
::
++  test-domain-rpb  ^-  tang
  =/  u  rpb:unum
  ;:  weld
    %+  expect-eq  !>(`@`0x80)  !>((log:u 0x0))            ::  log(0)=NaR
    %+  expect-eq  !>(`@`0x80)  !>((log:u 0xc0))           ::  log(-1)=NaR
    %+  expect-eq  !>(`@`0x80)  !>((log:u 0x80))           ::  log(NaR)=NaR
    %+  expect-eq  !>(`@`0x80)  !>((log:u 0xb4))           ::  log(-3)=NaR
    %+  expect-eq  !>(`@`0x80)  !>((pow-n:u 0x80 0))       ::  NaR^0=NaR (not 1)
    %+  expect-eq  !>(`@`0x40)  !>((pow-n:u 0x40 0))       ::  1^0=1 still
    %+  expect-eq  !>(`@`0x80)  !>((pow:u 0x0 (sun:u 2)))  ::  pow(0,2)=NaR
    %+  expect-eq  !>(`@`0x80)  !>((pow:u 0xc0 (sun:u 2))) ::  pow(-1,2)=NaR
  ==
::  New transcendentals (factorial, cbrt, atan, asin, acos) at posit8.  Expected
::  values from tools/posit_check.py's series replica; all correctly rounded vs
::  mpmath at posit8.  Inputs: 0x38 = .5, (sun 1) = 1, (sun 3/4) = 3/4.
++  test-transcendental-new-rpb  ^-  tang
  =/  u  rpb:unum
  ;:  weld
    %+  expect-eq  !>(`@`0x54)  !>((factorial:u (sun:u 3)))   ::  3! = 6
    %+  expect-eq  !>(`@`0x62)  !>((factorial:u (sun:u 4)))   ::  4! = 24
    %+  expect-eq  !>(`@`0x40)  !>((cbrt:u (sun:u 1)))        ::  cbrt 1 = 1
    %+  expect-eq  !>(`@`0x3d)  !>((atan:u (sun:u 1)))        ::  atan 1 = pi/4
    %+  expect-eq  !>(`@`0x38)  !>((atan:u 0x38))             ::  atan .5
    %+  expect-eq  !>(`@`0x39)  !>((asin:u 0x38))             ::  asin .5
    %+  expect-eq  !>(`@`0x41)  !>((acos:u 0x38))             ::  acos .5
    %+  expect-eq  !>(`@`0x45)  !>((acos:u 0x0))              ::  acos 0 = pi/2
  ==
::  Domain guards for the new arms: out-of-range -> NaR (unum nar convention),
::  exact +-1 / 0 boundaries, cbrt(0)=0.
++  test-domain-new-rpb  ^-  tang
  =/  u  rpb:unum
  ;:  weld
    %+  expect-eq  !>(`@`0x80)  !>((cbrt:u 0xc0))             ::  cbrt(-1)=NaR
    %+  expect-eq  !>(`@`0x0)   !>((cbrt:u 0x0))              ::  cbrt(0)=0
    %+  expect-eq  !>(`@`0x80)  !>((factorial:u 0xb4))        ::  factorial(-3)=NaR
    %+  expect-eq  !>(`@`0x80)  !>((asin:u (sun:u 4)))        ::  asin(4)=NaR (|x|>1)
    %+  expect-eq  !>(`@`0x80)  !>((acos:u (sun:u 4)))        ::  acos(4)=NaR
    %+  expect-eq  !>(`@`0x45)  !>((asin:u (sun:u 1)))        ::  asin(1)=pi/2
    %+  expect-eq  !>(`@`0x0)   !>((acos:u (sun:u 1)))        ::  acos(1)=0
  ==
::  Full transcendental set at posit16 (rph) and posit32 (rps).  Expected values
::  from the series replica; the naive Taylor/AGM series is correctly rounded
::  near the expansion point and within ~1 ULP elsewhere (see tools/posit_check).
++  test-transcendental-rph  ^-  tang
  =/  u  rph:unum
  ;:  weld
    %+  expect-eq  !>(`@`0x4531)  !>((exp:u 0x3800))          ::  exp .5
    %+  expect-eq  !>(`@`0x3757)  !>((sin:u 0x3800))          ::  sin .5
    %+  expect-eq  !>(`@`0x3e0b)  !>((cos:u 0x3800))          ::  cos .5
    %+  expect-eq  !>(`@`0x38bd)  !>((tan:u 0x3800))          ::  tan .5
    %+  expect-eq  !>(`@`0x3b18)  !>((log:u (sun:u 2)))       ::  log 2
    %+  expect-eq  !>(`@`0x5400)  !>((factorial:u (sun:u 3))) ::  3!
    %+  expect-eq  !>(`@`0x6200)  !>((factorial:u (sun:u 4))) ::  4!
    %+  expect-eq  !>(`@`0x4000)  !>((cbrt:u (sun:u 1)))      ::  cbrt 1
    %+  expect-eq  !>(`@`0x3c90)  !>((atan:u (sun:u 1)))      ::  atan 1
    %+  expect-eq  !>(`@`0x36d5)  !>((atan:u 0x3800))         ::  atan .5
    %+  expect-eq  !>(`@`0x3861)  !>((asin:u 0x3800))         ::  asin .5
    %+  expect-eq  !>(`@`0x4060)  !>((acos:u 0x3800))         ::  acos .5
    %+  expect-eq  !>(`@`0x4491)  !>((acos:u 0x0))            ::  acos 0
    %+  expect-eq  !>(`@`0x5800)  !>((pow-n:u (sun:u 2) 3))   ::  2^3
  ==
++  test-transcendental-rps  ^-  tang
  =/  u  rps:unum
  ;:  weld
    %+  expect-eq  !>(`@`0x4530.94c8)  !>((exp:u 0x3800.0000))  ::  exp .5
    %+  expect-eq  !>(`@`0x3757.743a)  !>((sin:u 0x3800.0000))  ::  sin .5
    %+  expect-eq  !>(`@`0x3e0a.9404)  !>((cos:u 0x3800.0000))  ::  cos .5
    %+  expect-eq  !>(`@`0x38bd.a7ad)  !>((tan:u 0x3800.0000))  ::  tan .5
    %+  expect-eq  !>(`@`0x3b17.2180)  !>((log:u (sun:u 2)))   ::  log 2
    %+  expect-eq  !>(`@`0x5400.0000)  !>((factorial:u (sun:u 3)))
    %+  expect-eq  !>(`@`0x6200.0000)  !>((factorial:u (sun:u 4)))
    %+  expect-eq  !>(`@`0x4000.0000)  !>((cbrt:u (sun:u 1)))  ::  cbrt 1
    %+  expect-eq  !>(`@`0x3c90.fdab)  !>((atan:u (sun:u 1)))  ::  atan 1
    %+  expect-eq  !>(`@`0x36d6.3381)  !>((atan:u 0x3800.0000)) ::  atan .5
    %+  expect-eq  !>(`@`0x3860.a91c)  !>((asin:u 0x3800.0000)) ::  asin .5
    %+  expect-eq  !>(`@`0x4060.a91d)  !>((acos:u 0x3800.0000)) ::  acos .5
    %+  expect-eq  !>(`@`0x4490.fdaa)  !>((acos:u 0x0))        ::  acos 0
    %+  expect-eq  !>(`@`0x5800.0000)  !>((pow-n:u (sun:u 2) 3))
  ==
--
