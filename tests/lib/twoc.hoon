  ::  /tests/lib/twoc
::::
::    Two's-complement integer arithmetic (modular).  The bulk of the
::    arithmetic is checked at int8 (bloq 3) through the +twoc facade; the
::    wrap boundaries are re-checked at int16/32/64.  The width-keyed +twid
::    door is exercised directly at a non-power-of-2 width (17) -- the case
::    /lib/fixed needs -- plus a facade/width parity check.
::
/+  *test,
    twoc
^|
=/  t  ~(. twoc:twoc 3)            :: int8 (facade)
=/  t16  ~(. twoc:twoc 4)          :: int16
=/  t32  ~(. twoc:twoc 5)          :: int32
=/  t64  ~(. twoc:twoc 6)          :: int64
=/  w17  ~(. twid:twoc 17)         :: width-17 (q8.8 fixed N)
=/  w8   ~(. twid:twoc 8)          :: width-8, should match the bloq-3 facade
|%
++  test-add  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x96)  !>((add:t 0x64 0x32))   ::  100+50 wraps -> -106
    %+  expect-eq  !>(`@`0x0)   !>((add:t 0xff 0x1))    ::  -1+1=0
    %+  expect-eq  !>(`@`0x7)   !>((add:t 0x5 0x2))     ::  5+2=7
  ==
::
++  test-neg-sub  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0xff)  !>((neg:t 0x1))         ::  -1
    %+  expect-eq  !>(`@`0x80)  !>((neg:t 0x80))        ::  min negates to self
    %+  expect-eq  !>(`@`0xfb)  !>((sub:t 0x5 0xa))     ::  5-10=-5
    %+  expect-eq  !>(`@`0x0)   !>((sub:t 0x7 0x7))
    %+  expect-eq  !>(`@`0x0)   !>((neg:t 0x0))         ::  -0=0
  ==
::
++  test-mul  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x1)   !>((mul:t 0xff 0xff))   ::  -1*-1=1
    %+  expect-eq  !>(`@`0x0)   !>((mul:t 0x10 0x10))   ::  16*16=256 wraps 0
    %+  expect-eq  !>(`@`0xf6)  !>((mul:t 0xfb 0x2))    ::  -5*2=-10
  ==
::
++  test-abs  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x5)   !>((abs:t 0xfb))        ::  |-5|=5
    %+  expect-eq  !>(`@`0x5)   !>((abs:t 0x5))
    %+  expect-eq  !>(`@`0x80)  !>((abs:t 0x80))        ::  |min| wraps to min
  ==
::
++  test-div-rem  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0xfd)  !>((div:t 0xf9 0x2))    ::  -7/2 trunc=-3
    %+  expect-eq  !>(`@`0xfd)  !>((div:t 0x7 0xfe))    ::  7/-2 trunc=-3
    %+  expect-eq  !>(`@`0x3)   !>((div:t 0xf9 0xfe))   ::  -7/-2=3
    %+  expect-eq  !>(`@`0xff)  !>((rem:t 0xf9 0x2))    ::  -7%2=-1
    %+  expect-eq  !>(`@`0x1)   !>((rem:t 0x7 0xfe))    ::  7%-2=1
  ==
::
++  test-pow  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0xf8)  !>((pow:t 0xfe 3))      ::  -2^3=-8
    %+  expect-eq  !>(`@`0x51)  !>((pow:t 0x3 4))       ::  3^4=81
    %+  expect-eq  !>(`@`0x1)   !>((pow:t 0x5 0))       ::  x^0=1
  ==
::
++  test-compare  ^-  tang
  ;:  weld
    %+  expect-eq  !>(%.n)  !>((gth:t 0xff 0x1))        ::  -1 > 1 ? no
    %+  expect-eq  !>(%.y)  !>((lth:t 0xff 0x1))        ::  -1 < 1 ? yes
    %+  expect-eq  !>(%.y)  !>((gth:t 0x1 0xff))        ::  1 > -1 ? yes
    %+  expect-eq  !>(%.y)  !>((lte:t 0x5 0x5))         ::  5 <= 5
    %+  expect-eq  !>(%.y)  !>((gte:t 0x5 0x5))         ::  5 >= 5
    %+  expect-eq  !>(%.n)  !>((lth:t 0x5 0x5))         ::  5 < 5 ? no
  ==
::
::  round-trip identity for the division algorithm: a == div*b + rem
++  test-div-identity  ^-  tang
  =/  pairs=(list [@ @])  ~[[0xf9 0x2] [0x7 0xfe] [0xf9 0xfe] [0x64 0x7] [0x9c 0x5]]
  %+  roll  pairs
  |=  [[a=@ b=@] acc=tang]
  =/  recon  (add:t (mul:t (div:t a b) b) (rem:t a b))
  %+  weld  acc
  (expect-eq !>(a) !>(recon))
::
::  int16 (bloq 4): wrap boundary at 0x7fff/0x8000, neg, signed mul/cmp
++  test-int16  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x7d0)   !>((add:t16 0x3e8 0x3e8))    ::  1000+1000=2000
    %+  expect-eq  !>(`@`0x8000)  !>((add:t16 0x7fff 0x1))     ::  maxint+1 -> min
    %+  expect-eq  !>(`@`0xffff)  !>((neg:t16 0x1))            ::  -1
    %+  expect-eq  !>(`@`0x8000)  !>((neg:t16 0x8000))         ::  min self
    %+  expect-eq  !>(`@`0xfffe)  !>((mul:t16 0xffff 0x2))     ::  -1*2=-2
    %+  expect-eq  !>(`@`0x1)     !>((mul:t16 0xffff 0xffff))  ::  -1*-1=1
    %+  expect-eq  !>(%.y)        !>((lth:t16 0xffff 0x1))     ::  -1 < 1
    %+  expect-eq  !>(`@`0x5)     !>((abs:t16 0xfffb))         ::  |-5|=5
  ==
::
::  int32 (bloq 5): same boundary structure at 32 bits
++  test-int32  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x8000.0000)  !>((add:t32 0x7fff.ffff 0x1))     ::  maxint+1 -> min
    %+  expect-eq  !>(`@`0xffff.ffff)  !>((neg:t32 0x1))                 ::  -1
    %+  expect-eq  !>(`@`0x1)          !>((mul:t32 0xffff.ffff 0xffff.ffff))  ::  -1*-1=1
    %+  expect-eq  !>(`@`0xffff.fffe)  !>((mul:t32 0xffff.ffff 0x2))     ::  -1*2=-2
    %+  expect-eq  !>(%.y)             !>((gth:t32 0x1 0xffff.ffff))     ::  1 > -1
  ==
::
::  int64 (bloq 6): boundary structure at 64 bits
++  test-int64  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x8000.0000.0000.0000)  !>((add:t64 0x7fff.ffff.ffff.ffff 0x1))  ::  maxint+1 -> min
    %+  expect-eq  !>(`@`0xffff.ffff.ffff.ffff)  !>((neg:t64 0x1))                         ::  -1
    %+  expect-eq  !>(`@`0x1)                    !>((mul:t64 0xffff.ffff.ffff.ffff 0xffff.ffff.ffff.ffff))  ::  -1*-1=1
    %+  expect-eq  !>(`@`0xffff.ffff.ffff.fffe)  !>((mul:t64 0xffff.ffff.ffff.ffff 0x2))   ::  -1*2=-2
    %+  expect-eq  !>(%.y)                       !>((lth:t64 0xffff.ffff.ffff.ffff 0x1))   ::  -1 < 1
  ==
::
::  +twid at a non-power-of-2 width (17): the case /lib/fixed needs.
::  Sign bit is bit 16 (0x1.0000); max positive is 0xffff, min is 0x1.0000.
++  test-twid17  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x1.ffff)  !>((neg:w17 0x1))             ::  -1
    %+  expect-eq  !>(`@`0x1.0000)  !>((add:w17 0xffff 0x1))      ::  maxpos+1 -> min (wrap)
    %+  expect-eq  !>(`@`0x1.fffa)  !>((mul:w17 (neg:w17 0x3) 0x2))  ::  -3*2=-6
    %+  expect-eq  !>(`@`0x6)       !>((mul:w17 (neg:w17 0x3) (neg:w17 0x2)))  ::  -3*-2=6
    %+  expect-eq  !>(%.y)          !>((lth:w17 0x1.ffff 0x1))    ::  -1 < 1
    %+  expect-eq  !>(`@`0x5)       !>((abs:w17 0x1.fffb))        ::  |-5|=5
    %+  expect-eq  !>(`@`0x5)       !>((s-to-twoc:w17 --5))       ::  +5
    %+  expect-eq  !>(`@`0x1.fffb)  !>((s-to-twoc:w17 -5))        ::  -5
    %+  expect-eq  !>(`@s`-5)       !>((twoc-to-s:w17 0x1.fffb))  ::  round trip back
    %+  expect-eq  !>(`@s`--5)      !>((twoc-to-s:w17 0x5))
  ==
::
::  facade/width parity: +twoc at bloq 3 must equal +twid at width 8.
++  test-facade-parity  ^-  tang
  ;:  weld
    %+  expect-eq  !>((add:t 0x64 0x32))   !>((add:w8 0x64 0x32))
    %+  expect-eq  !>((mul:t 0xfb 0x2))    !>((mul:w8 0xfb 0x2))
    %+  expect-eq  !>((div:t 0xf9 0x2))    !>((div:w8 0xf9 0x2))
    %+  expect-eq  !>((neg:t 0x1))         !>((neg:w8 0x1))
    %+  expect-eq  !>((rem:t 0xf9 0x2))    !>((rem:w8 0xf9 0x2))
  ==
--
