  ::  /tests/lib/unum-core
::::
::    Posits (2022 Posit Standard, es=2) -- decode/encode, constants,
::    comparison, and core arithmetic.  Split from a single oversized file so
::    each compiles quickly (cf. the lagoon test-by-category convention).
::
::  Heavy exhaustive cross-checks vs SoftPosit run once, offline, in Python
::  (libmath/tools/posit_check.py).  The on-ship suite is lean: round-trips,
::  a property sweep, and curated SoftPosit-verified spot values.
::
/+  *test,
    unum
^|
|%
++  test-round-trip-rpb  ^-  tang
  =|  i=@
  |-  ^-  tang
  ?:  =(256 i)  ~
  =/  rt  (bit:rpb:unum (sea:rpb:unum i))
  ?:  =(i rt)  $(i +(i))
  [(cat 3 'rpb round-trip failed at ' (scot %ux i)) $(i +(i))]
::
++  test-round-trip-rph  ^-  tang
  =|  i=@
  |-  ^-  tang
  ?:  =(65.536 i)  ~
  =/  rt  (bit:rph:unum (sea:rph:unum i))
  ?:  =(i rt)  $(i +(i))
  [(cat 3 'rph round-trip failed at ' (scot %ux i)) $(i +(i))]
::
++  test-values-rpb  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x0)   !>(zero:rpb:unum)
    %+  expect-eq  !>(`@`0x80)  !>(nar:rpb:unum)
    %+  expect-eq  !>(`@`0x40)  !>(one:rpb:unum)
    %+  expect-eq  !>(`@`0x7f)  !>(maxpos:rpb:unum)
    %+  expect-eq  !>(`@`0x1)   !>(minpos:rpb:unum)
  ==
::
++  test-consts-rpb  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x4d)  !>(pi:rpb:unum)
    %+  expect-eq  !>(`@`0x55)  !>(tau:rpb:unum)
    %+  expect-eq  !>(`@`0x4b)  !>(e:rpb:unum)
    %+  expect-eq  !>(`@`0x45)  !>(phi:rpb:unum)
    %+  expect-eq  !>(`@`0x43)  !>(sqt2:rpb:unum)
    %+  expect-eq  !>(`@`0x3b)  !>(invsqt2:rpb:unum)
    %+  expect-eq  !>(`@`0x3b)  !>(log2:rpb:unum)
    %+  expect-eq  !>(`@`0x44)  !>(invlog2:rpb:unum)
    %+  expect-eq  !>(`@`0x49)  !>(log10:rpb:unum)
  ==
::
++  test-consts-rph  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x4c91)  !>(pi:rph:unum)
    %+  expect-eq  !>(`@`0x5491)  !>(tau:rph:unum)
    %+  expect-eq  !>(`@`0x4ae0)  !>(e:rph:unum)
    %+  expect-eq  !>(`@`0x44f2)  !>(phi:rph:unum)
    %+  expect-eq  !>(`@`0x4350)  !>(sqt2:rph:unum)
    %+  expect-eq  !>(`@`0x3b50)  !>(invsqt2:rph:unum)
    %+  expect-eq  !>(`@`0x3b17)  !>(log2:rph:unum)
    %+  expect-eq  !>(`@`0x438b)  !>(invlog2:rph:unum)
    %+  expect-eq  !>(`@`0x4936)  !>(log10:rph:unum)
  ==
::
++  test-consts-rps  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x4c90.fdaa)  !>(pi:rps:unum)
    %+  expect-eq  !>(`@`0x5490.fdaa)  !>(tau:rps:unum)
    %+  expect-eq  !>(`@`0x4adf.8546)  !>(e:rps:unum)
    %+  expect-eq  !>(`@`0x44f1.bbce)  !>(phi:rps:unum)
    %+  expect-eq  !>(`@`0x4350.4f33)  !>(sqt2:rps:unum)
    %+  expect-eq  !>(`@`0x3b50.4f33)  !>(invsqt2:rps:unum)
    %+  expect-eq  !>(`@`0x3b17.217f)  !>(log2:rps:unum)
    %+  expect-eq  !>(`@`0x438a.a3b3)  !>(invlog2:rps:unum)
    %+  expect-eq  !>(`@`0x4935.d8de)  !>(log10:rps:unum)
  ==
::
++  test-sea-rpb  ^-  tang
  ;:  weld
    %+  expect-eq  !>([%z ~])             !>((sea:rpb:unum 0x0))
    %+  expect-eq  !>([%n ~])             !>((sea:rpb:unum 0x80))
    %+  expect-eq  !>([%p %.y -3 8])      !>((sea:rpb:unum 0x40))
    %+  expect-eq  !>([%p %.y -2 8])      !>((sea:rpb:unum 0x48))
    %+  expect-eq  !>([%p %.y -3 10])     !>((sea:rpb:unum 0x42))
  ==
::
++  test-bit-rpb  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x0)   !>((bit:rpb:unum [%z ~]))
    %+  expect-eq  !>(`@`0x80)  !>((bit:rpb:unum [%n ~]))
    %+  expect-eq  !>(`@`0x40)  !>((bit:rpb:unum [%p %.y --0 1]))
    %+  expect-eq  !>(`@`0x48)  !>((bit:rpb:unum [%p %.y -2 8]))
    %+  expect-eq  !>(`@`0xc0)  !>((bit:rpb:unum [%p %.n --0 1]))
  ==
::
++  test-compare-rpb  ^-  tang
  ;:  weld
    %+  expect-eq  !>(%.y)  !>((lth:rpb:unum 0x38 0x40))
    %+  expect-eq  !>(%.n)  !>((lth:rpb:unum 0x40 0x38))
    %+  expect-eq  !>(%.y)  !>((gth:rpb:unum 0x40 0xc0))
    %+  expect-eq  !>(%.y)  !>((lth:rpb:unum 0x80 0xc0))
    %+  expect-eq  !>(%.y)  !>((equ:rpb:unum 0x80 0x80))
    %+  expect-eq  !>(`@`0xc0)  !>((neg:rpb:unum 0x40))
    %+  expect-eq  !>(`@`0x40)  !>((abs:rpb:unum 0xc0))
    %+  expect-eq  !>(`@`0xc0)  !>((sgn:rpb:unum 0xa0))
  ==
::
++  test-arith-spot-rpb  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x48)  !>((add:rpb:unum 0x40 0x40))
    %+  expect-eq  !>(`@`0x40)  !>((add:rpb:unum 0x38 0x38))
    %+  expect-eq  !>(`@`0x4c)  !>((add:rpb:unum 0x40 0x48))
    %+  expect-eq  !>(`@`0x48)  !>((sub:rpb:unum 0x4c 0x40))
    %+  expect-eq  !>(`@`0x54)  !>((mul:rpb:unum 0x48 0x4c))
    %+  expect-eq  !>(`@`0x38)  !>((div:rpb:unum 0x40 0x48))
    %+  expect-eq  !>(`@`0x80)  !>((div:rpb:unum 0x40 0x0))
    %+  expect-eq  !>(`@`0x80)  !>((mul:rpb:unum 0x40 0x80))
  ==
::
++  test-arith-spot-rph  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@`0x4c00)  !>((add:rph:unum 0x4000 0x4800))
    %+  expect-eq  !>(`@`0x319a)  !>((mul:rph:unum 0x4c00 0x24cd))
  ==
::
++  test-arith-props-rpb  ^-  tang
  =/  mu  mul:rpb:unum
  =/  ad  add:rpb:unum
  =/  ng  neg:rpb:unum
  =/  on  one:rpb:unum
  =/  no  (ng on)
  =|  i=@
  |-  ^-  tang
  ?:  =(256 i)  ~
  =/  a  i
  =/  b  (mod (add (mul i 181) 67) 256)
  =/  e1=tang  ?:(=((mu a b) (mu b a)) ~ ~[(cat 3 'mul comm at ' (scot %ux i))])
  =/  e2=tang  ?:(=((ad a b) (ad b a)) ~ ~[(cat 3 'add comm at ' (scot %ux i))])
  =/  e3=tang  ?:(=(a (ad a 0x0)) ~ ~[(cat 3 'a+0 at ' (scot %ux i))])
  =/  e4=tang  ?:(=(a (mu a on)) ~ ~[(cat 3 'a*1 at ' (scot %ux i))])
  =/  e5=tang  ?:(=((ng a) (mu a no)) ~ ~[(cat 3 'a*-1 at ' (scot %ux i))])
  :(weld e1 e2 e3 e4 e5 $(i +(i)))
--
