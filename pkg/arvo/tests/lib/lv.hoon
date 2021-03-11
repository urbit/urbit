/+  *test, *lv
::
::::
  ::
=/  rtol  .1e-6
|%
::  Auxiliary tools
::    Replace element of c at index a with item b
++  nick
  |*  [a=@ b=* c=(list @)]
  (weld (scag a c) [b (slag +(a) c)])
::    Absolute value
++  absolute
  |=  x=@rs  ^-  @rs
  ?:  (gth:rs x .0)  x
  (sub:rs .0 x)
++  expect-near
  |=  [expected=vase actual=vase]
  ^-  tang
  ::
  =|  result=tang
  ::
  =?  result  !(all-close:lvs `@lvs`q.expected `@lvs`q.actual .1e-6)
    %+  weld  result
    ^-  tang
    :~  [%palm [": " ~ ~ ~] [leaf+"expected" (sell expected) ~]]
        [%palm [": " ~ ~ ~] [leaf+"actual" (sell actual) ~]]
    ==
  ::
  =?  result  !(~(nest ut p.actual) | p.expected)
    %+  weld  result
    ^-  tang
    :~  :+  %palm  [": " ~ ~ ~]
        :~  [%leaf "failed to nest"]
            (~(dunk ut p.actual) %actual)
            (~(dunk ut p.expected) %expected)
    ==  ==
  result
::
::  Tests for vector creation
::
++  test-zeros  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvs`0x1
      !>  (zeros:lvs 0)
    %+  expect-eq
      !>  `@lvs`0x1.0000.0000
      !>  (zeros:lvs 1)
    %+  expect-eq
      !>  (zeros:lvs 1)
      !>  (make:lvs ~[.0])
    %+  expect-eq
      !>  `@lvs`0x1.0000.0000.0000.0000.0000.0000
      !>  (zeros:lvs 3)
    %+  expect-eq
      !>  (zeros:lvs 3)
      !>  (make:lvs ~[.0 .0 .0])
    %+  expect-eq
      !>  `@lvs`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
      !>  (zeros:lvs 5)
    %+  expect-eq
      !>  (zeros:lvs 5)
      !>  (make:lvs `(list @rs)`(reap 5 .0))
    %+  expect-eq
      !>  (zeros:lvs 16)
      !>  `@lvs`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
    %+  expect-eq
      !>  (zeros:lvs 16)
      !>  (make:lvs `(list @rs)`(reap 16 .0))
  ==
++  test-ones  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (ones:lvs 0)
      !>  `@lvs`0x1
    %+  expect-eq
      !>  (ones:lvs 1)
      !>  `@lvs`0x1.3f80.0000
    %+  expect-eq
      !>  (ones:lvs 1)
      !>  (make:lvs ~[.1])
    %+  expect-eq
      !>  (ones:lvs 3)
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000
    %+  expect-eq
      !>  (ones:lvs 3)
      !>  (make:lvs ~[.1 .1 .1])
    %+  expect-eq
      !>  (ones:lvs 5)
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000
    %+  expect-eq
      !>  (ones:lvs 5)
      !>  (make:lvs `(list @rs)`(reap 5 .1))
    %+  expect-eq
      !>  (ones:lvs 16)
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000
    %+  expect-eq
      !>  (ones:lvs 16)
      !>  (make:lvs `(list @rs)`(reap 16 .1))
  ==
++  test-fill  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvs`0x1
      !>  (fill:lvs 0 .0)
    %+  expect-eq
      !>  `@lvs`0x1
      !>  (fill:lvs 0 .1)
    %+  expect-eq
      !>  (fill:lvs 1 .0)
      !>  `@lvs`0x1.0000.0000
    %+  expect-eq
      !>  (fill:lvs 1 .0)
      !>  (make:lvs ~[.0])
    %+  expect-eq
      !>  (fill:lvs 1 .1)
      !>  `@lvs`0x1.3f80.0000
    %+  expect-eq
      !>  (fill:lvs 1 .1)
      !>  (make:lvs ~[.1])
    %+  expect-eq
      !>  (fill:lvs 1 .-1)
      !>  `@lvs`0x1.bf80.0000
    %+  expect-eq
      !>  (fill:lvs 1 .-1)
      !>  (make:lvs ~[.-1])
    %+  expect-eq
      !>  (fill:lvs 3 .1)
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000
    %+  expect-eq
      !>  (fill:lvs 3 .1)
      !>  (make:lvs ~[.1 .1 .1])
    %+  expect-eq
      !>  (fill:lvs 5 .1)
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000
    %+  expect-eq
      !>  (fill:lvs 5 .1)
      !>  (make:lvs `(list @rs)`(reap 5 .1))
  ==
++  test-make  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvs`0x1.0000.0000
      !>  (make:lvs ~[.0])
    %+  expect-eq
      !>  `@lvs`0x1.0000.0000.0000.0000.0000.0000
      !>  (make:lvs ~[.0 .0 .0])
    %+  expect-eq
      !>  `@lvs`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
      !>  (make:lvs `(list @rs)`(reap 5 .0))
    %+  expect-eq
      !>  `@lvs`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
      !>  (make:lvs `(list @rs)`(reap 16 .0))
    %+  expect-eq
      !>  `@lvs`0x1.3f80.0000
      !>  (make:lvs ~[.1])
    %+  expect-eq
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000
      !>  (make:lvs ~[.1 .1 .1])
    %+  expect-eq
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000
      !>  (make:lvs `(list @rs)`(reap 5 .1))
    %+  expect-eq
      !>  (make:lvs `(list @rs)`(reap 16 .1))
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000
    %+  expect-eq
      !>  `@lvs`0x1.4000.0000.3f80.0000
      !>  (make:lvs ~[.1 .2])
    %+  expect-eq
      !>  `@lvs`0x1.4040.0000.4000.0000.3f80.0000
      !>  (make:lvs ~[.1 .2 .3])
    %+  expect-eq
      !>  `@lvs`0x1.4080.0000.4040.0000.4000.0000.3f80.0000.0000.0000.bf80.0000.c000.0000.c040.0000
      !>  (make:lvs ~[.-3 .-2 .-1 .0 .1 .2 .3 .4])
  ==
++  test-unmake  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `(list @rs)`~[.0]
      !>  (unmake:lvs `@lvs`0x1.0000.0000)
    %+  expect-eq
      !>  `(list @rs)`~[.0 .0 .0]
      !>  (unmake:lvs `@lvs`0x1.0000.0000.0000.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rs)`(reap 5 .0)
      !>  (unmake:lvs `@lvs`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rs)`(reap 16 .0)
      !>  (unmake:lvs `@lvs`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rs)`~[.1]
      !>  (unmake:lvs `@lvs`0x1.3f80.0000)
    %+  expect-eq
      !>  `(list @rs)`~[.1 .1 .1]
      !>  (unmake:lvs `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000)
    %+  expect-eq
      !>  `(list @rs)`(reap 5 .1)
      !>  (unmake:lvs `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000)
    %+  expect-eq
      !>  `(list @rs)`(reap 16 .1)
      !>  (unmake:lvs `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000)
    %+  expect-eq
      !>  `(list @rs)`~[.1 .2]
      !>  (unmake:lvs `@lvs`0x1.4000.0000.3f80.0000)
    %+  expect-eq
      !>  `(list @rs)`~[.1 .2 .3]
      !>  (unmake:lvs `@lvs`0x1.4040.0000.4000.0000.3f80.0000)
    %+  expect-eq
      !>  `(list @rs)`~[.-3 .-2 .-1 .0 .1 .2 .3 .4]
      !>  (unmake:lvs `@lvs`0x1.4080.0000.4040.0000.4000.0000.3f80.0000.0000.0000.bf80.0000.c000.0000.c040.0000)
  ==
++  test-make-unmake  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `(list @rs)`~[.1 .2 .3 .4 .5]
      !>  (unmake:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]))
    %+  expect-eq
      !>  `@lvs`0x1.40a0.0000.4080.0000.4040.0000.4000.0000.3f80.0000
      !>  (make:lvs (unmake:lvs `@lvs`0x1.40a0.0000.4080.0000.4040.0000.4000.0000.3f80.0000))
  ==
::
::  Tests for utility functions
::
++  test-length  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  0
      !>  (length:lvs `@lvs`0x1)
    %+  expect-eq
      !>  1
      !>  (length:lvs `@lvs`0x1.3ff0.0000)
    %+  expect-eq
      !>  5
      !>  (length:lvs `@lvs`0x1.40a0.0000.4080.0000.4040.0000.4000.0000.3f80.0000)
  ==
++  test-make-nat  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvs`0x1.40a0.0000.4080.0000.4040.0000.4000.0000.3f80.0000
      !>  (make-nat:lvs (gulf 1 5))
  ==
++  test-iota  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvs`0x1.40a0.0000.4080.0000.4040.0000.4000.0000.3f80.0000
      !>  (iota:lvs 5)
  ==
++  test-isclose  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (isclose:lvs .1 .1.00001 .0.0001)
      !>  %.y
    %+  expect-eq
      !>  (isclose:lvs .1 .1.00001 .0.00001)
      !>  %.n
    ::  XX should probably test some pathological cases too
    %+  expect-eq
      !>  (isclose:lvs .1e-6 .0 .1e-6)
      !>  (near0:lvs .1e-6)
    %+  expect-eq
      !>  (isclose:lvs .1e-6 .0 .1e-7)
      !>  (near0:lvs .1e-7)
  ==
++  test-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  .1
      !>  (get:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4]) 1)
    %+  expect-eq
      !>  .2
      !>  (get:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4]) 2)
    %+  expect-eq
      !>  .3
      !>  (get:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4]) 3)
    %+  expect-eq
      !>  .4
      !>  (get:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4]) 4)
    %-  expect-fail
      |.  (get:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4]) 0)
  ==
++  test-max  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  .5
      !>  (max:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]))
    %+  expect-eq
      !>  .5
      !>  (max:lvs (make:lvs `(list @rs)`~[.1 .2 .5 .4 .3]))
    %+  expect-eq
      !>  .5
      !>  (max-rs:lvs .1 .5)
    %+  expect-eq
      !>  .-1
      !>  (max-rs:lvs .-1 .-5)
    %+  expect-eq
      !>  5
      !>  (argmax:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]))
    %+  expect-eq
      !>  3
      !>  (argmax:lvs (make:lvs `(list @rs)`~[.1 .2 .5 .4 .3]))
  ==
::
::  Tests for vector alteration
::
++  test-append  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.2 .3 .4 .5 .6])
      !>  (append:lvs (make:lvs `(list @rs)`~[.2 .3 .4 .5]) .6)
  ==
++  test-set  ^-  tang
  ;:  weld
  %+  expect-eq
    !>  (make:lvs `(list @rs)`~[.5 .2 .3 .4 .5])
    !>  (set:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) 1 .5)
  %+  expect-eq
    !>  (make:lvs `(list @rs)`~[.1 .2 .5 .4 .5])
    !>  (set:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) 3 .5)
  %+  expect-eq
    !>  (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5])
    !>  (set:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) 5 .5)
    ::  TODO XX test out-of-bounds
  ==
++  test-catenate  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.2 .3 .4 .5 .6 .7 .8 .9])
      !>  (catenate:lvs (make:lvs `(list @rs)`~[.2 .3 .4 .5]) (make:lvs `(list @rs)`~[.6 .7 .8 .9]))
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.2 .3 .4 .5 .6 .7 .8 .9])
      !>  (catenate:lvs (make:lvs `(list @rs)`~[.2 .3 .4 .5]) (make:lvs `(list @rs)`~[.6 .7 .8 .9]))
  ==
::
::  Tests for vector arithmetic
::
++  test-adds  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.2 .3 .4 .5 .6])
      !>  (adds:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .1)
      :: argh, we really XX need an expect-close in lib/test
      :: XX or switch to expect isclose instead
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.11 .12 .13 .14 .15])
      !>  (adds:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .10)
  ==
++  test-subs  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.0 .1 .2 .3 .4])
      !>  (subs:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .1)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.-9 .-8 .-7 .-6 .-5])
      !>  (subs:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .10)
  ==
++  test-muls  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5])
      !>  (muls:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .1)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.2 .4 .6 .8 .10])
      !>  (muls:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .2)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.-1 .-2 .-3 .-4 .-5])
      !>  (muls:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .-1)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5])
      !>  (muls:lvs (make:lvs `(list @rs)`~[.0.1 .0.2 .0.3 .0.4 .0.5]) .10)
  ==
++  test-divs  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5])
      !>  (divs:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .1)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.5e-1 .1 .1.5 .2 .2.5])
      !>  (divs:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .2)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.-1 .-2 .-3 .-4 .-5])
      !>  (divs:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .-1)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.0.1 .0.2 .0.3 .0.4 .0.5])
      !>  (divs:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .10)
  ==
++  test-addv  ^-  tang
  =/  vec00000  (zeros:lvs 5)
  =/  vec11111  (ones:lvs 5)
  =/  vec12345  (iota:lvs 5)
  =/  vec54321  (make:lvs `(list @rs)`~[.5 .4 .3 .2 .1])
  =/  vec21012  (make:lvs `(list @rs)`~[.-2 .-1 .0 .1 .2])
  ;:  weld
    %+  expect-eq
      !>  (iota:lvs 5)
      !>  (addv:lvs vec00000 vec12345)
    %+  expect-eq
      !>  (fill:lvs 5 .2)
      !>  (addv:lvs vec11111 vec11111)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.6 .5 .4 .3 .2])
      !>  (addv:lvs vec11111 vec54321)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.-1 .1 .3 .5 .7])
      !>  (addv:lvs vec12345 vec21012)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.2 .3 .4 .5 .6])
      !>  (addv:lvs vec11111 vec12345)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.-4 .-2 .0 .2 .4])
      !>  (addv:lvs vec21012 vec21012)
  ::  TODO XX test expected failures like diff sizes
  ==
++  test-subv  ^-  tang
  =/  vec00000  (zeros:lvs 5)
  =/  vec11111  (ones:lvs 5)
  =/  vec12345  (iota:lvs 5)
  =/  vec54321  (make:lvs `(list @rs)`~[.5 .4 .3 .2 .1])
  =/  vec21012  (make:lvs `(list @rs)`~[.-2 .-1 .0 .1 .2])
  ;:  weld
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.-1 .-2 .-3 .-4 .-5])
      !>  (subv:lvs vec00000 vec12345)
    %+  expect-eq
      !>  (zeros:lvs 5)
      !>  (subv:lvs vec11111 vec11111)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.4 .3 .2 .1 .0])
      !>  (subv:lvs vec54321 vec11111)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.3 .2 .1 .0 .-1])
      !>  (subv:lvs vec11111 vec21012)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.0 .-1 .-2 .-3 .-4])
      !>  (subv:lvs vec11111 vec12345)
    %+  expect-eq
      !>  (zeros:lvs 5)
      !>  (subv:lvs vec21012 vec21012)
  ::  TODO XX test expected failures like diff sizes
  ==
++  test-mulv  ^-  tang
  =/  vec00000  (zeros:lvs 5)
  =/  vec11111  (ones:lvs 5)
  =/  vec12345  (iota:lvs 5)
  =/  vec54321  (make:lvs `(list @rs)`~[.5 .4 .3 .2 .1])
  =/  vec21012  (make:lvs `(list @rs)`~[.-2 .-1 .0 .1 .2])
  ;:  weld
    %+  expect-eq
      !>  (zeros:lvs 5)
      !>  (mulv:lvs vec00000 vec12345)
    %+  expect-eq
      !>  (ones:lvs 5)
      !>  (mulv:lvs vec11111 vec11111)
    %+  expect-eq
      !>  vec54321
      !>  (mulv:lvs vec11111 vec54321)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.1 .4 .9 .16 .25])
      !>  (mulv:lvs vec12345 vec12345)
    %+  expect-eq
      !>  vec21012
      !>  (mulv:lvs vec11111 vec21012)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.4 .1 .0 .1 .4])
      !>  (mulv:lvs vec21012 vec21012)
  ::  TODO XX test expected failures like diff sizes
  ==
++  test-divv  ^-  tang
  =/  vec11111  (ones:lvs 5)
  =/  vec12345  (iota:lvs 5)
  =/  vec54321  (make:lvs `(list @rs)`~[.5 .4 .3 .2 .1])
  =/  vec21012  (make:lvs `(list @rs)`~[.-2 .-1 .0 .1 .2])
  ;:  weld
    %+  expect-near
      !>  (make:lvs `(list @rs)`~[.1 .0.5 (div:rs .1 .3) .0.25 .0.2])
      !>  (divv:lvs vec11111 vec12345)
    %+  expect-near
      !>  (ones:lvs 5)
      !>  (divv:lvs vec11111 vec11111)
  ::  TODO XX test expected failures like diff sizes and div-by-zero
  ==
++  test-sum  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  .15
      !>  (sum:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]))
  ==
++  test-product  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  .120
      !>  (product:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]))
  ==
++  test-inner  ^-  tang
  =/  vec00000  (zeros:lvs 5)
  =/  vec11111  (ones:lvs 5)
  =/  vec12345  (iota:lvs 5)
  =/  vec54321  (make:lvs `(list @rs)`~[.5 .4 .3 .2 .1])
  =/  vec21012  (make:lvs `(list @rs)`~[.-2 .-1 .0 .1 .2])
  ;:  weld
    %+  expect-eq
      !>  .0
      !>  (inner:lvs vec00000 vec11111)
    %+  expect-eq
      !>  .5
      !>  (inner:lvs vec11111 vec11111)
    %+  expect-eq
      !>  .15
      !>  (inner:lvs vec11111 vec12345)
    %+  expect-eq
      !>  .35
      !>  (inner:lvs vec54321 vec12345)
    %+  expect-eq
      !>  .0
      !>  (inner:lvs vec11111 vec21012)
    %+  expect-eq
      !>  .-10
      !>  (inner:lvs vec54321 vec21012)
    %+  expect-eq
      !>  .10
      !>  (inner:lvs vec21012 vec21012)
  ==
--
