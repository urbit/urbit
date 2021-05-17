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
++  expect-near-lvs
  |=  [expected=@lvs actual=@lvs]
  ^-  tang
  ?:  (all-close:lvs `@lvs`expected `@lvs`actual .1e-6)
    ~
  :~  [%palm [": " ~ ~ ~] [leaf+"expected" leaf+(pprint:lvs expected) ~]]
      [%palm [": " ~ ~ ~] [leaf+"actual" leaf+(pprint:lvs actual) ~]]
  ==
++  expect-near-lvd
  |=  [expected=@lvd actual=@lvd]
  ^-  tang
  ?:  (all-close:lvd `@lvd`expected `@lvd`actual .~1e-6)
    ~
  :~  [%palm [": " ~ ~ ~] [leaf+"expected" leaf+(pprint:lvd expected) ~]]
      [%palm [": " ~ ~ ~] [leaf+"actual" leaf+(pprint:lvd actual) ~]]
  ==
::
::  Tests for vector creation
::
++  test-lvs-zeros  ^-  tang
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
      !>  (make:lvs ~[.0 .0 .0])
      !>  (zeros:lvs 3)
    %+  expect-eq
      !>  `@lvs`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
      !>  (zeros:lvs 5)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`(reap 5 .0))
      !>  (zeros:lvs 5)
    %+  expect-eq
      !>  `@lvs`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
      !>  (zeros:lvs 16)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`(reap 16 .0))
      !>  (zeros:lvs 16)
  ==
++  test-lvs-ones  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvs`0x1
      !>  (ones:lvs 0)
    %+  expect-eq
      !>  `@lvs`0x1.3f80.0000
      !>  (ones:lvs 1)
    %+  expect-eq
      !>  (make:lvs ~[.1])
      !>  (ones:lvs 1)
    %+  expect-eq
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000
      !>  (ones:lvs 3)
    %+  expect-eq
      !>  (make:lvs ~[.1 .1 .1])
      !>  (ones:lvs 3)
    %+  expect-eq
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000
      !>  (ones:lvs 5)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`(reap 5 .1))
      !>  (ones:lvs 5)
    %+  expect-eq
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000
      !>  (ones:lvs 16)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`(reap 16 .1))
      !>  (ones:lvs 16)
  ==
++  test-lvs-fill  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvs`0x1
      !>  (fill:lvs 0 .0)
    %+  expect-eq
      !>  `@lvs`0x1
      !>  (fill:lvs 0 .1)
    %+  expect-eq
      !>  `@lvs`0x1.0000.0000
      !>  (fill:lvs 1 .0)
    %+  expect-eq
      !>  (make:lvs ~[.0])
      !>  (fill:lvs 1 .0)
    %+  expect-eq
      !>  `@lvs`0x1.3f80.0000
      !>  (fill:lvs 1 .1)
    %+  expect-eq
      !>  (make:lvs ~[.1])
      !>  (fill:lvs 1 .1)
    %+  expect-eq
      !>  `@lvs`0x1.bf80.0000
      !>  (fill:lvs 1 .-1)
    %+  expect-eq
      !>  (make:lvs ~[.-1])
      !>  (fill:lvs 1 .-1)
    %+  expect-eq
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000
      !>  (fill:lvs 3 .1)
    %+  expect-eq
      !>  (make:lvs ~[.1 .1 .1])
      !>  (fill:lvs 3 .1)
    %+  expect-eq
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000
      !>  (fill:lvs 5 .1)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`(reap 5 .1))
      !>  (fill:lvs 5 .1)
  ==
++  test-lvs-make  ^-  tang
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
      !>  `@lvs`0x1.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000.3f80.0000
      !>  (make:lvs `(list @rs)`(reap 16 .1))
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
++  test-lvs-unmake  ^-  tang
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
++  test-lvs-make-unmake  ^-  tang
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
++  test-lvs-length  ^-  tang
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
++  test-lvs-make-nat  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvs`0x1.40a0.0000.4080.0000.4040.0000.4000.0000.3f80.0000
      !>  (make-nat:lvs (gulf 1 5))
  ==
++  test-lvs-iota  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvs`0x1.40a0.0000.4080.0000.4040.0000.4000.0000.3f80.0000
      !>  (iota:lvs 5)
  ==
++  test-lvs-isclose  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  %.y
      !>  (isclose:lvs .1 .1.00001 .0.0001)
    %+  expect-eq
      !>  %.n
      !>  (isclose:lvs .1 .1.00001 .0.00001)
    ::  XX should probably test some pathological cases too
    %+  expect-eq
      !>  (isclose:lvs .1e-6 .0 .1e-6)
      !>  (near0:lvs .1e-6)
    %+  expect-eq
      !>  (isclose:lvs .1e-7 .0 .1e-6)
      !>  (near0:lvs .1e-7)
  ==
++  test-lvs-get  ^-  tang
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
++  test-lvs-max  ^-  tang
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
++  test-lvs-append  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.2 .3 .4 .5 .6])
      !>  (append:lvs (make:lvs `(list @rs)`~[.2 .3 .4 .5]) .6)
  ==
++  test-lvs-set  ^-  tang
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
++  test-lvs-catenate  ^-  tang
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
++  test-lvs-adds  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.2 .3 .4 .5 .6])
      !>  (adds:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .1)
      :: XX switch to expect-near-lvs instead
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.11 .12 .13 .14 .15])
      !>  (adds:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .10)
  ==
++  test-lvs-subs  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.0 .1 .2 .3 .4])
      !>  (subs:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .1)
    %+  expect-eq
      !>  (make:lvs `(list @rs)`~[.-9 .-8 .-7 .-6 .-5])
      !>  (subs:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .10)
  ==
++  test-lvs-muls  ^-  tang
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
++  test-lvs-divs  ^-  tang
  ;:  weld
    %+  expect-near-lvs
      (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5])
      (divs:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .1)
    %+  expect-near-lvs
      (make:lvs `(list @rs)`~[.5e-1 .1 .1.5 .2 .2.5])
      (divs:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .2)
    %+  expect-near-lvs
      (make:lvs `(list @rs)`~[.-1 .-2 .-3 .-4 .-5])
      (divs:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .-1)
    %+  expect-near-lvs
      (make:lvs `(list @rs)`~[.0.1 .0.2 .0.3 .0.4 .0.5])
      (divs:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]) .10)
  ==
++  test-lvs-addv  ^-  tang
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
++  test-lvs-subv  ^-  tang
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
++  test-lvs-mulv  ^-  tang
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
++  test-lvs-divv  ^-  tang
  =/  vec11111  (ones:lvs 5)
  =/  vec12345  (iota:lvs 5)
  =/  vec54321  (make:lvs `(list @rs)`~[.5 .4 .3 .2 .1])
  =/  vec21012  (make:lvs `(list @rs)`~[.-2 .-1 .0 .1 .2])
  ;:  weld
    %+  expect-near-lvs
      (make:lvs `(list @rs)`~[.1 .0.5 (div:rs .1 .3) .0.25 .0.2])
      (divv:lvs vec11111 vec12345)
    %+  expect-near-lvs
      (ones:lvs 5)
      (divv:lvs vec11111 vec11111)
  ::  TODO XX test expected failures like diff sizes and div-by-zero
  ==
++  test-lvs-sum  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  .15
      !>  (sum:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]))
  ==
++  test-lvs-product  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  .120
      !>  (product:lvs (make:lvs `(list @rs)`~[.1 .2 .3 .4 .5]))
  ==
++  test-lvs-inner  ^-  tang
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
::
::  Tests for vector creation
::
++  test-lvd-zeros  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvd`0x1
      !>  (zeros:lvd 0)
    %+  expect-eq
      !>  `@lvd`0x1.0000.0000.0000.0000
      !>  (zeros:lvd 1)
    %+  expect-eq
      !>  (zeros:lvd 1)
      !>  (make:lvd ~[.~0])
    %+  expect-eq
      !>  `@lvd`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
      !>  (zeros:lvd 3)
    %+  expect-eq
      !>  (zeros:lvd 3)
      !>  (make:lvd ~[.~0 .~0 .~0])
    %+  expect-eq
      !>  `@lvd`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
      !>  (zeros:lvd 5)
    %+  expect-eq
      !>  (zeros:lvd 5)
      !>  (make:lvd `(list @rd)`(reap 5 .~0))
    %+  expect-eq
      !>  (zeros:lvd 16)
      !>  `@lvd`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
    %+  expect-eq
      !>  (zeros:lvd 16)
      !>  (make:lvd `(list @rd)`(reap 16 .~0))
  ==
++  test-lvd-ones  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (ones:lvd 0)
      !>  `@lvd`0x1
    %+  expect-eq
      !>  (ones:lvd 1)
      !>  `@lvd`0x1.3ff0.0000.0000.0000
    %+  expect-eq
      !>  (ones:lvd 1)
      !>  (make:lvd ~[.~1])
    %+  expect-eq
      !>  (ones:lvd 3)
      !>  `@lvd`0x1.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000
    %+  expect-eq
      !>  (ones:lvd 3)
      !>  (make:lvd ~[.~1 .~1 .~1])
    %+  expect-eq
      !>  (ones:lvd 5)
      !>  `@lvd`0x1.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000
    %+  expect-eq
      !>  (ones:lvd 5)
      !>  (make:lvd `(list @rd)`(reap 5 .~1))
    %+  expect-eq
      !>  (ones:lvd 16)
      !>  `@lvd`0x1.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000
    %+  expect-eq
      !>  (ones:lvd 16)
      !>  (make:lvd `(list @rd)`(reap 16 .~1))
  ==
++  test-lvd-fill  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvd`0x1
      !>  (fill:lvd 0 .~0)
    %+  expect-eq
      !>  `@lvd`0x1
      !>  (fill:lvd 0 .~1)
    %+  expect-eq
      !>  (fill:lvd 1 .~0)
      !>  `@lvd`0x1.0000.0000.0000.0000
    %+  expect-eq
      !>  (fill:lvd 1 .~0)
      !>  (make:lvd ~[.~0])
    %+  expect-eq
      !>  (fill:lvd 1 .~1)
      !>  `@lvd`0x1.3ff0.0000.0000.0000
    %+  expect-eq
      !>  (fill:lvd 1 .~1)
      !>  (make:lvd ~[.~1])
    %+  expect-eq
      !>  (fill:lvd 1 .~-1)
      !>  `@lvd`0x1.bff0.0000.0000.0000
    %+  expect-eq
      !>  (fill:lvd 1 .~-1)
      !>  (make:lvd ~[.~-1])
    %+  expect-eq
      !>  (fill:lvd 3 .~1)
      !>  `@lvd`0x1.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000
    %+  expect-eq
      !>  (fill:lvd 3 .~1)
      !>  (make:lvd ~[.~1 .~1 .~1])
    %+  expect-eq
      !>  (fill:lvd 5 .~1)
      !>  `@lvd`0x1.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000
    %+  expect-eq
      !>  (fill:lvd 5 .~1)
      !>  (make:lvd `(list @rd)`(reap 5 .~1))
  ==
++  test-lvd-make  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvd`0x1.0000.0000.0000.0000
      !>  (make:lvd ~[.~0])
    %+  expect-eq
      !>  `@lvd`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
      !>  (make:lvd ~[.~0 .~0 .~0])
    %+  expect-eq
      !>  `@lvd`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
      !>  (make:lvd `(list @rd)`(reap 5 .~0))
    %+  expect-eq
      !>  `@lvd`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
      !>  (make:lvd `(list @rd)`(reap 16 .~0))
    %+  expect-eq
      !>  `@lvd`0x1.3ff0.0000.0000.0000
      !>  (make:lvd ~[.~1])
    %+  expect-eq
      !>  `@lvd`0x1.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000
      !>  (make:lvd ~[.~1 .~1 .~1])
    %+  expect-eq
      !>  `@lvd`0x1.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000
      !>  (make:lvd `(list @rd)`(reap 5 .~1))
    %+  expect-eq
      !>  (make:lvd `(list @rd)`(reap 16 .~1))
      !>  `@lvd`0x1.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000
    %+  expect-eq
      !>  `@lvd`0x1.4000.0000.0000.0000.3ff0.0000.0000.0000
      !>  (make:lvd ~[.~1 .~2])
    %+  expect-eq
      !>  `@lvd`0x1.4008.0000.0000.0000.4000.0000.0000.0000.3ff0.0000.0000.0000
      !>  (make:lvd ~[.~1 .~2 .~3])
    %+  expect-eq
      !>  `@lvd`0x1.4010.0000.0000.0000.4008.0000.0000.0000.4000.0000.0000.0000.3ff0.0000.0000.0000.0000.0000.0000.0000.bff0.0000.0000.0000.c000.0000.0000.0000.c008.0000.0000.0000
      !>  (make:lvd ~[.~-3 .~-2 .~-1 .~0 .~1 .~2 .~3 .~4])
  ==
++  test-lvd-unmake  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `(list @rd)`~[.~0]
      !>  (unmake:lvd `@lvd`0x1.0000.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rd)`~[.~0 .~0 .~0]
      !>  (unmake:lvd `@lvd`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rd)`(reap 5 .~0)
      !>  (unmake:lvd `@lvd`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rd)`(reap 16 .~0)
      !>  (unmake:lvd `@lvd`0x1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rd)`~[.~1]
      !>  (unmake:lvd `@lvd`0x1.3ff0.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rd)`~[.~1 .~1 .~1]
      !>  (unmake:lvd `@lvd`0x1.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rd)`(reap 5 .~1)
      !>  (unmake:lvd `@lvd`0x1.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rd)`(reap 16 .~1)
      !>  (unmake:lvd `@lvd`0x1.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000.3ff0.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rd)`~[.~1 .~2]
      !>  (unmake:lvd `@lvd`0x1.4000.0000.0000.0000.3ff0.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rd)`~[.~1 .~2 .~3]
      !>  (unmake:lvd `@lvd`0x1.4008.0000.0000.0000.4000.0000.0000.0000.3ff0.0000.0000.0000)
    %+  expect-eq
      !>  `(list @rd)`~[.~-3 .~-2 .~-1 .~0 .~1 .~2 .~3 .~4]
      !>  (unmake:lvd `@lvd`0x1.4010.0000.0000.0000.4008.0000.0000.0000.4000.0000.0000.0000.3ff0.0000.0000.0000.0000.0000.0000.0000.bff0.0000.0000.0000.c000.0000.0000.0000.c008.0000.0000.0000)
  ==
++  test-lvd-make-unmake  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]
      !>  (unmake:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]))
    %+  expect-eq
      !>  `@lvd`0x1.4014.0000.0000.0000.4010.0000.0000.0000.4008.0000.0000.0000.4000.0000.0000.0000.3ff0.0000.0000.0000
      !>  (make:lvd (unmake:lvd `@lvd`0x1.4014.0000.0000.0000.4010.0000.0000.0000.4008.0000.0000.0000.4000.0000.0000.0000.3ff0.0000.0000.0000))
  ==
::
::  Tests for utility functions
::
++  test-lvd-length  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  0
      !>  (length:lvd `@lvd`0x1)
    %+  expect-eq
      !>  1
      !>  (length:lvd `@lvd`0x1.3ff0.0000.0000.0000)
    %+  expect-eq
      !>  5
      !>  (length:lvd `@lvd`0x1.4014.0000.0000.0000.4010.0000.0000.0000.4008.0000.0000.0000.4000.0000.0000.0000.3ff0.0000.0000.0000)
  ==
++  test-lvd-make-nat  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvd`0x1.4014.0000.0000.0000.4010.0000.0000.0000.4008.0000.0000.0000.4000.0000.0000.0000.3ff0.0000.0000.0000
      !>  (make-nat:lvd (gulf 1 5))
  ==
++  test-lvd-iota  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `@lvd`0x1.4014.0000.0000.0000.4010.0000.0000.0000.4008.0000.0000.0000.4000.0000.0000.0000.3ff0.0000.0000.0000
      !>  (iota:lvd 5)
  ==
++  test-lvd-isclose  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  %.y
      !>  (isclose:lvd .~1 .~1.00001 .~0.0001)
    %+  expect-eq
      !>  %.n
      !>  (isclose:lvd .~1 .~1.00001 .~0.00001)
    ::  XX should probably test some pathological cases too
    %+  expect-eq
      !>  (isclose:lvd .~1e-6 .~0 .~1e-6)
      !>  (near0:lvd .~1e-6)
    %+  expect-eq
      !>  (isclose:lvd .~1e-7 .~0 .~1e-6)
      !>  (near0:lvd .~1e-7)
  ==
++  test-lvd-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  .~1
      !>  (get:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4]) 1)
    %+  expect-eq
      !>  .~2
      !>  (get:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4]) 2)
    %+  expect-eq
      !>  .~3
      !>  (get:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4]) 3)
    %+  expect-eq
      !>  .~4
      !>  (get:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4]) 4)
    %-  expect-fail
      |.  (get:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4]) 0)
  ==
++  test-lvd-max  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  .~5
      !>  (max:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]))
    %+  expect-eq
      !>  .~5
      !>  (max:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~5 .~4 .~3]))
    %+  expect-eq
      !>  .~5
      !>  (max-rd:lvd .~1 .~5)
    %+  expect-eq
      !>  .~-1
      !>  (max-rd:lvd .~-1 .~-5)
    %+  expect-eq
      !>  5
      !>  (argmax:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]))
    %+  expect-eq
      !>  3
      !>  (argmax:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~5 .~4 .~3]))
  ==
::
::  Tests for vector alteration
::
++  test-lvd-append  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~2 .~3 .~4 .~5 .~6])
      !>  (append:lvd (make:lvd `(list @rd)`~[.~2 .~3 .~4 .~5]) .~6)
  ==
++  test-lvd-set  ^-  tang
  ;:  weld
  %+  expect-eq
    !>  (make:lvd `(list @rd)`~[.~5 .~2 .~3 .~4 .~5])
    !>  (set:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) 1 .~5)
  %+  expect-eq
    !>  (make:lvd `(list @rd)`~[.~1 .~2 .~5 .~4 .~5])
    !>  (set:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) 3 .~5)
  %+  expect-eq
    !>  (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5])
    !>  (set:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) 5 .~5)
    ::  TODO XX test out-of-bounds
  ==
++  test-lvd-catenate  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~2 .~3 .~4 .~5 .~6 .~7 .~8 .~9])
      !>  (catenate:lvd (make:lvd `(list @rd)`~[.~2 .~3 .~4 .~5]) (make:lvd `(list @rd)`~[.~6 .~7 .~8 .~9]))
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~2 .~3 .~4 .~5 .~6 .~7 .~8 .~9])
      !>  (catenate:lvd (make:lvd `(list @rd)`~[.~2 .~3 .~4 .~5]) (make:lvd `(list @rd)`~[.~6 .~7 .~8 .~9]))
  ==
::
::  Tests for vector arithmetic
::
++  test-lvd-adds  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~2 .~3 .~4 .~5 .~6])
      !>  (adds:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) .~1)
      :: XX switch to expect-near-lvd instead
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~11 .~12 .~13 .~14 .~15])
      !>  (adds:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) .~10)
  ==
++  test-lvd-subs  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~0 .~1 .~2 .~3 .~4])
      !>  (subs:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) .~1)
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~-9 .~-8 .~-7 .~-6 .~-5])
      !>  (subs:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) .~10)
  ==
++  test-lvd-muls  ^-  tang
  ;:  weld
    %+  expect-near-lvd
      (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5])
      (muls:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) .~1)
    %+  expect-near-lvd
      (make:lvd `(list @rd)`~[.~2 .~4 .~6 .~8 .~10])
      (muls:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) .~2)
    %+  expect-near-lvd
      (make:lvd `(list @rd)`~[.~-1 .~-2 .~-3 .~-4 .~-5])
      (muls:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) .~-1)
    %+  expect-near-lvd
      (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5])
      (muls:lvd (make:lvd `(list @rd)`~[.~0.1 .~0.2 .~0.3 .~0.4 .~0.5]) .~10)
  ==
++  test-lvd-divs  ^-  tang
  ;:  weld
    %+  expect-near-lvd
      (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5])
      (divs:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) .~1)
    %+  expect-near-lvd
      (make:lvd `(list @rd)`~[.~5e-1 .~1 .~1.5 .~2 .~2.5])
      (divs:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) .~2)
    %+  expect-near-lvd
      (make:lvd `(list @rd)`~[.~-1 .~-2 .~-3 .~-4 .~-5])
      (divs:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) .~-1)
    %+  expect-near-lvd
      (make:lvd `(list @rd)`~[.~0.1 .~0.2 .~0.3 .~0.4 .~0.5])
      (divs:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]) .~10)
  ==
++  test-lvd-addv  ^-  tang
  =/  vec00000  (zeros:lvd 5)
  =/  vec11111  (ones:lvd 5)
  =/  vec12345  (iota:lvd 5)
  =/  vec54321  (make:lvd `(list @rd)`~[.~5 .~4 .~3 .~2 .~1])
  =/  vec21012  (make:lvd `(list @rd)`~[.~-2 .~-1 .~0 .~1 .~2])
  ;:  weld
    %+  expect-eq
      !>  (iota:lvd 5)
      !>  (addv:lvd vec00000 vec12345)
    %+  expect-eq
      !>  (fill:lvd 5 .~2)
      !>  (addv:lvd vec11111 vec11111)
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~6 .~5 .~4 .~3 .~2])
      !>  (addv:lvd vec11111 vec54321)
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~-1 .~1 .~3 .~5 .~7])
      !>  (addv:lvd vec12345 vec21012)
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~2 .~3 .~4 .~5 .~6])
      !>  (addv:lvd vec11111 vec12345)
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~-4 .~-2 .~0 .~2 .~4])
      !>  (addv:lvd vec21012 vec21012)
  ::  TODO XX test expected failures like diff sizes
  ==
++  test-lvd-subv  ^-  tang
  =/  vec00000  (zeros:lvd 5)
  =/  vec11111  (ones:lvd 5)
  =/  vec12345  (iota:lvd 5)
  =/  vec54321  (make:lvd `(list @rd)`~[.~5 .~4 .~3 .~2 .~1])
  =/  vec21012  (make:lvd `(list @rd)`~[.~-2 .~-1 .~0 .~1 .~2])
  ;:  weld
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~-1 .~-2 .~-3 .~-4 .~-5])
      !>  (subv:lvd vec00000 vec12345)
    %+  expect-eq
      !>  (zeros:lvd 5)
      !>  (subv:lvd vec11111 vec11111)
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~4 .~3 .~2 .~1 .~0])
      !>  (subv:lvd vec54321 vec11111)
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~3 .~2 .~1 .~0 .~-1])
      !>  (subv:lvd vec11111 vec21012)
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~0 .~-1 .~-2 .~-3 .~-4])
      !>  (subv:lvd vec11111 vec12345)
    %+  expect-eq
      !>  (zeros:lvd 5)
      !>  (subv:lvd vec21012 vec21012)
  ::  TODO XX test expected failures like diff sizes
  ==
++  test-lvd-mulv  ^-  tang
  =/  vec00000  (zeros:lvd 5)
  =/  vec11111  (ones:lvd 5)
  =/  vec12345  (iota:lvd 5)
  =/  vec54321  (make:lvd `(list @rd)`~[.~5 .~4 .~3 .~2 .~1])
  =/  vec21012  (make:lvd `(list @rd)`~[.~-2 .~-1 .~0 .~1 .~2])
  ;:  weld
    %+  expect-eq
      !>  (zeros:lvd 5)
      !>  (mulv:lvd vec00000 vec12345)
    %+  expect-eq
      !>  (ones:lvd 5)
      !>  (mulv:lvd vec11111 vec11111)
    %+  expect-eq
      !>  vec54321
      !>  (mulv:lvd vec11111 vec54321)
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~1 .~4 .~9 .~16 .~25])
      !>  (mulv:lvd vec12345 vec12345)
    %+  expect-eq
      !>  vec21012
      !>  (mulv:lvd vec11111 vec21012)
    %+  expect-eq
      !>  (make:lvd `(list @rd)`~[.~4 .~1 .~0 .~1 .~4])
      !>  (mulv:lvd vec21012 vec21012)
  ::  TODO XX test expected failures like diff sizes
  ==
++  test-lvd-divv  ^-  tang
  =/  vec11111  (ones:lvd 5)
  =/  vec12345  (iota:lvd 5)
  =/  vec54321  (make:lvd `(list @rd)`~[.~5 .~4 .~3 .~2 .~1])
  =/  vec21012  (make:lvd `(list @rd)`~[.~-2 .~-1 .~0 .~1 .~2])
  ;:  weld
    %+  expect-near-lvd
      (make:lvd `(list @rd)`~[.~1 .~0.5 (div:rd .~1 .~3) .~0.25 .~0.2])
      (divv:lvd vec11111 vec12345)
    %+  expect-near-lvd
      (ones:lvd 5)
      (divv:lvd vec11111 vec11111)
  ::  TODO XX test expected failures like diff sizes and div-by-zero
  ==
++  test-lvd-sum  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  .~15
      !>  (sum:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]))
  ==
++  test-lvd-product  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  .~120
      !>  (product:lvd (make:lvd `(list @rd)`~[.~1 .~2 .~3 .~4 .~5]))
  ==
++  test-lvd-inner  ^-  tang
  =/  vec00000  (zeros:lvd 5)
  =/  vec11111  (ones:lvd 5)
  =/  vec12345  (iota:lvd 5)
  =/  vec54321  (make:lvd `(list @rd)`~[.~5 .~4 .~3 .~2 .~1])
  =/  vec21012  (make:lvd `(list @rd)`~[.~-2 .~-1 .~0 .~1 .~2])
  ;:  weld
    %+  expect-eq
      !>  .~0
      !>  (inner:lvd vec00000 vec11111)
    %+  expect-eq
      !>  .~5
      !>  (inner:lvd vec11111 vec11111)
    %+  expect-eq
      !>  .~15
      !>  (inner:lvd vec11111 vec12345)
    %+  expect-eq
      !>  .~35
      !>  (inner:lvd vec54321 vec12345)
    %+  expect-eq
      !>  .~0
      !>  (inner:lvd vec11111 vec21012)
    %+  expect-eq
      !>  .~-10
      !>  (inner:lvd vec54321 vec21012)
    %+  expect-eq
      !>  .~10
      !>  (inner:lvd vec21012 vec21012)
  ==
--
