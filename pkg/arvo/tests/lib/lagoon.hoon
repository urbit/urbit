/+  *test
/+  *lagoon
::
::::
  ::
^|
|_  $:  atol=_.1e-3          :: absolute tolerance for precision of operations
        rtol=_.1e-5          :: relative tolerance for precision of operations
    ==
::  Auxiliary tools
++  is-equal
  |=  [a=ray:la b=ray:la]  ^-  tang
  ?:  ?&  =(kind.meta.a kind.meta.b)
          =(bloq.meta.a bloq.meta.b)
          =(shape.meta.a shape.meta.b)
          =(data.a data.b)
      ==
    ~
  :~  [%palm [": " ~ ~ ~] [leaf+"expected" "{<a>}"]]
      [%palm [": " ~ ~ ~] [leaf+"actual" "{<b>}"]]
  ==
::
::  Builders
::
++  test-eye  ^-  tang
  =/  small-test  [[2 2 ~] 3 %unsigned]
  =/  large-test  [[5 5 ~] 3 %unsigned]
  =/  eye-small   (eye:la 3 %unsigned 2)
  =/  eye-small-data  0x1.0100.0001
  =/  eye-large   (eye:la 3 %unsigned 5)
  =/  eye-large-data  0x101.0000.0000.0001.0000.0000.0001.0000.0000.0001.0000.0000.0001
  ;:  weld
    %+  is-equal
      eye-small
      [small-test eye-small-data]
    %+  is-equal
      eye-large
      [large-test eye-large-data]
  ==
::
++  test-zeros  ^-  tang
  =/  small-test  [[2 2 ~] 3 %unsigned]
  =/  large-test  [[2 2 2 2 ~] 3 %unsigned]
  =/  zeros-small   (zeros:la small-test)
  =/  zero-small-data  0x1.0000.0000
  =/  zeros-large   (zeros:la large-test)
  =/  zero-large-data  0x1.0000.0000.0000.0000.0000.0000.0000.0000
  ;:  weld
    %+  is-equal
      zeros-small
      [small-test zero-small-data]
    %+  is-equal
      zeros-large
      [large-test zero-large-data]
  ==
::
++  test-ones  ^-  tang
  =/  small-test  [[2 2 ~] 3 %unsigned]
  =/  large-test  [[2 2 2 2 ~] 3 %unsigned]
  =/  ones-small   (ones:la small-test)
  =/  one-small-data  0x1.0101.0101
  =/  ones-large   (ones:la large-test)
  =/  one-large-data  0x1.0101.0101.0101.0101.0101.0101.0101.0101
  ;:  weld
    %+  is-equal
      ones-small
      [small-test one-small-data]
    %+  is-equal
      ones-large
      [large-test one-large-data]
  ==
::
++  test-iota  ^-  tang
  =/  small-test  [[10 ~] 3 %unsigned]
  =/  large-test  [[40 ~] 3 %unsigned]
  =/  iota-small   (iota:la small-test 10)
  =/  iota-small-data  0x1.0a09.0807.0605.0403.0201
  =/  iota-large   (iota:la large-test 40)
  =/  iota-large-data  0x1.2827.2625.2423.2221.201f.1e1d.1c1b.1a19.1817.1615.1413.1211.100f.0e0d.0c0b.0a09.0807.0605.0403.0201
  ;:  weld
    %+  is-equal
      iota-small
      [small-test iota-small-data]
    %+  is-equal
      iota-large
      [large-test iota-large-data]
  ==
::
::  Operators
::
++  test-cumsum  ^-  tang
  =/  small-test    [[2 2 2 2 ~] 3 %unsigned]
  =/  iota-test     (iota:la small-test 10)
  =/  iota-res=@ux  `@ux`55
  =/  ones-test     (ones:la small-test)
  =/  ones-res=@ux  `@ux`16
  ;:  weld
    %+  expect-eq
      !>((cumsum:la iota-test))
      !>(iota-res)
    %+  expect-eq
      !>((cumsum:la ones-test))
      !>(ones-res)
  ==
::
++  test-prod  ^-  tang
  =/  small-test    [[2 2 2 2 ~] 5 %float]
  =/  iota-test     (iota:la small-test 10)
  =/  iota-res=@ux  `@ux`3.628.800
  =/  ones-test     (set-item:la (ones:la small-test) ~[1 1 1 1] 2)
  =/  ones-res=@ux  `@ux`2
  ;:  weld
    %+  expect-eq
      !>((prod:la iota-test))
      !>(iota-res)
    %+  expect-eq
      !>((prod:la ones-test))
      !>(ones-res)
  ==
::
++  test-matmul-2d  ^-  tang
  =/  =meta:la  [~[3 3] 5 %unsigned]
  =/  eye-test  (eye:la 5 %unsigned 3)
  =/  iota-test  (reshape:la (iota:la meta 9) ~[3 3])
  =/  iota-res  (en-ray:la `baum:la`[meta ~[~[30 36 42] ~[66 81 96] ~[102 126 150]]])
  =/  col-vec  (ones:la [~[3 1] 5 %unsigned])
  =/  col-res  (en-ray:la [[~[3 1] 5 %unsigned] ~[~[6] ~[15] ~[24]]]) 
  ;:  weld
    %+  expect-eq
      !>((matmul-2d:la iota-test iota-test))
      !>(iota-res)
    %+  expect-eq
      !>((matmul-2d:la iota-test eye-test))
      !>(iota-test)
    %+  expect-eq
      !>((matmul-2d:la iota-test col-vec))
      !>(col-res)
  ==
::
++  test-add  ^-  tang
  =/  iota-test  (iota:la [~[4] 5 %unsigned] 4)
  =/  iota-res  (en-ray:la [~[4] 5 %unsigned] ~[2 4 6 8]) 
  ;:  weld
  %+  expect-eq
    !>((add:la iota-test iota-test))
    !>(iota-res)
  ==
--
