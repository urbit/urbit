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
  ?:  ?&  =(aura.meta.a aura.meta.b)
          =(bloq.meta.a bloq.meta.b)
          =(shape.meta.a shape.meta.b)
          =(data.a data.b)
      ==
    ~
  :~  [%palm [": " ~ ~ ~] [leaf+"expected" "{<a>}"]]
      [%palm [": " ~ ~ ~] [leaf+"actual" "{<b>}"]]
  ==
::
::  Creation
::++  test-isclose  !!
::++  test-allclose  !!
++  test-eye  ^-  tang
  =/  small-test  [[2 2 ~] 3 %ud]
  =/  large-test  [[5 5 ~] 3 %ud]
  =/  eye-small   (eye:la 3 %ud 2)
  =/  eye-small-data  0x1.0100.0001
  =/  eye-large   (eye:la 3 %ud 5)
  =/  eye-large-data  0x101.0000.0000.0001.0000.0000.0001.0000.0000.0001.0000.0000.0001
  ;:  weld
    %+  is-equal
      eye-small
      [small-test eye-small-data]
    %+  is-equal
      eye-large
      [large-test eye-large-data]
  ==
++  test-zeros  ^-  tang
  =/  small-test  [[2 2 ~] 3 %ud]
  =/  large-test  [[2 2 2 2 ~] 3 %ud]
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
++  test-ones  ^-  tang
  =/  small-test  [[2 2 ~] 3 %ud]
  =/  large-test  [[2 2 2 2 ~] 3 %ud]
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
--
