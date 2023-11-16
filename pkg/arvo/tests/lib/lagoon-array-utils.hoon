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
  ?:  =(a b)  ~
  :~  [%palm [": " ~ ~ ~] [leaf+"expected" "{<a>}"]]
      [%palm [": " ~ ~ ~] [leaf+"actual  " "{<b>}"]]
  ==
::
++  is-close
  |=  [a=ray:la b=ray:la =term]  ^-  tang
  ?:  (all:la (is-close:la a b term [atol rtol]))  ~
  :~  [%palm [": " ~ ~ ~] [leaf+"expected" "{<a>}"]]
      [%palm [": " ~ ~ ~] [leaf+"actual  " "{<b>}"]]
  ==
::
::  Utilities
::

++  test-get-item-1d  ^-  tang
  =/  input-iota-1x8-3u  (iota:la [shape=~[8] bloq=3 kind=%uint prec=~])
  ;:  weld
    %+  expect-eq
      !>(0x0)
      !>((get-item:la input-iota-1x8-3u ~[0]))
    %+  expect-eq
      !>(0x1)
      !>((get-item:la input-iota-1x8-3u ~[1]))
    %+  expect-eq
      !>(0x7)
      !>((get-item:la input-iota-1x8-3u ~[7]))
    %-  expect-fail
      |.((get-item:la input-iota-1x8-3u ~[8]))
  ==

++  test-get-item-2d  ^-  tang
  =/  input-magic-4x4-4u  (magic:la [shape=~[4 4] bloq=4 kind=%uint prec=~])
  ;:  weld
    %+  expect-eq
      !>(0x0)
      !>((get-item:la input-magic-4x4-4u ~[0 0]))
    %+  expect-eq
      !>(0x4)
      !>((get-item:la input-magic-4x4-4u ~[1 0]))
    %+  expect-eq
      !>(0xa)
      !>((get-item:la input-magic-4x4-4u ~[2 2]))
    %-  expect-fail
      |.((get-item:la input-magic-4x4-4u ~[4 4]))
  ==

++  test-get-item-3d  ^-  tang
  =/  input-magic-4x4x4-5u  (magic:la [shape=~[4 4 4] bloq=5 kind=%uint prec=~])
  ;:  weld
    %+  expect-eq
      !>(0x0)
      !>((get-item:la input-magic-4x4x4-5u ~[0 0 0]))
    %+  expect-eq
      !>(0x10)
      !>((get-item:la input-magic-4x4x4-5u ~[1 0 0]))
    %+  expect-eq
      !>(0x2a)
      !>((get-item:la input-magic-4x4x4-5u ~[2 2 2]))
    %-  expect-fail
      |.((get-item:la input-magic-4x4x4-5u ~[4 4 4]))
  ==

++  test-set-item-1d  ^-  tang
  =/  input-meta  [shape=~[8] bloq=3 kind=%uint prec=~]
  =/  input-iota-1x8-3u  (iota:la input-meta)
  ;:  weld
    %+  expect-eq
      !>((en-ray:la input-meta ~[0xf 0x1 0x2 0x3 0x4 0x5 0x6 0x7]))
      !>((set-item:la input-iota-1x8-3u ~[0] 0xf))
    %+  expect-eq
      !>((en-ray:la input-meta ~[0x0 0x1 0x2 0x3 0x4 0x5 0x6 0xf]))
      !>((set-item:la input-iota-1x8-3u ~[7] 0xf))
    %+  expect-eq
      !>((en-ray:la input-meta ~[0x0 0x1 0x2 0xf 0x4 0x5 0x6 0x7]))
      !>((set-item:la input-iota-1x8-3u ~[3] 0xf))
    %-  expect-fail
      |.((set-item:la input-iota-1x8-3u ~[8] 0xf))
  ==

++  test-set-item-2d  ^-  tang
  =/  input-meta  [shape=~[3 3] bloq=4 kind=%uint prec=~]
  =/  input-magic-3x3-4u  (magic:la input-meta)
  ;:  weld
    %+  expect-eq
      !>((en-ray:la input-meta ~[~[0xf 0x1 0x2] ~[0x3 0x4 0x5] ~[0x6 0x7 0x8]]))
      !>((set-item:la input-magic-3x3-4u ~[0 0] 0xf))
    %+  expect-eq
      !>((en-ray:la input-meta ~[~[0x0 0x1 0x2] ~[0x3 0xf 0x5] ~[0x6 0x7 0x8]]))
      !>((set-item:la input-magic-3x3-4u ~[1 1] 0xf))
    %+  expect-eq
      !>((en-ray:la input-meta ~[~[0x0 0x1 0x2] ~[0x3 0x4 0x5] ~[0x6 0xf 0x8]]))
      !>((set-item:la input-magic-3x3-4u ~[2 1] 0xf))
    %-  expect-fail
      |.((set-item:la input-magic-3x3-4u ~[3 3] 0xf))
  ==

++  test-set-item-3d  ^-  tang
  =/  input-meta  [shape=~[2 2 2] bloq=4 kind=%uint prec=~]
  =/  input-magic-2x2x2-5u  (magic:la input-meta)
  ;:  weld
    %+  expect-eq
      !>((en-ray:la input-meta ~[~[~[0xf 0x1] ~[0x2 0x3]] ~[~[0x4 0x5] ~[0x6 0x7]]]))
      !>((set-item:la input-magic-2x2x2-5u ~[0 0 0] 0xf))
    %+  expect-eq
      !>((en-ray:la input-meta ~[~[~[0x0 0x1] ~[0x2 0x3]] ~[~[0x4 0x5] ~[0x6 0xf]]]))
      !>((set-item:la input-magic-2x2x2-5u ~[1 1 1] 0xf))
    %+  expect-eq
      !>((en-ray:la input-meta ~[~[~[0x0 0x1] ~[0xf 0x3]] ~[~[0x4 0x5] ~[0x6 0x7]]]))
      !>((set-item:la input-magic-2x2x2-5u ~[0 1 0] 0xf))
    %-  expect-fail
      |.((set-item:la input-magic-2x2x2-5u ~[3 2 1] 0xf))
  ==


--
:: to-tank
:: get-term
:: squeeze
:: submatrix
:: product
:: gather

:: get-item √
:: set-item √
:: get-row
:: set-row
:: get-col
:: set-col
:: get-bloq-offset
:: get-item-number
:: strides
:: get-dim
:: get-item-index
:: ravel

:: en-ray
:: de-ray
:: get-item-baum
:: fill
:: spac
:: unspac
:: scalar-to-ray
