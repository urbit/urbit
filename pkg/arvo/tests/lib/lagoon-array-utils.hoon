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

++  test-get-row-1d  ^-  tang
  =/  input-iota-1x8-3u  (iota:la [shape=~[8] bloq=3 kind=%uint prec=~])
  ;:  weld
    %+  expect-eq
      !>((en-ray:la [shape=~[1] bloq=3 kind=%uint prec=~] ~[0x0]))
      !>((get-row:la input-iota-1x8-3u ~[0]))
    %+  expect-eq
      !>((en-ray:la [shape=~[1] bloq=3 kind=%uint prec=~] ~[0x7]))
      !>((get-row:la input-iota-1x8-3u ~[7]))
    %-  expect-fail
      |.((get-row:la input-iota-1x8-3u ~[8]))
  ==

++  test-get-row-2d  ^-  tang
  =/  input-magic-3x3-4u  (magic:la [shape=~[3 3] bloq=4 kind=%uint prec=~])
  ;:  weld
    %+  expect-eq
      !>((en-ray:la [shape=~[1 3] bloq=4 kind=%uint prec=~] ~[~[0x0 0x1 0x2]]))
      !>((get-row:la input-magic-3x3-4u ~[0]))
    %+  expect-eq
      !>((en-ray:la [shape=~[1 3] bloq=4 kind=%uint prec=~] ~[~[0x6 0x7 0x8]]))
      !>((get-row:la input-magic-3x3-4u ~[2]))
    %-  expect-fail
      |.((get-row:la input-magic-3x3-4u ~[3]))
  ==

++  test-get-row-3d  ^-  tang
  =/  input-magic-3x3-4u  (magic:la [shape=~[3 3 3] bloq=4 kind=%uint prec=~])
  ;:  weld
    %+  expect-eq
      !>((en-ray:la [shape=~[1 3] bloq=4 kind=%uint prec=~] ~[~[0x0 0x1 0x2]]))
      !>((get-row:la input-magic-3x3-4u ~[0 0]))
    %-  expect-fail
      |.((get-row:la input-magic-3x3-4u ~[3 3]))
  ==

++  test-set-row-1d  ^-  tang
  =/  input-iota-1x8-3u  (iota:la [shape=~[8] bloq=3 kind=%uint prec=~])
  ;:  weld
    %+  expect-eq
      !>((en-ray:la [shape=~[8] bloq=3 kind=%uint prec=~] ~[0xf 0x1 0x2 0x3 0x4 0x5 0x6 0x7]))
      !>((set-row:la input-iota-1x8-3u ~[0] (en-ray:la [~[1] 3 %uint ~] ~[0xf])))
    %+  expect-eq
      !>((en-ray:la [shape=~[8] bloq=3 kind=%uint prec=~] ~[0x0 0x1 0x2 0x3 0x4 0x5 0x6 0xf]))
      !>((set-row:la input-iota-1x8-3u ~[7] (en-ray:la [~[1] 3 %uint ~] ~[0xf])))
    %-  expect-fail
      |.((set-row:la input-iota-1x8-3u ~[8] (en-ray:la [~[1] 3 %uint ~] ~[0xf])))
  ==

++  test-set-row-2d  ^-  tang
  =/  input-magic-3x3-4u  (magic:la [shape=~[3 3] bloq=4 kind=%uint prec=~])
  ;:  weld
    %+  expect-eq
      !>((en-ray:la [shape=~[3 3] bloq=4 kind=%uint prec=~] ~[~[0x0 0x1 0x2] ~[0x0 0x1 0x2] ~[0x6 0x7 0x8]]))
      !>((set-row:la input-magic-3x3-4u ~[1] (en-ray:la [~[1 3] 4 %uint ~] ~[~[0x0 0x1 0x2]])))
    %-  expect-fail
      |.((set-row:la input-magic-3x3-4u ~[3] (en-ray:la [~[1 3] 4 %uint ~] ~[~[0x0 0x1 0x2]])))
  ==

++  test-set-row-3d  ^-  tang
  =/  input-magic-3x3x3-4u  (magic:la [shape=~[3 3 3] bloq=5 kind=%uint prec=~])
  ;:  weld
    %+  expect-eq
      !>((en-ray:la [shape=~[3 3 3] bloq=5 kind=%uint prec=~] ~[~[~[0x0 0x1 0x2] ~[0x0 0x1 0x2] ~[0x6 0x7 0x8]] ~[~[0x9 0xa 0xb] ~[0xc 0xd 0xe] ~[0xf 0x10 0x11]] ~[~[0x12 0x13 0x14] ~[0x15 0x16 0x17] ~[0x18 0x19 0x1a]]]))
      !>((set-row:la input-magic-3x3x3-4u ~[0 1] (en-ray:la [~[1 3] 4 %uint ~] ~[~[0x0 0x1 0x2]])))
    %-  expect-fail
      |.((set-row:la input-magic-3x3x3-4u ~[3 3] (en-ray:la [~[1 3] 4 %uint ~] ~[~[0x0 0x1 0x2]])))
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
:: get-row √
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
