::::  /tests/lib/lagoon-indexing -- structural indexing ops for /lib/lagoon.
::
::  Regression coverage for +submatrix (issue #8: a present upper bound of `0`
::  was read as "slice to the end").  A 4x4 magic array is row-major 0..15, so
::  entry [i j] = (4i + j); distinct values catch a transpose or an off-by-one
::  that a symmetric input would hide.  Slices are INCLUSIVE on both ends; an
::  absent bound `~` means "to the end".
::
/-  *lagoon
/+  *test
/+  *lagoon
|%
++  test-submatrix-2d  ^-  tang
  =/  magic-4x4  (magic:la [shape=~[4 4] bloq=4 kind=%uint prec=~])
  ;:  weld
    ::  issue #8: a present upper bound of `0` must not read as "to the end".
    ::  a[0:0, 2:2] is the single element [0 2] = 2, shape ~[1 1].
    %+  expect-eq
      !>((en-ray:la [~[1 1] 4 %uint ~] ~[~[0x2]]))
      !>((submatrix:la ~[`[`0 `0] `[`2 `2]] magic-4x4))
    ::  a[1:2, 2:3] is rows 1-2, cols 2-3 = [[6 7] [10 11]], shape ~[2 2].
    %+  expect-eq
      !>((en-ray:la [~[2 2] 4 %uint ~] ~[~[0x6 0x7] ~[0xa 0xb]]))
      !>((submatrix:la ~[`[`1 `2] `[`2 `3]] magic-4x4))
    ::  a[0:2, 3:3] is a non-square 3x1 column = [[3] [7] [11]]; a transpose
    ::  would give shape ~[1 3] or the wrong entries.
    %+  expect-eq
      !>((en-ray:la [~[3 1] 4 %uint ~] ~[~[0x3] ~[0x7] ~[0xb]]))
      !>((submatrix:la ~[`[`0 `2] `[`3 `3]] magic-4x4))
    ::  a[2:, :] slices rows 2-3 to the end and pads the omitted column dim,
    ::  = [[8 9 10 11] [12 13 14 15]], shape ~[2 4] (absent upper bound).
    %+  expect-eq
      !>((en-ray:la [~[2 4] 4 %uint ~] ~[~[0x8 0x9 0xa 0xb] ~[0xc 0xd 0xe 0xf]]))
      !>((submatrix:la ~[`[`2 ~]] magic-4x4))
    ::  a[:1, :] uses an absent lower bound = rows 0-1 = [[0..3] [4..7]].
    %+  expect-eq
      !>((en-ray:la [~[2 4] 4 %uint ~] ~[~[0x0 0x1 0x2 0x3] ~[0x4 0x5 0x6 0x7]]))
      !>((submatrix:la ~[`[~ `1]] magic-4x4))
  ==
--
