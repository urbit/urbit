/+  *test
|%
++  test-muk
  ;:  weld
    %+  expect-eq
      !>  0xfaf6.cdb3
      !>  (muk 1.234 13 'Hello, world!')
    ::
    %+  expect-eq
      !>  0xbf50.5788
      !>  (muk 4.321 13 'Hello, world!')
    ::
    %+  expect-eq
      !>  0xf2c.c00b
      !>  (muk 1.234 0 0)
    ::
    %+  expect-eq
      !>  0x8905.ac28
      !>  (muk 1.234 28 (crip (reap 28 'x')))
    ::
    %+  expect-eq
      !>  0x566f.7173
      !>  (muk 0xcafe.babe 16 (dec (bex 128)))
  ==
::
++  test-mur
  ;:  weld
    %+  expect-eq
      !>  0x4d44.1035
      !>  (mur 'Hello, world!')
    ::
    %+  expect-eq
      !>  0x79ff.04e8
      !>  (mur 0)
    ::
    %+  expect-eq
      !>  0x64df.da5c
      !>  (mur (crip (reap 28 'x')))
    ::
    %+  expect-eq
      !>  0x389c.a03a
      !>  (mur [0 0])
    ::
    %+  expect-eq
      !>  0x389c.a03a
      !>  (mur [1 1])
    ::
    %+  expect-eq
      !>  0x5258.a6c0
      !>  (mur [0 (bex 32)])
    ::
    %+  expect-eq
      !>  0x2ad3.9968
      !>  (mur [(dec (bex 128)) 1])
  ==
--
