/+  *test
|%
++  test-bits
  ;:  weld
    ::
    ::  Random sanity testing
    ::
    %+  expect-eq
      !>  ~[0x3 0x7 0x7]
      !>  (flop (ripn 3 0xff))
    %+  expect-eq
      !>  ~[0x1 0xee 0xff]
      !>  (flop (ripn 8 0x1.eeff))
    %+  expect-eq
      !>  ~[0x1 0xe 0xe 0xf 0xf]
      !>  (flop (ripn 4 0x1.eeff))
    ::
    :: Typical use-cases
    ::
    %+  expect-eq
      !>  ~[0x1 0x23.4567 0x89.abcd]
      !>  (flop (ripn 24 0x1.2345.6789.abcd))
    ::
    ::  Edge cases
    ::
    %+  expect-eq
      !>  ~
      !>  (flop (ripn 31 0x0))
    %+  expect-eq
      !>  ~
      !>  (flop (ripn 1 0x0))
    ::
    ::  Word boundaries
    ::
    %+  expect-eq
      !>  ~[0x7fff.ffff]
      !>  (flop (ripn 31 0x7fff.ffff))
    %+  expect-eq
      !>  ~[0x1 0x7fff.ffff]
      !>  (flop (ripn 31 0xffff.ffff))
    %+  expect-eq
      !>  ~[0x3 0x7fff.ffff]
      !>  (flop (ripn 31 0x1.ffff.ffff))
    %+  expect-eq
      !>  ~[0x3 0x7fff.ffff 0x7fff.ffff]
      !>  (flop (ripn 31 0xffff.ffff.ffff.ffff))
    %+  expect-eq
      !>  ~[0x1 0x1.ffff 0x1.ffff]
      !>  (flop (ripn 17 0x7.ffff.ffff))
    %+  expect-eq
      !>  ~[0x123 0x456 0x789 0xabc 0xdef 0x12 0x345 0x678]
      !>  (flop (ripn 12 0x1234.5678.9abc.def0.1234.5678))
  ==
::
--
