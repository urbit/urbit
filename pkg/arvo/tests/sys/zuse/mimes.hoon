::  tests for |mimes:html
::
/+  *test
=,  mimes:html
|%
++  test-en-base16
  ;:  weld
    %+  expect-eq
      !>  'aa'
      !>  (en:base16 (as-octs 0xaa))
  ::
    %+  expect-eq
      !>  '1234'
      !>  (en:base16 (as-octs 0x1234))
  ::
    %+  expect-eq
      !>  'f012'
      !>  (en:base16 (as-octs 0xf012))
  ::
    %+  expect-eq
      !>  '010b'
      !>  (en:base16 (as-octs 0x10b))
  ::
    %+  expect-eq
      !>  '001234'
      !>  (en:base16 3 0x1234)
  ::
    %+  expect-eq
      !>  '34'
      !>  (en:base16 1 0x1234)
  ==
::
++  test-de-base16
  ;:  weld
    %+  expect-eq
      !>  `[1 0xaa]
      !>  ^-  (unit [@ud @ux])
          (de:base16 'aa')
  ::
    %+  expect-eq
      !>  `[2 0x1234]
      !>  `(unit [@ud @ux])`(de:base16 '1234')
  ::
    %+  expect-eq
      !>  `[2 `@`0xf012]
      !>  ^-  (unit [@ud @ux])
          (de:base16 'f012')
  ::
    %+  expect-eq
      !>  lz=`[2 0x10b]
      !>  ^-  (unit [@ud @ux])
          (de:base16 '010b')
  ::
    %+  expect-eq
      !>  nlz=`[2 0x10b]
      !>  ^-  (unit [@ud @ux])
          (de:base16 '10b')
  ::
    %+  expect-eq
      !>  `[3 0x1234]
      !>  ^-  (unit [@ud @ux])
          (de:base16 '001234')
  ==
--
