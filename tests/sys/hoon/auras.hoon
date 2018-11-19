/+  *test
|%
++  test-parse-q
  ;:  weld
    %+  expect-eq
      !>  .~zod
      !>  `@q`0x0
    ::
    %+  expect-eq
      !>  .~marbud
      !>  `@q`0x102
    ::
    %+  expect-eq
      !>  .~nec-marbud
      !>  `@q`0x1.0102
    ::
    %+  expect-eq
      !>  .~marnec-marnec-marnec-marnec-marbud
      !>  `@q`0x101.0101.0101.0101.0102
    ::
  ==
::
++  test-render-q
  ;:  weld
    %+  expect-eq
      !>  '.~zod'
      !>  (scot %q 0x0)
    ::
    %+  expect-eq
      !>  '.~marbud'
      !>  (scot %q 0x102)
    ::
    %+  expect-eq
      !>  '.~nec-marbud'
      !>  (scot %q 0x1.0102)
    ::
    %+  expect-eq
      !>  '.~marnec-marnec-marnec-marnec-marbud'
      !>  (scot %q 0x101.0101.0101.0101.0102)
    ::
  ==
--
