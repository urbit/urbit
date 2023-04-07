/+  *test
|%
++  le-path        `path`/~hastuc-dibtux/15/22/c/x/3/base/sys/kelvin
++  hastuc-dibtux  [~hastuc-dibtux 15 22]
++  clay-x         [%c %x %ud 3]
++  test-en-path
  %+  expect-eq
    !>(le-path)
  !>  %-  en-path:balk
  [hastuc-dibtux clay-x /base/sys/kelvin]
++  test-de-path
  =/  bal=balk
    (de-path:balk le-path)
  %+  expect-eq  !>(bal)
  !>   ^-   balk
  [hastuc-dibtux clay-x /base/sys/kelvin]
--
