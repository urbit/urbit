/+  *test
|%
++  hastuc-dibtux  [~hastuc-dibtux 15 22]
++  clay-x         [%c %x %ud 3]
++  test-en-path
  %+  expect-eq
    !>(`path`/cx/~hastuc-dibtux/base/3/sys/hoon/hoon)
  !>  %-  en-path:balk
  [hastuc-dibtux clay-x /base/sys/hoon/hoon]
++  test-de-path
  =/  bal=balk
    (de-path:balk 15 22 /cx/~hastuc-dibtux/base/3/sys/hoon/hoon)
  %+  expect-eq  !>(bal)
  !>   ^-   balk
  [hastuc-dibtux clay-x /base/sys/hoon/hoon]

--
