/+  *test
|%
::NOTE  tests lightly modified from the examples here:
::      https://github.com/ethereum/wiki/wiki/RLP
::
++  test-encoding-string
  %+  expect-eq
    !>  %-  encode:rlp:ethereum
        b+3^0xaa.bbcc
    !>  0x83aa.bbcc
::
++  test-encoding-list
  %+  expect-eq
    !>  %-  encode:rlp:ethereum
        l+~[b+3^0xaa.bbcc b+3^0xdd.eeff]
    !>  0xc8.83aa.bbcc.83dd.eeff
::
++  test-encoding-empty-list
  %+  expect-eq
    !>  %-  encode:rlp:ethereum
        l+~
    !>  0xc0
::
++  test-encoding-zero-byte
  %+  expect-eq
    !>  %-  encode:rlp:ethereum
        b+1^0x0
    !>  0x0
::
++  test-encoding-empty-zero
  %+  expect-eq
    !>  %-  encode:rlp:ethereum
        b+0^0x0
    !>  0x80
::
++  test-encoding-15
  %+  expect-eq
    !>  %-  encode:rlp:ethereum
        b+1^15
    !>  0xf
::
++  test-encoding-1024
  %+  expect-eq
    !>  %-  encode:rlp:ethereum
        b+2^1.024
    !>  0x82.0400
::
++  test-encoding-set-of-three
  %+  expect-eq
    !>  %-  encode:rlp:ethereum
        l+[l+~ l+[l+~ ~] l+[l+~ l+[l+~ ~] ~] ~]
    !>  0xc7c0.c1c0.c3c0.c1c0
--
