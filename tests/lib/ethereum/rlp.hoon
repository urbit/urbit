::NOTE  tests lightly modified from the examples here:
::      https://github.com/ethereum/wiki/wiki/RLP
::
/+  ethereum, *test
=,  rlp:ethereum
::
=/  vectors=(list [nom=tape dat=@ rlp=item])
  :~  :+  "string"
        0x83aa.bbcc
      b+3^0xaa.bbcc
    ::
      :+  "list"
        0xc8.83aa.bbcc.83dd.eeff
      l+~[b+3^0xaa.bbcc b+3^0xdd.eeff]
    ::
      :+  "empty list"
        0xc0
      l+~
    ::
      :+  "zero byte"
        0x0
      b+1^0x0
    ::
      :+  "empty zero"
        0x80
      b+0^0x0
    ::
      :+  "value 15"
        0xf
      b+1^0xf
    ::
      :+  "value 1024"
        0x82.0400
      b+2^0x400
    ::
      :+  "set of three"
        0xc7c0.c1c0.c3c0.c1c0
      l+[l+~ l+[l+~ ~] l+[l+~ l+[l+~ ~] ~] ~]
    ::
      :+  "list with zero byte"
        0xc311.0022
      l+~[b+1^0x11 b+1^0x0 b+1^0x22]
    ::
      :+  "list of zero bytes"
        0xc300.0000
      l+~[b+1^0x0 b+1^0x0 b+1^0x0]
  ==
::
|%
++  test-all-vectors
  |-  ^-  tang
  ?~  vectors  ~
  %+  weld  $(vectors t.vectors)
  =,  i.vectors
  %+  category  nom
  %+  weld
    %+  category  "encode"
    %+  expect-eq
      !>  `@ux`dat
      !>  `@ux`(encode rlp)
  %+  category  "decode"
  %+  expect-eq
    !>  rlp
    !>  (decode dat)
--
