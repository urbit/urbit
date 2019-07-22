::  tests for the ripemd-160 hashing algorithm
::
::  test vectors from here:
::  http://homes.esat.kuleuven.be/~bosselae/ripemd160.html
::
/+  *test
=,  ripemd:crypto
::
|%
::
::  test a list of [msg result] vectors
::
++  do-test-vectors
  |=  ves=(list [msg=@t out=@ux])
  ^-  tang
  ?~  ves  ~
  =-  (weld - $(ves t.ves))
  =*  msg  msg.i.ves
  =+  wid=(met 3 msg)
  %+  category
    ::  only first 100 chars, meme happens for super long values
    ::
    (trip (end 3 100 msg))
  %+  expect-eq
    !>  out.i.ves
    !>  `@ux`(ripemd-160 wid (rev 3 wid msg))
::
++  test-ripemd-160
  %-  do-test-vectors
  :~
    :-  ''
    0x9c11.85a5.c5e9.fc54.6128.0897.7ee8.f548.b225.8d31
  ::
    :-  'a'
    0xbdc.9d2d.256b.3ee9.daae.347b.e6f4.dc83.5a46.7ffe
  ::
    :-  'abc'
    0x8eb2.08f7.e05d.987a.9b04.4a8e.98c6.b087.f15a.0bfc
  ::
    :-  'message digest'
    0x5d06.89ef.49d2.fae5.72b8.81b1.23a8.5ffa.2159.5f36
  ::
    :-  'abcdefghijklmnopqrstuvwxyz'
    0xf71c.2710.9c69.2c1b.56bb.dceb.5b9d.2865.b370.8dbc
  ::
    :-  'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'
    0x12a0.5338.4a9c.0c88.e405.a06c.27dc.f49a.da62.eb2b
  ::
    :-  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'
    0xb0e2.0b6e.3116.6402.86ed.3a87.a571.3079.b21f.5189
  ::
    :-  '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
    0x9b75.2e45.573d.4b39.f4db.d332.3cab.82bf.6332.6bfb
  ::
    :-  (crip (reap 1.000.000 'a'))
    0x5278.3243.c169.7bdb.e16d.37f9.7f68.f083.25dc.1528
  ==
--
