::  tests for the aes block cipher
::
::  test vectors from Appendix C of:
::  https://csrc.nist.gov/publications/fips/fips197/fips-197.pdf
::
/+  *test
=,  aes:crypto
::
|%
::
+$  vector-ecb  [key=@ux in=@ux out=@ux]
::
++  do-test-vectors-ecb
  |*  [ecbx=_ecba ves=(list vector-ecb)]
  ^-  tang
  |^  %+  weld
        %+  category  "encrypting"
        (zing (turn ves enc))
      %+  category  "decrypting"
      (zing (turn ves dec))
  ::
  ++  enc
    |=  vector-ecb
    %+  expect-eq
      !>  out
      !>  `@ux`(~(en ecbx key) in)
  ::
  ++  dec
    |=  vector-ecb
    %+  expect-eq
      !>  in
      !>  `@ux`(~(de ecbx key) out)
  --
::
++  test-aes-ecba
  %+  do-test-vectors-ecb  ecba
  :~
    :+  0x0
      0x0
    0x66e9.4bd4.ef8a.2c3b.884c.fa59.ca34.2b2e
  ::
    :+  0x0
      0x1
    0x58e2.fcce.fa7e.3061.367f.1d57.a4e7.455a
  ::
    :+  0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
      0x11.2233.4455.6677.8899.aabb.ccdd.eeff
    0x69c4.e0d8.6a7b.0430.d8cd.b780.70b4.c55a
  ==
::
++  test-aes-ecbb
  %+  do-test-vectors-ecb  ecbb
  :~
    :+  0x0
      0x0
    0xaae0.6992.acbf.52a3.e8f4.a96e.c930.0bd7
  ::
    :+  0x0
      0x1
    0xcd33.b28a.c773.f74b.a00e.d1f3.1257.2435
  ::
    :+  0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f.
       1011.1213.1415.1617
      0x11.2233.4455.6677.8899.aabb.ccdd.eeff
    0xdda9.7ca4.864c.dfe0.6eaf.70a0.ec0d.7191
  ==
::
++  test-aes-ecbc
  %+  do-test-vectors-ecb  ecbc
  :~
    :+  0x0
      0x0
    0xdc95.c078.a240.8989.ad48.a214.9284.2087
  ::
    :+  0x0
      0x1
    0x530f.8afb.c745.36b9.a963.b4f1.c4cb.738b
  ::
    :+  0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f.
       1011.1213.1415.1617.1819.1a1b.1c1d.1e1f
      0x11.2233.4455.6677.8899.aabb.ccdd.eeff
    0x8ea2.b7ca.5167.45bf.eafc.4990.4b49.6089
  ==
--
