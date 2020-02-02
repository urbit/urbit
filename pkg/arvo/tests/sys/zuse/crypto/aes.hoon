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
::
+$  vector-cbc  [key=@ux iv=@ux in=@ux out=@ux]
::
++  do-test-vectors-cbc
  |*  [cbcx=_cbca ves=(list vector-cbc)]
  ^-  tang
  |^  %+  weld
        %+  category  "encrypting"
        (zing (turn ves enc))
      %+  category  "decrypting"
      (zing (turn ves dec))
  ::
  ++  enc
    |=  vector-cbc
    %+  expect-eq
      !>  out
      !>  `@ux`(~(en cbcx key iv) in)
  ::
  ++  dec
    |=  vector-cbc
    %+  expect-eq
      !>  in
      !>  `@ux`(~(de cbcx key iv) out)
  --
::
++  test-aes-cbca
  %+  do-test-vectors-cbc  cbca
  :~
    :^    0x0
        0x1
      0x2
    0xf795.aaab.494b.5923.f7fd.89ff.948b.c1e0
  ::
    :^    0x2b7e.1516.28ae.d2a6.abf7.1588.09cf.4f3c
        0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a.
        ae2d.8a57.1e03.ac9c.9eb7.6fac.45af.8e51.
        30c8.1c46.a35c.e411.e5fb.c119.1a0a.52ef.
        f69f.2445.df4f.9b17.ad2b.417b.e66c.3710
    0x7649.abac.8119.b246.cee9.8e9b.12e9.197d.
      5086.cb9b.5072.19ee.95db.113a.9176.78b2.
      73be.d6b8.e3c1.743b.7116.e69e.2222.9516.
      3ff1.caa1.681f.ac09.120e.ca30.7586.e1a7
  ==
++  test-aes-cbcb
  %+  do-test-vectors-cbc  cbcb
  :~
    :^    0x0
        0x1
      0x2
    0x2a34.93e6.6235.ee67.deec.cd2f.3b39.3bd8
  ::
    :^    0x8e73.b0f7.da0e.6452.c810.f32b.8090.79e5.
            62f8.ead2.522c.6b7b
        0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a.
        ae2d.8a57.1e03.ac9c.9eb7.6fac.45af.8e51.
        30c8.1c46.a35c.e411.e5fb.c119.1a0a.52ef.
        f69f.2445.df4f.9b17.ad2b.417b.e66c.3710
    0x4f02.1db2.43bc.633d.7178.183a.9fa0.71e8.
      b4d9.ada9.ad7d.edf4.e5e7.3876.3f69.145a.
      571b.2420.12fb.7ae0.7fa9.baac.3df1.02e0.
      08b0.e279.8859.8881.d920.a9e6.4f56.15cd
  ==
++  test-aes-cbcc
  %+  do-test-vectors-cbc  cbcc
  :~
    :^    0x0
        0x1
      0x2
    0x7260.03ca.37a6.2a74.d1a2.f58e.7506.358e
  ::
    :^    0x603d.eb10.15ca.71be.2b73.aef0.857d.7781.
            1f35.2c07.3b61.08d7.2d98.10a3.0914.dff4
        0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a.
        ae2d.8a57.1e03.ac9c.9eb7.6fac.45af.8e51.
        30c8.1c46.a35c.e411.e5fb.c119.1a0a.52ef.
        f69f.2445.df4f.9b17.ad2b.417b.e66c.3710
    0xf58c.4c04.d6e5.f1ba.779e.abfb.5f7b.fbd6.
      9cfc.4e96.7edb.808d.679f.777b.c670.2c7d.
      39f2.3369.a9d9.bacf.a530.e263.0423.1461.
      b2eb.05e2.c39b.e9fc.da6c.1907.8c6a.9d1b
  ==
--
