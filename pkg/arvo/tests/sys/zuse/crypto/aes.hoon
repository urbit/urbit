::  tests for the aes block cipher
::
/+  *test
=,  aes:crypto
::
|%
::
::  ECB mode. Test vectors from Appendix C of FIPS-197:
::  https://csrc.nist.gov/publications/fips/fips197/fips-197.pdf
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
::  CBC mode. Test vectors from Appendix F of NIST SP 800-38A:
::  https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-38a.pdf
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
::
::  CTR mode. Test vectors from Appendix F of NIST SP 800-38A:
::  https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-38a.pdf
::
+$  vector-ctr  [key=@ux iv=@ux in=@ux out=@ux]
::
++  do-test-vectors-ctr
  |*  [ctrx=_ctra ves=(list vector-ctr)]
  ^-  tang
  |^  %+  weld
        %+  category  "encrypting"
        (zing (turn ves enc))
      %+  category  "decrypting"
      (zing (turn ves dec))
  ::
  ++  enc
    |=  vector-ctr
    %+  expect-eq
      !>  out
      !>  `@ux`(~(en ctrx key 7 (met 3 out) iv) in)
  ::
  ++  dec
    |=  vector-ctr
    %+  expect-eq
      !>  in
      !>  `@ux`(~(de ctrx key 7 (met 3 out) iv) out)
  --
::
++  test-aes-ctra
  %+  do-test-vectors-ctr  ctra
  :~
    :^    0x0
        0x1
      0x2
    0x58e2.fcce.fa7e.3061.367f.1d57.a4e7.4558
  ::
    :^    0x2b7e.1516.28ae.d2a6.abf7.1588.09cf.4f3c
        0xf0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a.
        ae2d.8a57.1e03.ac9c.9eb7.6fac.45af.8e51.
        30c8.1c46.a35c.e411.e5fb.c119.1a0a.52ef.
        f69f.2445.df4f.9b17.ad2b.417b.e66c.3710
    0x874d.6191.b620.e326.1bef.6864.990d.b6ce.
      9806.f66b.7970.fdff.8617.187b.b9ff.fdff.
      5ae4.df3e.dbd5.d35e.5b4f.0902.0db0.3eab.
      1e03.1dda.2fbe.03d1.7921.70a0.f300.9cee
  ==
++  test-aes-ctrb
  %+  do-test-vectors-ctr  ctrb
  :~
    :^    0x0
        0x1
      0x2
    0xcd33.b28a.c773.f74b.a00e.d1f3.1257.2437
  ::
    :^    0x8e73.b0f7.da0e.6452.c810.f32b.8090.79e5.
            62f8.ead2.522c.6b7b
        0xf0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a.
        ae2d.8a57.1e03.ac9c.9eb7.6fac.45af.8e51.
        30c8.1c46.a35c.e411.e5fb.c119.1a0a.52ef.
        f69f.2445.df4f.9b17.ad2b.417b.e66c.3710
    0x1abc.9324.1752.1ca2.4f2b.0459.fe7e.6e0b.
      0903.39ec.0aa6.faef.d5cc.c2c6.f4ce.8e94.
      1e36.b26b.d1eb.c670.d1bd.1d66.5620.abf7.
      4f78.a7f6.d298.0958.5a97.daec.58c6.b050
  ==
++  test-aes-ctrc
  %+  do-test-vectors-ctr  ctrc
  :~
    :^    0x0
        0x1
      0x2
    0x530f.8afb.c745.36b9.a963.b4f1.c4cb.7389
  ::
    :^    0x603d.eb10.15ca.71be.2b73.aef0.857d.7781.
            1f35.2c07.3b61.08d7.2d98.10a3.0914.dff4
        0xf0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a.
        ae2d.8a57.1e03.ac9c.9eb7.6fac.45af.8e51.
        30c8.1c46.a35c.e411.e5fb.c119.1a0a.52ef.
        f69f.2445.df4f.9b17.ad2b.417b.e66c.3710
    0x601e.c313.7757.89a5.b7a7.f504.bbf3.d228.
      f443.e3ca.4d62.b59a.ca84.e990.caca.f5c5.
      2b09.30da.a23d.e94c.e870.17ba.2d84.988d.
      dfc9.c58d.b67a.ada6.13c2.dd08.4579.41a6
  ==
::
::  AES-CMAC. Test vectors from Section D of NIST SP 800-38B:
::  https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-38b.pdf
::
+$  vector-mac  [key=@ux in=@ux out=@ux]
::
++  do-test-vectors-mac
  |*  [macx=_maca ves=(list vector-mac)]
  ^-  tang
  |^  (zing (turn ves case))
  ::
  ++  case
    |=  vector-mac
    %+  expect-eq
      !>  out
      !>  `@ux`(macx key ~ in)
  --
::
++  test-aes-maca
  %+  do-test-vectors-mac  maca
  :~
    :+  0x2b7e.1516.28ae.d2a6.abf7.1588.09cf.4f3c
      0x0
    0xbb1d.6929.e959.3728.7fa3.7d12.9b75.6746
  ::
    :+  0x2b7e.1516.28ae.d2a6.abf7.1588.09cf.4f3c
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a
    0x70a.16b4.6b4d.4144.f79b.dd9d.d04a.287c
  ::
    :+  0x2b7e.1516.28ae.d2a6.abf7.1588.09cf.4f3c
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a.
        ae2d.8a57.1e03.ac9c.9eb7.6fac.45af.8e51.
        30c8.1c46.a35c.e411
    0xdfa6.6747.de9a.e630.30ca.3261.1497.c827
  ::
    :+  0x2b7e.1516.28ae.d2a6.abf7.1588.09cf.4f3c
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a.
        ae2d.8a57.1e03.ac9c.9eb7.6fac.45af.8e51.
        30c8.1c46.a35c.e411.e5fb.c119.1a0a.52ef.
        f69f.2445.df4f.9b17.ad2b.417b.e66c.3710
    0x51f0.bebf.7e3b.9d92.fc49.7417.7936.3cfe
  ==
++  test-aes-macb
  %+  do-test-vectors-mac  macb
  :~
    :+  0x8e73.b0f7.da0e.6452.c810.f32b.8090.79e5.
          62f8.ead2.522c.6b7b
      0x0
    0xd17d.df46.adaa.cde5.31ca.c483.de7a.9367
  ::
    :+  0x8e73.b0f7.da0e.6452.c810.f32b.8090.79e5.
          62f8.ead2.522c.6b7b
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a
    0x9e99.a7bf.31e7.1090.0662.f65e.617c.5184
  ::
    :+  0x8e73.b0f7.da0e.6452.c810.f32b.8090.79e5.
          62f8.ead2.522c.6b7b
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a.
        ae2d.8a57.1e03.ac9c.9eb7.6fac.45af.8e51.
        30c8.1c46.a35c.e411
    0x8a1d.e5be.2eb3.1aad.089a.82e6.ee90.8b0e
  ::
    :+  0x8e73.b0f7.da0e.6452.c810.f32b.8090.79e5.
          62f8.ead2.522c.6b7b
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a.
        ae2d.8a57.1e03.ac9c.9eb7.6fac.45af.8e51.
        30c8.1c46.a35c.e411.e5fb.c119.1a0a.52ef.
        f69f.2445.df4f.9b17.ad2b.417b.e66c.3710
    0xa1d5.df0e.ed79.0f79.4d77.5896.59f3.9a11
  ==
++  test-aes-macc
  %+  do-test-vectors-mac  macc
  :~
    :+  0x603d.eb10.15ca.71be.2b73.aef0.857d.7781.
          1f35.2c07.3b61.08d7.2d98.10a3.0914.dff4
      0x0
    0x289.62f6.1b7b.f89e.fc6b.551f.4667.d983
  ::
    :+  0x603d.eb10.15ca.71be.2b73.aef0.857d.7781.
          1f35.2c07.3b61.08d7.2d98.10a3.0914.dff4
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a
    0x28a7.023f.452e.8f82.bd4b.f28d.8c37.c35c
  ::
    :+  0x603d.eb10.15ca.71be.2b73.aef0.857d.7781.
          1f35.2c07.3b61.08d7.2d98.10a3.0914.dff4
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a.
        ae2d.8a57.1e03.ac9c.9eb7.6fac.45af.8e51.
        30c8.1c46.a35c.e411
    0xaaf3.d8f1.de56.40c2.32f5.b169.b9c9.11e6
  ::
    :+  0x603d.eb10.15ca.71be.2b73.aef0.857d.7781.
          1f35.2c07.3b61.08d7.2d98.10a3.0914.dff4
      0x6bc1.bee2.2e40.9f96.e93d.7e11.7393.172a.
        ae2d.8a57.1e03.ac9c.9eb7.6fac.45af.8e51.
        30c8.1c46.a35c.e411.e5fb.c119.1a0a.52ef.
        f69f.2445.df4f.9b17.ad2b.417b.e66c.3710
    0xe199.2190.549f.6ed5.696a.2c05.6c31.5410
  ==
::
::  SIV mode. Test vectors from Project Wycheproof:
::  https://github.com/google/wycheproof/blob/master/testvectors/aes_siv_cmac_test.json
::
::  Additional test vector from Appendix A of RFC 5297:
::  https://tools.ietf.org/html/rfc5297#appendix-A
::
+$  vector-siv  [key=@ux ad=(list @ux) in=@ux out=(trel @ux @ud @ux)]
::
++  do-test-vectors-siv
  |*  [sivx=_siva ves=(list vector-siv)]
  ^-  tang
  |^  %+  weld
        %+  category  "encrypting"
        (zing (turn ves enc))
      %+  category  "decrypting"
      (zing (turn ves dec))
  ::
  ++  enc
    |=  vector-siv
    %+  expect-eq
      !>  out
      !>  (~(en sivx key ad) in)
  ::
  ++  dec
    |=  vector-siv
    %+  expect-eq
      !>  `in
      !>  (~(de sivx key ad) p.out q.out r.out)
  --
::
++  test-aes-siva
  %+  do-test-vectors-siv  siva
  ^-  (list vector-siv)
  :~
  ::
  :: from RFC 5297
  ::
    :^    0xfffe.fdfc.fbfa.f9f8.f7f6.f5f4.f3f2.f1f0.
            f0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff
        :~  0x1011.1213.1415.1617.1819.1a1b.1c1d.1e1f.
              2021.2223.2425.2627
        ==
      0x1122.3344.5566.7788.99aa.bbcc.ddee
    :+
      0x8563.2d07.c6e8.f37f.950a.cd32.0a2e.cc93
      14
      0x40c0.2b96.90c4.dc04.daef.7f6a.fe5c
  ::
  :: same as above, sans AD
  ::
    :^    0xfffe.fdfc.fbfa.f9f8.f7f6.f5f4.f3f2.f1f0.
            f0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff
        ~
      0x1122.3344.5566.7788.99aa.bbcc.ddee
    :+
      0xf1c5.fdea.c1f1.5a26.779c.1501.f9fb.7588
      14
      0x27e9.46c6.6908.8ab0.6da5.8c5c.831c
  ::
  :: same as above, sans message
  ::
    :^    0xfffe.fdfc.fbfa.f9f8.f7f6.f5f4.f3f2.f1f0.
            f0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff
        :~  0x1011.1213.1415.1617.1819.1a1b.1c1d.1e1f.
              2021.2223.2425.2627
        ==
      0x0
    :+
      0xb9d5.cc97.054d.cd3f.6dfd.a629.d4f4.d313
      0
      0x0
  ::
  :: same as above, sans AD and message
  ::
    :^    0xfffe.fdfc.fbfa.f9f8.f7f6.f5f4.f3f2.f1f0.
            f0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff
        ~
      0x0
    :+
      0xf200.7a5b.eb2b.8900.c588.a7ad.f599.f172
      0
      0x0
  ::
  :: all subsequent vectors from Wycheproof
  ::
    :^    0x612e.8378.43ce.ae7f.61d4.9625.faa7.e749.
            4f92.53e2.0cb3.adce.a686.512b.0439.36cd
        :~  0x865d.39ae.9b5e.9ff8.d630.8e00.2087.45bc
        ==
      0xcc37.fae1.5f74.5a2f.40e2.c8b1.92f2.b38d
    :+
      0xc79c.86cd.7509.e60a.16ca.8cec.6bca.a1c5
      16
      0x8fbd.6099.7189.91fe.775b.f5a6.59d3.0a24
  ::
    :^    0x71a7.adc7.222f.471c.28f6.82c1.2d45.feed.
            4555.6000.a986.0359.2292.4ad1.54ba.5fa5
        :~  0x46a6.5672.d269.9267.ab27.da82
        ==
      0x227e.714e.3efa.84e4.8049.142e.daa3.11da.
        b285.407f.9b62.8b14.6f1d.6132.c250.0ca2.
        8497.fbd6.e386.679c
    :+
      0xb30e.c3b9.c854.02c3.5672.8391.acf0.4fcc
      40
      0xd02.ba85.b6a9.e90c.f846.155d.4ab3.1589.
       52bd.1791.8853.70bf.23ba.26d8.d233.5963.
       7b6e.24e8.763e.d107
  ==
::
++  test-aes-sivb
  %+  do-test-vectors-siv  sivb
  ^-  (list vector-siv)
  :~
  ::
  :: from RFC 5297, with extended key
  ::
    :^    0xfffe.fdfc.fbfa.f9f8.f7f6.f5f4.f3f2.f1f0.
            f0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff.
            0011.2233.4455.6677.8899.aabb.ccdd.eeff
        :~  0x1011.1213.1415.1617.1819.1a1b.1c1d.1e1f.
              2021.2223.2425.2627
        ==
      0x1122.3344.5566.7788.99aa.bbcc.ddee
    :+
      0x89e8.69b9.3256.7851.54f0.9639.62fe.0740
      14
      0xf313.e667.b564.78a0.32b9.913e.923c
  ::
  :: same as above, sans AD
  ::
    :^    0xfffe.fdfc.fbfa.f9f8.f7f6.f5f4.f3f2.f1f0.
            f0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff.
            0011.2233.4455.6677.8899.aabb.ccdd.eeff
        ~
      0x1122.3344.5566.7788.99aa.bbcc.ddee
    :+
      0xefc5.3765.c5a5.b1e0.69df.8927.3c57.4dc3
      14
      0x1a67.edd1.8a33.6b1f.5eb2.5a95.4844
  ::
  :: same as above, sans message
  ::
    :^    0xfffe.fdfc.fbfa.f9f8.f7f6.f5f4.f3f2.f1f0.
            f0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff.
            0011.2233.4455.6677.8899.aabb.ccdd.eeff
        :~  0x1011.1213.1415.1617.1819.1a1b.1c1d.1e1f.
              2021.2223.2425.2627
        ==
      0x0
    :+
      0xad71.1b68.568c.e7dd.3669.8e4d.f947.8942
      0
      0x0
  ::
  :: same as above, sans AD and message
  ::
    :^    0xfffe.fdfc.fbfa.f9f8.f7f6.f5f4.f3f2.f1f0.
            f0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff.
            0011.2233.4455.6677.8899.aabb.ccdd.eeff
        ~
      0x0
    :+
      0x21ce.0f18.1d86.120b.0bf0.7cb6.1c5a.ac0d
      0
      0x0
  ::
  :: all subsequent vectors from Wycheproof
  ::
    :^    0xca9d.b622.14c3.afab.385b.9086.f1cb.90d1.
            7195.d495.ef47.642d.bad0.6f4e.7d0b.ab13.
            6c77.8850.29ad.442b.30c3.4c8b.5290.e7d0
        :~  0xd4db.fdce.11f1.147e.29dd.062e.a3bb.bd17
        ==
      0xded5.a13d.7599.03ec.d36c.b238.5277.76c6
    :+
      0xa4e0.8bdd.8ab8.cbef.46e0.fdb8.a7ca.1097
      16
      0xa8f9.63e4.5e55.4a58.8249.6270.f9fd.6de8
  ::
    :^    0x5f6b.fe19.987d.5bf6.471d.d9e4.3609.4f7f.
            e33f.8acc.a3b8.a41e.2778.61e2.02bd.7262.
            fc2b.0bd3.df9b.35fa.2fc3.c579.620f.00eb
        :~  0xb96c.8682.cd3c.e676.b0d7.9865
        ==
      0x1485.126d.0476.bb4b.86d0.87d1.8926.32b5.3cb4.f8a2
    :+
      0x8fd1.214e.8078.2d7c.1400.7d03.7feb.1ab1
      20
      0x81fd.cb20.9858.87b5.ee8d.4acf.5589.924b.
        e644.947d
  ::
    :^    0x25b0.b404.bb1f.7844.6d0e.5cde.012e.e583.
            2cb4.0339.8a3e.66e9.b5a2.44b5.9d89.94ee.
            1018.4a57.76f3.578f.aab8.30e8.65f8.133c
        :~  0x7fe4.97ba.cf30.af3a.8566.2aa1
        ==
      0xb1b1.97cd.7ff6.8b62.e274.f5d1.046f.42f9.
        8171.63f0.a105.a0fb.7736.fa9e.5e8f.7694.
        4a22.282a.f480.ee79
    :+
      0xb440.39f1.e5ba.808c.a055.aea6.bc2d.819d
      40
      0x388e.3c27.1cd9.7c04.6061.e572.23bb.c2a1.
        7aa9.b368.d5cf.281d.e46f.48b3.4d17.9c16.
        cc9e.9d46.00a8.7af4
  ==
::
++  test-aes-sivc
  %+  do-test-vectors-siv  sivc
  ^-  (list vector-siv)
  :~
  ::
  :: from RFC 5297, with extended key
  ::
    :^    0xfffe.fdfc.fbfa.f9f8.f7f6.f5f4.f3f2.f1f0.
            f0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff.
            0011.2233.4455.6677.8899.aabb.ccdd.eeff.
            0011.2233.4455.6677.8899.aabb.ccdd.eeff
        :~  0x1011.1213.1415.1617.1819.1a1b.1c1d.1e1f.
              2021.2223.2425.2627
        ==
      0x1122.3344.5566.7788.99aa.bbcc.ddee
    :+
      0x724d.fb2e.af94.dbb1.9b0b.a3a2.99a0.801e
      14
      0x1206.291a.35ad.3db0.2127.7344.0fd0
  ::
  :: same as above, sans AD
  ::
    :^    0xfffe.fdfc.fbfa.f9f8.f7f6.f5f4.f3f2.f1f0.
            f0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff.
            0011.2233.4455.6677.8899.aabb.ccdd.eeff.
            0011.2233.4455.6677.8899.aabb.ccdd.eeff
        ~
      0x1122.3344.5566.7788.99aa.bbcc.ddee
    :+
      0x1dc0.45af.2d4d.9793.b874.09c2.501a.9412
      14
      0x7942.c766.bc8b.0e92.c859.9a38.d4d9
  ::
  :: same as above, sans message
  ::
    :^    0xfffe.fdfc.fbfa.f9f8.f7f6.f5f4.f3f2.f1f0.
            f0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff.
            0011.2233.4455.6677.8899.aabb.ccdd.eeff.
            0011.2233.4455.6677.8899.aabb.ccdd.eeff
        :~  0x1011.1213.1415.1617.1819.1a1b.1c1d.1e1f.
              2021.2223.2425.2627
        ==
      0x0
    :+
      0xe99d.58c2.c500.7b5f.99a0.99b5.f68f.a556
      0
      0x0
  ::
  :: same as above, sans AD and message
  ::
    :^    0xfffe.fdfc.fbfa.f9f8.f7f6.f5f4.f3f2.f1f0.
            f0f1.f2f3.f4f5.f6f7.f8f9.fafb.fcfd.feff.
            0011.2233.4455.6677.8899.aabb.ccdd.eeff.
            0011.2233.4455.6677.8899.aabb.ccdd.eeff
        ~
      0x0
    :+
      0x99c6.6f27.246f.a973.0dcf.370b.e03c.a7a3
      0
      0x0
  ::
  :: all subsequent vectors from Wycheproof
  ::
    :^    0x27fa.f97f.b303.aa4f.2f36.4edd.2399.7f4c.
            77b8.e51e.bb82.93c5.9dfb.1d24.f0fb.629f.
            6c82.0fc2.d91b.f48f.0035.eeec.347e.37ec.
            4fb0.cb36.102b.cdc5.a248.c47a.2f97.eab9
        :~  0xcc94.f64e.14df.9026.5f7f.12a8.a038.6d0a
        ==
      0x6b1d.b0f5.a433.7688.5002.dc98.bd55.6f1d.
        ac9b.66b6.6213.a9fa.6069.df99.5a12.3384
    :+
      0xd001.6d67.5b49.f11e.a873.7074.12d4.5709
      32
      0xb770.3a02.8a0c.fc31.2cf6.1cbe.22b9.1e3c.
        f20b.7d4c.9308.ee15.f18b.4eba.0088.9284
  ::
    :^    0x3ab0.62db.dd38.b951.a9fb.0d8b.b185.959a.
            93da.b349.6b85.0a30.62b9.3003.036c.8bd2.
            aabb.f37d.5d3f.6a39.9d4c.edef.b70c.1b8a.
            7b45.639f.e118.c10e.39f3.6fa5.8618.a84d
        :~  0xe860.5f13.db8c.482d.48bd.ba2d
        ==
      0x5c6f
    :+
      0xfdde.a9ac.778b.978a.9680.33ce.52ec.6116
      2
      0x2588
  ::
    :^    0xcd86.89f8.2181.7f59.bfaa.7551.31f2.5651.
            61c7.f448.9f89.b657.ac9f.a127.a976.8535.
            a702.d001.b9b9.9cc1.1c39.7646.7b1b.4586.
            5ff4.17dc.256e.bb50.79b7.f1b3.e083.07b5
        :~  0xb3ed.ffbb.89b3.73fe.04da.244b
        ==
      0xebcf.b2ff.b681.cc5d.fa0c.5c52.4c1b.1cc8.
        7cc6.b2bf.a35d.c36d.15e8.0505.118b.84a0.
        72a7.8a15.7b4d.1837
    :+
      0x1ac7.8aae.2ede.04eb.4792.4d8f.9f99.fe75
      40
      0xdeb6.1bf6.93da.7f3a.2147.c05f.6d29.d173.
        9235.6fe0.0f82.b24c.dbce.774f.d864.5615.
        48f3.3dd3.192d.806f
  ==
--
