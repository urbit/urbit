::  tests for the blake2b and blake3 hashing algorithms
::
::  test vectors from here:
::  https://github.com/emilbayes/blake2b/blob/master/test-vectors.json
::  https://github.com/BLAKE3-team/BLAKE3/blob/master/test_vectors/test_vectors.json
::
/+  *test
=,  blake:crypto
::
|%
::
::  test a list of vectors
::
++  do-test-vectors
  |=  ves=(list [out=@ud msg=byts key=byts res=@ux])
  ^-  tang
  ?~  ves  ~
  =-  (weld - $(ves t.ves))
  =,  i.ves
  %+  expect-eq
    !>  res
    !>  `@ux`(blake2b msg key out)
::
++  test-blake2b
  =/  non=byts  0^0x0
  =/  key=byts
    :-  64
    0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f.1011.1213.1415.1617.
        1819.1a1b.1c1d.1e1f.2021.2223.2425.2627.2829.2a2b.2c2d.
        2e2f.3031.3233.3435.3637.3839.3a3b.3c3d.3e3f
  =/  sml=byts
    :-  33
    0x102.0304.0506.0708.090a.0b0c.0d0e.0f10.
     1112.1314.1516.1718.191a.1b1c.1d1e.1f20
  =/  lrg=byts
    :-  255
    0x102.0304.0506.0708.090a.0b0c.0d0e.0f10.1112.1314.1516.
     1718.191a.1b1c.1d1e.1f20.2122.2324.2526.2728.292a.2b2c.
     2d2e.2f30.3132.3334.3536.3738.393a.3b3c.3d3e.3f40.4142.
     4344.4546.4748.494a.4b4c.4d4e.4f50.5152.5354.5556.5758.
     595a.5b5c.5d5e.5f60.6162.6364.6566.6768.696a.6b6c.6d6e.
     6f70.7172.7374.7576.7778.797a.7b7c.7d7e.7f80.8182.8384.
     8586.8788.898a.8b8c.8d8e.8f90.9192.9394.9596.9798.999a.
     9b9c.9d9e.9fa0.a1a2.a3a4.a5a6.a7a8.a9aa.abac.adae.afb0.
     b1b2.b3b4.b5b6.b7b8.b9ba.bbbc.bdbe.bfc0.c1c2.c3c4.c5c6.
     c7c8.c9ca.cbcc.cdce.cfd0.d1d2.d3d4.d5d6.d7d8.d9da.dbdc.
     ddde.dfe0.e1e2.e3e4.e5e6.e7e8.e9ea.ebec.edee.eff0.f1f2.
     f3f4.f5f6.f7f8.f9fa.fbfc.fdfe
  ::
  %-  do-test-vectors
  :~
    :^    64
        non
      non
    0x786a.02f7.4201.5903.c6c6.fd85.2552.d272.912f.4740.e158.
      4761.8a86.e217.f71f.5419.d25e.1031.afee.5853.1389.6444.
      934e.b04b.903a.685b.1448.b755.d56f.701a.fe9b.e2ce
  ::
    :^    64
        sml
      non
    0x83b0.98f2.6225.1bf6.6006.4a9d.3511.ce76.87a0.9e6d.fbb8.
      7829.9c30.e93d.fb43.a931.4db9.a600.337d.b26e.beed.af22.
      56a9.6dab.e9b2.9e75.73ad.11c3.523d.874d.de5b.e7ed
  ::
    :^    64
        lrg
      non
    0x5b21.c5fd.8868.3676.1247.4fa2.e70e.9cfa.2201.ffee.e8fa.
      fab5.797a.d58f.efa1.7c9b.5b10.7da4.a3db.6320.baaf.2c86.
      17d5.a51d.f914.ae88.da38.67c2.d41f.0cc1.4fa6.7928
  ::
    :^    64
        non
      key
    0x10eb.b677.00b1.868e.fb44.1798.7acf.4690.ae9d.972f.b7a5.
      90c2.f028.7179.9aaa.4786.b5e9.96e8.f0f4.eb98.1fc2.14b0.
      05f4.2d2f.f423.3499.3916.53df.7aef.cbc1.3fc5.1568
  ::
    :^    64
        sml
      key
    0x5595.e05c.13a7.ec4d.c8f4.1fb7.0cb5.0a71.bce1.7c02.4ff6.
      de7a.f618.d0cc.4e9c.32d9.570d.6d3e.a45b.8652.5491.030c.
      0d8f.2b18.36d5.778c.1ce7.35c1.7707.df36.4d05.4347
  ::
    :^    64
        lrg
      key
    0x1427.09d6.2e28.fccc.d0af.97fa.d0f8.465b.971e.8220.1dc5.
      1070.faa0.372a.a43e.9248.4be1.c1e7.3ba1.0906.d5d1.853d.
      b6a4.106e.0a7b.f980.0d37.3d6d.ee2d.46d6.2ef2.a461
  ==
::
++  do-blake3-test-vectors
  |=  ves=(list [out=@ msglen=@ud res=@ keyres=@])
  ^-  tang
  ?~  ves  ~
  =-  (weld - $(ves t.ves))
  =,  i.ves
  =/  msg  (blake3-test-msg msglen)
  =/  keyed  (keyed:blake3 32^'whats the Elvish word for friend')
  ;:  weld
    %+  expect-eq
      !>  `@ux`res
      !>  `@ux`(hash:blake3 msg out)
    %+  expect-eq
      !>  `@ux`keyres
      !>  `@ux`(hash:keyed msg out)
  ==
::
++  blake3-test-msg
  |=  len=@
  =/  iota  ?:(=(0 len) ~ (gulf 0 (dec len)))
  len^(rep 3 (turn iota (curr mod 251)))
::
++  test-blake3
  ::
  %-  do-blake3-test-vectors
  :~
    :^    32
        0
      0xaf13.49b9.f5f9.a1a6.a040.4dea.36dc.c949.
        9bcb.25c9.adc1.12b7.cc9a.93ca.e41f.3262
    0x92b2.b756.04ed.3c76.1f9d.6f62.392c.8a92.
      27ad.0ea3.f095.73e7.83f1.498a.4ed6.0d26
  ::
    :^    128
        0
      0xaf13.49b9.f5f9.a1a6.a040.4dea.36dc.c949.
        9bcb.25c9.adc1.12b7.cc9a.93ca.e41f.3262.
        e00f.03e7.b69a.f26b.7faa.f09f.cd33.3050.
        338d.dfe0.85b8.cc86.9ca9.8b20.6c08.243a.
        26f5.4877.89e8.f660.afe6.c99e.f9e0.c52b.
        92e7.3930.24a8.0459.cf91.f476.f9ff.dbda.
        7001.c22e.159b.4026.31f2.77ca.96f2.defd.
        f107.8282.314e.7636.99a3.1c53.6316.5421
    0x92b2.b756.04ed.3c76.1f9d.6f62.392c.8a92.
      27ad.0ea3.f095.73e7.83f1.498a.4ed6.0d26.
      b181.71a2.f22a.4b94.822c.701f.1071.53db.
      a249.18c4.bae4.d294.5c20.ece1.3387.627d.
      3b73.cbf9.7b79.7d5e.5994.8c7e.f788.f543.
      72df.45e4.5e42.93c7.dc18.c1d4.1144.a975.
      8be5.8960.856b.e1ea.bbe2.2c26.5319.0de5.
      60ca.3b2a.c4aa.692a.9210.6942.54c3.71e8
  ::
    :^    32
        1
      0x2d3a.dedf.f11b.61f1.4c88.6e35.afa0.3673.
        6dcd.87a7.4d27.b5c1.5102.25d0.f592.e213
    0x6d78.78df.ff2f.4856.35d3.9013.278a.e14f.
      1454.b8c0.a3a2.d34b.c1ab.3822.8a80.c95b
  ::
    :^    32
        1.024
      0x4221.4739.f095.a406.f3fc.83de.b889.744a.
        c00d.f831.c10d.aa55.189b.5d12.1c85.5af7
    0x75c4.6f6f.3d9e.b4f5.5eca.aee4.80db.732e.
      6c21.0554.6f1e.6750.0368.7c31.719c.7ba4
  ::
    :^    32
        31.744
      0x62b6.960e.1a44.bcc1.eb1a.611a.8d62.35b6.
        b4b7.8f32.e7ab.c4fb.4c6c.dcce.9489.5c47
    0xefa5.3b38.9ab6.7c59.3dba.624d.898d.0f73.
      53ab.99e4.ac9d.4230.2ee6.4cbf.9939.a419
  ==
--
