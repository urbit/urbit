::  tests for the blake2b hashing algorithm
::
::  test vectors from here:
::  https://github.com/emilbayes/blake2b/blob/master/test-vectors.json
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
--
