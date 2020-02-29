::  tests for the bip32 lib
::
::  Test vectors from:
::  https://en.bitcoin.it/wiki/BIP_0032_TestVectors
::
/+  *test, bip32
=,  bip32
::
|%
+$  vector  [mk=byts dp=tape id=@ux sk=@ux pk=@ux cc=@ux]
::
++  test-vectors
  ^-  tang
  |^  ;:  weld
        %+  category  "identity"
        (zing (turn vectors check-id))
        %+  category  "public-key"
        (zing (turn vectors check-pk))
        %+  category  "private-key"
        (zing (turn vectors check-sk))
        %+  category  "chaincode"
        (zing (turn vectors check-cc))
      ==
  ::
  ++  check-id
    |=  vector
    %+  expect-eq
      !>  id
      !>  `@ux`identity:(derive-path:(from-seed mk) dp)
  ::
  ++  check-pk
    |=  vector
    %+  expect-eq
      !>  pk
      !>  `@ux`public-key:(derive-path:(from-seed mk) dp)
  ::
  ++  check-sk
    |=  vector
    %+  expect-eq
      !>  sk
      !>  `@ux`private-key:(derive-path:(from-seed mk) dp)
  ::
  ++  check-cc
    |=  vector
    %+  expect-eq
      !>  cc
      !>  `@ux`chain-code:(derive-path:(from-seed mk) dp)
  ::
  ++  vectors
    ^-  (list vector)
    :~
      :*
        16^0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
        "m/0'"
        0x5c1b.d648.ed23.aa5f.d50b.a52b.2457.c11e.9e80.a6a7
        0xedb2.e14f.9ee7.7d26.dd93.b4ec.ede8.d16e.d408.ce14.9b6c.d80b.0715.a2d9.11a0.afea
        0x3.5a78.4662.a4a2.0a65.bf6a.ab9a.e98a.6c06.8a81.c52e.4b03.2c0f.b540.0c70.6cfc.cc56
        0x47fd.acbd.0f10.9704.3b78.c63c.20c3.4ef4.ed9a.111d.9800.47ad.1628.2c7a.e623.6141
      ==
    ::
      :*
        16^0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
        "m/0'/1"
        0xbef5.a2f9.a56a.94aa.b124.59f7.2ad9.cf8c.f19c.7bbe
        0x3c6c.b8d0.f6a2.64c9.1ea8.b503.0fad.aa8e.538b.020f.0a38.7421.a12d.e931.9dc9.3368
        0x3.501e.454b.f007.51f2.4b1b.489a.a925.215d.66af.2234.e389.1c3b.21a5.2bed.b3cd.711c
        0x2a78.5763.1386.ba23.daca.c341.80dd.1983.734e.444f.dbf7.7404.1578.e9b6.adb3.7c19
      ==
    ::
      :*
        16^0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
        "m/0'/1/2'"
        0xee7a.b90c.de56.a8c0.e2bb.086a.c497.48b8.db9d.ce72
        0xcbce.0d71.9ecf.7431.d88e.6a89.fa14.83e0.2e35.092a.f60c.042b.1df2.ff59.fa42.4dca
        0x3.57bf.e1e3.41d0.1c69.fe56.5430.9956.cbea.5168.22fb.a8a6.0174.3a01.2a78.96ee.8dc2
        0x446.6b9c.c8e1.61e9.6640.9ca5.2986.c584.f07e.9dc8.1f73.5db6.83c3.ff6e.c7b1.503f
      ==
    ::
      :*
        16^0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
        "m/0'/1/2'/2"
        0xd880.d7d8.9384.8509.a62d.8fb7.4e32.148d.ac68.412f
        0xf47.9245.fb19.a38a.1954.c5c7.c0eb.ab2f.9bdf.d96a.1756.3ef2.8a6a.4b1a.2a76.4ef4
        0x2.e844.5082.a72f.29b7.5ca4.8748.a914.df60.622a.609c.acfc.e8ed.0e35.8045.6074.1d29
        0xcfb7.1883.f016.76f5.87d0.23cc.53a3.5bc7.f88f.724b.1f8c.2892.ac12.75ac.822a.3edd
      ==
    ::
      :*
        16^0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
        "m/0'/1/2'/2/1000000000"
        0xd69a.a102.255f.ed74.3782.78c7.8127.01ea.641f.df32
        0x471b.76e3.89e5.28d6.de6d.8168.57e0.12c5.4550.51ca.d666.0850.e583.72a6.c3e6.e7c8
        0x2.2a47.1424.da5e.6574.99d1.ff51.cb43.c474.81a0.3b1e.77f9.51fe.64ce.c9f5.a48f.7011
        0xc783.e67b.921d.2beb.8f6b.389c.c646.d726.3b41.4570.1dad.d216.1548.a8b0.78e6.5e9e
      ==
    ::
      :*
        64^0xfffc.f9f6.f3f0.edea.e7e4.e1de.dbd8.d5d2.
             cfcc.c9c6.c3c0.bdba.b7b4.b1ae.aba8.a5a2.
             9f9c.9996.9390.8d8a.8784.817e.7b78.7572.
             6f6c.6966.6360.5d5a.5754.514e.4b48.4542
        "m/0"
        0x5a61.ff8e.b7aa.ca30.10db.97eb.da76.1216.10b7.8096
        0xabe7.4a98.f6c7.eabe.e042.8f53.798f.0ab8.
          aa1b.d378.7399.9041.703c.742f.15ac.7e1e
        0x2.fc9e.5af0.ac8d.9b3c.ecfe.2a88.8e21.17ba.
            3d08.9d85.8588.6c9c.826b.6b22.a98d.12ea
        0xf090.9aff.aa7e.e7ab.e5dd.4e10.0598.d4dc.53cd.709d.5a5c.2cac.40e7.412f.232f.7c9c
      ==
    ::
      :*
        64^0xfffc.f9f6.f3f0.edea.e7e4.e1de.dbd8.d5d2.
             cfcc.c9c6.c3c0.bdba.b7b4.b1ae.aba8.a5a2.
             9f9c.9996.9390.8d8a.8784.817e.7b78.7572.
             6f6c.6966.6360.5d5a.5754.514e.4b48.4542
        "m/0/2147483647'"
        0xd8ab.4937.36da.02f1.1ed6.82f8.8339.e720.fb03.79d1
        0x877c.779a.d968.7164.e9c2.f4f0.f4ff.0340.
          8143.9233.0693.ce95.a58f.e18f.d52e.6e93
      0x3.c01e.7425.647b.defa.82b1.2d9b.ad5e.3e68.
          65be.e050.2694.b94c.a58b.666a.bc0a.5c3b
        0xbe17.a268.474a.6bb9.c61e.1d72.0cf6.215e.2a88.c540.6c4a.ee7b.3854.7f58.5c9a.37d9
      ==
    ::
      :*
        64^0xfffc.f9f6.f3f0.edea.e7e4.e1de.dbd8.d5d2.
             cfcc.c9c6.c3c0.bdba.b7b4.b1ae.aba8.a5a2.
             9f9c.9996.9390.8d8a.8784.817e.7b78.7572.
             6f6c.6966.6360.5d5a.5754.514e.4b48.4542
        "m/0/2147483647'/1"
        0x7841.2e3a.2296.a40d.e124.307b.6485.bd19.833e.2e34
        0x704a.ddf5.44a0.6e5e.e4be.a370.9846.3c23.
          613d.a320.20d6.0450.6da8.c051.8e1d.a4b7
      0x3.a7d1.d856.deb7.4c50.8e05.031f.9895.dab5.
          4626.251b.3806.e16b.4bd1.2e78.1a7d.f5b9
        0xf366.f48f.1ea9.f2d1.d3fe.958c.95ca.84ea.18e4.c4dd.b936.6c33.6c92.7eb2.46fb.38cb
      ==
    ::
      :*
        64^0xfffc.f9f6.f3f0.edea.e7e4.e1de.dbd8.d5d2.
             cfcc.c9c6.c3c0.bdba.b7b4.b1ae.aba8.a5a2.
             9f9c.9996.9390.8d8a.8784.817e.7b78.7572.
             6f6c.6966.6360.5d5a.5754.514e.4b48.4542
        "m/0/2147483647'/1/2147483646'"
        0x31a5.07b8.1559.3dfc.51ff.c724.5ae7.e5ae.e304.246e
        0xf1c7.c871.a54a.804a.fe32.8b4c.83a1.c33b.
          8e5f.f48f.5087.273f.04ef.a83b.247d.6a2d
      0x2.d2b3.6900.396c.9282.fa14.6285.6658.2f20.
          6a5d.d0bc.c8d5.e892.6118.06ca.fb03.01f0
        0x6378.0703.0d55.d01f.9a0c.b3a7.8395.15d7.96bd.0770.6386.a6ed.df06.cc29.a65a.0e29
      ==
    ::
      :*
        64^0xfffc.f9f6.f3f0.edea.e7e4.e1de.dbd8.d5d2.
             cfcc.c9c6.c3c0.bdba.b7b4.b1ae.aba8.a5a2.
             9f9c.9996.9390.8d8a.8784.817e.7b78.7572.
             6f6c.6966.6360.5d5a.5754.514e.4b48.4542
        "m/0/2147483647'/1/2147483646'/2"
        0x2613.2fdb.e7bf.89cb.c64c.f8da.fa3f.9f88.b866.6220
        0xbb7d.39bd.b83e.cf58.f2fd.82b6.d918.341c.
          bef4.2866.1ef0.1ab9.7c28.a484.2125.ac23
      0x2.4d90.2e1a.2fc7.a875.5ab5.b694.c575.fce7.
          42c4.8d9f.f192.e63d.f519.3e4c.7afe.1f9c
        0x9452.b549.be8c.ea3e.cb7a.84be.c10d.cfd9.4afe.4d12.9ebf.d3b3.cb58.eedf.394e.d271
      ==
    ==
  --
--
