::  tests for the ed25519 signature algorithm
::
/+  *test
=,  ed:crypto
::
|%
::
::  Test vectors from Section 7.1 of RFC 8032:
::  https://tools.ietf.org/html/rfc8032#section-7.1
::
+$  vector  [seed=@ux pk=@ux msg=@ux sig=@ux]
::
++  test-vectors
  ^-  tang
  |^  ;:  weld
        %+  category  "puck"
        (zing (turn vectors check-puck))
        %+  category  "sign"
        (zing (turn vectors check-sign))
        %+  category  "veri"
        (zing (turn vectors check-veri))
      ==
  ::
  ++  check-puck
    |=  vector
    %+  expect-eq
      !>  pk
      !>  `@ux`(puck seed)
  ::
  ++  check-sign
    |=  vector
    %+  expect-eq
      !>  sig
      !>  `@ux`(sign msg seed)
  ::
  ++  check-veri
    |=  vector
    %-  expect
      !>  (veri sig msg pk)
  ::
  ++  vectors
    ^~  (turn vectors-raw swp-vec)
  ++  swp-vec
    |=  vector
    :^  (swp 3 seed)
        (swp 3 pk)
        (swp 3 msg)
        (swp 3 sig)
  ++  vectors-raw
    :~
      :^    0x9d61.b19d.effd.5a60.ba84.4af4.92ec.2cc4.
              4449.c569.7b32.6919.703b.ac03.1cae.7f60
          0xd75a.9801.82b1.0ab7.d54b.fed3.c964.073a.
            0ee1.72f3.daa6.2325.af02.1a68.f707.511a
        0x0
      0xe556.4300.c360.ac72.9086.e2cc.806e.828a.
      8487.7f1e.b8e5.d974.d873.e065.2249.0155.
      5fb8.8215.90a3.3bac.c61e.3970.1cf9.b46b.
      d25b.f5f0.595b.be24.6551.4143.8e7a.100b
    ::
      :^    0x4ccd.089b.28ff.96da.9db6.c346.ec11.4e0f.
              5b8a.319f.35ab.a624.da8c.f6ed.4fb8.a6fb
          0x3d40.17c3.e843.895a.92b7.0aa7.4d1b.7ebc.
            9c98.2ccf.2ec4.968c.c0cd.55f1.2af4.660c
        0x72
      0x92a0.09a9.f0d4.cab8.720e.820b.5f64.2540.
      a2b2.7b54.1650.3f8f.b376.2223.ebdb.69da.
      085a.c1e4.3e15.996e.458f.3613.d0f1.1d8c.
      387b.2eae.b430.2aee.b00d.2916.12bb.0c00
    ::
      :^    0xc5aa.8df4.3f9f.837b.edb7.442f.31dc.b7b1.
              66d3.8535.076f.094b.85ce.3a2e.0b44.58f7
          0xfc51.cd8e.6218.a1a3.8da4.7ed0.0230.f058.
            0816.ed13.ba33.03ac.5deb.9115.4890.8025
        0xaf82
      0x6291.d657.deec.2402.4827.e69c.3abe.01a3.
        0ce5.48a2.8474.3a44.5e36.80d7.db5a.c3ac.
        18ff.9b53.8d16.f290.ae67.f760.984d.c659.
        4a7c.15e9.716e.d28d.c027.bece.ea1e.c40a
    ::
      :^    0xf5e5.767c.f153.3195.1763.0f22.6876.b86c.
              8160.cc58.3bc0.1374.4c6b.f255.f5cc.0ee5
          0x2781.17fc.144c.7234.0f67.d0f2.316e.8386.
            ceff.bf2b.2428.c9c5.1fef.7c59.7f1d.426e
        0x8.b8b2.b733.4242.4376.0fe4.26a4.b549.
       0863.2110.a66c.2f65.91ea.bd33.45e3.e4eb.
       98fa.6e26.4bf0.9efe.12ee.50f8.f54e.9f77.
       b1e3.55f6.c505.44e2.3fb1.433d.df73.be84.
       d879.de7c.0046.dc49.96d9.e773.f4bc.9efe.
       5738.829a.db26.c81b.37c9.3a1b.270b.2032.
       9d65.8675.fc6e.a534.e081.0a44.3282.6bf5.
       8c94.1efb.65d5.7a33.8bbd.2e26.640f.89ff.
       bc1a.858e.fcb8.550e.e3a5.e199.8bd1.77e9.
       3a73.63c3.44fe.6b19.9ee5.d02e.82d5.22c4.
       feba.1545.2f80.288a.821a.5791.16ec.6dad.
       2b3b.310d.a903.401a.a621.00ab.5d1a.3655.
       3e06.203b.3389.0cc9.b832.f79e.f805.60cc.
       b9a3.9ce7.6796.7ed6.28c6.ad57.3cb1.16db.
       efef.d754.99da.96bd.68a8.a97b.928a.8bbc.
       103b.6621.fcde.2bec.a123.1d20.6be6.cd9e.
       c7af.f6f6.c94f.cd72.04ed.3455.c68c.83f4.
       a41d.a4af.2b74.ef5c.53f1.d8ac.70bd.cb7e.
       d185.ce81.bd84.359d.4425.4d95.629e.9855.
       a94a.7c19.58d1.f8ad.a5d0.532e.d8a5.aa3f.
       b2d1.7ba7.0eb6.248e.594e.1a22.97ac.bbb3.
       9d50.2f1a.8c6e.b6f1.ce22.b3de.1a1f.40cc.
       2455.4119.a831.a9aa.d607.9cad.8842.5de6.
       bde1.a918.7ebb.6092.cf67.bf2b.13fd.65f2.
       7088.d78b.7e88.3c87.59d2.c4f5.c65a.db75.
       5387.8ad5.75f9.fad8.78e8.0a0c.9ba6.3bcb.
       cc27.32e6.9485.bbc9.c90b.fbd6.2481.d908.
       9bec.cf80.cfe2.df16.a2cf.65bd.92dd.597b.
       0707.e091.7af4.8bbb.75fe.d413.d238.f555.
       5a7a.569d.80c3.414a.8d08.59dc.65a4.6128.
       bab2.7af8.7a71.314f.318c.782b.23eb.fe80.
       8b82.b0ce.2640.1d2e.22f0.4d83.d125.5dc5.
       1add.d3b7.5a2b.1ae0.7845.04df.543a.f896.
       9be3.ea70.82ff.7fc9.888c.144d.a2af.5842.
       9ec9.6031.dbca.d3da.d9af.0dcb.aaaf.268c.
       b8fc.ffea.d94f.3c7c.a495.e056.a9b4.7acd.
       b751.fb73.e666.c6c6.55ad.e829.7297.d07a.
       d1ba.5e43.f1bc.a323.0165.1339.e229.04cc.
       8c42.f58c.30c0.4aaf.db03.8dda.0847.dd98.
       8dcd.a6f3.bfd1.5c4b.4c45.2500.4aa0.6eef.
       f8ca.6178.3aac.ec57.fb3d.1f92.b0fe.2fd1.
       a85f.6724.517b.65e6.14ad.6808.d6f6.ee34.
       dff7.310f.dc82.aebf.d904.b01e.1dc5.4b29.
       2709.4b2d.b68d.6f90.3b68.401a.debf.5a7e.
       08d7.8ff4.ef5d.6365.3a65.040c.f9bf.d4ac.
       a798.4a74.d371.4598.6780.fc0b.16ac.4516.
       49de.6188.a7db.df19.1f64.b5fc.5e2a.b47b.
       57f7.f727.6cd4.19c1.7a3c.a8e1.b939.ae49.
       e488.acba.6b96.5610.b548.0109.c8b1.7b80.
       e1b7.b750.dfc7.598d.5d50.11fd.2dcc.5600.
       a32e.f5b5.2a1e.cc82.0e30.8aa3.4272.1aac.
       0943.bf66.86b6.4b25.7937.6504.ccc4.93d9.
       7e6a.ed3f.b0f9.cd71.a43d.d497.f01f.17c0.
       e2cb.3797.aa2a.2f25.6656.168e.6c49.6afc.
       5fb9.3246.f6b1.1163.98a3.46f1.a641.f3b0.
       41e9.89f7.914f.90cc.2c7f.ff35.7876.e506.
       b50d.334b.a77c.225b.c307.ba53.7152.f3f1.
       610e.4eaf.e595.f6d9.d90d.11fa.a933.a15e.
       f136.9546.868a.7f3a.45a9.6768.d40f.d9d0.
       3412.c091.c631.5cf4.fde7.cb68.6069.3738.
       0db2.eaaa.707b.4c41.85c3.2edd.cdd3.0670.
       5e4d.c1ff.c872.eeee.475a.64df.ac86.aba4.
       1c06.1898.3f87.41c5.ef68.d3a1.01e8.a3b8.
       cac6.0c90.5c15.fc91.0840.b94c.00a0.b9d0
      ::
      0xaab.4c90.0501.b3e2.4d7c.df46.6332.6a3a.
       87df.5e48.43b2.cbdb.67cb.f6e4.60fe.c350.
       aa53.71b1.508f.9f45.28ec.ea23.c436.d94b.
       5e8f.cd4f.681e.30a6.ac00.a970.4a18.8a03
    ==
  --
--
