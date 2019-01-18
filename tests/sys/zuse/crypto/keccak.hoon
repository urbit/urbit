::  tests for the keccak hashing algorithms
::
::  answers for keccak from the keccak team:
::  https://keccak.team/archives.html
::  (we swp the inputs because ++keccak wants to work with @t's, which are in
::   reverse byte order)
::
::  since all other hashing algorithms are implemented as keccak, their only
::  difference a single padding byte, we can safely test just one input.
::  answers for those pulled from the nist website:
::  https://csrc.nist.gov/projects/cryptographic-standards-and-guidelines/example-values
::
::  answer for rawshake-256 was found here:
::  https://github.com/maandree/libkeccak/blob/master/test.c
::  for rawshake-128, the implementation was assumed to be correct at the time
::  these tests were written.
::
::
/+  *test
=,  keccak:crypto
::
::  per bytelength, an example input.
=/  keccak-inputs=(map @ud @)
  =-  (~(run by -) (cury swp 3))
  %-  ~(gas by *(map @ud @))
  ^-  (list (pair @ud @))
  :~
    :-  0
    0x0
  ::
    :-  1
    0xcc
  ::
    :-  4
    0xc1ec.fdfc
  ::
    :-  8
    0x4a4f.2024.8451.2526
  ::
    :-  64
    0xe926.ae8b.0af6.e531.76db.ffcc.2a6b.88c6.
      bd76.5f93.9d3d.178a.9bde.9ef3.aa13.1c61.
      e31c.1e42.cdfa.f4b4.dcde.579a.37e1.50ef.
      bef5.555b.4c1c.b404.39d8.35a7.24e2.fae7
  ::
    :-  128
    0x2b6d.b7ce.d866.5ebe.9deb.0802.9521.8426.
      bdaa.7c6d.a9ad.d208.8932.cdff.baa1.c141.
      29bc.cdd7.0f36.9efb.1492.8585.8d2b.1d15.
      5d14.de2f.db68.0a8b.0272.8405.5182.a0ca.
      e275.234c.c9c9.2863.c1b4.ab66.f304.cf06.
      21cd.5456.5f5b.ff46.1d3b.461b.d40d.f281.
      98e3.7325.01b4.860e.add5.03d2.6d6e.6933.
      8f4e.0456.e9e9.baf3.d827.ae68.5fb1.d817
  ::
    :-  255
      0x3a.3a81.9c48.efde.2ad9.14fb.f00e.18ab.
      6bc4.f145.13ab.27d0.c178.a188.b614.31e7.
      f562.3cb6.6b23.3467.75d3.86b5.0e98.2c49.
      3adb.bfc5.4b9a.3cd3.8338.2336.a1a0.b215.
      0a15.358f.336d.03ae.18f6.66c7.573d.55c4.
      fd18.1c29.e6cc.fde6.3ea3.5f0a.df58.85cf.
      c0a3.d84a.2b2e.4dd2.4496.db78.9e66.3170.
      cef7.4798.aa1b.bcd4.574e.a0bb.a404.89d7.
      64b2.f83a.adc6.6b14.8b4a.0cd9.5246.c127.
      d587.1c4f.1141.8690.a5dd.f012.46a0.c80a.
      43c7.0088.b618.3639.dcfd.a412.5bd1.13a8.
      f49e.e23e.d306.faac.576c.3fb0.c1e2.5667.
      1d81.7fc2.534a.52f5.b439.f72e.424d.e376.
      f4c5.65cc.a823.07dd.9ef7.6da5.b7c4.eb7e.
      0851.72e3.2880.7c02.d011.ffbf.3378.5378.
      d79d.c266.f6a5.be6b.b0e4.a92e.ceeb.aeb1
  ==
::
|%
::
::  check a list of bytelength-answer pairs to see if
::  they match the output given by {hash} for the
::  corresponding example input from {keccak-inputs}.
++  verify-known-answers
  |=  $:  hash=$-(octs @)
          name=tape
          answers=(list (pair @ud @))
      ==
  ^-  tang
  ?~  answers  ~
  %+  weld  $(answers t.answers)
  =+  `[bytes=@ud answer=@]`i.answers
  %+  category  name
  %+  expect-eq
    !>  answer
    !>  (hash bytes (~(got by keccak-inputs) bytes))
::
::  keccak
::
++  test-keccak-224
  %^  verify-known-answers  keccak-224  "keccak-224"
  :~  :-  0
      0xf718.3750.2ba8.e108.37bd.d8d3.65ad.b855.
        9189.5602.fc55.2b48.b739.0abd
    ::
      :-  1
      0xa9ca.b59e.b40a.10b2.4629.0f2d.6086.e32e.
        3689.faf1.d26b.470c.899f.2802
    ::
      :-  4
      0xe405.869d.a146.4a70.5700.a3cb.ce13.1aab.
        eeba.9c8d.2fe6.576b.21bc.be16
    ::
      :-  8
      0x7a5c.2cb3.f999.dd00.eff7.3999.6331.4ca6.
        47dd.0e5a.e1bd.dec6.11f8.338d
    ::
      :-  64
      0xc533.dcf8.8cd1.a5df.f22b.914d.3875.bd57.
        fc17.b2e1.f474.ae36.0c38.77d2
    ::
      :-  128
      0xaf3e.0cc6.e645.01f1.0fd3.9722.e852.355f.
        d6d8.0d32.1906.31e2.f06c.22ad
    ::
      :-  255
      0x5af5.6987.ea9c.f11f.cd0e.ac5e.bc14.b037.
        365e.9b11.23e3.1cb2.dfc7.929a
  ==
::
++  test-keccak-256
  %^  verify-known-answers  keccak-256  "keccak-256"
  :~  :-  0
      0xc5d2.4601.86f7.233c.927e.7db2.dcc7.03c0.
        e500.b653.ca82.273b.7bfa.d804.5d85.a470
    ::
      :-  1
      0xeead.6dbf.c734.0a56.caed.c044.696a.1688.
        7054.9a6a.7f6f.5696.1e84.a54b.d997.0b8a
    ::
      :-  4
      0xb149.e766.d761.2eaf.7d55.f74e.1a4f.dd63.
        709a.8115.b14f.61fc.d22a.a4ab.c8b8.e122
    ::
      :-  8
      0xe620.d8f2.982b.24fe.daaa.3baa.9b46.c3f9.
        ce20.4ee3.5666.6553.ecb3.5e15.c3ff.9bf9
    ::
      :-  64
      0x5742.71cd.1395.9e8d.deae.5bfb.db02.a3fd.
        f54f.2bab.fd0c.beb8.9308.2a97.4957.d0c1
    ::
      :-  128
      0xd82e.257d.000d.c9fa.279a.00e2.961e.3286.
        d2fe.1c02.ef59.833a.b8a6.a710.1bc2.5054
    ::
      :-  255
      0x348f.b774.adc9.70a1.6b11.0566.9442.625e.
        6ada.a825.7a89.effd.b5a8.02f1.61b8.62ea
  ==
::
++  test-keccak-384
  %^  verify-known-answers  keccak-384  "keccak-384"
  :~  :-  0
      0x2c23.146a.63a2.9acf.99e7.3b88.f8c2.4eaa.
        7dc6.0aa7.7178.0ccc.006a.fbfa.8fe2.479b.
        2dd2.b213.6233.7441.ac12.b515.9119.57ff
    ::
      :-  1
      0x1b84.e62a.46e5.a201.8617.54af.5dc9.5c4a.
        1a69.caf4.a796.ae40.5680.161e.2957.2641.
        f5fa.1e86.41d7.9583.36ee.7b11.c58f.73e9
    ::
      :-  4
      0xf185.0b2a.bb24.f3fd.683c.7015.8278.9d9e.
        92b6.a45f.9c34.5f9d.ae7f.7997.c8c9.10e8.
        8003.e592.e592.81cf.92c9.2d6b.51a1.afd1
    ::
      :-  8
      0x638e.6575.8a29.7cb0.9ded.1ac5.b9e8.f779.
        8020.00ab.791f.67f3.3c60.be36.4437.93ad.
        cc8a.4a58.e986.8815.7a41.784f.02a4.bcb2
    ::
      :-  64
      0x14aa.679b.0c11.f9c3.63f5.4933.0261.b45e.
        1e90.ce31.f4a1.b0ce.5cb9.eb81.bd60.79a3.
        742d.8602.356c.5098.5d0d.3e54.0fdf.dcfb
    ::
      :-  128
      0x3ade.b7ee.ecf9.069f.143a.1015.1fd4.506a.
        eef3.a0ef.94ca.65d4.448a.cf1e.892b.8ebb.
        0887.6318.04dd.64e1.53ad.41fa.e012.7a85
    ::
      :-  255
      0x6bff.1c84.05a3.fe59.4e36.0e3b.ccea.1ebc.
        d509.310d.c79b.9e45.c263.783d.7a5d.d662.
        c678.9b18.bd56.7dbd.da15.54f5.bee6.a860
  ==
::
++  test-keccak-512
  %^  verify-known-answers  keccak-512  "keccak-512"
  :~  :-  0
       0xeab.42de.4c3c.eb92.35fc.91ac.ffe7.46b2.
        9c29.a8c3.66b7.c60e.4e67.c466.f36a.4304.
        c00f.a9ca.f9d8.7976.ba46.9bcb.e067.13b4.
        35f0.91ef.2769.fb16.0cda.b33d.3670.680e
    ::
      :-  1
      0x8630.c13c.bd06.6ea7.4bbe.7fe4.68fe.c1de.
        e10e.dc12.54fb.4c1b.7c5f.d69b.646e.4416.
        0b8c.e01d.05a0.908c.a790.dfb0.80f4.b513.
        bc3b.6225.ece7.a810.3714.41a5.ac66.6eb9
    ::
      :-  4
      0x952d.4c0a.6f0e.f5ce.438c.52e3.edd3.45ea.
        00f9.1cf5.da80.97c1.168a.1606.9e95.8fc0.
        5bad.90a0.c5fb.4dd9.ec28.e84b.226b.94a8.
        47d6.bb89.2356.92ef.4c97.12f0.c703.0fae
    ::
      :-  8
      0xf326.c7c1.26dd.c277.9227.60fe.ef77.c9ba.
        b6fb.5d34.30f6.5259.3703.d7c5.e301.35cd.
        0b05.7525.7509.a624.1843.30d6.ab1f.508a.
        6663.91b5.d469.0426.b4e0.5301.891d.f897
    ::
      :-  64
      0xc0a4.d8dc.a967.772d.bf6e.5508.c913.e7be.
        ba1b.749a.2b1a.c963.d067.6e6f.1dcd.4eba.
        a3f9.09ef.87dd.8498.82dc.8253.347a.5f65.
        20b5.b9f5.1097.3f44.3976.455f.923c.fcb9
    ::
      :-  128
      0xaebb.a57c.8ed5.af6e.c93f.4aa4.5772.ff51.
        67b7.ea88.dfa7.1364.f37d.8fc5.fdb7.dc3b.
        2c83.31a0.8023.f21d.110b.7d82.1e2d.c7e8.
        6082.6235.e7e6.2919.12ac.5213.8474.7354
    ::
      :-  255
      0x8195.0e70.96d3.1d4f.22e3.db71.cac7.25bf.
        59e8.1af5.4c7c.a9e6.aeee.71c0.10fc.5467.
        4663.12a0.1aa5.c137.cfb1.4064.6941.5567.
        96f6.12c9.3512.6873.7c7e.9a2b.9631.d1fa
  ==
::
::  sha3
::
++  test-sha3-224
  %+  expect-eq
    !>  0x6b4e.0342.3667.dbb7.3b6e.1545.4f0e.b1ab.
          d459.7f9a.1b07.8e3f.5b5a.6bc7
    !>  (sha3-224 0 `@`0)
::
++  test-sha3-256
  %+  expect-eq
    !>  0xa7ff.c6f8.bf1e.d766.51c1.4756.a061.d662.
          f580.ff4d.e43b.49fa.82d8.0a4b.80f8.434a
    !>  (sha3-256 0 `@`0)
::
++  test-sha3-384
  %+  expect-eq
    !>  0xc63.a75b.845e.4f7d.0110.7d85.2e4c.2485.
          c51a.50aa.aa94.fc61.995e.71bb.ee98.3a2a.
          c371.3831.264a.db47.fb6b.d1e0.58d5.f004
    !>  (sha3-384 0 `@`0)
::
++  test-sha3-512
  %+  expect-eq
    !>  0xa69f.73cc.a23a.9ac5.c8b5.67dc.185a.756e.
          97c9.8216.4fe2.5859.e0d1.dcc1.475c.80a6.
          15b2.123a.f1f5.f94c.11e3.e940.2c3a.c558.
          f500.199d.95b6.d3e3.0175.8586.281d.cd26
    !>  (sha3-512 0 `@`0)
  ::
::  shake
::
++  test-shake-128
  %+  expect-eq
    !>  0x7f9c.2ba4.e88f.827d.6160.4550.7605.853e.
          d73b.8093.f6ef.bc88.eb1a.6eac.fa66.ef26.
          3cb1.eea9.8800.4b93.103c.fb0a.eefd.2a68.
          6e01.fa4a.58e8.a363.9ca8.a1e3.f9ae.57e2
    !>  (shake-128 512 0 `@`0)
::
++  test-shake-256
  %+  expect-eq
    !>  0x46b9.dd2b.0ba8.8d13.233b.3feb.743e.eb24.
          3fcd.52ea.62b8.1b82.b50c.2764.6ed5.762f.
          d75d.c4dd.d8c0.f200.cb05.019d.67b5.92f6.
          fc82.1c49.479a.b486.4029.2eac.b3b7.c4be
    !>  (shake-256 512 0 `@`0)
::
++  test-rawshake-128
  %+  expect-eq
    !>  0xfa01.9a3b.1763.0df6.0148.53b5.4707.73f1.
          3c3a.b704.4782.11d7.a658.6751.5dea.1cc7.
          926b.2147.e396.076b.22cb.7263.3af5.0647.
          c7f2.3d0d.8f00.1d6d.8daf.0f6f.2e92.fc0e
    !>  (rawshake-128 512 0 `@`0)
::
++  test-rawshake-256
  %+  expect-eq
    !>  0x3a11.08d4.a90a.31b8.5a10.bdce.77f4.bfbd.
          cc5b.1d70.dd40.5686.f8bb.de83.4aa1.a410.
          db8c.9e1c.166c.3e23.9cd7.6a55.f6a6.92aa.
          2d17.49f2.ec79.cd0b.a3b1.7bb6.5995.9b6e
    !>  (rawshake-256 512 0 `@`0)
--
