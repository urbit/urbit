/+  *test
|%
++  test-muk
  ;:  weld
    %+  expect-eq
      !>  0xfaf6.cdb3
      !>  (muk 1.234 13 'Hello, world!')
    ::
    %+  expect-eq
      !>  0xbf50.5788
      !>  (muk 4.321 13 'Hello, world!')
    ::
    %+  expect-eq
      !>  0xf2c.c00b
      !>  (muk 1.234 0 0)
    ::
    %+  expect-eq
      !>  0x8905.ac28
      !>  (muk 1.234 28 (crip (reap 28 'x')))
    ::
    %+  expect-eq
      !>  0x566f.7173
      !>  (muk 0xcafe.babe 16 (dec (bex 128)))
  ==
::
++  test-mug
  ;:  weld
    %+  expect-eq
      !>  0x4d44.1035
      !>  (mug 'Hello, world!')
    ::
    %+  expect-eq
      !>  0x79ff.04e8
      !>  (mug 0)
    ::
    %+  expect-eq
      !>  0x64df.da5c
      !>  (mug (crip (reap 28 'x')))
    ::
    %+  expect-eq
      !>  0x389c.a03a
      !>  (mug [0 0])
    ::
    %+  expect-eq
      !>  0x389c.a03a
      !>  (mug [1 1])
    ::
    %+  expect-eq
      !>  0x5258.a6c0
      !>  (mug [0 (bex 32)])
    ::
    %+  expect-eq
      !>  0x2ad3.9968
      !>  (mug [(dec (bex 128)) 1])
  ==
::  SHA tests
::  For references see FIPS180-4 and related test vectors
::  https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf
::  https://csrc.nist.gov/projects/cryptographic-algorithm-validation-program
::
++  test-sha-1
  =/  a  ''
  =/  b  'abc'
  =/  c  'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'
  =/  d  'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn\
         /hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'
  ;:  weld
    %+  expect-eq
      !>  0xda39.a3ee.5e6b.4b0d.3255.bfef.9560.1890.afd8.0709
      !>  `@ux`(sha-1:sha (swp 3 a))
    ::
    %+  expect-eq
      !>  0xa999.3e36.4706.816a.ba3e.2571.7850.c26c.9cd0.d89d
      !>  `@ux`(sha-1:sha (swp 3 b))
    ::
    %+  expect-eq
      !>  0x8498.3e44.1c3b.d26e.baae.4aa1.f951.29e5.e546.70f1
      !>  `@ux`(sha-1:sha (swp 3 c))
    ::
    %+  expect-eq
      !>  0xa49b.2446.a02c.645b.f419.f995.b670.9125.3a04.a259
      !>  `@ux`(sha-1:sha (swp 3 d))
    ::
  ==
::
++  test-sha-1l
  =/  a  0x0
  =/  b  0x549e.959e
  =/  c  0x7c.9c67.323a.1df1.adbf.e5ce.b415.eaef.0155.ece2.820f.4d50.c1ec.22cb.
         a492.8ac6.56c8.3fe5.85db.6a78.ce40.bc42.757a.ba7e.5a3f.5824.28d6.ca68.
         d0c3.9783.36a6.efb7.2961.3e8d.9979.0162.04bf.d921.322f.dd52.2218.3554.
         447d.e5e6.e9bb.e6ed.f76d.7b71.e18d.c2e8.d6dc.89b7.3983.64f6.52fa.fc73.
         4329.aafa.3dcd.45d4.f31e.388e.4faf.d7fc.6495.f37c.a5cb.ab7f.54d5.8646.
         3da4.bfea.a3ba.e09f.7b8e.9239.d832.b4f0.a733.aa60.9cc1.f8d4
  =/  d  0x938.f2e2.ebb6.4f8a.f8bb.fc91
  ;:  weld
    ::  Empty string
    ::
    %+  expect-eq
      !>  0xda39.a3ee.5e6b.4b0d.3255.bfef.9560.1890.afd8.0709
      !>  `@ux`(sha-1l:sha [(met 3 a) a])
    ::  Short Message
    ::
    %+  expect-eq
      !>  0xb78b.ae6d.1433.8ffc.cfd5.d5b5.674a.275f.6ef9.c717
      !>  `@ux`(sha-1l:sha [(met 3 b) b])
    ::  Long Message
    ::
    %+  expect-eq
      !>  0xd8fd.6a91.ef3b.6ced.05b9.8358.a991.07c1.fac8.c807
      !>  `@ux`(sha-1l:sha [(met 3 c) c])
    ::  Leading-zero byte
    ::
    %+  expect-eq
      !>  0x9f4e.66b6.ceea.40dc.f4b9.166c.28f1.c884.7414.1da9
      !>  `@ux`(sha-1l:sha [(met 3 d) d])
    ::
  ==
::
++  test-shay
  =/  a  0x0
  =/  b  0xb4.190e
  =/  c  0x45.1101.250e.c6f2.6652.249d.59dc.974b.7361.d571.a810.1cdf.d36a.ba3b.
         5854.d3ae.086b.5fdd.4597.721b.66e3.c0dc.5d8c.606d.9657.d0e3.2328.3a52.
         17d1.f53f.2f28.4f57.b85c.8a61.ac89.2471.1f89.5c5e.d90e.f177.45ed.2d72.
         8abd.22a5.f7a1.3479.a462.d71b.56c1.9a74.a40b.655c.58ed.fe0a.188a.d2cf.
         46cb.f305.24f6.5d42.3c83.7dd1.ff2b.f462.ac41.9800.7345.bb44.dbb7.b1c8.
         6129.8cdf.6198.2a83.3afc.728f.ae1e.da2f.87aa.2c94.8085.8bec
  =/  d  0x777.fc1e.1ca4.7304.c2e2.6569.2838.109e.26aa.b9e5.c4ae.4e86.00df.4b1f
  ;:  weld
    ::  Empty string
    ::
    %+  expect-eq
      !>  0xe3b0.c442.98fc.1c14.9afb.f4c8.996f.b924.27ae.41e4.649b.934c.a495.
            991b.7852.b855
      !>  `@ux`(sha-256l:sha [(met 3 a) a])
    ::  Short Message
    ::
    %+  expect-eq
      !>  0xdff2.e730.91f6.c05e.5288.96c4.c831.b944.8653.dc2f.f043.528f.6769.
            437b.c7b9.75c2
      !>  `@ux`(sha-256l:sha [(met 3 b) b])
    ::  Long Message
    ::
    %+  expect-eq
      !>  0x3c59.3aa5.39fd.cdae.516c.df2f.1500.0f66.3418.5c88.f505.b397.75fb.
            9ab1.37a1.0aa2
      !>  `@ux`(sha-256l:sha [(met 3 c) c])
    ::  Leading-zero byte
    ::
    %+  expect-eq
      !>  0xffb4.fc03.e054.f8ec.bc31.470f.c023.bedc.d4a4.06b9.dd56.c71d.a1b6.
            60dc.c484.2c65
      !>  `@ux`(sha-256l:sha [(met 3 d) d])
  ==
++  test-shal
  =/  a  0x0
  =/  b  0x23be.86d5
  =/  c  0x4f.0560.0950.664d.5190.a2eb.c29c.9edb.89c2.0079.a4d3.e6bc.3b27.d75e.
         34e2.fa3d.0276.8502.bd69.7900.7859.8d5f.cf3d.6779.bfed.1284.bbe5.ad72.
         fb45.6015.181d.9587.d6e8.64c9.4056.4eaa.fb4f.2fea.d434.6ea0.9b68.77d9.
         340f.6b82.eb15.1588.0872.213d.a3ad.88fe.ba9f.4f13.817a.71d6.f90a.1a17.
         c43a.15c0.38d9.88b5.b29e.dffe.2d6a.0628.13ce.dbe8.52cd.e302.b3e3.3b69.
         6846.d2a8.e36b.d680.efcc.6cd3.f9e9.a4c1.ae8c.ac10.cc52.44d1.3167.7140.
         3991.76ed.4670.0019.a004.a163.806f.7fa4.67fc.4e17.b461.7bbd.7641.aaff.
         7ff5.6396.ba8c.08a8.be10.0b33.a20b.5daf.134a.2aef.a5e1.c349.6770.dcf6.
         baa4.f7bb
  =/  d  0xa.55db
  ;:  weld
    ::  Empty string
    ::
    %+  expect-eq
      !>  0xcf83.e135.7eef.b8bd.f154.2850.d66d.8007.d620.e405.0b57.15dc.83f4.
            a921.d36c.e9ce.47d0.d13c.5d85.f2b0.ff83.18d2.877e.ec2f.63b9.31bd.
            4741.7a81.a538.327a.f927.da3e
      !>  `@ux`(sha-512l:sha [(met 3 a) a])
    ::  Short message
    ::
    %+  expect-eq
      !>  0x76d4.2c8e.adea.35a6.9990.c63a.762f.3306.14a4.6999.77f0.58ad.b988.
            f406.fb0b.e8f2.ea3d.ce3a.2bbd.1d82.7b70.b9b2.99ae.6f9e.5058.ee97.
            b50b.d492.2d6d.37dd.c761.f8eb
      !>  `@ux`(sha-512l:sha [(met 3 b) b])
    ::  Long message
    ::
    %+  expect-eq
      !>  0xa9db.490c.708c.c725.48d7.8635.aa7d.a79b.b253.f945.d710.e5cb.677a.
            474e.fc7c.65a2.aab4.5bc7.ca11.13c8.ce0f.3c32.e139.9de9.c459.535e.
            8816.521a.b714.b2a6.cd20.0525
      !>  `@ux`(sha-512l:sha [(met 3 c) c])
    ::  Leading-zero byte
    ::
    %+  expect-eq
      !>  0x7952.585e.5330.cb24.7d72.bae6.96fc.8a6b.0f7d.0804.577e.347d.99bc.
            1b11.e52f.3849.85a4.2844.9382.306a.8926.1ae1.43c2.f3fb.6138.04ab.
            20b4.2dc0.97e5.bf4a.96ef.919b
      !>  `@ux`(sha-512l:sha [(met 3 d) d])
    ::
  ==
::
++  test-shax
  =/  a  ''
  =/  b  'abc'
  =/  c  'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'
  ;:  weld
    ::  Empty string
    ::
    %+  expect-eq
      !>  0xe3b0.c442.98fc.1c14.9afb.f4c8.996f.b924.27ae.41e4.649b.934c.a495.
            991b.7852.b855
      !>  `@ux`(sha-256:sha (swp 3 a))
    ::  Short Message
    ::
    %+  expect-eq
      !>  0xba78.16bf.8f01.cfea.4141.40de.5dae.2223.b003.61a3.9617.7a9c.b410.
            ff61.f200.15ad
      !>  `@ux`(sha-256:sha (swp 3 b))
    ::  Long Message
    ::
    %+  expect-eq
      !>  0x248d.6a61.d206.38b8.e5c0.2693.0c3e.6039.a33c.e459.64ff.2167.f6ec.
            edd4.19db.06c1
      !>  `@ux`(sha-256:sha (swp 3 c))
    ::
  ==
::
++  test-shaz
  =/  a  ''
  =/  b  'abc'
  =/  c  'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijkl\
          /mnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'
  ;:  weld
    ::  Empty string
    ::
    %+  expect-eq
      !>  0xcf83.e135.7eef.b8bd.f154.2850.d66d.8007.d620.e405.0b57.15dc.83f4.
            a921.d36c.e9ce.47d0.d13c.5d85.f2b0.ff83.18d2.877e.ec2f.63b9.31bd.
            4741.7a81.a538.327a.f927.da3e
      !>  `@ux`(sha-512:sha (swp 3 a))
    ::  Short Message
    ::
    %+  expect-eq
      !>  0xddaf.35a1.9361.7aba.cc41.7349.ae20.4131.12e6.fa4e.89a9.7ea2.0a9e.
            eee6.4b55.d39a.2192.992a.274f.c1a8.36ba.3c23.a3fe.ebbd.454d.4423.
            643c.e80e.2a9a.c94f.a54c.a49f
      !>  `@ux`(sha-512:sha (swp 3 b))
    ::  Long Message
    ::
    %+  expect-eq
      !>  0x8e95.9b75.dae3.13da.8cf4.f728.14fc.143f.8f77.79c6.eb9f.7fa1.7299.
            aead.b688.9018.501d.289e.4900.f7e4.331b.99de.c4b5.433a.c7d3.29ee.
            b6dd.2654.5e96.e55b.874b.e909
      !>  `@ux`(sha-512:sha (swp 3 c))
    ::
  ==
::
++  test-sham
  ;:  weld
    %+  expect-eq
      !>  0v3.71s52.4bqnp.ki2b8.9hhsp.2ufgg
      !>  (sham [2 4])
    ::
    %+  expect-eq
      !>  0v1.hg8mv.t7s3f.u4f8a.q5noe.dvqvh
      !>  (sham "hello")
    ::
  ==
::
++  test-raw
  ;:  weld
    %+  expect-eq
      !>  0b1001
      !>  `@ub`(~(raw og 27) 4)
    ::
    %+  expect-eq
      !>  0b0
      !>  `@ub`(~(raw og 27) 3)
    ::
    %+  expect-eq
      !>  0b1111
      !>  `@ub`(~(raw og 11) 4)
    ::
    %+  expect-eq
      !>  0b100
      !>  `@ub`(~(raw og 11) 3)
    ::
  ==
::
++  test-raws
  =/  rng  ~(. og 7)
  =^  a  rng  (rads:rng 4)
  =^  b  rng  (rads:rng 4)
  %+  expect-eq
    !>  [0b10 0b1]
    !>  [`@ub`a `@ub`b]
::
++  test-rad
  ;:  weld
    %+  expect-eq
      !>  4
      !>  (~(rad og 5) 11)
    ::
    %+  expect-eq
      !>  2
      !>  (~(rad og 758.716.593) 11)
    ::
    %+  expect-eq
      !>  71.499
      !>  (~(rad og 1) 100.000)
    ::
  ==
::
++  test-rads
  =/  rng  ~(. og 7)
  =^  a  rng  (rads:rng 10)
  =^  b  rng  (rads:rng 10)
  %+  expect-eq
    !>  [2 8]
    !>  [a b]
::
++  test-shaw
  ;:  weld
    %+  expect-eq
      !>  0b11.0111
      !>  `@ub`(shaw 3 6 98)
    ::
    %+  expect-eq
      !>  0b11
      !>  `@ub`(shaw 2 6 98)
    ::
  ==
::  End SHA tests
--
