::  tests for secp256k1 elliptic curve cryptography
::
/+  *test
=/  ecc  secp256k1:secp:crypto
|%
::  from libsecp256k1 src/modules/recovery/tests_impl.h
::  there are more tests there, ports would be welcome
++  test-ecdsa-recovery-end-to-end
  =/  util
    =/  eny=@  'ecdsa recovery test "entropy"'
    =/  rnd  ~(. og eny)
    =/  dom  t.ecc
    |%
    ++  random-scalar-order
      =*  core  .
      =^  z  rnd  (rads:rnd (dec n.dom))
      [`@`.+(z) core]
    --
  ::  generate a random key and message
  %+  category  "random"
  %-  zing
  =|  [i=@ out=(list tang)]
  |-  ^+  out
  ?:  =(i 64)  out
  =^  message  util  random-scalar-order:util
  =^  privkey  util  random-scalar-order:util
  =/  pubkey   (priv-to-pub.ecc privkey)
  =/  msghash  (shax (shax message))
  =/  sig      (ecdsa-raw-sign.ecc msghash privkey)
  =/  reckey   (ecdsa-raw-recover.ecc msghash sig)
  %=  $
    i    .+(i)
    out  :_  out
         %+  expect-eq
           !>  pubkey
           !>  reckey
  ==
  ::
++  test-ecdsa-recovery-edge-cases
  =<  %+  category  "edge cases"
     (zing ~[t1 t2 t3 t4 t5])
  =/  msg32=@  '...egassem terces yrev a si sihT'
  =/  r=@ux    0x67cb.285f.9cd1.94e8.
                 40d6.2939.7af5.5696.
                 62fd.e446.4999.5963.
                 179a.7dd1.7bd2.3532
  =/  s=@ux    0x4b1b.7df3.4ce1.f68e.
                 694f.f6f1.1ac7.51dd.
                 7dd7.3e38.7ee4.fc86.
                 6e1b.e8ec.c7dd.9557
  =/  r   %+  turn  (gulf 0 3)
          |=  v=@
          (mule |.((ecdsa-raw-recover.ecc msg32 v r s)))
  =/  t1  %+  expect-eq
            !>  %.n
            !>  -.&1.r
  =/  t3  %+  expect-eq
            !>  %.n
            !>  -.&3.r
  =/  t4  %+  expect-eq
            !>  %.n
            !>  -.&4.r
  =/  t2  %+  expect-eq
            !>  :+  %.y
                  0x8687.4a6b.24a7.5462.
                    7116.560e.7ae1.5cd6.
                    9eb3.3e73.b4d8.c810.
                    33b2.7c2f.a9cf.5d1c
                  0xe13f.19fa.8dea.0d1a.
                    e3e8.4c91.146c.3386.
                    8f87.730e.31bb.486e.
                    b370.05d1.40cc.7a55
            !>  &2.r
  ::  (4,4) should recover with all 4 recids
  :_  .
  ^=  t5
  %-  expect-eq  :_
    !>  %+  turn  (gulf 0 3)
        |=  v=@
        (mule |.((ecdsa-raw-recover.ecc msg32 v 4 4)))
    !>
    :~  :+  %.y
           0x8a3d.70c0.4104.68e4.
             5739.39af.01b9.9ea7.
             b206.4910.6d55.acf9.
             f558.eba2.8ed5.9a2e
           0x77eb.58dd.36ed.385b.
             3dcf.e7d3.62c8.16f3.
             7d3b.ef3e.4a34.94b8.
             6fcc.8357.5184.9329
         :+  %.y
           0x3e99.0254.a50d.6599.
             26c9.28ef.8b54.181e.
             e67e.27ff.bf63.eb69.
             294b.9ab6.d27b.a225
           0xa898.847e.931e.9b10.
             2c0f.9b0f.9597.07ba.
             f9b8.5e93.6425.fc72.
             e80c.a868.e535.dfb4
         :+  %.y
           0x7e15.24fa.06ba.fd6e.
             b9c0.2f27.9e13.1314.
             be93.0570.0fc6.9e80.
             d54d.29ab.3606.3f23
           0x3f86.a967.33e7.723d.
             fdde.4e03.382d.8c45.
             3493.fa88.9050.5ba5.
             cfc4.0a8b.226b.1b00
         :+  %.y
           0xb337.c9b7.4ca9.9ea9.
             63c6.560d.2558.cdf0.
             9c73.0120.8409.649a.
             8a6d.1fb1.0e1c.b946
           0x11df.5391.ee11.6de0.
             a722.bc0f.be5f.6575.
             3d07.03a9.9925.0581.
             f7de.cd5e.f0f4.f809
    ==
++  test-schnorr
  =>  |%
      +$  case-sec
        $:  sec=@
            pub=@
            aux=@
            mes=@
            sig=@
        ==
      +$  case-pub
        $:  pub=@
            mes=@
            sig=@
            res=?
        ==
      --
  =<  %+  category  "bip-0340 vectors"
      (zing :(weld t1 t2 t3))
  =/  cases-sec=(list case-sec)
    :~
      :*  0x3
          0xf930.8a01.9258.c310.4934.4f85.f89d.5229.
            b531.c845.836f.99b0.8601.f113.bce0.36f9
          0
          0
          0xe907.831f.8084.8d10.69a5.371b.4024.1036.
            4bdf.1c5f.8307.b008.4c55.f1ce.2dca.8215.
            25f6.6a4a.85ea.8b71.e482.a74f.382d.2ce5.
            ebee.e8fd.b217.2f47.7df4.900d.3105.36c0
      ==
      :*  0xb7e1.5162.8aed.2a6a.bf71.5880.9cf4.f3c7.
            62e7.160f.38b4.da56.a784.d904.5190.cfef
          0xdff1.d77f.2a67.1c5f.3618.3726.db23.41be.
            58fe.ae1d.a2de.ced8.4324.0f7b.502b.a659
          1
          0x243f.6a88.85a3.08d3.1319.8a2e.0370.7344.
            a409.3822.299f.31d0.082e.fa98.ec4e.6c89
          0x6896.bd60.eeae.296d.b48a.229f.f71d.fe07.
            1bde.413e.6d43.f917.dc8d.cf8c.78de.3341.
            8906.d11a.c976.abcc.b20b.0912.92bf.f4ea.
            897e.fcb6.39ea.871c.fa95.f6de.339e.4b0a
      ==
      :*  0xc90f.daa2.2168.c234.c4c6.628b.80dc.1cd1.
            2902.4e08.8a67.cc74.020b.bea6.3b14.e5c9
          0xdd30.8afe.c577.7e13.121f.a72b.9cc1.b7cc.
            0139.7153.09b0.86c9.60e1.8fd9.6977.4eb8
          0xc87a.a538.24b4.d7ae.2eb0.35a2.b5bb.bccc.
            080e.76cd.c6d1.692c.4b0b.62d7.98e6.d906
          0x7e2d.58d8.b3bc.df1a.bade.c782.9054.f90d.
            da98.05aa.b56c.7733.3024.b9d0.a508.b75c
          0x5831.aaee.d7b4.4bb7.4e5e.ab94.ba9d.4294.
            c49b.cf2a.6072.8d8b.4c20.0f50.dd31.3c1b.
            ab74.5879.a5ad.954a.72c4.5a91.c3a5.1d3c.
            7ade.a98d.82f8.481e.0e1e.0367.4a6f.3fb7
      ==
      :*  0xb43.2b26.7793.7381.aef0.5bb0.2a66.ecd0.
            1277.3062.cf3f.a254.9e44.f58e.d240.1710
          0x25d1.dff9.5105.f525.3c40.22f6.28a9.96ad.
            3a0d.95fb.f21d.468a.1b33.f8c1.60d8.f517
          0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
            ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff
          0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
            ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff
          0x7eb0.5097.57e2.46f1.9449.8856.5161.1cb9.
            65ec.c1a1.87dd.51b6.4fda.1edc.9637.d5ec.
            9758.2b9c.b13d.b393.3705.b32b.a982.af5a.
            f25f.d788.81eb.b327.71fc.5922.efc6.6ea3
      ==
    ==
  =/  t1
    %+  turn  cases-sec
    |=  case-sec
    ^-  tang
    %+  expect-eq
      !>  sig
      !>  (sign:schnorr:ecc sec mes aux)
  =/  t2
    %+  turn  cases-sec
    |=  case-sec
    ^-  tang
    %-  expect
    !>  (verify:schnorr:ecc pub mes sig)
  =/  cases-pub=(list case-pub)
    :~
      :*  0xd69c.3509.bb99.e412.e68b.0fe8.544e.7283.
            7dfa.3074.6d8b.e2aa.6597.5f29.d22d.c7b9
          0x4df3.c3f6.8fcc.83b2.7e9d.42c9.0431.a724.
            99f1.7875.c81a.599b.566c.9889.b969.6703
          0x3b.78ce.563f.89a0.ed94.14f5.aa28.ad0d.
            96d6.795f.9c63.76af.b154.8af6.03b3.eb45.
            c9f8.207d.ee10.60cb.71c0.4e80.f593.060b.
            07d2.8308.d7f4
          %.y
      ==
      :*  0xeefd.ea4c.db67.7750.a420.fee8.07ea.cf21.
            eb98.98ae.79b9.7687.66e4.faa0.4a2d.4a34
          0x243f.6a88.85a3.08d3.1319.8a2e.0370.7344.
            a409.3822.299f.31d0.082e.fa98.ec4e.6c89
          0x6cff.5c3b.a86c.69ea.4b73.76f3.1a9b.cb4f.
            74c1.9760.89b2.d996.3da2.e554.3e17.7769.
            69e8.9b4c.5564.d003.4910.6b84.9778.5dd7.
            d1d7.13a8.ae82.b32f.a79d.5f7f.c407.d39b
          %.n
      ==
      :*  0xdff1.d77f.2a67.1c5f.3618.3726.db23.41be.
            58fe.ae1d.a2de.ced8.4324.0f7b.502b.a659
          0x243f.6a88.85a3.08d3.1319.8a2e.0370.7344.
            a409.3822.299f.31d0.082e.fa98.ec4e.6c89
          0xfff9.7bd5.755e.eea4.2045.3a14.3552.35d3.
            82f6.472f.8568.a18b.2f05.7a14.6029.7556.
            3cc2.7944.640a.c607.cd10.7ae1.0923.d9ef.
            7a73.c643.e166.be5e.beaf.a34b.1ac5.53e2
          %.n
      ==
      :*  0xdff1.d77f.2a67.1c5f.3618.3726.db23.41be.
            58fe.ae1d.a2de.ced8.4324.0f7b.502b.a659
          0x243f.6a88.85a3.08d3.1319.8a2e.0370.7344.
            a409.3822.299f.31d0.082e.fa98.ec4e.6c89
          0x1fa6.2e33.1edb.c21c.3947.92d2.ab11.00a7.
            b432.b013.df3f.6ff4.f99f.cb33.e0e1.515f.
            2889.0b3e.db6e.7189.b630.448b.515c.e4f8.
            622a.954c.fe54.5735.aaea.5134.fccd.b2bd
          %.n
      ==
      :*  0xdff1.d77f.2a67.1c5f.3618.3726.db23.41be.
            58fe.ae1d.a2de.ced8.4324.0f7b.502b.a659
          0x243f.6a88.85a3.08d3.1319.8a2e.0370.7344.
            a409.3822.299f.31d0.082e.fa98.ec4e.6c89
          0x6cff.5c3b.a86c.69ea.4b73.76f3.1a9b.cb4f.
            74c1.9760.89b2.d996.3da2.e554.3e17.7769.
            9617.64b3.aa9b.2ffc.b6ef.947b.6887.a226.
            e8d7.c93e.00c5.ed0c.1834.ff0d.0c2e.6da6
          %.n
      ==
      :*  0xdff1.d77f.2a67.1c5f.3618.3726.db23.41be.
            58fe.ae1d.a2de.ced8.4324.0f7b.502b.a659
          0x243f.6a88.85a3.08d3.1319.8a2e.0370.7344.
            a409.3822.299f.31d0.082e.fa98.ec4e.6c89
          0x123d.da83.28af.9c23.a94c.1fee.cfd1.23ba.
            4fb7.3476.f0d5.94dc.b65c.6425.bd18.6051
          %.n
      ==
      :*  0xdff1.d77f.2a67.1c5f.3618.3726.db23.41be.
            58fe.ae1d.a2de.ced8.4324.0f7b.502b.a659
          0x243f.6a88.85a3.08d3.1319.8a2e.0370.7344.
            a409.3822.299f.31d0.082e.fa98.ec4e.6c89
          0x1.7615.fbaf.5ae2.8864.013c.0997.42de.
            adb4.dba8.7f11.ac67.54f9.3780.d5a1.837c.
            f197
          %.n
      ==
      :*  0xdff1.d77f.2a67.1c5f.3618.3726.db23.41be.
            58fe.ae1d.a2de.ced8.4324.0f7b.502b.a659
          0x243f.6a88.85a3.08d3.1319.8a2e.0370.7344.
            a409.3822.299f.31d0.082e.fa98.ec4e.6c89
          0x4a29.8dac.ae57.395a.15d0.795d.dbfd.1dcb.
            564d.a82b.0f26.9bc7.0a74.f822.0429.ba1d.
            69e8.9b4c.5564.d003.4910.6b84.9778.5dd7.
            d1d7.13a8.ae82.b32f.a79d.5f7f.c407.d39b
          %.n
      ==
      :*  0xdff1.d77f.2a67.1c5f.3618.3726.db23.41be.
            58fe.ae1d.a2de.ced8.4324.0f7b.502b.a659
          0x243f.6a88.85a3.08d3.1319.8a2e.0370.7344.
            a409.3822.299f.31d0.082e.fa98.ec4e.6c89
          0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
            ffff.ffff.ffff.ffff.ffff.fffe.ffff.fc2f.
            69e8.9b4c.5564.d003.4910.6b84.9778.5dd7.
            d1d7.13a8.ae82.b32f.a79d.5f7f.c407.d39b
          %.n
      ==
      :*  0xdff1.d77f.2a67.1c5f.3618.3726.db23.41be.
            58fe.ae1d.a2de.ced8.4324.0f7b.502b.a659
          0x243f.6a88.85a3.08d3.1319.8a2e.0370.7344.
            a409.3822.299f.31d0.082e.fa98.ec4e.6c89
          0x6cff.5c3b.a86c.69ea.4b73.76f3.1a9b.cb4f.
            74c1.9760.89b2.d996.3da2.e554.3e17.7769.
            ffff.ffff.ffff.ffff.ffff.ffff.ffff.fffe.
            baae.dce6.af48.a03b.bfd2.5e8c.d036.4141
          %.n
      ==
      :*  0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
            ffff.ffff.ffff.ffff.ffff.fffe.ffff.fc30
          0x243f.6a88.85a3.08d3.1319.8a2e.0370.7344.
            a409.3822.299f.31d0.082e.fa98.ec4e.6c89
          0x6cff.5c3b.a86c.69ea.4b73.76f3.1a9b.cb4f.
            74c1.9760.89b2.d996.3da2.e554.3e17.7769.
            69e8.9b4c.5564.d003.4910.6b84.9778.5dd7.
            d1d7.13a8.ae82.b32f.a79d.5f7f.c407.d39b
          %.n
      ==
    ==
  :_  .
  ^=  t3
    %+  turn  cases-pub
    |=  case-pub
    ^-  tang
    %+  expect-eq
      !>  res
      !>  (verify:schnorr:ecc pub mes sig)
++  test-schnorr-bounds
  =>  |%  +$  case  [sec=@ pub=@ aux=@ mes=@ sig=@]  --
  =<  %+  category  "bounds"
      (zing (weld t1 t2))
  =/  too-big
    0xff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
      ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff
  =/  big-sig
    0xff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
      ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
      ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
      ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff
  =/  cases-big-sec=(list case)
    :~  [too-big 0 0 0 0]
        [1 0 too-big 0 0]
        [1 0 0 too-big 0]
    ==
  =/  cases-big-pub=(list case)
    :~  [0 too-big 0 0 0]
        [0 0 0 too-big 0]
        [0 0 0 0 big-sig]
    ==
  =/  t1
    %+  turn  cases-big-sec
    |=  case
    %-  expect-fail
    |.  (sign:schnorr:ecc sec mes aux)
  :_  .
  ^=  t2
    %+  turn  cases-big-pub
    |=  case
    %-  expect-fail
    |.  (verify:schnorr:ecc pub mes sig)
--
