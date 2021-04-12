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
--
