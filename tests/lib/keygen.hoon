::  tests for keygen-hoon
::
::  largely copied from keygen-js here:
::  https://github.com/urbit/keygen-js/blob/master/test/test.js
::
/+  *test, keygen
=,  keygen
::
|%
++  test-child-seed-from-seed-0
  %+  expect-eq
    !>  ;:  weld
          "bonus favorite swallow panther frequent random "
          "essence loop motion apology skull ginger "
          "subject exchange please series meadow tree "
          "latin smile bring process excite tornado"
        ==
    !>  %+  from-entropy:bip39  32
        %+  seed:ds
          ^-  byts
          :-  32
          0xb2bd.f8de.8452.b18f.0219.5b6e.7bfc.82b9.
            00fb.cc25.681f.07ae.10f3.8f11.e5af.53af
        "management"
::
++  test-child-seed-from-seed-1
  %+  expect-eq
    !>  ;:  weld
          "impact keep magnet two rice country "
          "girl jungle cabin mystery usual tree "
          "horn skull winter palace supreme reform "
          "sphere cabbage cry athlete puppy misery"
        ==
    !>  %+  from-entropy:bip39  32
        %+  seed:ds
          ^-  byts
          :-  32
          0xb2bd.f8de.8452.b18f.0219.5b6e.7bfc.82b9.
            00fb.cc25.681f.07ae.10f3.8f11.e5af.53af
        "ownership"
::
++  test-urbit-keys-from-seed-0
  %+  expect-eq
    !>  :-  :-  0x9a09.b7e8.1646.7b10.ebdc.d47d.7186.1a2d.
                  0896.189f.2e66.690b.636a.d3bd.f8fc.c343
                0x52c8.30cd.009a.4c65.9977.8b25.8fa8.898c.
                  b8b4.9dd7.1279.c7ca.502c.b04c.2f53.0d7a
            :-  0xf718.7602.dff5.e3ee.a27b.4c46.3686.0110.
                  6916.d704.a2ef.2e45.1838.d4ea.80b3.95e0
                0x9c51.3a22.7951.4766.1234.eea1.3ee5.fa5b.
                  1a8b.9bed.1aa4.b1cf.87f6.bcf3.53af.a8bb
    !>  %-  urbit:ds
        0x8835.9ba6.1d76.6e1c.2ec9.5988.3166.8d42.
          33b0.f8f5.8b29.da8c.f33d.25b2.590d.62a0
::
++  test-urbit-keys-from-seed-1
  %+  expect-eq
    !>  :-  :-  0x76fa.3a85.8dfe.b18f.d7cd.1a63.b2fe.84f9.
                  5840.e3e5.b19e.0e69.5069.2cbd.c55f.3821
                0x2561.9a9c.e945.8f86.9f06.4ca5.6937.8528.
                  3a20.aa7a.8433.baf5.fccc.bb0a.84f7.6cae
            :-  0xfade.a0a3.7e6d.111e.b080.6648.fa66.d689.
                  f200.f38c.fd1e.3fe6.83fc.148d.76c6.9d27
                0x972b.d3bc.056a.c414.a3c3.8bc9.48a7.4506.
                  120b.2fc8.4ae2.2ba6.05c9.eeef.2b7c.c1db
    !>  %-  urbit:ds
        0x52dc.7422.d68c.0209.6105.02e7.1009.d6e9.
          f054.da2a.ccaf.b612.180c.853f.3f77.d606
::
++  test-generate-wallet-0
  %+  expect-eq
    !>  ^-  vault
        :*  ::  ownership
            ^-  node
            :+  "ownership"
              ;:  weld
                "fabric impulse bone solar six turkey "
                "midnight loud pizza catalog diet slice "
                "problem hover market type light shift "
                "response fit fantasy glory member wisdom"
              ==
            :+  :-  0x2.53e3.cfd7.4b20.ac63.3d35.defa.4d42.1b8a.
                        99cc.5dbd.d180.b149.fe8b.dff8.f394.7c70
                0xbc48.4a9d.6056.1386.75fd.8f24.e5c1.bdda.
                  649c.7acb.3fcf.bc95.52ff.fcf1.4a20.023f
              0x9988.685f.c1e5.c407.9aae.a6a4.05e0.f289.56b1.bbbe
            0x6544.c8c4.3d87.5da4.b3bb.9871.d0d2.d1b8.
              253d.c6a1.e2c1.666b.ca86.5fe1.8543.828b
            ::  voting
            ^-  node
            :+  "voting"
              ;:  weld
                "noise vote enter doctor double charge "
                "above car tomato brick dutch miracle "
                "narrow seed aware harsh bottom armor "
                "bonus property census hybrid target symbol"
              ==
            :+  :-  0x2.8df2.5e7f.abe0.b2c7.4d9d.65dc.c800.7598.
                        a560.f863.498c.98b4.90da.420a.58e1.15c1
                0x479f.d82a.0695.563d.29b3.942b.f180.f461.
                  4a67.36f1.b731.b9df.3389.4bb6.6a79.ca28
              0x38a9.e280.124b.3ba1.eaac.4466.43b6.0752.a066.3769
            0x4f0.d00f.571a.833e.bd62.5764.24fd.0841.
              94c3.e148.8c16.2bb9.7922.7a03.fdc3.8d16
            ::  management
            ^-  node
            :+  "management"
              ;:  weld
                "veteran flash code abuse course front "
                "better grace fabric chef puzzle outside "
                "visual sauce spice auction cactus galaxy "
                "need view tomorrow spirit chimney course"
              ==
            :+  :-  0x3.bc94.9927.1df2.7b4e.90cd.13a6.3ba6.f48b.
                        88aa.aca7.5ed9.a7ed.9742.9ef9.d7bb.4dea
                0x5bed.7cc5.0cb7.4cd8.0a79.8bdb.2bc4.c7ef.
                  fb7d.6875.a60d.0d0d.6ec8.1efd.90fc.e724
              0x177.441d.1b78.faee.ee95.c217.bebd.f02d.eb2c.e5ba
            0x1135.c11e.8478.9663.0c8b.d3c3.5609.7f93.
              7b6d.f92d.88e2.c239.f517.fa06.53ab.205a
            ::  transfer
            ^-  node
            :+  "transfer"
              ;:  weld
                "pill ensure explain submit rifle street "
                "ranch holiday slot carpet memory market "
                "stamp asset prefer black promote insane "
                "green lemon tornado across stem talent"
              ==
            :+  :-  0x2.ba44.096a.396f.1d8c.922f.dacf.5648.47a1.
                        d8e8.5a6a.44d9.ba03.e162.4413.21e0.6e8b
                0xc260.5506.df25.f841.dc00.b43c.1359.65f7.
                  ffc9.5f7e.15c1.98ef.e59b.47bc.8fd9.35e4
              0x6cb2.540c.39e6.7313.8881.744f.00c2.dfc2.382e.75c2
            0x1603.e233.be5f.8c05.88ef.783e.ec46.d86d.
              88ad.f253.ebab.5920.0073.a5bf.187e.9c48
            ::  spawn
            ^-  node
            :+  "spawn"
              ;:  weld
                "confirm act become tortoise vocal session "
                "nuclear enemy auction dirt elephant produce "
                "outside symptom involve attack course receive "
                "grit solid stand expand brisk ranch"
              ==
            :+  :-  0x2.3c12.4789.c98e.8fea.ef80.e467.ad00.cfd7.
                        5973.4824.8b31.8859.813c.e383.0a22.1d86
                0x74cb.bc06.a433.9ce8.b72e.d7e8.4597.d6eb.
                  6b2e.3d90.e672.dded.5048.b200.0ed9.e33e
              0xd0f7.d562.9067.5609.d37f.c75f.8413.535f.7aa5.db04
            0x6374.0b42.257a.eb76.ef53.9fd2.08e5.7bc0.
              d15f.2d34.b27a.9972.9fab.2025.a977.1387
            ::  network
            *uode
          ==
    !>  =-  -(network *uode)
        %-  full-wallet-from-ticket
        :+  ~nec  ::  1
          4^.~doznec-marbud
        [0 ~]
::
++  test-generate-wallet-1
  %+  expect-eq
    !>  ^-  vault
        :*  ::  ownership
            ^-  node
            :+  "ownership"
              ;:  weld
                "polar rich inch innocent image swear "
                "among sense rent kit bench scissors "
                "menu coil sunny plate virus robot "
                "neutral craft have attend wish pig"
              ==
            :+  :-  0x3.5274.be52.86fe.d3f2.9391.c90a.f9b2.2c9e.
                        0458.c3ed.93b0.b119.1ce1.0c4c.083c.878b
                0x907.827f.7709.0a9d.2e71.8f67.1e78.8567.
                  2bd9.242a.cbc7.e607.1ccf.baf0.c30a.2939
              0x88f.d420.0d66.71c3.95c9.a1d3.1d07.b6db.c416.ba80
            0xd6cb.6883.c443.2813.cc5c.a921.5147.ea94.
              82ea.4cc9.000d.44a1.ace6.c931.4782.bf6b
            ::  voting
            *node
            ::  management
            ^-  node
            :+  "management"
              ;:  weld
                "know legal prefer express valve minor "
                "tornado three have future bean acoustic "
                "sound helmet crane bronze also royal "
                "choice expire dust priority nation wide"
              ==
            :+  :-  0x3.0182.61db.f21d.1785.9b27.b42c.c02e.f68a.
                        82ee.10af.b70b.3ff0.c2bd.fdc1.31ee.4921
                0xeb3c.de29.0801.0f01.8a9f.b0ec.9566.209d.
                  38a2.9bf5.ad3e.42fe.a393.2e5b.ee33.7519
              0xc3df.999b.5424.d27d.f303.5b58.e9e3.ffd8.6692.eb6d
            0xb883.c3f7.1d40.1721.17b4.e260.563f.c048.
              313a.0740.7774.c621.812e.ae20.3498.47a3
            ::  transfer
            ^-  node
            :+  "transfer"
              ;:  weld
                "worth decrease silly hint hurdle fiction "
                "lunar brush habit soda caught wisdom "
                "rifle talk insane approve athlete glory "
                "choice glance kidney latin clown employ"
              ==
            :+  :-  0x3.27f3.65a6.52bb.7431.d3a2.36cf.9cb4.12c9.
                        96bc.7cea.17a2.b101.6d78.34d0.3cc2.b7e0
                0xb5dc.5724.c072.5a37.a85f.9a8b.d55c.d3f6.
                  6312.7c72.ead8.35c4.733a.f8d1.0ebe.396d
              0xce3.c04f.ff81.238f.59c8.9cd3.3ab9.bdf0.5410.8e25
            0x80d1.2b9c.5f15.5258.6982.6ad9.1da1.549b.
              d5a8.6065.5f4d.3303.5a56.c9aa.5d8a.a8d4
            ::  spawn
            ^-  node
            :+  "spawn"
              ;:  weld
                "shine noise foil stage walnut shell "
                "police slogan video humor use tower "
                "ostrich offer thank cook include business "
                "online cream ring mobile that access"
              ==
            :+  :-  0x2.fdab.05be.5bb2.70f8.cdc5.dbf8.db5c.6cab.
                        d3e0.d5f9.7fdf.5655.1dd9.3e1a.6b9b.9597
                0x3085.571d.bbce.342d.b210.4a0a.7684.cf6b.
                  ab99.58f6.ac03.eb49.5638.e2ab.eb26.1251
              0xdb27.8eb8.332b.d905.f008.e784.865f.88bc.f82a.dfdf
            0x19da.8efe.9f29.0ea8.48b3.4835.c3ea.ab37.
              8ed7.8259.2a67.de87.bc77.2c93.e50f.7915
            ::  network
            ^-  uode
            :+  0
              0xbd9d.88d3.0966.03b5.de36.2259.e97b.78b4.
                d9ca.21c8.d4e2.64ac.698c.abde.683b.936a
            :-  :-  0x25bd.0bb6.2018.1c1c.80f8.07a0.4bb7.d770.
                      1711.bdf6.793d.dbf9.ed49.d6f0.3fbb.a34f
                    0x4bd5.0bfd.a11f.f126.23b5.6a4c.20bb.9821.
                      ea41.a2f4.b125.e9af.0b19.5c4d.f833.fe7c
                :-  0x6889.eede.9463.4a06.0d34.03ca.0653.1bc3.
                      8bb4.a44a.c68e.6cea.99c2.c3b7.e76f.3a8a
                    0xcc33.8853.7bf9.1f94.1a7a.af2b.007a.e010.
                      bee5.94ce.5814.ad93.0c2e.1c72.e8b1.1e3f
          ==
    !>  ^-  vault
        =-  -(voting *node)
        %-  full-wallet-from-ticket
        :+  ~matbyr  ::  65.012
          8^.~marbud-tidsev-litsut-hidfep
        [0 ~]
::
++  test-generate-wallet-2
  %+  expect-eq
    !>  ^-  vault
        :*  ::  ownership
            ^-  node
            :+  "ownership"
              ;:  weld
                "today submit park slow present junior "
                "fall metal spend misery begin together "
                "buffalo enroll rebuild dash cry dove "
                "notable sniff morning forum else pizza"
              ==
            :+  :-  0x3.f70d.c088.7e14.8945.a66a.09b5.f261.db67.
                        669b.7eda.4d92.ce34.6081.7710.b294.04f4
                0x865a.ac06.8660.526f.021e.9cce.dfd1.e91a.
                  59bd.266b.b02d.cdd6.7d35.785a.716b.8ad8
              0x8bb9.d38a.2b40.a673.06d1.c1c9.a3fc.7a20.ee97.2841
            0xdcc7.bd8d.a07b.b80c.dff0.5539.e2c9.b8bd.
              d41a.a016.8efb.4cb1.ad91.d442.356c.7e5a
            ::  voting
            *node
            ::  management
            ^-  node
            :+  "management"
              ;:  weld
                "ramp stem peanut away relax improve "
                "school excess era loan razor crane "
                "cruise shift junk piece regret immune "
                "pencil inspire lab core enforce ball"
              ==
            :+  :-  0x3.9ce0.93db.f3eb.f742.e836.db1b.f76c.bf58.
                        4c3f.d015.6157.05a7.ab87.da36.0576.e598
                0x445e.2ed7.b485.19a5.1f03.489e.0a8d.72b3.
                  c7b8.d326.2a7e.af30.5325.cec4.500c.928c
              0xd73.bd00.8a2b.5201.dd2e.8db3.1371.769d.2005.6614
            0x1c5f.1486.38d0.0ccb.a199.5209.5f59.0262.
              9bc4.370e.3e8a.0c77.4096.682d.ccaa.7c85
            ::  transfer
            ^-  node
            :+  "transfer"
              ;:  weld
                "shove proof ceiling energy phone dumb "
                "offer hidden lava seminar autumn design "
                "unfair either gentle salmon magic account "
                "weather rapid antenna skill noise enough"
              ==
            :+  :-  0x2.3382.d237.b7fa.4270.5a60.64d0.6cfa.6128.
                        e9c8.8f15.9c1e.12ea.f775.c879.a335.2dfe
                0x712f.4a83.9303.daf1.12bf.3b75.81bd.1a51.
                  1f86.6c7c.4f34.1c83.3344.9c53.e605.743f
              0x2d95.a6b6.63ca.4781.889c.81d1.e432.2e6e.89e8.ec23
            0xe316.4110.6e4b.5c76.2118.7c58.c52d.1d37.
              c590.c8e4.f4bc.ae52.cb59.6cbe.2a33.6c9c
            ::  spawn
            ^-  node
            :+  "spawn"
              ;:  weld
                "magnet nose valve saddle chicken become "
                "ice scorpion sting top claw media "
                "ghost bamboo charge agent volume hidden "
                "sudden impose hand nephew canoe yard"
              ==
            :+  :-  0x3.db2f.c991.8be0.f132.6c48.50c2.ce52.ab6e.
                        5b85.873e.2cf3.1775.93a1.80d4.6ddd.af5c
                0xebba.82ca.e8b7.af26.268a.7114.2326.5b9d.
                  9cfb.579e.ae75.a4b9.8700.57ed.a183.4b40
              0x7989.e11f.9855.f2be.7ca4.ac7d.485a.1a77.337b.b602
            0x7a3c.9445.c476.7623.eb6e.9c98.92c9.b9fb.
              13e8.2d3c.5443.62c7.7033.ad62.343e.a92e
            ::  network
            *uode
          ==
    !>  ^-  vault
        =-  -(voting *node, network *uode)
        %-  full-wallet-from-ticket
        :+  ~zod
          :-  48
          .~wacfus-dabpex-danted-mosfep-pasrud-lavmer-
            nodtex-taslus-pactyp-milpub-pildeg-fornev-
            ralmed-dinfeb-fopbyr-sanbet-sovmyl-dozsut-
            mogsyx-mapwyc-sorrup-ricnec-marnys-lignex
        [6 `'froot loops']
::
++  test-derive-network-seed-rev-0
  %+  expect-eq
    !>  (seed:ds 64^0x5eed "network0")
    !>  (derive-network-seed 0x5eed 0)
::
++  test-derive-network-seed-rev-up
  %+  expect-eq
    !>  |
    !>  .=  (seed:ds 64^0x5eed "network1")
        (derive-network-seed 0x5eed 1)
--
