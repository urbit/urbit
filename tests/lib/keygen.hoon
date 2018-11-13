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
                "over excess hand before discover empower "
                "two bottom during boat force throw "
                "fade high wild stem pet rose "
                "current planet gaze always regular express"
              ==
            :+  :-  0x2.4618.3b85.7bc5.3443.ff5e.0ede.0109.d013.
                        5a30.2639.a952.272d.305e.0fac.75e8.e3fe
                0xfcad.12e2.5bae.e45d.7dea.d6ba.df18.07c2.
                  2141.0019.897e.7eb3.20a1.19f2.386d.3c18
              0x9199.2041.c3f9.e5cc.af4c.817b.adce.17b1.3400.118c
            0xf2f1.0b14.249b.2d8e.8534.68ce.172d.6ff7.
              b533.9635.50e0.abfa.8676.ec8e.3b7a.6cfa
            ::  voting
            ^-  node
            :+  "voting"
              ;:  weld
                "throw album measure arctic romance fuel "
                "same sphere kingdom tiny once gesture "
                "trumpet route shrug craft pulse verb "
                "someone filter proud test elbow before"
              ==
            :+  :-  0x3.b0a9.7d8c.d2e3.6d84.3a84.4cc8.8a31.a1c1.
                        d688.22b6.4831.9ea8.defc.7929.925e.baa0
                0xe584.43a5.895d.f87f.3f43.e8d0.b168.560d.
                  9f9b.cf63.2455.d171.1de9.b3aa.e763.0ee0
              0x6065.20eb.ac4b.2a16.7f29.e578.0d79.53e8.1dca.889b
            0x320c.4f2b.65ae.0469.2974.1748.b0dd.812c.
              f914.afe8.2cec.40a8.ec30.ee0d.0f7d.c5ac
            ::  management
            ^-  node
            :+  "management"
              ;:  weld
                "senior claim file tooth before rally "
                "crucial enforce lady cat goddess rent "
                "shadow patch relief demise faculty nose "
                "faint jewel scorpion fury salt harvest"
              ==
            :+  :-  0x2.ce80.3c83.e6c9.7608.c0fa.40b4.3051.fc60.
                        9297.4b55.b95c.6fcb.5652.773c.9cbf.ced4
                0x9164.e953.ccbb.4e31.3c35.ac31.cdd3.e006.
                  bc2c.9e90.a179.d4b1.cf4d.e86c.4ae1.bae6
              0x94cc.39a3.354d.08b2.50de.0b7d.ca58.285e.2dea.f7d2
            0x33c2.6095.0001.cb4b.18ac.f58d.4ab6.8d09.
              e0c7.cb3c.3fa5.a165.14e6.9b6d.aff8.849c
            ::  transfer
            ^-  node
            :+  "transfer"
              ;:  weld
                "verify input pencil typical moon erupt "
                "rose misery shed embrace vintage bench "
                "deny battle review educate title unveil "
                "coyote dentist world cause shallow theory"
              ==
            :+  :-  0x3.bedd.8637.57e1.2f86.9008.6e89.c121.5e03.
                        69f6.cfcb.a33b.cf06.ac9c.ddd9.9fca.f14d
                0x6537.9bc9.a0ef.f68b.b6d5.6741.e450.4f28.
                  172b.b928.c623.8ebe.169b.0e9f.c21e.c1ff
              0xf30c.d3a6.b19f.4e95.9373.a663.e362.fbd3.0156.a7b3
            0xfd2b.ea4f.675e.d4bf.7b1e.d32e.45d6.df8d.
              9bc4.d1c0.e64a.9be8.3d9b.319a.862a.ca35
            ::  spawn
            ^-  node
            :+  "spawn"
              ;:  weld
                "image raccoon boil potato ritual notable "
                "bright slot ivory theme jaguar assist "
                "fork easy ranch change book country "
                "code domain focus frown crowd prepare"
              ==
            :+  :-  0x2.87d4.aa3d.9b31.e0e9.6f33.35e5.a2bd.f586.
                        b9aa.51f7.ee9a.243a.8753.41e7.bcf0.7fd6
                0xf188.4d89.105a.f7af.ad15.3a0a.44fa.9218.
                  5ebe.996b.ee03.fee9.8dbf.2be7.5ec6.5c3b
              0x6af2.8227.6025.e8df.d372.b167.3cf4.292b.8515.b736
            0x40a2.97ef.7549.32e2.3d37.818e.ca76.172c.
              70e9.18ce.b51e.d45b.c5d3.6030.9946.af75
            ::  network
            *uode
          ==
    !>  =-  -(network *uode)
        %-  full-wallet-from-ticket
        :+  ~nec  ::  1
          4^0x1.0102  ::TODO  .~doznec-marbud
        [0 ~]
::
++  test-generate-wallet-1
  %+  expect-eq
    !>  ^-  vault
        :*  ::  ownership
            ^-  node
            :+  "ownership"
              ;:  weld
                "strike hollow mix before hundred food "
                "moment reduce mountain chief tonight scale "
                "series funny tackle praise total kiwi "
                "wild funny acoustic height volume square"
              ==
            :+  :-  0x2.6f6d.0041.810e.8379.58d4.b4d5.34f8.f414.
                        2a4b.1923.f3dc.39cf.4044.2c12.13a8.c251
                0x9164.47ba.3542.3385.2eb2.86c2.3782.d8f7.
                  0494.a565.2683.0f8a.03f8.dbca.9e7b.9ef5
              0xcba2.aab6.8259.16ef.d62e.8d99.a332.98ab.8d8a.2208
            0x9a96.9dcc.d4c2.8cca.4a8e.4e90.af2b.ff05.
              a60e.6ee5.cd9e.e754.3c16.f801.1ae9.ace7
            ::  voting
            *node
            ::  management
            ^-  node
            :+  "management"
              ;:  weld
                "reveal axis speed stereo essence train "
                "fury rigid waste embody area income "
                "account tissue borrow employ retire edge "
                "brand alarm cigar tonight edge scheme"
              ==
            :+  :-  0x2.9121.71ab.0c2c.8e11.bbed.6dd9.3297.ccf7.
                        483e.e148.062b.328b.8c0d.d5c2.d649.cdfd
                0x6a97.02c2.9c51.28b9.4d2a.9503.3830.b0d6.
                  1599.a4b1.74ed.f802.601f.c54c.546f.b6fc
              0xd06e.94c3.5553.789d.e46d.5a3c.d5e5.1608.1dc7.39f8
            0xb23e.6e60.fae7.3042.0fb8.422b.6e80.0851.
              00bc.7dd5.a11f.4b73.ed61.ea53.04fb.bb67
            ::  transfer
            ^-  node
            :+  "transfer"
              ;:  weld
                "strategy glare desk what junior make "
                "bless clump tool scissors pause rapid "
                "random feel canal flavor combine baby "
                "cash salon possible leaf action heart"
              ==
            :+  :-  0x2.9afd.9bd4.0ab4.149f.4731.76ba.d5d1.eafc.
                        9a75.facd.0f5e.12c8.c5af.9430.864c.fa5f
                0x4e61.dba5.1346.ab8d.8269.752d.95e3.e82b.
                  762e.f800.3eee.e7df.1950.f189.7813.36a3
              0x625f.5a6f.1913.8b27.84db.0616.a779.354c.40d2.2475
            0x95b.c9cf.972f.16d7.37a6.0881.410c.5c3e.
             16a0.182d.8bf0.7a6e.9505.082c.9536.ed7e
            ::  spawn
            ^-  node
            :+  "spawn"
              ;:  weld
                "border edit absorb mistake bubble pave "
                "discover monster nice trouble answer laptop "
                "session denial paper deposit you jeans "
                "stick teach history salute umbrella vivid"
              ==
            :+  :-  0x3.4990.5050.c4dc.15df.af49.9755.f000.033a.
                        e63e.6c28.f6fd.aabd.fb5c.2945.3416.783c
                0x3847.0da4.3251.1795.c244.8208.fb20.a8d2.
                  f3cc.f4f9.c25c.86d6.a906.069d.6342.4150
              0xec60.195c.7f68.d5c7.7884.4cdc.2b4d.11d5.9542.7045
            0xbd4d.3149.15aa.06d3.fb83.2008.3881.27b0.
              23ef.2fba.369a.83a3.bc1e.d7a1.e25a.2b83
            ::  network
            ^-  uode
            :+  0
              0xdb23.2882.0f10.7eaa.895b.9c37.ea3a.e911.
                ef47.6539.2598.8df6.6a38.1b3d.9d0f.8008
            :-  :-  0x36db.69e1.dd09.c309.07c9.67ce.9b1f.8cd1.
                      ebf1.a01f.3046.1c1b.b6c2.7982.53ee.ec16
                    0x38a.3528.eeca.9102.38ec.5995.e5d6.84e3.
                     eb8b.5458.edd3.694f.8c64.1fff.9852.cdf5
                :-  0x9c8c.da16.7d88.e9a0.fa04.8679.3487.c0e8.
                      49d2.acae.167d.17a5.6d62.6767.218c.5bda
                    0xf985.5d6d.5a85.a2dd.f286.6325.2ee0.1b26.
                      8a70.34b5.5c61.02fd.75fa.0d74.38f9.3396
          ==
    !>  ^-  vault
        =-  -(voting *node)
        %-  full-wallet-from-ticket
        :+  ~matbyr  ::  65.012
          8^0x102.af04.0506.0798  ::TODO  .~marbud-tidsev-litsut-hidfep
        [0 ~]
::
++  test-generate-wallet-2
  %+  expect-eq
    !>  ^-  vault
        :*  ::  ownership
            ^-  node
            :+  "ownership"
              ;:  weld
                "sweet song can jealous where business "
                "kitten antique access foam cousin arctic "
                "puzzle ring tide pizza sea ramp "
                "bleak casino erupt put sword hat"
              ==
            :+  :-  0x2.5c4a.7cd7.2fd5.75c6.6e09.b3fb.cbc8.f724.
                        5926.749e.39d8.c634.c821.350e.5bd6.c4a4
                0xf2e0.ce7d.228d.ac19.a0c6.5bf7.ac73.3663.
                  f5c9.751c.bbce.8307.3a2c.19f9.c6bf.0b8d
              0xbf7a.38c7.903d.0cdd.2cf4.e384.e6f4.1ab2.abbb.cd52
            0x6305.dc78.2313.0292.59c9.fe8a.ed13.fb7f.
              92e1.6378.5b4a.a77a.82a8.2ddf.d462.e9f6
            ::  voting
            *node
            ::  management
            ^-  node
            :+  "management"
              ;:  weld
                "vehicle liar call grid arrow adult "
                "gesture distance episode item antique scissors "
                "carry scare oppose fat what thank "
                "slim hurdle correct perfect utility word"
              ==
            :+  :-  0x2.0b63.bec5.489a.c714.d911.0117.9338.0ebd.
                        69ec.c1f2.5dec.29fc.9fd7.90b3.78cf.d7d1
                0x41ec.0959.d807.42f7.7f12.a62d.7be1.8cdd.
                  30c3.03b5.bab2.6060.a01c.560b.40a4.32e9
              0xe8f.07b9.3f25.94ec.accd.4d69.72cf.50be.8110.97e3
            0x55a9.dd2f.e4b4.116d.9be3.b70e.cc33.3acb.
              389f.7bcb.51c7.633e.ba17.f705.0a34.f575
            ::  transfer
            ^-  node
            :+  "transfer"
              ;:  weld
                "neutral when enforce shallow match bridge "
                "collect leg silk match post field "
                "canal page lunch polar artwork follow "
                "indicate awesome orange later pen attend"
              ==
            :+  :-  0x3.7009.f37e.3a46.949a.9b08.fd08.2d85.a13f.
                        6299.27ca.5d92.ce58.4803.3901.20ef.a7d1
                0xa4a2.9864.9ddb.0603.e47e.181a.66c2.1b5f.
                  b768.0c90.374c.119f.1d75.e173.a4a4.4022
              0x768.779a.17d5.ba52.67a2.2912.95d4.7ac1.7efe.77bf
            0xb15.c53a.a3be.5661.04a1.1dff.6ce4.174c.
              d7c3.2a27.e767.bdc5.149f.c417.e252.2a58
            ::  spawn
            ^-  node
            :+  "spawn"
              ;:  weld
                "truly relief grow first balance envelope "
                "expire twelve change evidence adjust usual "
                "convince mechanic dynamic code wheel brother "
                "wash help core argue now must"
              ==
            :+  :-  0x2.d2db.844d.3c3c.e6d5.92e3.6857.efe7.c614.
                        0357.21fc.4b45.48eb.2d6c.9525.3e2a.d83c
                0x6625.9499.8c9a.c4b5.1b84.ecdb.2067.f521.
                  7c58.8021.56f5.144b.9efd.1331.d0e5.6dbd
              0xc414.01fe.f3a3.8fb3.0d29.b8ab.a5bc.4f80.4cbd.4cff
            0xf04.ebc4.4015.2d4e.6615.357e.c821.6f6a.
              b318.6d92.6798.b1ef.4f16.92b9.b4e5.1433
            ::  network
            *uode
          ==
    !>  ^-  vault
        =-  -(voting *node, network *uode)
        %-  full-wallet-from-ticket
        :+  ~zod
          :-  48
          0xc5a.b5ba.ea8e.e798.21d3.fc9f.8876.6da1.
           95bd.d4a6.3375.32fe.8f7f.d92f.d5f4.446a.
           f9bf.0006.a211.823b.fbc9.a701.01e6.6f1f
          ::TODO  .~wacfus-dabpex-danted-mosfep-pasrud-lavmer-
          ::        nodtex-taslus-pactyp-milpub-pildeg-fornev-
          ::        ralmed-dinfeb-fopbyr-sanbet-sovmyl-dozsut-
          ::        mogsyx-mapwyc-sorrup-ricnec-marnys-lignex
        [6 `'froot loops']
--
