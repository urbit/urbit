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
          "forum equal youth afford sketch piece "
          "direct room clarify dumb autumn soon "
          "capable elegant nest cover lawn drive "
          "motion vault river athlete vicious blush"
        ==
    !>  %+  from-entropy:bip39  32
        %+  seed:ds
          ^-  byts
          :-  32
          0xb2bd.f8de.8452.b18f.0219.5b6e.7bfc.82b9.
            00fb.cc25.681f.07ae.10f3.8f11.e5af.53af
        ^-  meta
        ["management" 0 `@`10]
::
++  test-child-seed-from-seed-1
  %+  expect-eq
    !>  ;:  weld
          "crime pistol actress sentence thunder tide "
          "consider estate robot lava arena undo "
          "nominee baby ladder opinion congress private "
          "print tube mango arrange father prison"
        ==
    !>  %+  from-entropy:bip39  32
        %+  seed:ds
          ^-  byts
          :-  32
          0xb2bd.f8de.8452.b18f.0219.5b6e.7bfc.82b9.
            00fb.cc25.681f.07ae.10f3.8f11.e5af.53af
        ^-  meta
        ["ownership" 0 `@`10]
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
  =/  mea=meta  ["" 0 `@p`1]
  %+  expect-eq
    !>  ^-  vault
        :*  ::  ownership
            =-  (~(gas by *nodes) ~[who.mea^-])
            ^-  node
            :+  mea(typ "ownership")
              ;:  weld
                "various quiz first cliff resemble rough "
                "priority sibling topple coin copper merit "
                "spend demand kite cargo key stone "
                "judge dignity beauty boring document actress"
              ==
            :+  :-  0x3.4c55.2c53.cda2.50b7.34eb.a4b9.e54c.97f8.
                        f81d.95fa.bf88.4ca4.d973.6754.c50b.3b0e
                0xbaa6.ef7e.3e47.a255.1100.b12f.f073.e1ef.
                  b761.361f.ccb6.a134.71ca.8416.36d8.4610
              0x2325.342b.e552.bb91.0a33.82c4.318d.c3cc.70db.1604
            0x15f0.1ce6.429e.70f1.443a.eacc.3a40.6330.
              a009.a641.dbdd.d626.3d97.fead.6105.1e7c
            ::  voting
            =-  (~(gas by *nodes) ~[who.mea^-])
            :+  mea(typ "voting")
              ;:  weld
                "load future seven beauty phone admit "
                "flight funny couple chicken differ drum "
                "club raise average shove address dune "
                "point true sense pioneer finger buyer"
              ==
            :+  :-  0x3.139a.fb22.8bc0.6286.6167.82f7.9a7e.767c.
                        d013.b572.65c9.4f34.9a39.90a6.74d3.c431
                0xe60a.675a.64ea.1bcd.e43c.0fef.b006.d913.
                  67f2.b8d0.e33c.6203.bda5.0a58.9e40.8e2f
              0x20c8.e4b8.fef4.ddea.7256.8430.2553.c92b.72df.d394
            0x481b.f89a.7e25.7de0.6626.314a.b6e8.e505.
              6352.e6f7.4c6f.eff2.ec77.0f35.7c15.f53b
            ::  management
            =-  (~(gas by *nodes) ~[who.mea^-])
            :+  mea(typ "management")
              ;:  weld
                "speed stick feel purse camera breeze "
                "quiz inject whale connect cheap crop "
                "ranch walk neck merry giggle limb "
                "second bike hope riot glad reject"
              ==
            :+  :-  0x3.0ef9.784b.a4a9.803e.9c7c.f351.3cda.51be.
                        e475.4235.9def.78c8.4263.5a89.2f18.8d13
                0xc88b.a959.acc0.6ffe.7e3f.035b.8e06.9ea3.
                  0211.ca2e.d69b.7f2a.b306.b5cb.4a78.1595
              0x3365.318e.7689.21c4.00ec.0110.e72a.97e4.deb1.4df9
            0x2a48.b639.7b90.15e0.0dbb.bb12.e499.f3fa.
              9432.738b.68db.3a01.d39b.89f1.5153.c651
            ::  transfer
            =-  (~(gas by *nodes) ~[who.mea^-])
            :+  mea(typ "transfer")
              ;:  weld
                "wine foil elbow mail wreck peasant "
                "inner scheme viable foil thing elder "
                "slim obvious soldier parent version derive "
                "imitate tissue palm trick clean increase"
              ==
            :+  :-  0x2.4b87.f9d9.8441.3dda.c213.8081.884b.35ae.
                        c3e3.617d.8620.fabe.0506.29ee.cab9.025f
                0xfde2.52d2.bace.6bac.5eeb.d8ef.ef3d.9f8c.
                  5d90.d725.6f89.79bf.5c72.4cf8.9f51.3c5f
              0x7b99.8e3c.2ff2.c97f.167d.c7c8.afe6.80ca.231f.26be
            0x520f.6f12.41bc.a8ef.c349.d5c6.4740.eeee.
              b69e.5d2f.9db2.0798.c794.fff2.5da5.4ae3
            ::  spawn
            =-  (~(gas by *nodes) ~[who.mea^-])
            :+  mea(typ "spawn")
              ;:  weld
                "attitude entire between school lawsuit wisdom "
                "barely false symbol nose weasel donkey "
                "artist blue noble gown worry notable "
                "farm crush glimpse gain merge police"
              ==
            :+  :-  0x3.544f.172c.b777.a6ff.3847.0d91.8212.ad24.
                        8c34.8334.4699.e921.c673.955c.e053.d8b3
                0xa0e1.5b48.6283.954d.2fe5.776e.f7a2.05e9.
                  bdff.6f83.e8d1.20ad.b83a.0bf5.b0e6.d26c
              0x2984.164d.d3be.3a07.acf1.8537.db9b.1034.3c40.92c1
            0x2fbd.49a6.3ec4.ec58.1ead.7c09.e820.fad6.
              47de.6561.5549.7fdb.be68.0dc0.90d1.7fed
            ::  network
            ~
          ==
    !>  =-  -(network *uodes)
        %+  full-wallet-from-ticket
          4^0x1.0102  ::TODO  .~doznec-marbud
        :+  [who.mea ~ ~]
          *revisions
        ~
::
++  test-generate-wallet-1
  =/  mea=meta  ["" 0 `@p`65.012]
  %+  expect-eq
    !>  ^-  vault
        :*  ::  ownership
            =-  (~(gas by *nodes) ~[who.mea^-])
            ^-  node
            :+  mea(typ "ownership")
              ;:  weld
                "flame nothing issue first fine pottery "
                "save before guitar twist guard round "
                "burst actual mesh wrestle spirit disagree "
                "then green grid hole lobster local"
              ==
            :+  :-  0x3.90f0.a6c8.0ed8.b397.15e5.6e4e.24ed.0d3a.
                        92e5.425d.3863.c40b.4764.59ec.9b5d.4d7b
                0x9162.3b57.60c5.0437.639e.c92e.9bb8.8ac7.
                  7144.420e.3a9c.2e91.81a5.614b.3484.e097
              0x7544.42e3.bb87.4777.2572.7dc0.db1d.f693.630d.bac9
            0xd685.6120.aad8.1367.63ef.6ff8.de9c.010f.
              c345.b424.38c2.106e.2dfc.6aec.fd26.0cc4
            ::  voting
            ~
            ::  management
            =-  (~(gas by *nodes) ~[who.mea^-])
            :+  mea(typ "management")
              ;:  weld
                "enough federal present expire setup obtain "
                "taste above chronic tuition spike gas "
                "middle muscle mail vicious muffin employ "
                "silver ten about shiver purchase service"
              ==
            :+  :-  0x2.ffd4.fdbf.2cf7.8bbf.707b.d0c1.c84d.aac8.
                        e51f.7e1f.242d.fb27.f7c0.bdf0.96c3.bbf0
                0x6aea.d877.7076.de3a.4d3e.92db.a1a8.91df.
                  b986.98bd.4539.99d2.5668.f085.1eaf.3cb3
              0xa20b.c714.38ff.0bb7.7b96.9af9.690b.74ca.0d73.452c
            0xc509.6fca.b714.0e32.8cda.8a79.d18b.1096.
              ac3c.b39e.1cec.17ed.b0a1.6964.6b56.1e8b
            ::  transfer
            =-  (~(gas by *nodes) ~[who.mea^-])
            :+  mea(typ "transfer")
              ;:  weld
                "page wild bread settle news around "
                "reduce receive urban shrug coffee acid "
                "unknown crisp million also grass volume "
                "summer apology meadow poem slab sure"
              ==
            :+  :-  0x3.45ba.e5dc.335f.0a19.86cd.4213.5e30.f98c.
                        0708.1651.8685.9ec4.7c15.be67.debd.3930
                0xc111.2499.bab5.45a4.112d.a3de.3590.eee9.
                  2a62.c9c1.a949.0c89.bd94.47fd.06d6.3c87
              0x2602.34db.9947.50a4.6965.a2d0.949a.c4e1.4d6e.975e
            0xf7b9.3a65.885d.0f6d.ff88.cd9e.cc30.ae43.
              b3a1.cc74.77b8.0bc4.2863.5601.8bdd.82b7
            ::  spawn
            =-  (~(gas by *nodes) ~[who.mea^-])
            :+  mea(typ "spawn")
              ;:  weld
                "false sort bomb damage govern exclude "
                "that unknown away display kit attract "
                "toilet brick delay useless squirrel garment "
                "sunset awake easy wage arrow warrior"
              ==
            :+  :-  0x2.ea8b.99eb.99b9.c041.c019.0e77.0db8.db99.
                        4695.6509.867b.c158.8078.b2cd.c56a.2bc8
                0xf879.648c.8139.0d94.d9ae.c849.5324.e4f0.
                  186c.d446.aaea.e77f.7a25.8d9c.99aa.30cd
              0x7b73.ce24.5268.5d4c.58d5.f2c6.9000.5847.7547.f6d1
            0xd2e8.599c.158c.de70.0e2e.6cef.d0c0.6cf4.
              7bf2.f636.c460.fd8e.1ef3.6764.67d7.d295
            ::  network
            =-  (~(gas by *uodes) ~[who.mea^-])
            :+  mea(typ "network")
              0xebd5.6c6d.ee34.e9b7.b2bc.7701.9fa5.85c7.
                0194.5f0f.e28d.aa27.78d2.9146.4c03.6345
            :-  :-  0xa08c.526d.af6c.5fce.b5e2.9d9a.5c26.251a.
                      b9b8.b01f.9339.cb9d.3dc4.6cb4.239e.3113
                    0x817c.a3f9.16f3.5554.e931.2de4.4ed5.e32e.
                      3ef8.ea0b.222f.c81d.74c2.f502.d053.b58c
                :-  0x1141.254e.602a.cae0.def1.a799.bbb8.3bb2.
                      f6ab.c992.78c4.ebcf.6671.ed57.ff4e.ef64
                    0x8f2b.14e5.5eed.db75.90f4.c6a5.6c84.faff.
                      0444.a647.4bde.9082.8733.b7b0.395e.a2fa
          ==
    !>  ^-  vault
        =-  -(voting ~)
        %+  full-wallet-from-ticket
          8^0x102.af04.0506.0798  ::TODO  .~marbud-tidsev-litsut-hidfep
        :+  [who.mea ~ ~]
          *revisions
        ~
::
++  test-generate-wallet-2
  =/  mea=meta  ["" 6 `@p`~]
  %+  expect-eq
    !>  ^-  vault
        :*  ::  ownership
            =-  (~(gas by *nodes) ~[who.mea^-])
            ^-  node
            :+  mea(typ "ownership")
              ;:  weld
                "thought story peasant will layer street "
                "genius become interest country erupt crater "
                "still stone pupil vessel wink second "
                "habit sell spray rate spend size"
              ==
            :+  :-  0x2.c87a.89ac.fdbe.1379.24d6.59f9.f9c5.b10f.
                        7606.7130.bafe.5aba.9510.7ab6.c6c4.0f3b
                0xc8c5.3b8c.70d2.f465.5eb5.8a7d.66e4.ca38.
                  6dcc.262b.43e8.63ff.5f8a.5bee.7172.08b5
              0xcbe5.5640.b324.0fae.d26f.ae0f.c7a4.5c99.d999.fae1
            0x65fd.9ac0.04de.3e37.530e.ea3c.8b24.ec40.
              441e.5403.6e02.fd67.5d54.0108.828c.1700
            ::  voting
            ~
            ::  management
            =-  (~(gas by *nodes) ~[who.mea^-])
            :+  mea(typ "management")
              ;:  weld
                "effort police leader fish glare decide "
                "gaze jacket law forest area call "
                "away tooth extend sick setup trouble "
                "scene equip visa nothing bonus say"
              ==
            :+  :-  0x2.004e.f41a.81ef.fa4b.6d3a.378d.c289.94a6.
                        0577.0a50.903f.cdd6.7d86.dae8.7096.d804
                0x67d1.34d6.e0f8.b92e.b4c4.ed4e.3bf1.82cc.
                  18fd.9adb.3d50.de3a.3b59.f29d.582d.f058
              0xce6c.beee.1912.7323.d546.df6d.d7f1.04bf.a540.4536
            0x6f3.47ff.8e62.fdc9.b0fc.286d.1a7f.02fe.
             97b7.b922.9a4e.6f84.b82b.de53.d2e6.f896
            ::  transfer
            =-  (~(gas by *nodes) ~[who.mea^-])
            :+  mea(typ "transfer")
              ;:  weld
                "miracle purpose soccer require brand egg "
                "grocery rough eternal drip finish truck "
                "lend invest draw viable kid useless "
                "kick dish ensure equal tobacco devote"
              ==
            :+  :-  0x3.fa3e.883d.03b8.1839.572d.adab.dba4.4905.
                        98a0.c98b.00d3.22bd.5eb8.f5c0.4320.b44e
                0x7331.d92c.bc15.166b.57c6.7ea4.dbd6.bb6d.
                  5e01.4533.90f3.ab5d.7fd2.0d5c.8034.5afe
              0xcbb0.0675.f8c6.e1eb.7971.efd2.22f5.3d4e.7e47.723b
            0x8462.5f52.4497.b121.d181.70c2.64bc.1b2e.
              b417.975c.2d7e.d6f9.7912.0a24.d51d.e508
            ::  spawn
            =-  (~(gas by *nodes) ~[who.mea^-])
            :+  mea(typ "spawn")
              ;:  weld
                "good wealth crystal convince sing above "
                "like bag second oval lucky tell "
                "icon jazz neck ivory dragon prevent "
                "book love blood edit machine example"
              ==
            :+  :-  0x2.c647.f064.55aa.3a5c.eecd.bee2.93d4.9c78.
                        64e2.ca68.de28.ada6.c4fe.d23f.db5d.da81
                0x9196.6df3.16f6.5eb2.712d.5860.5c29.5cd7.
                  fd3d.627f.084b.05e0.d358.27e6.90ae.8a51
              0x34f1.1d21.9c19.0a9f.6983.2be5.402d.4aef.b8e4.1e9b
            0xbb36.4194.fd30.3707.786f.2e85.11ae.2123.
              77c6.ed07.a3b1.21b6.2b43.5953.534e.ef52
            ::  network
            ~
          ==
    !>  ^-  vault
        =-  -(voting ~, network ~)
        %+  full-wallet-from-ticket
          :-  48
          0xc5a.b5ba.ea8e.e798.21d3.fc9f.8876.6da1.
           95bd.d4a6.3375.32fe.8f7f.d92f.d5f4.446a.
           f9bf.0006.a211.823b.fbc9.a701.01e6.6f1f
          ::TODO  .~wacfus-dabpex-danted-mosfep-pasrud-lavmer-
          ::        nodtex-taslus-pactyp-milpub-pildeg-fornev-
          ::        ralmed-dinfeb-fopbyr-sanbet-sovmyl-dozsut-
          ::        mogsyx-mapwyc-sorrup-ricnec-marnys-lignex
        :+  [who.mea ~ ~]
          [. . . . . .]:rev.mea
        `'froot loops'
--
