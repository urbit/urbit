/+  *test, keygen
=,  keygen
|%
::NOTE  tests lightly modified from the keygen-js tests:
::      https://github.com/urbit/keygen-js/blob/master/tests/test.js
::
++  test-child-seed-from-seed
  ;:  weld
    %+  expect-eq
      !>  `byts`9^0xb1.5035.4a72.552c.9efd
      !>  (~(seed sd ~) 9^'dees emos' "type" 0 ~)
    ::
    %+  expect-eq
      !>  `byts`10^0xd613.009d.343c.fc90.b471
      !>  (~(seed sd ~) 10^'!dees emos' "type" 0 ~)
    ::
    %+  expect-eq
      !>  `byts`9^0xb5.0817.d05c.920f.a6b3
      !>  (~(seed sd ~) 9^'dees emos' "type" 0 ``@p`2)
    ::
    %+  expect-eq
      !>  `byts`9^0xb5.0817.d05c.920f.a6b3
      !>  (~(seed sd `'') 9^'dees emos' "type" 0 ``@p`2)
    ::
    %+  expect-eq
      !>  `byts`9^0x8c.cb09.3740.2801.8690
      !>  (~(seed sd `'pass') 9^'dees emos' "type" 0 ``@p`2)
  ==
::
++  test-wallet-from-seed
  ;:  weld
    %+  expect-eq
      !>  ^-  wallet
          :+  :-  0x2.bb80.a59f.d51e.d853.285f.3b77.38b4.542f.
                      619a.5281.9a04.680e.5f36.c4d7.6547.eec9
              0x733f.ce1a.6a6d.c996.4159.0a45.4532.2984.
                23c2.c65f.0df3.0ca0.7069.8d92.df55.196e
            0xef2c.cb72.ef65.6cef.2256.d5fb.0a43.bbfa.
              b04c.ed88.3668.7658.0e34.e4e5.7c96.c48c
          0x856a.3146.9d4c.f3ca.6898.3b37.e072.bdbc.ad60.f25f
      !>  (~(wallet sd ~) 9^'dees emos')
    ::
    %+  expect-eq
      !>  ^-  wallet
          :+  :-  0x2.0123.9b9f.2b94.0f7c.e29d.1963.3f66.bcdd.
                      46dd.b647.8129.2156.2aa1.e402.584c.b0a6
                  0xf551.b64d.202e.4749.d869.53d4.aa2e.e525.
                    2093.fb33.5853.104b.fdd4.4360.c3b9.5032
            0xd3ad.3620.177f.98d6.00c1.73a3.0b9a.5707.
              4ded.e600.ded5.08b4.87f2.9359.c684.c3dc
          0x36f3.b8f2.e8a4.1efc.b345.73fa.b59b.9e41.b1b6.44de
      !>  (~(wallet sd `'pass') 9^'dees emos')
  ==
::
++  test-urbit-keys-from-seed
  ;:  weld
    %+  expect-eq
        !>  ^-  edkeys
            :-  :-  0x220c.0db4.f436.d253.2f0f.ddb5.6555.bf69.
                      26d6.bcfb.073d.790b.8f1e.9c42.58eb.b43e
                    0x15ef.9b02.0606.faf2.5dd4.b622.d34a.5f2b.
                      a83e.3498.f78e.35c6.d256.379f.4871.391e
            :-  0xbbba.375a.6dd2.8dc9.e44d.6a98.c75e.deb6.
                  99c1.0d78.e92c.cad7.8c89.2efa.2466.c666
                0xfd81.6b63.558f.3f4e.e5ea.fedb.abe5.6293.
                  ee1f.64e8.37f0.8172.4bfd.d47d.6e4b.9815
        !>  (~(urbit sd `'') 9^'dees emos')
    ::
    %+  expect-eq
      !>  ^-  edkeys
          :-  :-  0xedb3.1a2d.442b.50d3.7983.ac06.ab7c.5d97.
                    6a71.eca8.4ed1.6573.bf6e.258b.082e.a9f9
                  0xe3ec.0524.9eaa.ffbf.ca91.8dd9.048a.0365.
                    6b68.e568.5f9a.2452.8509.17e2.b349.96ed
          :-  0x9b49.31da.f2c0.cccd.34df.0772.f70e.aaa9.
                b5b3.41c4.6e1a.8cbf.063b.7cdd.2591.7e13
              0x5dee.3371.f15a.f6df.dd4c.8c50.037c.3f33.
                50e2.6440.af32.57ed.62f9.da94.45e9.946b
      !>  (~(urbit sd `'pass') 9^'dees emos')
  ==
::
++  test-child-node-from-seed
  ;:  weld
    %+  expect-eq
      !>  ^-  node
          :+  ["type" 0 ~]
            0xb1.5035.4a72.552c.9efd
          :+  :-  0x3.e68b.2b54.10be.60af.a3a2.8de9.815c.9357.
                      bfad.a54b.af9a.a75e.8544.cc98.ac9b.0a0b
                  0x2a3d.0c6f.cb30.1685.46e4.dc86.e03a.c09f.
                    6740.f5e1.f687.a78e.fcd8.a81b.233b.161f
            0xcb7a.53bc.e7d0.329b.3bed.e0c3.acb3.9d05.
              e000.1acf.53d9.9044.92b0.0b3a.575c.b03a
          0xf5bf.7a7e.20a4.48eb.8fbc.6fd8.06e0.902a.d7de.04da
      !>  (child-node-from-seed 9^'dees emos' ["type" 0 ~] ~)
    ::
    %+  expect-eq
      !>  ^-  node
          :+  ["type" 0 ``@`2]
            0x8c.cb09.3740.2801.8690
          :+  :-  0x3.1d0a.a7c9.21fe.64db.6bba.7bfe.ca1b.a522.
                      9606.02d1.8909.976d.3491.1085.6634.d779
                  0xcce1.477c.8039.b14b.f995.f68e.f640.e3ea.
                    d75d.bdc7.6250.7dc6.3c61.d196.3d65.4d13
            0x5d20.c3a6.0470.9b93.2dc7.a629.8f37.bdc7.
              13d6.4e65.f175.6e17.b462.4be6.01ba.f379
          0x9143.f41c.27c6.48de.1740.cf14.16b7.cf31.1220.1286
      !>  (child-node-from-seed 9^'dees emos' ["type" 0 ``@`2] `'pass')
  ==
::
++  test-full-wallet-from-ticket
  =+  ships=(~(gas in *(set ship)) ~[~zod ~marzod ~marzod-marzod])
  =/  boot
    %-  full-wallet-from-ticket
    :*  4^0xabcd.ef12
        16
        ships
        `'pass'
        [0 0 0 0 0]
        &
    ==
  =/  noboot
    %-  full-wallet-from-ticket
    :*  4^0xabcd.ef12
        16
        ships
        `'pass'
        [0 0 0 0 0]
        |
    ==
  ;:  weld
    %+  expect-eq
      !>  `(set ship)`[~zod ~ ~]
      !>  ~(key by voting.boot)
    ::
    %+  expect-eq
      !>  ships
      !>  ~(key by network.boot)
    ::
    %+  expect-eq
      !>  `(set ship)`~
      !>  ~(key by network.noboot)
  ==
--
