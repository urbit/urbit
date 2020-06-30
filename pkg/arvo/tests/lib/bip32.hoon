::  tests for the bip32 lib
::
::  Test vectors from:
::  https://en.bitcoin.it/wiki/BIP_0032_TestVectors
::
/+  *test, bip32
=,  bip32
::
|%
+$  vector
  $:  mk=byts
      pf=@ux
      dp=tape
      ad=@uc
      id=@ux
      sk=@ux
      pk=@ux
      cc=@ux
      xpub=tape
      xprv=tape
  ==
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
        %+  category  "fingerprint"
        (zing (turn vectors check-pf))
        %+  category  "address-from-xprv"
        (zing (check-addr-xprv vectors))
        %+  category  "address-from-xpub"
        (zing (check-addr-xpub vectors))
        %+  category  "extended-private"
        (zing (turn vectors check-xprv))
        %+  category  "extended-public"
        (zing (turn vectors check-xpub))
      ==
  ::
  ++  check-id
    |=  vector
    =/  identity=@ux
      =<  identity
      ?:  =("m" dp)
        (from-seed mk)
      (derive-path:(from-seed mk) dp)
    %+  expect-eq
      !>  id
      !>  identity
  ::
  ++  check-pk
    |=  vector
    =/  public-key=@ux
      =<  public-key
      ?:  =("m" dp)
        (from-seed mk)
      (derive-path:(from-seed mk) dp)
    %+  expect-eq
      !>  pk
      !>  public-key
  ::
  ++  check-sk
    |=  vector
    =/  private-key=@ux
      =<  private-key
      ?:  =("m" dp)
        (from-seed mk)
      (derive-path:(from-seed mk) dp)
    %+  expect-eq
      !>  sk
      !>  private-key
  ::
  ++  check-cc
    |=  vector
    =/  chain-code=@ux
      =<  chain-code
      ?:  =("m" dp)
        (from-seed mk)
      (derive-path:(from-seed mk) dp)
    %+  expect-eq
      !>  cc
      !>  chain-code
  ::
  ++  check-pf
    |=  vector
    =/  parent-fingerprint=@ux
      =<  fingerprint
      ?:  =("m" dp)
        (from-seed mk)
      (derive-path:(from-seed mk) dp)
    %+  expect-eq
      !>  pf
      !>  parent-fingerprint
  ::
  ++  check-addr-xprv
    |=  vectors=(list vector)
    ?>  ?=(^ vectors)
    =/  base=vector  i.vectors
    =/  tests=(list vector)  t.vectors
    |-  ^-  (list tang)
    ?~  tests  ~
    =*  deriv  i.tests
    :_  %_  $
          tests  t.tests
          base   ?:(=(dp.deriv "m") deriv base)
        ==
    ::  force success before starting second round of vectors
    ::
    ?:  =(dp.deriv "m")  *tang
    =/  address=@uc
      =<  (address %main)
      (derive-path:(from-extended xprv.base) dp.deriv)
    %+  expect-eq
      !>  ad.deriv
      !>  address
  ::
  ++  check-addr-xpub
    |=  vectors=(list vector)
    ::  we can only derive non-hardened keys from an xpub key
    ::  e.g. from m/0'/1/2' to m/0'/1/2'/2/1000000000
    ::
    ?>  ?=([^ ^ ^ ^ *] vectors)
    =/  base=vector  i.t.t.t.vectors
    =/  tests=(list vector)  t.t.t.t.vectors
    |-  ^-  (list tang)
    ?~  tests  ~
    =*  deriv  i.tests
    :_  ::  Second list of vectors has hardened paths. we skip those.
        ::
        $(tests ?:(=(dp.deriv "m") ~ t.tests), base base)
    ?:  =(dp.deriv "m")  *tang
    ::  strips the hardened part of the path (at index=9)
    ::  m/0'/1/2'/2
    ::  --------^
    ::
    =.  dp.deriv  ['m' q:(trim 9 dp.deriv)]
    =/  address=@uc
      =<  (address %main)
      (derive-path:(from-extended xpub.base) dp.deriv)
    %+  expect-eq
      !>  ad.deriv
      !>  address
  ::
  ++  check-xprv
    |=  vector
    =/  extended-xprv=tape
      =<  (prv-extended %main)
      ?:  =("m" dp)
        (from-seed mk)
      (derive-path:(from-seed mk) dp)
    %+  expect-eq
      !>  xprv
      !>  extended-xprv
  ::
  ++  check-xpub
    |=  vector
    =/  extended-pub=tape
      =<  (pub-extended %main)
      ?:  =("m" dp)
        (from-seed mk)
      (derive-path:(from-seed mk) dp)
    %+  expect-eq
      !>  xpub
      !>  extended-pub
  ::
  ++  vectors
    ^-  (list vector)
    :~  :*  16^0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
            0x3442.193e
            "m"
            0c15mKKb2eos1hWa6tisdPwwDC1a5J1y9nma
            0x3442.193e.1bb7.0916.e914.5521.72cd.4e2d.bc9d.f811
          ::
            0xe8f3.2e72.3dec.f405.1aef.ac8e.2c93.c9c5.
              b214.3138.17cd.b01a.1494.b917.c843.6b35
          ::
            0x3.39a3.6013.3015.97da.ef41.fbe5.93a0.2cc5.
                13d0.b555.27ec.2df1.050e.2e8f.f49c.85c2
          ::
            0x873d.ff81.c02f.5256.23fd.1fe5.167e.ac3a.
              55a0.49de.3d31.4bb4.2ee2.27ff.ed37.d508
          ::
            %+  weld
              "xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2g"
            "Z29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8"
          ::
            %+  weld
              "xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiC"
            "hkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi"
        ==
      ::
        :*  16^0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
            0x5c1b.d648
            "m/0'"
            0c19Q2WoS5hSS6T8GjhK8KZLMgmWaq4neXrh
            0x5c1b.d648.ed23.aa5f.d50b.a52b.2457.c11e.9e80.a6a7
          ::
            0xedb2.e14f.9ee7.7d26.dd93.b4ec.ede8.d16e.
              d408.ce14.9b6c.d80b.0715.a2d9.11a0.afea
          ::
            0x3.5a78.4662.a4a2.0a65.bf6a.ab9a.e98a.6c06.
                 8a81.c52e.4b03.2c0f.b540.0c70.6cfc.cc56
          ::
            0x47fd.acbd.0f10.9704.3b78.c63c.20c3.4ef4.
              ed9a.111d.9800.47ad.1628.2c7a.e623.6141
          ::
            %+  weld
              "xpub68Gmy5EdvgibQVfPdqkBBCHxA5htiqg55crXYuXoQRKfDBFA1WEjWgP6"
            "LHhwBZeNK1VTsfTFUHCdrfp1bgwQ9xv5ski8PX9rL2dZXvgGDnw"
          ::
            %+  weld
              "xprv9uHRZZhk6KAJC1avXpDAp4MDc3sQKNxDiPvvkX8Br5ngLNv1TxvUxt4c"
            "V1rGL5hj6KCesnDYUhd7oWgT11eZG7XnxHrnYeSvkzY7d2bhkJ7"
          ==
      ::
        :*  16^0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
            0xbef5.a2f9
            "m/0'/1"
            0c1JQheacLPdM5ySCkrZkV66G2ApAXe1mqLj
            0xbef5.a2f9.a56a.94aa.b124.59f7.2ad9.cf8c.f19c.7bbe
          ::
            0x3c6c.b8d0.f6a2.64c9.1ea8.b503.0fad.aa8e.
              538b.020f.0a38.7421.a12d.e931.9dc9.3368
          ::
            0x3.501e.454b.f007.51f2.4b1b.489a.a925.215d.
                66af.2234.e389.1c3b.21a5.2bed.b3cd.711c
          ::
            0x2a78.5763.1386.ba23.daca.c341.80dd.1983.
              734e.444f.dbf7.7404.1578.e9b6.adb3.7c19
          ::
            %+  weld
              "xpub6ASuArnXKPbfEwhqN6e3mwBcDTgzisQN1wXN9BJcM47sSikHjJf3UFHKk"
            "NAWbWMiGj7Wf5uMash7SyYq527Hqck2AxYysAA7xmALppuCkwQ"
          ::
            %+  weld
              "xprv9wTYmMFdV23N2TdNG573QoEsfRrWKQgWeibmLntzniatZvR9BmLnvSxqu5"
            "3Kw1UmYPxLgboyZQaXwTCg8MSY3H2EU4pWcQDnRnrVA1xe8fs"
        ==
      ::
        :*  16^0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
            0xee7a.b90c
            "m/0'/1/2'"
            0c1NjxqbA9aZWnh17q1UW3rB4EPu79wDXj7x
            0xee7a.b90c.de56.a8c0.e2bb.086a.c497.48b8.db9d.ce72
          ::
            0xcbce.0d71.9ecf.7431.d88e.6a89.fa14.83e0.
              2e35.092a.f60c.042b.1df2.ff59.fa42.4dca
          ::
            0x3.57bf.e1e3.41d0.1c69.fe56.5430.9956.cbea.
                5168.22fb.a8a6.0174.3a01.2a78.96ee.8dc2
          ::
            0x446.6b9c.c8e1.61e9.6640.9ca5.2986.c584.f07e.
                  9dc8.1f73.5db6.83c3.ff6e.c7b1.503f
          ::
            %+  weld
              "xpub6D4BDPcP2GT577Vvch3R8wDkScZWzQzMMUm3PWbmWvVJrZwQY4VUNgqFJPM"
            "M3No2dFDFGTsxxpG5uJh7n7epu4trkrX7x7DogT5Uv6fcLW5"
          ::
            %+  weld
              "xprv9z4pot5VBttmtdRTWfWQmoH1taj2axGVzFqSb8C9xaxKymcFzXBDptWmT7F"
            "wuEzG3ryjH4ktypQSAewRiNMjANTtpgP4mLTj34bhnZX7UiM"
        ==
      ::
        :*  16^0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
            0xd880.d7d8
            "m/0'/1/2'/2"
            0c1LjmJcdPnDHhNTUgrWyhLGnRDKxQjoxAgt
            0xd880.d7d8.9384.8509.a62d.8fb7.4e32.148d.ac68.412f
          ::
            0xf47.9245.fb19.a38a.1954.c5c7.c0eb.ab2f.
              9bdf.d96a.1756.3ef2.8a6a.4b1a.2a76.4ef4
          ::
            0x2.e844.5082.a72f.29b7.5ca4.8748.a914.df60.
                622a.609c.acfc.e8ed.0e35.8045.6074.1d29
          ::
            0xcfb7.1883.f016.76f5.87d0.23cc.53a3.5bc7.
              f88f.724b.1f8c.2892.ac12.75ac.822a.3edd
          ::
            %+  weld
              "xpub6FHa3pjLCk84BayeJxFW2SP4XRrFd1JYnxeLeU8EqN3vDfZmbqBqaGJAyiL"
            "jTAwm6ZLRQUMv1ZACTj37sR62cfN7fe5JnJ7dh8zL4fiyLHV"
          ::
            %+  weld
              "xprvA2JDeKCSNNZky6uBCviVfJSKyQ1mDYahRjijr5idH2WwLsEd4Hsb2Tyh8Rf"
            "QMuPh7f7RtyzTtdrbdqqsunu5Mm3wDvUAKRHSC34sJ7in334"
        ==
      ::
        :*  16^0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f
            0xd69a.a102
            "m/0'/1/2'/2/1000000000"
            0c1LZiqrop2HGR4qrH1ULZPyBpU6AUP49Uam
            0xd69a.a102.255f.ed74.3782.78c7.8127.01ea.641f.df32
          ::
            0x471b.76e3.89e5.28d6.de6d.8168.57e0.12c5.
              4550.51ca.d666.0850.e583.72a6.c3e6.e7c8
          ::
            0x2.2a47.1424.da5e.6574.99d1.ff51.cb43.c474.
                81a0.3b1e.77f9.51fe.64ce.c9f5.a48f.7011
          ::
            0xc783.e67b.921d.2beb.8f6b.389c.c646.d726.
              3b41.4570.1dad.d216.1548.a8b0.78e6.5e9e
          ::
            %+  weld
              "xpub6H1LXWLaKsWFhvm6RVpEL9P4KfRZSW7abD2ttkWP3SSQvnyA8FSVqNTEcYF"
            "gJS2UaFcxupHiYkro49S8yGasTvXEYBVPamhGW6cFJodrTHy"
          ::
            %+  weld
              "xprvA41z7zogVVwxVSgdKUHDy1SKmdb533PjDz7J6N6mV6uS3ze1ai8FHa8kmHS"
            "cGpWmj4WggLyQjgPie1rFSruoUihUZREPSL39UNdE3BBDu76"
        ==
      ::
        :*  64^0xfffc.f9f6.f3f0.edea.e7e4.e1de.dbd8.d5d2.
            cfcc.c9c6.c3c0.bdba.b7b4.b1ae.aba8.a5a2.9f9c.9996.9390.8d8a.
            8784.817e.7b78.7572.6f6c.6966.6360.5d5a.5754.514e.4b48.4542
          ::
            0xbd16.bee5
            "m"
            0c1JEoxevbLLG8cVqeoGKQiAwoWbNYSUyYjg
            0xbd16.bee5.3961.a47d.6ad8.88e2.9545.434a.89bd.fe95
          ::
            0x4b03.d6fc.3404.55b3.63f5.1020.ad3e.cca4.
              f085.0280.cf43.6c70.c727.923f.6db4.6c3e
          ::
            0x3.cbca.a9c9.8c87.7a26.977d.0082.5c95.6a23.
                8e8d.ddfb.d322.cce4.f74b.0b5b.d6ac.e4a7
          ::
            0x6049.9f80.1b89.6d83.179a.4374.aeb7.822a.
              aeac.eaa0.db1f.85ee.3e90.4c4d.efbd.9689
          ::
            %+  weld
              "xpub661MyMwAqRbcFW31YEwpkMuc5THy2PSt5bDMsktWQcFF8syAmRUapSCGu8E"
            "D9W6oDMSgv6Zz8idoc4a6mr8BDzTJY47LJhkJ8UB7WEGuduB"
          ::
            %+  weld
              "xprv9s21ZrQH143K31xYSDQpPDxsXRTUcvj2iNHm5NUtrGiGG5e2DtALGdso3pG"
            "z6ssrdK4PFmM8NSpSBHNqPqm55Qn3LqFtT2emdEXVYsCzC2U"
        ==
      ::
        :*  64^0xfffc.f9f6.f3f0.edea.e7e4.e1de.dbd8.d5d2.
            cfcc.c9c6.c3c0.bdba.b7b4.b1ae.aba8.a5a2.9f9c.9996.9390.8d8a.
            8784.817e.7b78.7572.6f6c.6966.6360.5d5a.5754.514e.4b48.4542
          ::
            0x5a61.ff8e
            "m/0"
            0c19EuDJdgfRkwCmRzbzVBHZWQG9QNWhftbZ
            0x5a61.ff8e.b7aa.ca30.10db.97eb.da76.1216.10b7.8096
          ::
            0xabe7.4a98.f6c7.eabe.e042.8f53.798f.0ab8.
              aa1b.d378.7399.9041.703c.742f.15ac.7e1e
          ::
            0x2.fc9e.5af0.ac8d.9b3c.ecfe.2a88.8e21.17ba.
                3d08.9d85.8588.6c9c.826b.6b22.a98d.12ea
          ::
            0xf090.9aff.aa7e.e7ab.e5dd.4e10.0598.d4dc.
              53cd.709d.5a5c.2cac.40e7.412f.232f.7c9c
          ::
            %+  weld
              "xpub69H7F5d8KSRgmmdJg2KhpAK8SR3DjMwAdkxj3ZuxV27CprR9LgpeyGmXUbC"
            "6wb7ERfvrnKZjXoUmmDznezpbZb7ap6r1D3tgFxHmwMkQTPH"
          ::
            %+  weld
              "xprv9vHkqa6EV4sPZHYqZznhT2NPtPCjKuDKGY38FBWLvgaDx45zo9WQRUT3dKY"
            "njwih2yJD9mkrocEZXo1ex8G81dwSM1fwqWpWkeS3v86pgKt"
        ==
      ::
        :*  64^0xfffc.f9f6.f3f0.edea.e7e4.e1de.dbd8.d5d2.
             cfcc.c9c6.c3c0.bdba.b7b4.b1ae.aba8.a5a2.
             9f9c.9996.9390.8d8a.8784.817e.7b78.7572.
             6f6c.6966.6360.5d5a.5754.514e.4b48.4542
          ::
            0xd8ab.4937
            "m/0/2147483647'"
            0c1Lke9bXGhn5VPrBuXgN12uGUphrttUErmk
            0xd8ab.4937.36da.02f1.1ed6.82f8.8339.e720.fb03.79d1
          ::
            0x877c.779a.d968.7164.e9c2.f4f0.f4ff.0340.
              8143.9233.0693.ce95.a58f.e18f.d52e.6e93
          ::
            0x3.c01e.7425.647b.defa.82b1.2d9b.ad5e.3e68.
                65be.e050.2694.b94c.a58b.666a.bc0a.5c3b
          ::
            0xbe17.a268.474a.6bb9.c61e.1d72.0cf6.215e.
              2a88.c540.6c4a.ee7b.3854.7f58.5c9a.37d9
          ::
            %+  weld
              "xpub6ASAVgeehLbnwdqV6UKMHVzgqAG8Gr6riv3Fxxpj8ksbH9ebxaEyBLZ85y"
            "SDhKiLDBrQSARLq1uNRts8RuJiHjaDMBU4Zn9h8LZNnBC5y4a"
          ::
            %+  weld
              "xprv9wSp6B7kry3Vj9m1zSnLvN3xH8RdsPP1Mh7fAaR7aRLcQMKTR2vidYEeEg"
            "2mUCTAwCd6vnxVrcjfy2kRgVsFawNzmjuHc2YmYRmagcEPdU9"
          ==
      ::
        :*  64^0xfffc.f9f6.f3f0.edea.e7e4.e1de.dbd8.d5d2.
                 cfcc.c9c6.c3c0.bdba.b7b4.b1ae.aba8.a5a2.
                 9f9c.9996.9390.8d8a.8784.817e.7b78.7572.
                 6f6c.6966.6360.5d5a.5754.514e.4b48.4542
          ::
            0x7841.2e3a
            "m/0/2147483647'/1"
            0c1BxrAr2pHpeBheusmd6fHDP2tSLAUa3qsW
            0x7841.2e3a.2296.a40d.e124.307b.6485.bd19.833e.2e34
          ::
            0x704a.ddf5.44a0.6e5e.e4be.a370.9846.3c23.
              613d.a320.20d6.0450.6da8.c051.8e1d.a4b7
          ::
            0x3.a7d1.d856.deb7.4c50.8e05.031f.9895.dab5.
                4626.251b.3806.e16b.4bd1.2e78.1a7d.f5b9
          ::
            0xf366.f48f.1ea9.f2d1.d3fe.958c.95ca.84ea.
              18e4.c4dd.b936.6c33.6c92.7eb2.46fb.38cb
          ::
            %+  weld
              "xpub6DF8uhdarytz3FWdA8TvFSvvAh8dP3283MY7p2V4SeE2wyWmG5mg5EwVvm"
            "dMVCQcoNJxGoWaU9DCWh89LojfZ537wTfunKau47EL2dhHKon"
          ::
            %+  weld
              "xprv9zFnWC6h2cLgpmSA46vutJzBcfJ8yaJGg8cX1e5StJh45BBciYTRXSd25U"
            "EPVuesF9yog62tGAQtHjXajPPdbRCHuWS6T8XA2ECKADdw4Ef"
        ==
      ::
        :*  64^0xfffc.f9f6.f3f0.edea.e7e4.e1de.dbd8.d5d2.
               cfcc.c9c6.c3c0.bdba.b7b4.b1ae.aba8.a5a2.
               9f9c.9996.9390.8d8a.8784.817e.7b78.7572.
               6f6c.6966.6360.5d5a.5754.514e.4b48.4542
          ::
            0x31a5.07b8
            "m/0/2147483647'/1/2147483646'"
            0c15XVotxCAV7sRx1PSCkQNsGw3W9jT9A94R
            0x31a5.07b8.1559.3dfc.51ff.c724.5ae7.e5ae.e304.246e
          ::
            0xf1c7.c871.a54a.804a.fe32.8b4c.83a1.c33b.
              8e5f.f48f.5087.273f.04ef.a83b.247d.6a2d
          ::
            0x2.d2b3.6900.396c.9282.fa14.6285.6658.2f20.
                6a5d.d0bc.c8d5.e892.6118.06ca.fb03.01f0
          ::
            0x6378.0703.0d55.d01f.9a0c.b3a7.8395.15d7.
              96bd.0770.6386.a6ed.df06.cc29.a65a.0e29
          ::
            %+  weld
              "xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZR"
            "krgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL"
          ::
            %+  weld
              "xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iA"
            "xn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc"
        ==
      ::
        :*  64^0xfffc.f9f6.f3f0.edea.e7e4.e1de.dbd8.d5d2.
                 cfcc.c9c6.c3c0.bdba.b7b4.b1ae.aba8.a5a2.
                 9f9c.9996.9390.8d8a.8784.817e.7b78.7572.
                 6f6c.6966.6360.5d5a.5754.514e.4b48.4542
          ::
            0x2613.2fdb
            "m/0/2147483647'/1/2147483646'/2"
            0c14UKfRV9ZPUp6ZC9PLhqbRtxdihW9em3xt
            0x2613.2fdb.e7bf.89cb.c64c.f8da.fa3f.9f88.b866.6220
          ::
            0xbb7d.39bd.b83e.cf58.f2fd.82b6.d918.341c.
              bef4.2866.1ef0.1ab9.7c28.a484.2125.ac23
          ::
            0x2.4d90.2e1a.2fc7.a875.5ab5.b694.c575.fce7.
                42c4.8d9f.f192.e63d.f519.3e4c.7afe.1f9c
          ::
            0x9452.b549.be8c.ea3e.cb7a.84be.c10d.cfd9.
              4afe.4d12.9ebf.d3b3.cb58.eedf.394e.d271
          ::
            %+  weld
              "xpub6FnCn6nSzZAw5Tw7cgR9bi15UV96gLZhjDstkXXxvCLsUXBGXPdSnLFbd"
            "pq8p9HmGsApME5hQTZ3emM2rnY5agb9rXpVGyy3bdW6EEgAtqt"
          ::
            %+  weld
              "xprvA2nrNbFZABcdryreWet9Ea4LvTJcGsqrMzxHx98MMrotbir7yrKCEXw7n"
            "adnHM8Dq38EGfSh6dqA9QWTyefMLEcBYJUuekgW4BYPJcr9E7j"
    ==  ==
  --
--
