/+  *test, *btc, bip32
=,  secp:crypto
=+  ecc=secp256k1
|%
+$  chyg  ?(%0 %1)
+$  bits-vector  [bitwidth=@ atoms=(list @) =bits]
+$  tx-vector  [hex-cord=@t txid=hexb]
+$  psbt-vector
  $:  =hdkey
      hdkey-hex=hexb
  ==
+$  xpub-vector
  $:  =xpub
      =network
      hdpath=[=bipt =chyg =idx]
      pubkey=hexb
      =address
  ==
+$  script-pubkey-vector  [=address spk=hexb]
::
++  fprint  4^0xdead.beef
++  bits-vectors
  ^-  (list bits-vector)
  :~  :*  5
          ~[0 31 31 0 31 0]
          [30 0b1.1111.1111.1000.0011.1110.0000]
      ==
  ==
::
++  tx-vectors
  ^-  (list tx-vector)
  :~  :*  '0200000002ab0949a08c5af7c49b8212f417e2f15ab3f5c33dcf153821a8139f877a5b7be40000000000feffffffab0949a08c5af7c49b8212f417e2f15ab3f5c33dcf153821a8139f877a5b7be40100000000feffffff02603bea0b000000001976a914768a40bbd740cbe81d988e71de2a4d5c71396b1d88ac8e240000000000001976a9146f4620b553fa095e721b9ee0efe9fa039cca459788ac00000000'
          32^0xfed6.cd1f.de4d.b4e1.3e7e.8003.17e3.7f9c.bd75.ec36.4389.670e.eff8.0da9.93c7.e560
      ==
  ==
::
++  psbt-vectors
  ^-  (list psbt-vector)
  :~  :*  [fprint 33^0x1 %testnet %44 %0 1]
          20^0x2c00.0080.0100.0080.0000.0080.0000.0000.0100.0000
  ==
      ::
      :*  [fprint 33^0x1 %testnet %49 %0 1]
          20^0x3100.0080.0100.0080.0000.0080.0000.0000.0100.0000
      ==
      ::
      :*  [fprint 33^0x1 %testnet %84 %0 1]
          20^0x5400.0080.0100.0080.0000.0080.0000.0000.0100.0000
      ==
      ::
      :*  [fprint 33^0x1 %main %44 %0 1]
          20^0x2c00.0080.0000.0080.0000.0080.0000.0000.0100.0000
      ==
      ::
      :*  [fprint 33^0x1 %main %49 %0 1]
          20^0x3100.0080.0000.0080.0000.0080.0000.0000.0100.0000
      ==
      ::
      :*  [fprint 33^0x1 %main %84 %0 1]
          20^0x5400.0080.0000.0080.0000.0080.0000.0000.0100.0000
      ==
  ==
::  below use mnemonic:
::  abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about
::
++  xpub-vectors
  ^-  (list xpub-vector)
  :~  :*  'tpubDC5FSnBiZDMmhiuCmWAYsLwgLYrrT9rAqvTySfuCCrgsWz8wxMXUS9Tb9iVMvcRbvFcAHGkMD5Kx8koh4GquNGNTfohfk7pgjhaPCdXpoba'
          %testnet
          [%44 %0 0]
          33^0x2.a745.1395.7353.69f2.ecdf.c829.c0f7.74e8.8ef1.303d.fe5b.2f04.dbaa.b30a.535d.fdd6
          [%base58 0cmkpZhYtJu2r87Js3pDiWJDmPte2NRZ8bJV]
      ==
      ::
      :*  'upub5EFU65HtV5TeiSHmZZm7FUffBGy8UKeqp7vw43jYbvZPpoVsgU93oac7Wk3u6moKegAEWtGNF8DehrnHtv21XXEMYRUocHqguyjknFHYfgY'
          %testnet
          [%49 %0 0]
          33^0x3.a1af.804a.c108.a8a5.1782.198c.2d03.4b28.bf90.c880.3f5a.53f7.6276.fa69.a4ea.e77f
          [%base58 0c2Mww8dCYPUpKHofjgcXcBCEGmniw9CoaiD2]
      ==
      ::
      :*  'vpub5Y6cjg78GGuNLsaPhmYsiw4gYX3HoQiRBiSwDaBXKUafCt9bNwWQiitDk5VZ5BVxYnQdwoTyXSs2JHRPAgjAvtbBrf8ZhDYe2jWAqvZVnsc'
          %testnet
          [%84 %0 0]
          33^0x2.e7ab.2537.b5d4.9e97.0309.aae0.6e9e.49f3.6ce1.c9fe.bbd4.4ec8.e0d1.cca0.b4f9.c319
          [%bech32 'tb1q6rz28mcfaxtmd6v789l9rrlrusdprr9pqcpvkl']
      ==
      ::
      :*  'xpub6BosfCnifzxcFwrSzQiqu2DBVTshkCXacvNsWGYJVVhhawA7d4R5WSWGFNbi8Aw6ZRc1brxMyWMzG3DSSSSoekkudhUd9yLb6qx39T9nMdj'
          %main
          [%44 %0 0]
          33^0x3.aaeb.52dd.7494.c361.049d.e67c.c680.e83e.bcbb.bdbe.b136.37d9.2cd8.45f7.0308.af5e
          [%base58 0c1LqBGSKuX5yYUonjxT5qGfpUsXKYYWeabA]
      ==
      ::
      :*  'ypub6Ww3ibxVfGzLrAH1PNcjyAWenMTbbAosGNB6VvmSEgytSER9azLDWCxoJwW7Ke7icmizBMXrzBx9979FfaHxHcrArf3zbeJJJUZPf663zsP'
          %main
          [%49 %0 0]
          33^0x3.9b3b.694b.8fc5.b5e0.7fb0.69c7.83ca.c754.f5d3.8c3e.08be.d196.0e31.fdb1.dda3.5c24
          [%base58 0c37VucYSaXLCAsxYyAPfbSi9eh4iEcbShgf]
      ==
      ::
      :*  'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
          %main
          [%84 %0 0]
          33^0x3.30d5.4fd0.dd42.0a6e.5f8d.3624.f5f3.482c.ae35.0f79.d5f0.753b.f5be.ef9c.2d91.af3c
          [%bech32 'bc1qcr8te4kr609gcawutmrza0j4xv80jy8z306fyu']
      ==
  ==
::
++  script-pubkey-vectors
  ^-  (list script-pubkey-vector)
  :~  :*  [%bech32 'bc1qcr8te4kr609gcawutmrza0j4xv80jy8z306fyu']
          [wid=22 dat=0x14.c0ce.bcd6.c3d3.ca8c.75dc.5ec6.2ebe.5533.0ef9.10e2]
      ==
      ::
      :*  [%bech32 'bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3']
          [wid=34 dat=0x20.1863.143c.14c5.1668.04bd.1920.3356.da13.6c98.5678.cd4d.27a1.b8c6.3296.0490.3262]
      ==
      ::
      :*  [%bech32 'tb1q6rz28mcfaxtmd6v789l9rrlrusdprr9pqcpvkl']
          [wid=22 dat=0x14.d0c4.a3ef.09e9.97b6.e99e.397e.518f.e3e4.1a11.8ca1]
      ==
      ::
      :*  [%base58 0c1LqBGSKuX5yYUonjxT5qGfpUsXKYYWeabA]
          [wid=25 dat=0x76.a914.d986.ed01.b7a2.2225.a70e.dbf2.ba7c.fb63.a15c.b3aa.88ac]
      ==
      ::
      :*  [%base58 0cmxVFsFW5N4mu1HPkxPttorvocvzeZ7KZyk]
          [wid=25 dat=0x76.a914.ba27.f99e.007c.7f60.5a83.05e3.18c1.abde.3cd2.20ac.88ac]
      ==
      ::
      :*  [%base58 0cmfWxJ45yp2SFn7UciZyNpvDKrzbhyfKrY8]
          [wid=25 dat=0x76.a914.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.88ac]
      ==
      ::
      :*  [%base58 0c37VucYSaXLCAsxYyAPfbSi9eh4iEcbShgf]
          [wid=23 dat=0xa9.143f.b6e9.5812.e57b.b469.1f9a.4a62.8862.a61a.4f76.9b87]
      ==
      ::
      :*  [%base58 0c2MvLWCyKPQQ6oqJKSJ9ic8hYVmLyNry6yuF]
          [wid=23 dat=0xa9.1421.e7fe.f309.cf6f.6cfc.fe94.c572.e541.d74f.d848.5487]
      ==
  ==
::
++  mk-pubkey
|=  [=xpub =chyg =idx]
  ^-  hexb
  =/  pk=@ux
    %-  compress-point:ecc
    pub:(derive-public:(derive-public:(from-extended:bip32 (trip xpub)) (@ chyg)) idx)
 [(met 3 pk) pk]
::
++  test-all-vectors
^-  tang
  |^  ;:  weld
          %+  category  "bit manipulation"
          (zing (turn bits-vectors check-bits))
          %+  category  "check PSBT"
          (zing (turn psbt-vectors check-psbt))
          %+  category  "xpub parsing"
          (zing (turn xpub-vectors check-xpub-parsing))
          %+  category  "pubkey derivation"
          (zing (turn xpub-vectors check-pubkey-derivation))
          %+  category  "address derivation"
          (zing (turn xpub-vectors check-address-derivation))
          %+  category  "script-pubkey derivation"
          (zing (turn script-pubkey-vectors check-script-pubkey-derivation))
      ==
  ::
  ++  check-bits
    |=  v=bits-vector
    ;:  weld
      %+  expect-eq
        !>(bits.v)
        !>((from-atoms:bit bitwidth.v atoms.v))
      %+  expect-eq
        !>(atoms.v)
        !>((to-atoms:bit bitwidth.v bits.v))
    ==
  ::
  ++  check-psbt
    |=  v=psbt-vector
    =/  key=hexb
      (cat:byt ~[1^0x6 pubkey.hdkey.v])          ::  %input target
    %+  expect-eq
      !>([key (cat:byt ~[fprint.hdkey.v hdkey-hex.v])])
      !>((hdkey:en:pbt %input hdkey.v))
  ::
  ++  check-xpub-parsing
    |=  v=xpub-vector
    =/  [b=bipt n=network]  (xpub-type xpub.v)
    %+  expect-eq
      !>([b n])
      !>([bipt.hdpath.v network.v])
  ::
  ++  check-pubkey-derivation
    |=  v=xpub-vector
    %+  expect-eq
      !>(pubkey.v)
      !>((mk-pubkey xpub.v chyg.hdpath.v idx.hdpath.v))
  ::
  ++  check-address-derivation
    |=  v=xpub-vector
    =/  [b=bipt n=network]  (xpub-type xpub.v)
    %+  expect-eq
      !>(address.v)
      !>((pubkey-to-address b n pubkey.v))
  ::
  ++  check-script-pubkey-derivation
    |=  v=script-pubkey-vector
    %+  expect-eq
      !>(spk.v)
      !>((script-pubkey address.v))
  --
::
--
