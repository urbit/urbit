/+  *test, *bitcoin, *bitcoin-utils, bip32
=,  secp:crypto
=+  ecc=secp256k1
|%
+$  chyg  ?(%0 %1)
+$  bits-vector  [bitwidth=@ atoms=(list @) =bits]
+$  compact-size-vector  @ux
+$  tx-vector  [hex-cord=@t txid=hexb]
+$  xpub-vector
  $:  =xpub
      =network
      hdpath=[=bipt =chyg =idx]
      pubkey=hexb
      =address
  ==
+$  script-pubkey-vector  [=address spk=hexb]
::
++  bits-vectors
  ^-  (list bits-vector)
  :~  :*  5
          ~[0 31 31 0 31 0]
          [30 0b1.1111.1111.1000.0011.1110.0000]
      ==
  ==
::
++  compact-size-vectors
  ^-  (list compact-size-vector)
  :~  0x98
      0x302
      0xaa.bbcc
      0xaabb.ccdd
      0xaa.bbcc.ddee
      0xaabb.ccdd.eeff.1122 
  ==
::
++  tx-vectors
  ^-  (list tx-vector)
  :~  :*  '0200000002ab0949a08c5af7c49b8212f417e2f15ab3f5c33dcf153821a8139f877a5b7be40000000000feffffffab0949a08c5af7c49b8212f417e2f15ab3f5c33dcf153821a8139f877a5b7be40100000000feffffff02603bea0b000000001976a914768a40bbd740cbe81d988e71de2a4d5c71396b1d88ac8e240000000000001976a9146f4620b553fa095e721b9ee0efe9fa039cca459788ac00000000'
          32^0xfed6.cd1f.de4d.b4e1.3e7e.8003.17e3.7f9c.bd75.ec36.4389.670e.eff8.0da9.93c7.e560
      ==
      ::
      :*  '01000000000102267b34b058a44e678ca0609825fe37c5bb893462337334ad5a2e887b8ebef65c000000002322002049f80613b30fe3063d4e6ce75c53a7bc573ea17fe49dcb805aca416a8af5d991ffffffff666f99bf914bb18e28ec39af93d99a1938767fbec892408acced7c80663f0df40000000023220020096def7756afb4dba661ec58602cf6af1f7881e48e4b8a8c12be9985073f5adeffffffff06602d59010000000017a914572290324c72e6842e8a77c2cbb9882a3b9c2a9f87195c0e00000000001976a914abfdf3698ceef95986b31b763e6764cfe3ce584e88ac68642400000000001976a91456cf5fcc3654c5646b930e8773a95dce98c49e0588acfbb34b00000000001976a914518ee0d1b48f3d99f76e6e8283006610e39aeeba88acf492560000000000160014d1930fff9862af879ee14ecd3e1b9dc1099524ce0374c802000000001976a9148a6727bc345abeae523b6af7828053f95332918688ac03483045022100fcc8336b7c81e67cc7b53587fb06c3f950a6d3349e658594a444618c75988e45022065b3b957e0f7d2def98565a34c1ee7843d60525c4337730fa5d6ac8ff7aacb89012102ac604909ed86488338ec6255b0bdc0162562b299e66b860480a8bd2b99c7f3291976a9140e60a2ad39efd10ad61e2a3e6e5c1baa73190ee088ac0347304402202ec01f623cd48ba990caea70463de86b37ffb2363640b510ce9d80c750a54eec02204915d11649d636e5e8503b226d7a6a45da0163f3f0ca4b07bdaaa9af4d6f850f012102a52b3f9958c0f4b57b99f287832ea75775ddf7c83fa0648b6b1545ec4881ef2d1976a91401121fe150c9b05f9146bac57ad9947ea5c1478e88ac00000000'
          32^0x2b9c.60c4.dfcd.0aa2.b1b8.83a5.0a4a.2a96.197b.07d8.cdd1.e749.f0a1.f296.0f43.b339
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
          %+  category  "compact-size en/decoding"
          (zing (turn compact-size-vectors check-compact-size))
          %+   category  "check TX en/decoding"
          (zing (turn tx-vectors check-tx))
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
      ::  TODO: from-atoms works, but to-atoms doesn't
      %+  expect-eq
        !>(bits.v)
        !>((from-atoms:bit bitwidth.v atoms.v))
      %+  expect-eq
        !>(atoms.v)
        !>((to-atoms:bit bitwidth.v bits.v))
    ==
  ::
  ++  check-compact-size
    |=  v=compact-size-vector
    %+  expect-eq
      !>(v)
      !>(dat:n:(de:csiz (en:csiz v)))
  ::
  ++  check-tx
    |=  v=tx-vector
    %+  expect-eq
      !>(txid.v)
      !>((get-id:txu (decode:txu (from-cord:hxb hex-cord.v))))
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
      !>((from-pubkey:adr b n pubkey.v))
  ::
  ++  check-script-pubkey-derivation
    |=  v=script-pubkey-vector
    %+  expect-eq
      !>(spk.v)
      !>((to-script-pubkey:adr address.v))
  --
::
--
