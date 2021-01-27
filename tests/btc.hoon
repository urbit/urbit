/+  *test, *btc
|%
+$  pubkey-vec  [=bipt =network pubkey=hexb =address]
+$  script-pubkey-vec  [=address spk=hexb]
++  vectors
  |%
  ++  base32
    :*  atoms=~[0 31 31 0 31 0] 
        bs=[30 0b1.1111.1111.1000.0011.1110.0000]
    ==
  ::  below use mnemonic:
  ::  abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about
  ::
  ++  pubkeys
    ^-  (list pubkey-vec)
    :~  :*  %84
            %main
            33^0x3.30d5.4fd0.dd42.0a6e.5f8d.3624.f5f3.482c.ae35.0f79.d5f0.753b.f5be.ef9c.2d91.af3c
            [%bech32 'bc1qcr8te4kr609gcawutmrza0j4xv80jy8z306fyu']
        ==
        ::
        :*  %44
            %main
            33^0x3.aaeb.52dd.7494.c361.049d.e67c.c680.e83e.bcbb.bdbe.b136.37d9.2cd8.45f7.0308.af5e
            [%base58 0c1LqBGSKuX5yYUonjxT5qGfpUsXKYYWeabA]
        ==
        ::
        :*  %49
            %main
            33^0x3.9b3b.694b.8fc5.b5e0.7fb0.69c7.83ca.c754.f5d3.8c3e.08be.d196.0e31.fdb1.dda3.5c24
            [%base58 0c37VucYSaXLCAsxYyAPfbSi9eh4iEcbShgf]
        ==
        ::
        :*  %84
            %testnet
            33^0x2.e7ab.2537.b5d4.9e97.0309.aae0.6e9e.49f3.6ce1.c9fe.bbd4.4ec8.e0d1.cca0.b4f9.c319
            [%bech32 'tb1q6rz28mcfaxtmd6v789l9rrlrusdprr9pqcpvkl']
        ==
        ::
        :*  %44
            %testnet
            33^0x2.a745.1395.7353.69f2.ecdf.c829.c0f7.74e8.8ef1.303d.fe5b.2f04.dbaa.b30a.535d.fdd6
            [%base58 0cmkpZhYtJu2r87Js3pDiWJDmPte2NRZ8bJV]
        ==
        ::
        :*  %49
            %testnet
            33^0x3.a1af.804a.c108.a8a5.1782.198c.2d03.4b28.bf90.c880.3f5a.53f7.6276.fa69.a4ea.e77f
            [%base58 0c2Mww8dCYPUpKHofjgcXcBCEGmniw9CoaiD2]
        ==
     ==
  ::
  ++  script-pubkeys
    ^-  (list script-pubkey-vec)
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
  --
::
++  run
  ::  bit manipulation
  ::
  =/  atoms=(list @)  -:base32:vectors
  =/  =bits           +:base32:vectors
  ?.  ?&  =((from-atoms:bit 5 atoms) bits)
          =((to-atoms:bit 5 bits) atoms)
      ==
    ~|("base32 bit manipulation failed" !!)
  ::  pubkey to address
  ?.  %+  levy  pubkeys:vectors
      |=  [=bipt =network pubkey=hexb =address]
      =(address (pubkey-to-address bipt network pubkey))
    ~|("pubkey doesn't encode to address" !!)
  ::
  ::  script-pubkey from address
  ::
  ?.  %+  levy  script-pubkeys:vectors
      |=  [a=address spk=hexb]
      =(spk (script-pubkey a))
    ~|("script-pubkey doesn't encode from address" !!)
  "All tests passed."
--
