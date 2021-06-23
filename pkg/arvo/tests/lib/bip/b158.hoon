/+  *test, *bip-b158, *bitcoin-utils
|%
+$  filter-vector
  $:  filter=hexb
      expect=[parse=[n=@ux gcs-set=bits] decode=[delta=@ rest=bits]]
==
+$  siphash-vector
  $:  blockhash=tape
      filter=hexb
      item=hexb
      expect=@
  ==
+$  match-vector
  $:  blockhash=tape
      filter=hexb
      inc-spks=(list hexb)
      exc-spks=(list hexb)
      expect=(list @)
  ==
::
++  filter-vectors
  ^-  (list filter-vector)
  :~
    ::  testnet genesis block
    ::
    :*  4^0x19d.fca8
        :*  0x1
            24^0b1001.1101.1111.1100.1010.1000
        ==
        [769.941 [3 0b0]]
    ==
    ::  testnet block 926485
    ::
    :*  25^0x9.027a.cea6.1b6c.c3fb.33f5.d52f.7d08.8a6b.2f75.d234.e89c.a800
        :*  0x9
            192^0b10.0111.1010.1100.1110.1010.0110.0001.1011.0110.1100.1100.0011.1111.1011.0011.0011.1111.0101.1101.0101.0010.1111.0111.1101.0000.1000.1000.1010.0110.1011.0010.1111.0111.0101.1101.0010.0011.0100.1110.1000.1001.1100.1010.1000.0000.0000
        ==
        [10.156 172^0b1110.1010.0110.0001.1011.0110.1100.1100.0011.1111.1011.0011.0011.1111.0101.1101.0101.0010.1111.0111.1101.0000.1000.1000.1010.0110.1011.0010.1111.0111.0101.1101.0010.0011.0100.1110.1000.1001.1100.1010.1000.0000.0000]
    ==
    ::  3 vectors with large Ns (i.e. CompactSize starting with 0xfd/fe/ff)
    ::
    :*  6^0xfd88.279d.fca8
        :*  0x2788
            24^0b1001.1101.1111.1100.1010.1000
        ==
        [769.941 [3 0b0]]
    ==
    ::
    :*   8^0xfe11.2233.449d.fca8
         :*  0x4433.2211
             24^0b1001.1101.1111.1100.1010.1000
         ==
         [769.941 [3 0b0]]
    ==
    ::
    :*  12^0xff11.2233.4455.6677.889d.fca8
        :*  0x8877.6655.4433.2211
            24^0b1001.1101.1111.1100.1010.1000
        ==
        [769.941 [3 0b0]]
    ==
  ==
::
++  siphash-vectors
  ^-  (list siphash-vector)
  ::  testnet genesis block
  :~  :*  "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"
          4^0x19d.fca8
          67^0x41.0467.8afd.b0fe.5548.2719.67f1.a671.30b7.105c.d6a8.28e0.3909.a679.62e0.ea1f.61de.b649.f6bc.3f4c.ef38.c4f3.5504.e51e.c112.de5c.384d.f7ba.0b8d.578a.4c70.2b6b.f11d.5fac
          769.941
      ==
  ==
::
++  match-vectors
  ^-  (list match-vector)
  ::  testnet genesis block
  :~  :*  "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"
          4^0x19d.fca8
          ~[67^0x41.0467.8afd.b0fe.5548.2719.67f1.a671.30b7.105c.d6a8.28e0.3909.a679.62e0.ea1f.61de.b649.f6bc.3f4c.ef38.c4f3.5504.e51e.c112.de5c.384d.f7ba.0b8d.578a.4c70.2b6b.f11d.5fac]
          ~[25^0x76.a914.3ebc.40e4.11ed.3c76.f867.1150.7ab9.5230.0890.3972.88ac]
          ~[271.501 769.941]
      ==
  ::  testnet block 926485
  ::
      :*  "000000000000015d6077a411a8f5cc95caf775ccf11c54e27df75ce58d187313"
          25^0x9.027a.cea6.1b6c.c3fb.33f5.d52f.7d08.8a6b.2f75.d234.e89c.a800
          :~  25^0x76.a914.3ebc.40e4.11ed.3c76.f867.1150.7ab9.5230.0890.3972.88ac
              25^0x76.a914.5033.3046.115e.aa0a.c9e0.2165.65f9.4507.0e44.5739.88ac
          ==
          :~  21^0x14.7e69.a44c.1a94.2139.c8ab.4127.8325.5e1e.46d0.f0da
          ==
          ~[176.536 2.341.508 3.078.625]
      ==
  ==
::
++  test-all-vectors
  =/  [p=@ m=@]  [p:params m:params]
  ^-  tang
  |^  ;:  weld
          %+  category  "parse filters"
          (zing (turn filter-vectors check-filter-parse))
          %+  category  "decode GCS"
          (zing (turn filter-vectors check-gcs-decode))
          %+  category  "siphash"
          (zing (turn siphash-vectors check-siphash))
          %+  category  "hash script-pubkeys"
          (zing (turn match-vectors check-hashing))
          %+  category  "whether filter matches any script-pubkey"
          (zing (turn match-vectors check-match))
          %+  category  "get all script-pubkey matches for a block filter"
          (zing (turn match-vectors check-all-match))
      ==
  ::
  ++  check-filter-parse
    |=  v=filter-vector
    %+  expect-eq
      !>(parse.expect.v)
      !>((parse-filter filter.v))
   ::
   ++  check-gcs-decode
    |=  v=filter-vector
    %+  expect-eq
      !>(decode.expect.v)
      !>((de:gol gcs-set:(parse-filter filter.v) p))
  ::
  ++  check-siphash
    |=  v=siphash-vector
    =+  f=(mul n:(parse-filter filter.v) m)
    %+  expect-eq
      !>(expect.v)
      !>((to-range:hsh item.v f (to-key blockhash.v)))
  ::
  ++  check-hashing
    |=  v=match-vector
    =/  [n=@ux gcs-set=bits]  (parse-filter filter.v)
    =+  k=(to-key blockhash.v)
    %+  expect-eq
      !>(expect.v)
      !>((set-construct:hsh (weld inc-spks.v exc-spks.v) k (mul n m)))
  ::
  ++  check-match
    |=  v=match-vector
    =+  k=(to-key blockhash.v)
    %+  weld
      %+  expect-eq
        !>(%.y)
        !>((match filter.v k inc-spks.v))
    %+  expect-eq
      !>(%.n)
      !>((match filter.v k exc-spks.v))
  ::
  ++  check-all-match
    |=  v=match-vector
    =/  b=hexb  (from-cord:hxb (crip blockhash.v))
    =/  inc=(list [address hexb])  (turn inc-spks.v |=(h=hexb [*address h]))
    =/  exc=(list [address hexb])  (turn exc-spks.v |=(h=hexb [*address h]))
    %+  expect-eq
      !>(`(set [address hexb])`(sy inc))
      !>(`(set [address hexb])`(all-match filter.v b (weld inc exc)))
  --
--
