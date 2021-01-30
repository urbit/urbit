/+  *test, *bip158
|%
+$  filter  [blockhash=tape fil=@ux]
+$  vector
  $:  =filter
      included-script-pubkeys=(list hexb)
      excluded-script-pubkeys=(list hexb)
      expect=[hashed-set=(list @)]
  ==
::
++  block-filter-vectors
  ^-  (list filter)
  :~
    ::  genesis block
    ::
    :*  "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"
        0x19d.fca8
    ==
    ::  block 926485
    ::
    :*  "000000000000015d6077a411a8f5cc95caf775ccf11c54e27df75ce58d187313"
        0x9.027a.cea6.1b6c.c3fb.33f5.d52f.7d08.8a6b.2f75.d234.e89c.a800
    ==
  ==
::
++  vectors  3
::
++  run
  =/  [p=@ m=@]  [p:params m:params]
  ::  Filter parsing and GCS decoding
  ::
  =/  bfil=filter  test-genesis:block-filters
  =/  [n=@ux gcs-set=bits]  (parse-filter fil.bfil)
  =/  result
    [delta=769.941 [3 0b0]]
  ?.  =(result (de:gol gcs-set p))
    ~|("Filter Parse/GCS Decoding Failed" !!)
  ::  Filter parsing for large N
  ::  TODO: tests for N when prefixed with 0xfd/0xfe/0xff
  ::  TODO:
  ::
  ::  Siphash
  ::
  =/  bfil=filter  test-genesis:block-filters
  =/  [n=@ux gcs-set=bits]  (parse-filter fil.bfil)
  =+  f=(mul n m:params)
  =+  k=(to-key blockhash.bfil)
  =/  item
    [67 0x41.0467.8afd.b0fe.5548.2719.67f1.a671.30b7.105c.d6a8.28e0.3909.a679.62e0.ea1f.61de.b649.f6bc.3f4c.ef38.c4f3.5504.e51e.c112.de5c.384d.f7ba.0b8d.578a.4c70.2b6b.f11d.5fac]
  =/  result  (rsh [0 64] (mul f (swp 3 dat:(siphash k item))))
  ?.  =(769.941 result)
    ~|("Siphash and mod F failed" !!)
  ::  Hashing and matching scriptpubkeys
  ::
  =/  bfil=filter  test-926485:block-filters
  =/  [n=@ux gcs-set=bits]  (parse-filter fil.bfil)
  =+  k=(to-key blockhash.bfil)
  =+  in1=[25 0x76.a914.3ebc.40e4.11ed.3c76.f867.1150.7ab9.5230.0890.3972.88ac]
  =+  in2=[25 0x76.a914.5033.3046.115e.aa0a.c9e0.2165.65f9.4507.0e44.5739.88ac]
  =+  out1=[21 0x14.7e69.a44c.1a94.2139.c8ab.4127.8325.5e1e.46d0.f0da]
  =/  result
    (set-construct:hsh ~[in1 in2 out1] k (mul n m:params))
  ?.  =(~[176.536 2.341.508 3.078.625] result)
    ~|("Hashing scriptpubkeys failed" !!)
  ?.  =([%.y %.n] [(match fil.bfil k ~[in1 in2 out1]) (match fil.bfil k ~[out1])])
    ~|("Matching scriptpubkeys to block failed" !!)
  "Tests passed"
--
