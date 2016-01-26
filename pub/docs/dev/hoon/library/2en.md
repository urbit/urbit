section 2eN, pseudo-cryptography
================================

### `++un`

Reversible scrambling core

    ++  un                                                  ::  =(x (wred (wren x)))
      |%

A core that contains arms that perform reversible scrambling operations.
Used in the `@p` phonetic base.

------------------------------------------------------------------------

### `++wren`

Conceal structure

      ++  wren                                              ::  conceal structure
        |=  pyn=@  ^-  @
        =+  len=(met 3 pyn)
        ?:  =(0 len)
          0
        =>  .(len (dec len))
        =+  mig=(zaft (xafo len (cut 3 [len 1] pyn)))
        %+  can  3
        %-  flop  ^-  (list ,[@ @])
        :-  [1 mig]
        |-  ^-  (list ,[@ @])
        ?:  =(0 len)
          ~
        =>  .(len (dec len))
        =+  mog=(zyft :(mix mig (end 3 1 len) (cut 3 [len 1] pyn)))
        [[1 mog] $(mig mog)]
      ::

Scrambles a bytestring `pyn` by adding the current position to each
byte, looking it up in an s-box, and then performing the XOR operation
on the result, pushing it forward. Produces an atom.

`pyn` is an [atom]().

    ~zod/try=> `@ux`(wren:un 'testing')
    0x30.bf6a.b9fe.7d8f
    ~zod/try=> `@ux`'testing'
    0x67.6e69.7473.6574
    ~zod/try=> `@da`(wred:un (wren:un ~2001.2.5))
    ~2001.2.5

------------------------------------------------------------------------

### `++wred`

Restore structure

      ++  wred                                              ::  restore structure
        |=  cry=@  ^-  @
        =+  len=(met 3 cry)
        ?:  =(0 len)
          0
        =>  .(len (dec len))
        =+  mig=(cut 3 [len 1] cry)
        %+  can  3
        %-  flop  ^-  (list ,[@ @])
        :-  [1 (xaro len (zart mig))]
        |-  ^-  (list ,[@ @])
        ?:  =(0 len)
          ~
        =>  .(len (dec len))
        =+  mog=(cut 3 [len 1] cry)
        [[1 :(mix mig (end 3 1 len) (zyrt mog))] $(mig mog)]
      ::

Unscrambles a bytestring `cry` by subtracting the current position from
each byte, looking it up in an s-box, and performing the XOR operation
on the result, pushing it forward. Produces an atom.

`cry` is an [atom]().

    ~zod/try=> (wred:un 0x30.bf6a.b9fe.7d8f)
    29.113.321.805.538.676
    ~zod/try=> `@t`(wred:un 0x30.bf6a.b9fe.7d8f)
    'testing'
    ~zod/try=> (wred:un (wren:un 200.038.426))
    200.038.426

------------------------------------------------------------------------

### `++xafo`

Add modulo 255

      ++  xafo  |=([a=@ b=@] +((mod (add (dec b) a) 255)))

Produces the sum of two atoms modulo 255, encoded as a nonzero byte.

    ~zod/try=> (xafo:un 5 6)
    11
    ~zod/try=> (xafo:un 256 20)
    21

------------------------------------------------------------------------

### `++xaro`

Subtract modulo 255

      ++  xaro  |=([a=@ b=@] +((mod (add (dec b) (sub 255 (mod a 255))) 255)))

Produces the difference between two atoms modulo 255, encoded as a
nonzero byte.

    ~zod/try=> (xaro:un 17 57)
    40
    ~zod/try=> (xaro:un 265 12)
    2

------------------------------------------------------------------------

### `++zaft`

Look up in 255 sub box

      ++  zaft                                              ::  forward 255-sbox
        |=  a=@D
        =+  ^=  b
            0xcc.75bc.86c8.2fb1.9a42.f0b3.79a0.92ca.21f6.1e41.cde5.fcc0.
            7e85.51ae.1005.c72d.1246.07e8.7c64.a914.8d69.d9f4.59c2.8038.
            1f4a.dca2.6fdf.66f9.f561.a12e.5a16.f7b0.a39f.364e.cb70.7318.
            1de1.ad31.63d1.abd4.db68.6a33.134d.a760.edee.5434.493a.e323.
            930d.8f3d.3562.bb81.0b24.43cf.bea5.a6eb.52b4.0229.06b2.6704.
            78c9.45ec.d75e.58af.c577.b7b9.c40e.017d.90c3.87f8.96fa.1153.
            0372.7f30.1c32.ac83.ff17.c6e4.d36d.6b55.e2ce.8c71.8a5b.b6f3.
            9d4b.eab5.8b3c.e7f2.a8fe.9574.5de0.bf20.3f15.9784.9939.5f9c.
            e609.564f.d8a4.b825.9819.94aa.2c08.8e4c.9b22.477a.2840.3ed6.
            3750.6ef1.44dd.89ef.6576.d00a.fbda.9ed2.3b6c.7b0c.bde9.2ade.
            5c88.c182.481a.1b0f.2bfd.d591.2726.57ba
        (cut 3 [(dec a) 1] b)
      ::

The inverse of [`++zart`](). Looks up a nonzero byte`a\` in a substiution
box with 255 values, producing a unique nonzero byte.

`a` is an [atom]() of one byte in length.

    ~zod/try=> (zaft:un 0x12)
    42
    ~zod/try=> (zaft:un 0xff)
    204
    ~zod/try=> (zaft:un 0x0)
    ! decrement-underflow
    ! exit

------------------------------------------------------------------------

### `++zart`

Reverse look up in 255 sub box

      ++  zart                                              ::  reverse 255-sbox
        |=  a=@D
        =+  ^=  b
            0x68.4f07.ea1c.73c9.75c2.efc8.d559.5125.f621.a7a8.8591.5613.
            dd52.40eb.65a2.60b7.4bcb.1123.ceb0.1bd6.3c84.2906.b164.19b3.
            1e95.5fec.ffbc.f187.fbe2.6680.7c77.d30e.e94a.9414.fd9a.017d.
            3a7e.5a55.8ff5.8bf9.c181.e5b6.6ab2.35da.50aa.9293.3bc0.cdc6.
            f3bf.1a58.4130.f844.3846.744e.36a0.f205.789e.32d8.5e54.5c22.
            0f76.fce7.4569.0d99.d26e.e879.dc16.2df4.887f.1ffe.4dba.6f5d.
            bbcc.2663.1762.aed7.af8a.ca20.dbb4.9bc7.a942.834c.105b.c4d4.
            8202.3e61.a671.90e6.273d.bdab.3157.cfa4.0c2e.df86.2496.f7ed.
            2b48.2a9d.5318.a343.d128.be9c.a5ad.6bb5.6dfa.c5e1.3408.128d.
            2c04.0339.97a1.2ff0.49d0.eeb8.6c0a.0b37.b967.c347.d9ac.e072.
            e409.7b9f.1598.1d3f.33de.8ce3.8970.8e7a
        (cut 3 [(dec a) 1] b)
      ::

The inverse of [`++zaft`](). Looks up the index of a nonzero byte `a` in
the substitution box with 255 values, producing a unique nonzero byte.

`a` is an [atom]() of one byte in length.

    ~zod/try=> `@ux`(zart:un 204)
    0xff
    ~zod/try=> `@ux`(zart:un 42)
    0x12

------------------------------------------------------------------------

### `++zyft`

Lookup byte in 256 sub box

      ++  zyft                                              ::  forward 256-sbox
        |=  a=@D
        =+  ^=  b
            0xbb49.b71f.b881.b402.17e4.6b86.69b5.1647.115f.dddb.7ca5.
              8371.4bd5.19a9.b092.605d.0d9b.e030.a0cc.78ba.5706.4d2d.
              986a.768c.f8e8.c4c7.2f1c.effe.3cae.01c0.253e.65d3.3872.
              ce0e.7a74.8ac6.daac.7e5c.6479.44ec.4143.3d20.4af0.ee6c.
              c828.deca.0377.249f.ffcd.7b4f.eb7d.66f2.8951.042e.595a.
              8e13.f9c3.a79a.f788.6199.9391.7fab.6200.4ce5.0758.e2f1.
              7594.c945.d218.4248.afa1.e61a.54fb.1482.bea4.96a2.3473.
              63c2.e7cb.155b.120a.4ed7.bfd8.b31b.4008.f329.fca3.5380.
              9556.0cb2.8722.2bea.e96e.3ac5.d1bc.10e3.2c52.a62a.b1d6.
              35aa.d05e.f6a8.0f3b.31ed.559d.09ad.f585.6d21.fd1d.8d67.
              370b.26f4.70c1.b923.4684.6fbd.cf8b.5036.0539.9cdc.d93f.
              9068.1edf.8f33.b632.d427.97fa.9ee1
        (cut 3 [a 1] b)
      ::

The inverse of [`++zyrt`](). Looks up a byte `a` in a substituion box
with 256 values, producing a byte.

`a` is an [atom]() of one byte in length.

    ~zod/try=> (zyft:un 0x12)
    57
    ~zod/try=> (zyft:un 0x0)
    225
    ~zod/try=> (zyft:un 0xff)
    187

------------------------------------------------------------------------

### `++zyrt`

Reverse lookup byte in 256 sub box

      ++  zyrt                                              ::  reverse 256-sbox
        |=  a=@D
        =+  ^=  b
            0x9fc8.2753.6e02.8fcf.8b35.2b20.5598.7caa.c9a9.30b0.9b48.
              47ce.6371.80f6.407d.00dd.0aa5.ed10.ecb7.0f5a.5c3a.e605.
              c077.4337.17bd.9eda.62a4.79a7.ccb8.44cd.8e64.1ec4.5b6b.
              1842.ffd8.1dfb.fd07.f2f9.594c.3be3.73c6.2cb6.8438.e434.
              8d3d.ea6a.5268.72db.a001.2e11.de8c.88d3.0369.4f7a.87e2.
              860d.0991.25d0.16b9.978a.4bf4.2a1a.e96c.fa50.85b5.9aeb.
              9dbb.b2d9.a2d1.7bba.66be.e81f.1946.29a8.f5d2.f30c.2499.
              c1b3.6583.89e1.ee36.e0b4.6092.937e.d74e.2f6f.513e.9615.
              9c5d.d581.e7ab.fe74.f01b.78b1.ae75.af57.0ec2.adc7.3245.
              12bf.2314.3967.0806.31dc.cb94.d43f.493c.54a6.0421.c3a1.
              1c4a.28ac.fc0b.26ca.5870.e576.f7f1.616d.905f.ef41.33bc.
              df4d.225e.2d56.7fd6.1395.a3f8.c582
        (cut 3 [a 1] b)

The inverse of [`++zyft`](/doc/hoon/library/2en#++zyft). Looks up a byte `a` in a substituion box
with 256 values, producing a byte.

`a` is an [atom]() of one byte in length.

    ~zod/try=> `@ux`(zyrt:un 57)
    0x12
    ~zod/try=> `@ux`(zyrt:un 225)
    0x0
    ~zod/try=> `@ux`(zyrt:un 187)
    0xff

### `++ob`

Reversible scrambling core, v2

    ++  ob
      |%

A core for performing reversible scrambling operations for the `@p` phonetic base.

------------------------------------------------------------------------

### `++feen`

Conceal structure, v2

    ++  feen                                              ::  conceal structure v2
      |=  pyn=@  ^-  @
      ?:  &((gte pyn 0x1.0000) (lte pyn 0xffff.ffff))
        (add 0x1.0000 (fice (sub pyn 0x1.0000)))
      ?:  &((gte pyn 0x1.0000.0000) (lte pyn 0xffff.ffff.ffff.ffff))
        =+  lo=(dis pyn 0xffff.ffff)
        =+  hi=(dis pyn 0xffff.ffff.0000.0000)
        %+  con  hi
        (add 0x1.0000 (fice (sub lo 0x1.0000)))
      pyn

Randomly permutes atoms that fit into 17 to 32 bits into one another. If the atom fits into 33 to 64 bits, does the same permutation on the low 32 bits only. Otherwise, passes the atom through unchanged.

------------------------------------------------------------------------

### `++fend`

    ++  fend                                              ::  restore structure v2
      |=  cry=@  ^-  @
      ?:  &((gte cry 0x1.0000) (lte cry 0xffff.ffff))
        (add 0x1.0000 (teil (sub cry 0x1.0000)))
      ?:  &((gte cry 0x1.0000.0000) (lte cry 0xffff.ffff.ffff.ffff))
        =+  lo=(dis cry 0xffff.ffff)
        =+  hi=(dis cry 0xffff.ffff.0000.0000)
        %+  con  hi
        (add 0x1.0000 (teil (sub lo 0x1.0000)))
      cry

Randomly permutes atoms that fit into 17 to 32 bits into one another, and randomly permutes the low 32 bits of atoms that fit into 33 to 64 bits; otherwise, passes the atom through unchanged. The permutation is the inverse of the one applied by [`++feen`]().

------------------------------------------------------------------------

### `++fice`

    ++  fice                                              ::  adapted from
      |=  nor=@                                           ::  black and rogaway
      ^-  @                                               ::  "ciphers with
      =+  ^=  sel                                         ::   arbitrary finite
      %+  rynd  2                                         ::   domains", 2002
      %+  rynd  1
      %+  rynd  0
      [(mod nor 65.535) (div nor 65.535)]
      (add (mul 65.535 -.sel) +.sel)

Applies a 3-round Feistel-like cipher to randomly permute atoms in the range `0` to `2^32 - 2^16`. The construction given in Black and Rogaway is ideal for a domain with a size of that form, and as with a conventionel Feistel cipher, three rounds suffice to make the permutation pseudorandom.

------------------------------------------------------------------------

### `++teil`

    ++  teil                                              ::  reverse ++fice
      |=  vip=@
      ^-  @
      =+  ^=  sel
      %+  rund  0
      %+  rund  1
      %+  rund  2
      [(mod vip 65.535) (div vip 65.535)]
      (add (mul 65.535 -.sel) +.sel)

Applies the reverse of the Feistel-like cipher applied by [`++fice`](). Unlike a conventional Feistel cipher that is its own inverse if keys are used in reverse order, this Feistel-like cipher uses two moduli that must be swapped when applying the reverse transformation.

------------------------------------------------------------------------

### `++rynd`

    ++  rynd                                              ::  feistel round
      |=  [n=@ l=@ r=@]
      ^-  [@ @]
      :-  r
      ?~  (mod n 2)
        (~(sum fo 65.535) l (en:aesc (snag n raku) r))
      (~(sum fo 65.536) l (en:aesc (snag n raku) r))

A single round of the Feistel-like cipher [`++fice`](). AES ([`++aesc`]()) is used as the round function.

------------------------------------------------------------------------

### `++rund`

    ++  rund                                              ::  reverse round
      |=  [n=@ l=@ r=@]
      ^-  [@ @]
      :-  r
      ?~  (mod n 2)
        (~(dif fo 65.535) l (en:aesc (snag n raku) r))
      (~(dif fo 65.536) l (en:aesc (snag n raku) r))

A single round of the Feistel-like reverse cipher [`++teil`]().

------------------------------------------------------------------------

### `++raku`

    ++  raku
      ^-  (list ,@ux)
      :~  0x15f6.25e3.083a.eb3e.7a55.d4db.fb99.32a3.
            43af.2750.219e.8a24.e5f8.fac3.6c36.f968
          0xf2ff.24fe.54d0.1abd.4b2a.d8aa.4402.8e88.
            e82f.19ec.948d.b1bb.ed2e.f791.83a3.8133
          0xa3d8.6a7b.400e.9e91.187d.91a7.6942.f34a.
            6f5f.ab8e.88b9.c089.b2dc.95a6.aed5.e3a4
      ==

Arbitrary keys for use with [`++aesc`]().
