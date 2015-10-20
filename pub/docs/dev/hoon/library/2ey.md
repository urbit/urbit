section 2eY, SHA-256
====================

### `++shad`

    ++  shad  |=(ruz=@ (shax (shax ruz)))                   ::  double sha-256

XX document

### `++shaf`

    ++  shaf                                                ::  half sha-256
      |=  [sal=@ ruz=@]
      =+  haz=(shas sal ruz)
      (mix (end 7 1 haz) (rsh 7 1 haz))
    ::

XX document

### `++shak`

    ++  shak                                                ::  XX shd be PBKDF
      |=  [who=@p wud=@]
      (shas (mix %shak who) wud)
    ::

XX document

### `++sham`

    ++  sham                                                ::  noun hash
      |=  yux=*  ^-  @uvH  ^-  @
      ?@  yux
        (shaf %mash yux)
      (shaf %sham (jam yux))
    ::

XX document

### `++shas`

    ++  shas                                                ::  salted hash
      |=  [sal=@ ruz=@]
      (shax (mix sal (shax ruz)))
    ::

XX document

### `++shax`

    ++  shax                                                ::  sha-256
      ~/  %shax
      |=  ruz=@  ^-  @
      ~|  %sha
      =+  [few==>(fe .(a 5)) wac=|=([a=@ b=@] (cut 5 [a 1] b))]
      =+  [sum=sum.few ror=ror.few net=net.few inv=inv.few]
      =+  ral=(lsh 0 3 (met 3 ruz))
      =+  ^=  ful
          %+  can  0
          :~  [ral ruz]
              [8 128]
              [(mod (sub 960 (mod (add 8 ral) 512)) 512) 0]
              [64 (~(net fe 6) ral)]
          ==
      =+  lex=(met 9 ful)
      =+  ^=  kbx  0xc671.78f2.bef9.a3f7.a450.6ceb.90be.fffa.
                     8cc7.0208.84c8.7814.78a5.636f.748f.82ee.
                     682e.6ff3.5b9c.ca4f.4ed8.aa4a.391c.0cb3.
                     34b0.bcb5.2748.774c.1e37.6c08.19a4.c116.
                     106a.a070.f40e.3585.d699.0624.d192.e819.
                     c76c.51a3.c24b.8b70.a81a.664b.a2bf.e8a1.
                     9272.2c85.81c2.c92e.766a.0abb.650a.7354.
                     5338.0d13.4d2c.6dfc.2e1b.2138.27b7.0a85.
                     1429.2967.06ca.6351.d5a7.9147.c6e0.0bf3.
                     bf59.7fc7.b003.27c8.a831.c66d.983e.5152.
                     76f9.88da.5cb0.a9dc.4a74.84aa.2de9.2c6f.
                     240c.a1cc.0fc1.9dc6.efbe.4786.e49b.69c1.
                     c19b.f174.9bdc.06a7.80de.b1fe.72be.5d74.
                     550c.7dc3.2431.85be.1283.5b01.d807.aa98.
                     ab1c.5ed5.923f.82a4.59f1.11f1.3956.c25b.
                     e9b5.dba5.b5c0.fbcf.7137.4491.428a.2f98
      =+  ^=  hax  0x5be0.cd19.1f83.d9ab.9b05.688c.510e.527f.
                     a54f.f53a.3c6e.f372.bb67.ae85.6a09.e667
      =+  i=0
      |-  ^-  @
      ?:  =(i lex)
        (rep 5 (turn (rip 5 hax) net))
      =+  ^=  wox
          =+  dux=(cut 9 [i 1] ful)
          =+  wox=(rep 5 (turn (rip 5 dux) net))
          =+  j=16
          |-  ^-  @
          ?:  =(64 j)
            wox
          =+  :*  l=(wac (sub j 15) wox)
                  m=(wac (sub j 2) wox)
                  n=(wac (sub j 16) wox)
                  o=(wac (sub j 7) wox)
              ==
          =+  x=:(mix (ror 0 7 l) (ror 0 18 l) (rsh 0 3 l))
          =+  y=:(mix (ror 0 17 m) (ror 0 19 m) (rsh 0 10 m))
          =+  z=:(sum n x o y)
          $(wox (con (lsh 5 j z) wox), j +(j))
      =+  j=0
      =+  :*  a=(wac 0 hax)
              b=(wac 1 hax)
              c=(wac 2 hax)
              d=(wac 3 hax)
              e=(wac 4 hax)
              f=(wac 5 hax)
              g=(wac 6 hax)
              h=(wac 7 hax)
          ==
      |-  ^-  @
      ?:  =(64 j)
        %=  ^$
          i  +(i)
          hax  %+  rep  5
               :~  (sum a (wac 0 hax))
                   (sum b (wac 1 hax))
                   (sum c (wac 2 hax))
                   (sum d (wac 3 hax))
                   (sum e (wac 4 hax))
                   (sum f (wac 5 hax))
                   (sum g (wac 6 hax))
                   (sum h (wac 7 hax))
               ==
        ==
      =+  l=:(mix (ror 0 2 a) (ror 0 13 a) (ror 0 22 a))    ::  s0
      =+  m=:(mix (dis a b) (dis a c) (dis b c))            ::  maj
      =+  n=(sum l m)                                       ::  t2
      =+  o=:(mix (ror 0 6 e) (ror 0 11 e) (ror 0 25 e))    ::  s1
      =+  p=(mix (dis e f) (dis (inv e) g))                 ::  ch
      =+  q=:(sum h o p (wac j kbx) (wac j wox))            ::  t1
      $(j +(j), a (sum q n), b a, c b, d c, e (sum d q), f e, g f, h g)
    ::

XX document

### `++shaw`

    ++  shaw                                                ::  hash to nbits
      |=  [sal=@ len=@ ruz=@]
      (~(raw og (shas sal (mix len ruz))) len)
    ::

XX document

### `++og`

    ++  og                                                  ::  shax-powered rng
      ~/  %og
      |_  a=@

XX document

### `++rad`

      ++  rad                                               ::  random in range
        |=  b=@  ^-  @
        =+  c=(raw (met 0 b))
        ?:((lth c b) c $(a +(a)))
        ::

XX document

### `++rads`

      ++  rads                                              ::  random continuation
        |=  b=@
        =+  r=(rad b)
        [r +>.$(a (shas %og-s r))]

XX document

### `++raw`

      ++  raw                                               ::  random bits
        ~/  %raw
        |=  b=@  ^-  @
        %+  can
          0
        =+  c=(shas %og-a (mix b a))
        |-  ^-  (list ,[@ @])
        ?:  =(0 b)
          ~
        =+  d=(shas %og-b (mix b (mix a c)))
        ?:  (lth b 256)
          [[b (end 0 b d)] ~]
        [[256 d] $(c d, b (sub b 256))]

XX document

### `++raws`

      ++  raws                                              ::  random bits continuation
        |=  b=@
        =+  r=(raw b)
        [r +>.$(a (shas %og-s r))]
      --

XX document

### `++shaz`

    ++  shaz                                                ::  sha-512
      |=  ruz=@  ^-  @
      (shal [(met 3 ruz) ruz])

XX document

### `++shal`

    ++  shal                                                ::  sha-512 with length
      ~/  %shal
      |=  [len=@ ruz=@]  ^-  @
      =>  .(ruz (cut 3 [0 len] ruz))
      =+  [few==>(fe .(a 6)) wac=|=([a=@ b=@] (cut 6 [a 1] b))]
      =+  [sum=sum.few ror=ror.few net=net.few inv=inv.few]
      =+  ral=(lsh 0 3 len)
      =+  ^=  ful
          %+  can  0
          :~  [ral ruz]
              [8 128]
              [(mod (sub 1.920 (mod (add 8 ral) 1.024)) 1.024) 0]
              [128 (~(net fe 7) ral)]
          ==
      =+  lex=(met 10 ful)
      =+  ^=  kbx  0x6c44.198c.4a47.5817.5fcb.6fab.3ad6.faec.
                     597f.299c.fc65.7e2a.4cc5.d4be.cb3e.42b6.
                     431d.67c4.9c10.0d4c.3c9e.be0a.15c9.bebc.
                     32ca.ab7b.40c7.2493.28db.77f5.2304.7d84.
                     1b71.0b35.131c.471b.113f.9804.bef9.0dae.
                     0a63.7dc5.a2c8.98a6.06f0.67aa.7217.6fba.
                     f57d.4f7f.ee6e.d178.eada.7dd6.cde0.eb1e.
                     d186.b8c7.21c0.c207.ca27.3ece.ea26.619c.
                     c671.78f2.e372.532b.bef9.a3f7.b2c6.7915.
                     a450.6ceb.de82.bde9.90be.fffa.2363.1e28.
                     8cc7.0208.1a64.39ec.84c8.7814.a1f0.ab72.
                     78a5.636f.4317.2f60.748f.82ee.5def.b2fc.
                     682e.6ff3.d6b2.b8a3.5b9c.ca4f.7763.e373.
                     4ed8.aa4a.e341.8acb.391c.0cb3.c5c9.5a63.
                     34b0.bcb5.e19b.48a8.2748.774c.df8e.eb99.
                     1e37.6c08.5141.ab53.19a4.c116.b8d2.d0c8.
                     106a.a070.32bb.d1b8.f40e.3585.5771.202a.
                     d699.0624.5565.a910.d192.e819.d6ef.5218.
                     c76c.51a3.0654.be30.c24b.8b70.d0f8.9791.
                     a81a.664b.bc42.3001.a2bf.e8a1.4cf1.0364.
                     9272.2c85.1482.353b.81c2.c92e.47ed.aee6.
                     766a.0abb.3c77.b2a8.650a.7354.8baf.63de.
                     5338.0d13.9d95.b3df.4d2c.6dfc.5ac4.2aed.
                     2e1b.2138.5c26.c926.27b7.0a85.46d2.2ffc.
                     1429.2967.0a0e.6e70.06ca.6351.e003.826f.
                     d5a7.9147.930a.a725.c6e0.0bf3.3da8.8fc2.
                     bf59.7fc7.beef.0ee4.b003.27c8.98fb.213f.
                     a831.c66d.2db4.3210.983e.5152.ee66.dfab.
                     76f9.88da.8311.53b5.5cb0.a9dc.bd41.fbd4.
                     4a74.84aa.6ea6.e483.2de9.2c6f.592b.0275.
                     240c.a1cc.77ac.9c65.0fc1.9dc6.8b8c.d5b5.
                     efbe.4786.384f.25e3.e49b.69c1.9ef1.4ad2.
                     c19b.f174.cf69.2694.9bdc.06a7.25c7.1235.
                     80de.b1fe.3b16.96b1.72be.5d74.f27b.896f.
                     550c.7dc3.d5ff.b4e2.2431.85be.4ee4.b28c.
                     1283.5b01.4570.6fbe.d807.aa98.a303.0242.
                     ab1c.5ed5.da6d.8118.923f.82a4.af19.4f9b.
                     59f1.11f1.b605.d019.3956.c25b.f348.b538.
                     e9b5.dba5.8189.dbbc.b5c0.fbcf.ec4d.3b2f.
                     7137.4491.23ef.65cd.428a.2f98.d728.ae22
      =+  ^=  hax  0x5be0.cd19.137e.2179.1f83.d9ab.fb41.bd6b.
                     9b05.688c.2b3e.6c1f.510e.527f.ade6.82d1.
                     a54f.f53a.5f1d.36f1.3c6e.f372.fe94.f82b.
                     bb67.ae85.84ca.a73b.6a09.e667.f3bc.c908
      =+  i=0
      |-  ^-  @
      ?:  =(i lex)
        (rep 6 (turn (rip 6 hax) net))
      =+  ^=  wox
          =+  dux=(cut 10 [i 1] ful)
          =+  wox=(rep 6 (turn (rip 6 dux) net))
          =+  j=16
          |-  ^-  @
          ?:  =(80 j)
            wox
          =+  :*  l=(wac (sub j 15) wox)
                  m=(wac (sub j 2) wox)
                  n=(wac (sub j 16) wox)
                  o=(wac (sub j 7) wox)
              ==
          =+  x=:(mix (ror 0 1 l) (ror 0 8 l) (rsh 0 7 l))
          =+  y=:(mix (ror 0 19 m) (ror 0 61 m) (rsh 0 6 m))
          =+  z=:(sum n x o y)
          $(wox (con (lsh 6 j z) wox), j +(j))
      =+  j=0
      =+  :*  a=(wac 0 hax)
              b=(wac 1 hax)
              c=(wac 2 hax)
              d=(wac 3 hax)
              e=(wac 4 hax)
              f=(wac 5 hax)
              g=(wac 6 hax)
              h=(wac 7 hax)
          ==
      |-  ^-  @
      ?:  =(80 j)
        %=  ^$
          i  +(i)
          hax  %+  rep  6
               :~  (sum a (wac 0 hax))
                   (sum b (wac 1 hax))
                   (sum c (wac 2 hax))
                   (sum d (wac 3 hax))
                   (sum e (wac 4 hax))
                   (sum f (wac 5 hax))
                   (sum g (wac 6 hax))
                   (sum h (wac 7 hax))
               ==
        ==
      =+  l=:(mix (ror 0 28 a) (ror 0 34 a) (ror 0 39 a))   ::  S0
      =+  m=:(mix (dis a b) (dis a c) (dis b c))            ::  maj
      =+  n=(sum l m)                                       ::  t2
      =+  o=:(mix (ror 0 14 e) (ror 0 18 e) (ror 0 41 e))   ::  S1
      =+  p=(mix (dis e f) (dis (inv e) g))                 ::  ch
      =+  q=:(sum h o p (wac j kbx) (wac j wox))            ::  t1
      $(j +(j), a (sum q n), b a, c b, d c, e (sum d q), f e, g f, h g)
    ::

XX document

### `++shan`

    ++  shan                                                ::  sha-1 (deprecated)
      |=  ruz=@
      =+  [few==>(fe .(a 5)) wac=|=([a=@ b=@] (cut 5 [a 1] b))]
      =+  [sum=sum.few ror=ror.few rol=rol.few net=net.few inv=inv.few]
      =+  ral=(lsh 0 3 (met 3 ruz))
      =+  ^=  ful
          %+  can  0
          :~  [ral ruz]
              [8 128]
              [(mod (sub 960 (mod (add 8 ral) 512)) 512) 0]
              [64 (~(net fe 6) ral)]
          ==
      =+  lex=(met 9 ful)
      =+  kbx=0xca62.c1d6.8f1b.bcdc.6ed9.eba1.5a82.7999
      =+  hax=0xc3d2.e1f0.1032.5476.98ba.dcfe.efcd.ab89.6745.2301
      =+  i=0
      |-
      ?:  =(i lex)
        (rep 5 (flop (rip 5 hax)))
      =+  ^=  wox
          =+  dux=(cut 9 [i 1] ful)
          =+  wox=(rep 5 (turn (rip 5 dux) net))
          =+  j=16
          |-  ^-  @
          ?:  =(80 j)
            wox
          =+  :*  l=(wac (sub j 3) wox)
                  m=(wac (sub j 8) wox)
                  n=(wac (sub j 14) wox)
                  o=(wac (sub j 16) wox)
              ==
          =+  z=(rol 0 1 :(mix l m n o))
          $(wox (con (lsh 5 j z) wox), j +(j))
      =+  j=0
      =+  :*  a=(wac 0 hax)
              b=(wac 1 hax)
              c=(wac 2 hax)
              d=(wac 3 hax)
              e=(wac 4 hax)
          ==
      |-  ^-  @
      ?:  =(80 j)
        %=  ^$
          i  +(i)
          hax  %+  rep  5
               :~
                   (sum a (wac 0 hax))
                   (sum b (wac 1 hax))
                   (sum c (wac 2 hax))
                   (sum d (wac 3 hax))
                   (sum e (wac 4 hax))
               ==
        ==
      =+  fx=(con (dis b c) (dis (not 5 1 b) d))
      =+  fy=:(mix b c d)
      =+  fz=:(con (dis b c) (dis b d) (dis c d))
      =+  ^=  tem
          ?:  &((gte j 0) (lte j 19))
            :(sum (rol 0 5 a) fx e (wac 0 kbx) (wac j wox))
          ?:  &((gte j 20) (lte j 39))
            :(sum (rol 0 5 a) fy e (wac 1 kbx) (wac j wox))
          ?:  &((gte j 40) (lte j 59))
            :(sum (rol 0 5 a) fz e (wac 2 kbx) (wac j wox))
          :(sum (rol 0 5 a) fy e (wac 3 kbx) (wac j wox))
      $(j +(j), a tem, b a, c (rol 0 30 b), d c, e d)

XX document

--
