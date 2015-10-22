section 2eX, jetted crypto
==========================

------------------------------------------------------------------------

### `++aesc`

    ++  aesc                                                ::  AES-256
      ~%  %aesc  +  ~
      |%

XX document

### `++en`

      ++  en                                                ::  ECB enc
        ~/  %en
        |=  [a=@I b=@H]  ^-  @uxH
        =+  ahem
        (be & (ex a) b)

XX document

### `++de`

      ++  de                                                ::  ECB dec
        ~/  %de
        |=  [a=@I b=@H]  ^-  @uxH
        =+  ahem
        (be | (ix (ex a)) b)
      --

XX document

### `++ahem`

    ++  ahem                                                ::  AES helpers
    ::  XX should be in aesc, isn't for performance reasons
      =>
        =+  =+  [gr=(ga 8 0x11b 3) few==>(fe .(a 5))]
            =+  [pro=pro.gr dif=dif.gr pow=pow.gr ror=ror.few]
            [pro=pro dif=dif pow=pow ror=ror nnk=8 nnb=4 nnr=14]
        =>  |%

XX document

### `++cipa`

            ++  cipa                                        ::  AES params
              $_  ^?  |%

XX document

### `++co`

          ++  co  [0xe 0xb 0xd 0x9]

XX document

### `++ix`

      ++  ix                                                ::  key expand, inv
        |=  a=@  ^-  @
        =+  [i=1 j=_@ b=_@ c=co:pin]
        |-
        ?:  =(nnr i)
          a
        =>  .(b (cut 7 [i 1] a))
        =>  .(b (rep 5 (mcol (pode 5 4 b) c)))
        =>  .(j (sub nnr i))
        %=    $
            i  +(i)
            a
          %+  can  7
          :~  [i (cut 7 [0 i] a)]
              [1 b]
              [j (cut 7 [+(i) j] a)]
          ==
        ==
      --
    ::

XX document

### `++ro`

          ++  ro  [0 3 2 1]

XX document

### `++su`

          ++  su  0x7d0c.2155.6314.69e1.26d6.77ba.7e04.2b17.
                    6199.5383.3cbb.ebc8.b0f5.2aae.4d3b.e0a0.
                    ef9c.c993.9f7a.e52d.0d4a.b519.a97f.5160.
                    5fec.8027.5910.12b1.31c7.0788.33a8.dd1f.
                    f45a.cd78.fec0.db9a.2079.d2c6.4b3e.56fc.
                    1bbe.18aa.0e62.b76f.89c5.291d.711a.f147.
                    6edf.751c.e837.f9e2.8535.ade7.2274.ac96.
                    73e6.b4f0.cecf.f297.eadc.674f.4111.913a.
                    6b8a.1301.03bd.afc1.020f.3fca.8f1e.2cd0.
                    0645.b3b8.0558.e4f7.0ad3.bc8c.00ab.d890.
                    849d.8da7.5746.155e.dab9.edfd.5048.706c.
                    92b6.655d.cc5c.a4d4.1698.6886.64f6.f872.
                    25d1.8b6d.49a2.5b76.b224.d928.66a1.2e08.
                    4ec3.fa42.0b95.4cee.3d23.c2a6.3294.7b54.
                    cbe9.dec4.4443.8e34.87ff.2f9b.8239.e37c.
                    fbd7.f381.9ea3.40bf.38a5.3630.d56a.0952
          --
        ::

XX document

### `++pen`

        ++  pen                                             ::  encrypt
          ^-  cipa
          |%

XX document

### `++co`

          ++  co  [0xe 0xb 0xd 0x9]

XX document

### `++ix`

      ++  ix                                                ::  key expand, inv
        |=  a=@  ^-  @
        =+  [i=1 j=_@ b=_@ c=co:pin]
        |-
        ?:  =(nnr i)
          a
        =>  .(b (cut 7 [i 1] a))
        =>  .(b (rep 5 (mcol (pode 5 4 b) c)))
        =>  .(j (sub nnr i))
        %=    $
            i  +(i)
            a
          %+  can  7
          :~  [i (cut 7 [0 i] a)]
              [1 b]
              [j (cut 7 [+(i) j] a)]
          ==
        ==
      --
    ::

XX document

### `++ro`

          ++  ro  [0 3 2 1]

XX document

### `++su`

          ++  su  0x7d0c.2155.6314.69e1.26d6.77ba.7e04.2b17.
                    6199.5383.3cbb.ebc8.b0f5.2aae.4d3b.e0a0.
                    ef9c.c993.9f7a.e52d.0d4a.b519.a97f.5160.
                    5fec.8027.5910.12b1.31c7.0788.33a8.dd1f.
                    f45a.cd78.fec0.db9a.2079.d2c6.4b3e.56fc.
                    1bbe.18aa.0e62.b76f.89c5.291d.711a.f147.
                    6edf.751c.e837.f9e2.8535.ade7.2274.ac96.
                    73e6.b4f0.cecf.f297.eadc.674f.4111.913a.
                    6b8a.1301.03bd.afc1.020f.3fca.8f1e.2cd0.
                    0645.b3b8.0558.e4f7.0ad3.bc8c.00ab.d890.
                    849d.8da7.5746.155e.dab9.edfd.5048.706c.
                    92b6.655d.cc5c.a4d4.1698.6886.64f6.f872.
                    25d1.8b6d.49a2.5b76.b224.d928.66a1.2e08.
                    4ec3.fa42.0b95.4cee.3d23.c2a6.3294.7b54.
                    cbe9.dec4.4443.8e34.87ff.2f9b.8239.e37c.
                    fbd7.f381.9ea3.40bf.38a5.3630.d56a.0952
          --
        ::

XX document

### `++pin`

        ++  pin                                             :: decrypt
          ^-  cipa
          |%

XX document

### `++co`

          ++  co  [0xe 0xb 0xd 0x9]

XX document

### `++ix`

      ++  ix                                                ::  key expand, inv
        |=  a=@  ^-  @
        =+  [i=1 j=_@ b=_@ c=co:pin]
        |-
        ?:  =(nnr i)
          a
        =>  .(b (cut 7 [i 1] a))
        =>  .(b (rep 5 (mcol (pode 5 4 b) c)))
        =>  .(j (sub nnr i))
        %=    $
            i  +(i)
            a
          %+  can  7
          :~  [i (cut 7 [0 i] a)]
              [1 b]
              [j (cut 7 [+(i) j] a)]
          ==
        ==
      --
    ::

XX document

### `++ro`

          ++  ro  [0 3 2 1]

XX document

### `++su`

          ++  su  0x7d0c.2155.6314.69e1.26d6.77ba.7e04.2b17.
                    6199.5383.3cbb.ebc8.b0f5.2aae.4d3b.e0a0.
                    ef9c.c993.9f7a.e52d.0d4a.b519.a97f.5160.
                    5fec.8027.5910.12b1.31c7.0788.33a8.dd1f.
                    f45a.cd78.fec0.db9a.2079.d2c6.4b3e.56fc.
                    1bbe.18aa.0e62.b76f.89c5.291d.711a.f147.
                    6edf.751c.e837.f9e2.8535.ade7.2274.ac96.
                    73e6.b4f0.cecf.f297.eadc.674f.4111.913a.
                    6b8a.1301.03bd.afc1.020f.3fca.8f1e.2cd0.
                    0645.b3b8.0558.e4f7.0ad3.bc8c.00ab.d890.
                    849d.8da7.5746.155e.dab9.edfd.5048.706c.
                    92b6.655d.cc5c.a4d4.1698.6886.64f6.f872.
                    25d1.8b6d.49a2.5b76.b224.d928.66a1.2e08.
                    4ec3.fa42.0b95.4cee.3d23.c2a6.3294.7b54.
                    cbe9.dec4.4443.8e34.87ff.2f9b.8239.e37c.
                    fbd7.f381.9ea3.40bf.38a5.3630.d56a.0952
          --
        ::

XX document

### `++mcol`

        ++  mcol
          |=  [a=(list ,@) b=[p=@ q=@ r=@ s=@]]  ^-  (list ,@)
          =+  c=[p=_@ q=_@ r=_@ s=_@]
          |-  ^-  (list ,@)
          ?~  a  ~
          =>  .(p.c (cut 3 [0 1] i.a))
          =>  .(q.c (cut 3 [1 1] i.a))
          =>  .(r.c (cut 3 [2 1] i.a))
          =>  .(s.c (cut 3 [3 1] i.a))
          :_  $(a t.a)
          %+  rep  3
          %+  turn
            %-  limo
            :~  [[p.c p.b] [q.c q.b] [r.c r.b] [s.c s.b]]
                [[p.c s.b] [q.c p.b] [r.c q.b] [s.c r.b]]
                [[p.c r.b] [q.c s.b] [r.c p.b] [s.c q.b]]
                [[p.c q.b] [q.c r.b] [r.c s.b] [s.c p.b]]
            ==
          |=  [a=[@ @] b=[@ @] c=[@ @] d=[@ @]]
          :(dif (pro a) (pro b) (pro c) (pro d))
        ::

XX document

### `++pode`

        ++  pode                                            ::  explode to block
          |=  [a=bloq b=@ c=@]  ^-  (list ,@)
          =+  d=(rip a c)
          =+  m=(met a c)
          |-
          ?:  =(m b)
            d
          $(m +(m), d (weld d (limo [0 ~])))

XX document

### `++sube`

        ++  sube                                            ::  s-box word
          |=  [a=@ b=@]  ^-  @
          (rep 3 (turn (pode 3 4 a) |=(c=@ (cut 3 [c 1] b))))
        --
      |%

XX document

### `++be`

      ++  be                                                ::  block cipher
        |=  [a=? b=@ c=@H]  ^-  @uxH
        ~|  %be-aesc
        =>  %=    .
                +
              =>  +
              |%

XX document

### `++ankh`

              ++  ankh
                |=  [a=cipa b=@ c=@]
                (pode 5 nnb (cut 5 [(mul (ix.a b) nnb) nnb] c))

XX document

### `++sark`

              ++  sark
                |=  [c=(list ,@) d=(list ,@)]  ^-  (list ,@)
                ?~  c  ~
                ?~  d  !!
                [(mix i.c i.d) $(c t.c, d t.d)]

XX document

### `++srow`

              ++  srow
                |=  [a=cipa b=(list ,@)]  ^-  (list ,@)
                =+  [c=0 d=~ e=ro.a]
                |-
                ?:  =(c nnb)
                  d
                :_  $(c +(c))
                %+  rep  3
                %+  turn
                  (limo [0 p.e] [1 q.e] [2 r.e] [3 s.e] ~)
                |=  [f=@ g=@]
                (cut 3 [f 1] (snag (mod (add g c) nnb) b))

XX document

### `++subs`

              ++  subs
                |=  [a=cipa b=(list ,@)]  ^-  (list ,@)
                ?~  b  ~
                [(sube i.b su.a) $(b t.b)]
              --
            ==
        =+  [d=?:(a pen pin) e=(pode 5 nnb c) f=1]
        =>  .(e (sark e (ankh d 0 b)))
        |-
        ?.  =(nnr f)
          =>  .(e (subs d e))
          =>  .(e (srow d e))
          =>  .(e (mcol e co.d))
          =>  .(e (sark e (ankh d f b)))
          $(f +(f))
        =>  .(e (subs d e))
        =>  .(e (srow d e))
        =>  .(e (sark e (ankh d nnr b)))
        (rep 5 e)

XX document

### `++ex`

      ++  ex                                                ::  key expand
        |=  a=@I  ^-  @
        =+  [b=a c=0 d=su:pen i=nnk]
        |-
        ?:  =(i (mul nnb +(nnr)))
          b
        =>  .(c (cut 5 [(dec i) 1] b))
        =>  ?:  =(0 (mod i nnk))
              =>  .(c (ror 3 1 c))
              =>  .(c (sube c d))
              .(c (mix c (pow (dec (div i nnk)) 2)))
            ?:  &((gth nnk 6) =(4 (mod i nnk)))
              .(c (sube c d))
            .
        =>  .(c (mix c (cut 5 [(sub i nnk) 1] b)))
        =>  .(b (can 5 [i b] [1 c] ~))
        $(i +(i))

XX document

### `++ix`

      ++  ix                                                ::  key expand, inv
        |=  a=@  ^-  @
        =+  [i=1 j=_@ b=_@ c=co:pin]
        |-
        ?:  =(nnr i)
          a
        =>  .(b (cut 7 [i 1] a))
        =>  .(b (rep 5 (mcol (pode 5 4 b) c)))
        =>  .(j (sub nnr i))
        %=    $
            i  +(i)
            a
          %+  can  7
          :~  [i (cut 7 [0 i] a)]
              [1 b]
              [j (cut 7 [+(i) j] a)]
          ==
        ==
      --
    ::

XX document

### `++curt`

    ++  curt                                                ::  curve25519
      |=  [a=@ b=@]
      =>  %=    .
              +
            =>  +
            =+  =+  [p=486.662 q=(sub (bex 255) 19)]
                =+  fq=~(. fo q)
                [p=p q=q fq=fq]
            |%

XX document

### `++cla`

            ++  cla
              |=  raw=@
              =+  low=(dis 248 (cut 3 [0 1] raw))
              =+  hih=(con 64 (dis 127 (cut 3 [31 1] raw)))
              =+  mid=(cut 3 [1 30] raw)
              (can 3 [[1 low] [30 mid] [1 hih] ~])

XX document

### `++sqr`

            ++  sqr  |=(a=@ (mul a a))

XX document

### `++inv`

            ++  inv  |=(a=@ (~(exp fo q) (sub q 2) a))

XX document

### `++cad`

            ++  cad
              |=  [n=[x=@ z=@] m=[x=@ z=@] d=[x=@ z=@]]
              =+  ^=  xx
                  ;:  mul  4  z.d
                    %-  sqr  %-  abs:si
                    %+  dif:si
                      (sun:si (mul x.m x.n))
                    (sun:si (mul z.m z.n))
                  ==
              =+  ^=  zz
                  ;:  mul  4  x.d
                    %-  sqr  %-  abs:si
                    %+  dif:si
                      (sun:si (mul x.m z.n))
                    (sun:si (mul z.m x.n))
                  ==
              [(sit.fq xx) (sit.fq zz)]

XX document

### `++cub`

            ++  cub
              |=  [x=@ z=@]
              =+  ^=  xx
                  %+  mul
                    %-  sqr  %-  abs:si
                    (dif:si (sun:si x) (sun:si z))
                  (sqr (add x z))
              =+  ^=  zz
                  ;:  mul  4  x  z
                    :(add (sqr x) :(mul p x z) (sqr z))
                  ==
              [(sit.fq xx) (sit.fq zz)]
            --
          ==
      =+  one=[b 1]
      =+  i=253
      =+  r=one
      =+  s=(cub one)
      |-
      ?:  =(i 0)
        =+  x=(cub r)
        (sit.fq (mul -.x (inv +.x)))
      =+  m=(rsh 0 i a)
      ?:  =(0 (mod m 2))
         $(i (dec i), s (cad r s one), r (cub r))
      $(i (dec i), r (cad r s one), s (cub s))
    ::

XX document

### `++ed`

    ++  ed                                                  ::  ed25519
      =>
        =+  =+  [b=256 q=(sub (bex 255) 19)]
            =+  fq=~(. fo q)
            =+  ^=  l
                 %+  add
                   (bex 252)
                 27.742.317.777.372.353.535.851.937.790.883.648.493
            =+  d=(dif.fq 0 (fra.fq 121.665 121.666))
            =+  ii=(exp.fq (div (dec q) 4) 2)
            [b=b q=q fq=fq l=l d=d ii=ii]
        ~%  %coed  +>  ~
        |%

### `++norm`

        ++  norm  |=(x=@ ?:(=(0 (mod x 2)) x (sub q x)))
        ::

XX document

### `++xrec`

        ++  xrec                                            ::  recover x-coord
          |=  y=@  ^-  @
          =+  ^=  xx
              %+  mul  (dif.fq (mul y y) 1)
                       (inv.fq +(:(mul d y y)))
          =+  x=(exp.fq (div (add 3 q) 8) xx)
          ?:  !=(0 (dif.fq (mul x x) (sit.fq xx)))
            (norm (pro.fq x ii))
          (norm x)
        ::

XX document

### `++ward`

        ++  ward                                            ::  edwards multiply
          |=  [pp=[@ @] qq=[@ @]]  ^-  [@ @]
          =+  dp=:(pro.fq d -.pp -.qq +.pp +.qq)
          =+  ^=  xt
              %+  pro.fq
                %+  sum.fq
                  (pro.fq -.pp +.qq)
                (pro.fq -.qq +.pp)
              (inv.fq (sum.fq 1 dp))
          =+  ^=  yt
              %+  pro.fq
                %+  sum.fq
                  (pro.fq +.pp +.qq)
                (pro.fq -.pp -.qq)
              (inv.fq (dif.fq 1 dp))
          [xt yt]
        ::

XX document

### `++scam`

        ++  scam                                            ::  scalar multiply
          |=  [pp=[@ @] e=@]  ^-  [@ @]
          ?:  =(0 e)
            [0 1]
          =+  qq=$(e (div e 2))
          =>  .(qq (ward qq qq))
          ?:  =(1 (dis 1 e))
            (ward qq pp)
          qq
        ::

XX document

### `++etch`

        ++  etch                                            ::  encode point
          |=  pp=[@ @]  ^-  @
          (can 0 ~[[(sub b 1) +.pp] [1 (dis 1 -.pp)]])
        ::

XX document

### `++curv`

        ++  curv                                            ::  point on curve?
          |=  [x=@ y=@]  ^-  ?
          .=  0
              %+  dif.fq
                %+  sum.fq
                  (pro.fq (sub q (sit.fq x)) x)
                (pro.fq y y)
              (sum.fq 1 :(pro.fq d x x y y))
        ::

XX document

### `++deco`

        ++  deco                                            ::  decode point
          |=  s=@  ^-  (unit ,[@ @])
          =+  y=(cut 0 [0 (dec b)] s)
          =+  si=(cut 0 [(dec b) 1] s)
          =+  x=(xrec y)
          =>  .(x ?:(!=(si (dis 1 x)) (sub q x) x))
          =+  pp=[x y]
          ?.  (curv pp)
            ~
          [~ pp]
        ::

XX document

### `++bb`

        ++  bb
          =+  bby=(pro.fq 4 (inv.fq 5))
        [(xrec bby) bby]
        ::
        --
      ~%  %ed  +  ~
      |%

XX document

### `++puck`

      ++  puck                                                ::  public key
        ~/  %puck
        |=  sk=@I  ^-  @
        ?:  (gth (met 3 sk) 32)  !!
        =+  h=(shal (rsh 0 3 b) sk)
        =+  ^=  a
            %+  add
              (bex (sub b 2))
            (lsh 0 3 (cut 0 [3 (sub b 5)] h))
        =+  aa=(scam bb a)
        (etch aa)

XX document

### `++suck`

      ++  suck                                                ::  keypair from seed
        |=  se=@I  ^-  @uJ
        =+  pu=(puck se)
        (can 0 ~[[b se] [b pu]])
      ::

XX document

### `++sign`

      ++  sign                                                ::  certify
        ~/  %sign
        |=  [m=@ se=@]  ^-  @
        =+  sk=(suck se)
        =+  pk=(cut 0 [b b] sk)
        =+  h=(shal (rsh 0 3 b) sk)
        =+  ^=  a
            %+  add
              (bex (sub b 2))
            (lsh 0 3 (cut 0 [3 (sub b 5)] h))
        =+  ^=  r
            =+  hm=(cut 0 [b b] h)
            =+  ^=  i
                %+  can  0
                :~  [b hm]
                    [(met 0 m) m]
                ==
            (shaz i)
        =+  rr=(scam bb r)
        =+  ^=  ss
            =+  er=(etch rr)
            =+  ^=  ha
                %+  can  0
                :~  [b er]
                    [b pk]
                    [(met 0 m) m]
                ==
            (~(sit fo l) (add r (mul (shaz ha) a)))
        (can 0 ~[[b (etch rr)] [b ss]])
      ::

XX document

### `++veri`

      ++  veri                                                ::  validate
        ~/  %veri
        |=  [s=@ m=@ pk=@]  ^-  ?
        ?:  (gth (div b 4) (met 3 s))  |
        ?:  (gth (div b 8) (met 3 pk))  |
        =+  cb=(rsh 0 3 b)
        =+  rr=(deco (cut 0 [0 b] s))
        ?~  rr  |
        =+  aa=(deco pk)
        ?~  aa  |
        =+  ss=(cut 0 [b b] s)
        =+  ha=(can 3 ~[[cb (etch u.rr)] [cb pk] [(met 3 m) m]])
        =+  h=(shaz ha)
        =((scam bb ss) (ward u.rr (scam u.aa h)))
      ::
      --
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

XX document

------------------------------------------------------------------------
