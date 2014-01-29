!:
::  Crypto library for toys. Don't use these for anything real.
::  They will take forever and skywrite your secret key over Fort Meade.
::
|%
++  aes
  =>  |%
      ++  ga                                              ::  GF (bex a)
        |=  [a=@ p=@ g=@]
        =+  ma=(dec (bex a))
        =>  |%
            ::
            ++  dif                                       ::  add and sub
              |=  [b=@ c=@]
              (sit (mix b c))
            ::
            ++  dub                                       ::  double
              |=  b=@
              ?:  =(1 (cut 0 [(dec a) 1] b))
                (dif p (lsh 0 1 b))
              (lsh 0 1 b)
            ::
            ++  elo                                       ::  exp/log tables
              =+  ^=  nu
                  |=  [b=@ n=@]
                  ^-  (map ,@ ,@)
                  =+  c=*(map ,@ ,@)
                  |-
                  ?:  =(0 n)
                    c
                  %=  $
                    n  (dec n)
                    c  (~(put by c) n b)
                  ==
              =+  [q=(nu 0 (bex a)) r=(nu ma ma)]
              =+  [i=0 a=1]
              |-  ^-  [q=(map ,@ ,@) r=(map ,@ ,@)]
              ?:  =(ma i)
                [(~(put by q) i a) r]
              %=  $
                i  +(i)
                q  (~(put by q) i a)
                r  (~(put by r) a i)
                a  (dif a (dub a))
              ==
            ::
            ++  sit                                       ::  reduce
              |=  b=@
              (mod b (bex a))
            ::
            --
        =+  elo
        |%
        ++  fra                                           ::  divide
          |=  [b=@ c=@]
          (pro b (inv c))
        ::
        ++  inv                                           ::  invert
          |=  b=@
          =+  l=(~(get by r) b)
          ?~  l  !!
          =+  r=(~(get by q) (sub ma u.l))
          ?~  r  !!
          u.r
        ::
        ++  pow
          |=  [b=@ c=@]
          =+  [d=1 e=c i=0]
          |-
          ?:  =(a i)
            d
          ?:  =(1 (cut 0 [i 1] b))
            $(d (pro d e), e (pro e e), i +(i))
          $(e (pro e e), i +(i))
        ::
        ++  pro                                           ::  multiply
          |=  [b=@ c=@]
          =+  d=(~(get by r) b)
          ?~  d  0
          =+  e=(~(get by r) c)
          ?~  e  0
          =+  f=(~(get by q) (mod (add u.d u.e) ma))
          ?~  f  !!
          u.f
        ::
        --
      --
  =+  [gr=(ga 8 0x11b 3) few==>(fe .(a 5))]
  =+  [pro=pro.gr dif=dif.gr pow=pow.gr ror=ror.few]
  =+  [nnk=8 nnb=4 nnr=14]
  =>
    =>  |%
        ++  cipa                                          ::  AES params
          $_  ^?  |%
          ++  co  _[p=@ q=@ r=@ s=@]                      ::  col coefs
          ++  ix  |+(a=@ _@)                              ::  key index
          ++  ro  _[p=@ q=@ r=@ s=@]                      ::  row shifts
          ++  su  _@                                      ::  s-box
          --
        --
    |%
    ++  fort
      ^-  cipa
      |%
      ++  co  [0x2 0x3 1 1]
      ++  ix  |+(a=@ a)
      ++  ro  [0 1 2 3]
      ++  su  0x16bb.54b0.0f2d.9941.6842.e6bf.0d89.a18c.
                df28.55ce.e987.1e9b.948e.d969.1198.f8e1.
                9e1d.c186.b957.3561.0ef6.0348.66b5.3e70.
                8a8b.bd4b.1f74.dde8.c6b4.a61c.2e25.78ba.
                08ae.7a65.eaf4.566c.a94e.d58d.6d37.c8e7.
                79e4.9591.62ac.d3c2.5c24.0649.0a3a.32e0.
                db0b.5ede.14b8.ee46.8890.2a22.dc4f.8160.
                7319.5d64.3d7e.a7c4.1744.975f.ec13.0ccd.
                d2f3.ff10.21da.b6bc.f538.9d92.8f40.a351.
                a89f.3c50.7f02.f945.8533.4d43.fbaa.efd0.
                cf58.4c4a.39be.cb6a.5bb1.fc20.ed00.d153.
                842f.e329.b3d6.3b52.a05a.6e1b.1a2c.8309.
                75b2.27eb.e280.1207.9a05.9618.c323.c704.
                1531.d871.f1e5.a534.ccf7.3f36.2693.fdb7.
                c072.a49c.afa2.d4ad.f047.59fa.7dc9.82ca.
                76ab.d7fe.2b67.0130.c56f.6bf2.7b77.7c63
      --
    ::
    ++  firs
      ^-  cipa
      |%
      ++  co  [0xe 0xb 0xd 0x9]
      ++  ix  |=(a=@ (sub nnr a))
      ++  ro  [0 3 2 1]
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
      %+  can  3
      :~  [1 :(dif (pro p.c p.b) (pro q.c q.b) (pro r.c r.b) (pro s.c s.b))]
          [1 :(dif (pro p.c s.b) (pro q.c p.b) (pro r.c q.b) (pro s.c r.b))]
          [1 :(dif (pro p.c r.b) (pro q.c s.b) (pro r.c p.b) (pro s.c q.b))]
          [1 :(dif (pro p.c q.b) (pro q.c r.b) (pro r.c s.b) (pro s.c p.b))]
      ==
    ::
    ++  pode                                                ::  explode to block
      |=  [a=bloq b=@ c=@]  ^-  (list ,@)
      =+  d=(rip a c)
      =+  m=(met a c)
      |-
      ?:  =(m b)
        d
      $(m +(m), d (weld d (limo [0 ~])))
    ++  sube                                                ::  s-box word
      |=  [a=@ b=@]  ^-  @
      (rep 3 (turn (pode 3 4 a) |=(c=@ (cut 3 [c 1] b))))
    --
  |%
  ++  ciph                                                  ::  AES cipher
    |=  [a=@ b=@ t=cipa]  ^-  @
    =>  %=    .
            +
          =>  +
          |%
          ++  ankh
            |=  [a=@ b=@]
            (pode 5 nnb (cut 5 [(mul (ix.t a) nnb) nnb] b))
          ++  sark
            |=  [c=(list ,@) d=(list ,@)]  ^-  (list ,@)
            ?~  c  ~
            ?~  d  !!
            [(mix i.c i.d) $(c t.c, d t.d)]
          ++  srow
            |=  a=(list ,@)  ^-  (list ,@)
            =+  [i=0 b=~ c=ro.t]
            |-
            ?:  =(i nnb)
              b
            :_  $(i +(i))
            %+  can  3
            :~  [1 (cut 3 [0 1] (snag (mod (add p.c i) nnb) a))]
                [1 (cut 3 [1 1] (snag (mod (add q.c i) nnb) a))]
                [1 (cut 3 [2 1] (snag (mod (add r.c i) nnb) a))]
                [1 (cut 3 [3 1] (snag (mod (add s.c i) nnb) a))]
            ==
          ++  subs
            |=  a=(list ,@)  ^-  (list ,@)
            ?~  a  ~
            [(sube i.a su.t) $(a t.a)]
          --
        ==
    =+  [s=(pode 5 nnb a) i=1]
    =>  .(s (sark s (ankh 0 b)))
    |-
    ?.  =(nnr i)
      =>  .(s (subs s))
      =>  .(s (srow s))
      =>  .(s (mcol s co.t))
      =>  .(s (sark s (ankh i b)))
      $(i +(i))
    =>  .(s (subs s))
    =>  .(s (srow s))
    =>  .(s (sark s (ankh nnr b)))
    (rep 5 s)
  ++  keen                                                  ::  key expansion
    |=  a=@  ^-  @
    =+  [b=a c=0 d=su:fort i=nnk]
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
    =>  .(b (cat 5 b c))
    $(i +(i))
  ++  keep                                                  ::  inv expansion
    |=  a=@  ^-  @
    =+  [i=1 j=_@ b=_@ c=firs]
    |-
    ?:  =(nnr i)
      a
    =>  .(b (cut 7 [i 1] a))
    =>  .(b (rep 5 (mcol (pode 5 4 b) co.c)))
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
  ++  en  |=([k=@ m=@] (ciph m (keen k) fort))            ::  AES en, one block
  ++  de  |=([k=@ m=@] (ciph m (keep (keen k)) firs))     ::  AES de, one block
  --
++  ed                                                    ::  ed25519
  =>  =+  b=256
      =+  q=(sub (bex 255) 19)
      =+  fq=~(. fo q)
      =+  ^=  l
        %+  add
          (bex 252)
        27.742.317.777.372.353.535.851.937.790.883.648.493
      =+  d=(dif.fq 0 (fra.fq 121.665 121.666))
      =+  ii=(exp.fq (div (dec q) 4) 2)
      |%
      ++  norm  |=(x=@ ?:(=(0 (mod x 2)) x (sub q x)))
      ::
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
      ++  etch                                            ::  encode point
        |=  pp=[@ @]  ^-  @
        (can 0 ~[[(sub b 1) +.pp] [1 (dis 1 -.pp)]])
      ::
      ++  curv                                            ::  point on curve?
        |=  [x=@ y=@]  ^-  ?
        .=  0
            %+  dif.fq
              %+  sum.fq
                (pro.fq (sub q (sit.fq x)) x)
              (pro.fq y y)
            (sum.fq 1 :(pro.fq d x x y y))
      ::
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
      --
  =+  ^=  bb
        =+  bby=(pro.fq 4 (inv.fq 5))
      [(xrec bby) bby]
  |%
  ++  puck                                                ::  public key
    |=  sk=@  ^-  @
    =+  h=(shal (rsh 0 3 b) sk)
    =+  ^=  a
        %+  add
          (bex (sub b 2))
        (lsh 0 3 (cut 0 [3 (sub b 5)] h))
    =+  aa=(scam bb a)
    (etch aa)
  ::
  ++  sign                                                ::  certify
    |=  [m=@ sk=@ pk=@]  ^-  @
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
  ++  veri                                                ::  validate
    |=  [s=@ m=@ pk=@]  ^-  ?
    ?:  (gth (div b 4) (met 3 s))  |
    ?:  (gth (div b 8) (met 3 pk))  |
    =+  rr=(deco (cut 0 [0 b] s))
    ?~  rr  |
    =+  aa=(deco pk)
    ?~  aa  |
    =+  ss=(cut 0 [b b] s)
    =+  ha=(can 0 ~[[b (etch u.rr)] [b pk] [(met 0 m) m]])
    =+  h=(shaz ha)
    =((scam bb ss) (ward u.rr (scam u.aa h)))
  ::
  --
--
