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
  =+  [pro=pro.gr dif=dif.gr pow=pow.gr rol=rol.few]
  =+  [nnk=8 nnb=4 nnr=14]
  =>
    |%
    ++  pode
      |=  [a=bloq b=@ c=@]  ^-  (list ,@)
      =+  d=(rip a c)
      =+  m=(met a c)
      |-
      ?:  =(m b)
        d
      $(m +(m), d (weld d (limo [0 ~])))
    ++  sube
      |=  a=@  ^-  @
      (rep 3 (turn (pode 3 4 a) suby))
    ++  suby
      |=  a=@
      =+  ^=  b
          0x16bb.54b0.0f2d.9941.6842.e6bf.0d89.a18c.
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
      (cut 3 [a 1] b)
    ::  ++  subz
    ::    |=  a=@
    ::    =+  ^=  b
    ::        0x5209.6ad5.3036.a538.bf40.a39e.81f3.d7fb.
    ::          7ce3.3982.9b2f.ff87.348e.4344.c4de.e9cb.
    ::          547b.9432.a6c2.233d.ee4c.950b.42fa.c34e.
    ::          082e.a166.28d9.24b2.765b.a249.6d8b.d125.
    ::          72f8.f664.8668.9816.d4a4.5ccc.5d65.b692.
    ::          6c70.4850.fded.b9da.5e15.4657.a78d.9d84.
    ::          90d8.ab00.8cbc.d30a.f7e4.5805.b8b3.4506.
    ::          d02c.1e8f.ca3f.0f02.c1af.bd03.0113.8a6b.
    ::          3a91.1141.4f67.dcea.97f2.cfce.f0b4.e673.
    ::          96ac.7422.e7ad.3585.e2f9.37e8.1c75.df6e.
    ::          47f1.1a71.1d29.c589.6fb7.620e.aa18.be1b.
    ::          fc56.3e4b.c6d2.7920.9adb.c0fe.78cd.5af4.
    ::          1fdd.a833.8807.c731.b112.1059.2780.ec5f.
    ::          6051.7fa9.19b5.4a0d.2de5.7a9f.93c9.9cef.
    ::          a0e0.3b4d.ae2a.f5b0.c8eb.bb3c.8353.9961.
    ::          172b.047e.ba77.d626.e169.1463.5521.0c7d
    ::    (cut 3 [a 1] b)
    ++  rcon
      |=  a=@
      (can 3 ~[[3 0] [1 (pow (dec a) 2)]])
    ++  rote
      |=  a=@  ^-  @
      (rol 3 1 a)
    --
  |%
  ++  ciph
    |=  [a=@ b=@]  ^-  @
    =>  %=    .
            +
          =>  +
          |%
          ++  ankh
            |=  [c=@ d=@]
            (pode 5 nnb (cut 5 [(mul (sub nnr c) nnb) nnb] d))
          ++  sark
            |=  [c=(list ,@) d=(list ,@)]  ^-  (list ,@)
            ?~  c  ~
            ?~  d  !!
            [(mix i.c i.d) $(c t.c, d t.d)]
          ++  subs
            |=  c=(list ,@)  ^-  (list ,@)
            ?~  c  ~
            [(rep 3 (turn (pode 3 4 i.c) suby)) $(c t.c)]
          ++  srow
            |=  c=(list ,@)  ^-  (list ,@)
            =+  ^=  col
                |=  i=@
                %+  can  3
                :~  [1 (cut 3 [0 1] (snag (mod i nnb) c))]
                    [1 (cut 3 [1 1] (snag (mod +(i) nnb) c))]
                    [1 (cut 3 [2 1] (snag (mod +(+(i)) nnb) c))]
                    [1 (cut 3 [3 1] (snag (mod (add 3 i) nnb) c))]
                ==
            (turn (limo ~[1 2 3 0]) col)
          ++  mcol
            |=  c=(list ,@)  ^-  (list ,@)
            =+  [p=_@ q=_@ r=_@ s=_@]
            |-  ^-  (list ,@)
            ?~  c
              ~
            =>  .(p (cut 3 [3 1] i.c))
            =>  .(q (cut 3 [2 1] i.c))
            =>  .(r (cut 3 [1 1] i.c))
            =>  .(s (cut 3 [0 1] i.c))
            :-  %+  can  3
                :~  [1 :(dif (pro 0x3 p) q r (pro 0x2 s))]
                    [1 :(dif p q (pro 0x2 r) (pro 0x3 s))]
                    [1 :(dif p (pro 0x2 q) (pro 0x3 r) s)]
                    [1 :(dif (pro 0x2 p) (pro 0x3 q) r s)]
                ==
            $(c t.c)
          --
        ==
    =+  [s=(pode 5 nnb a) i=1]
    =>  .(s (sark s (ankh 0 b)))
    |-
    ?.  =(nnr i)
      =>  .(s (subs s))
      =>  .(s (srow s))
      =>  .(s (mcol s))
      =>  .(s (sark s (ankh i b)))
      $(i +(i))
    =>  .(s (subs s))
    =>  .(s (srow s))
    =>  .(s (sark s (ankh nnr b)))
    (rep 5 s)
  ++  keen
    |=  a=@  ^-  @
    =+  [b=a c=0 i=nnk]
    |-
    ?:  =(i (mul nnb +(nnr)))
      b
    =>  .(c (cut 5 [0 1] b))
    =>  ?:  =(0 (mod i nnk))
          .(c (mix (sube (rote c)) (rcon (div i nnk))))
        ?:  &((gth nnk 6) =(4 (mod i nnk)))
          .(c (sube c))
        .
    =>  .(b (cat 5 (mix c (cut 5 [7 1] b)) b))
    $(i +(i))
  ++  en  |=([k=@ m=@] (ciph m (keen k)))
  ++  de  |=([k=@ m=@] _@)
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
