::  /sys/zuse
::  %zuse: arvo library
::
=>  ..lull
~%  %zuse  ..part  ~
|%
++  zuse  %412
::                                                      ::  ::
::::                                                    ::  ::  (2) engines
  ::                                                    ::  ::
::                                                      ::::
::::                      ++number                      ::  (2a) number theory
  ::                                                    ::::
++  number  ^?
  |%
  ::                                                    ::  ++fu:number
  ++  fu                                                ::  modulo (mul p q)
    |=  a=[p=@ q=@]
    =+  b=?:(=([0 0] a) 0 (~(inv fo p.a) (~(sit fo p.a) q.a)))
    |%
    ::                                                  ::  ++dif:fu:number
    ++  dif                                             ::  subtract
      |=  [c=[@ @] d=[@ @]]
      [(~(dif fo p.a) -.c -.d) (~(dif fo q.a) +.c +.d)]
    ::                                                  ::  ++exp:fu:number
    ++  exp                                             ::  exponent
      |=  [c=@ d=[@ @]]
      :-  (~(exp fo p.a) (mod c (dec p.a)) -.d)
      (~(exp fo q.a) (mod c (dec q.a)) +.d)
    ::                                                  ::  ++out:fu:number
    ++  out                                             ::  garner's formula
      |=  c=[@ @]
      %+  add  +.c
      %+  mul  q.a
      %+  ~(pro fo p.a)  b
      (~(dif fo p.a) -.c (~(sit fo p.a) +.c))
    ::                                                  ::  ++pro:fu:number
    ++  pro                                             ::  multiply
      |=  [c=[@ @] d=[@ @]]
      [(~(pro fo p.a) -.c -.d) (~(pro fo q.a) +.c +.d)]
    ::                                                  ::  ++sum:fu:number
    ++  sum                                             ::  add
      |=  [c=[@ @] d=[@ @]]
      [(~(sum fo p.a) -.c -.d) (~(sum fo q.a) +.c +.d)]
    ::                                                  ::  ++sit:fu:number
    ++  sit                                             ::  represent
      |=  c=@
      [(mod c p.a) (mod c q.a)]
    --  ::fu
  ::                                                    ::  ++pram:number
  ++  pram                                              ::  rabin-miller
    |=  a=@  ^-  ?
    ?:  ?|  =(0 (end 0 a))
            =(1 a)
            =+  b=1
            |-  ^-  ?
            ?:  =(512 b)
              |
            ?|(=+(c=+((mul 2 b)) &(!=(a c) =(a (mul c (div a c))))) $(b +(b)))
        ==
      |
    =+  ^=  b
        =+  [s=(dec a) t=0]
        |-  ^-  [s=@ t=@]
        ?:  =(0 (end 0 s))
          $(s (rsh 0 s), t +(t))
        [s t]
    ?>  =((mul s.b (bex t.b)) (dec a))
    =+  c=0
    |-  ^-  ?
    ?:  =(c 64)
      &
    =+  d=(~(raw og (add c a)) (met 0 a))
    =+  e=(~(exp fo a) s.b d)
    ?&  ?|  =(1 e)
            =+  f=0
            |-  ^-  ?
            ?:  =(e (dec a))
              &
            ?:  =(f (dec t.b))
              |
            $(e (~(pro fo a) e e), f +(f))
        ==
        $(c +(c))
    ==
  ::                                                    ::  ++ramp:number
  ++  ramp                                              ::  make r-m prime
    |=  [a=@ b=(list @) c=@]  ^-  @ux                   ::  [bits snags seed]
    =>  .(c (shas %ramp c))
    =+  d=*@
    |-
    ?:  =((mul 100 a) d)
      ~|(%ar-ramp !!)
    =+  e=(~(raw og c) a)
    ?:  &((levy b |=(f=@ !=(1 (mod e f)))) (pram e))
      e
    $(c +(c), d (shax d))
  ::                                                    ::  ++curt:number
  ++  curt                                              ::  curve25519
    |=  [a=@ b=@]
    =>  %=    .
            +
          =>  +
          =+  =+  [p=486.662 q=(sub (bex 255) 19)]
              =+  fq=~(. fo q)
              [p=p q=q fq=fq]
          |%
          ::                                            ::  ++cla:curt:number
          ++  cla                                       ::
            |=  raw=@
            =+  low=(dis 248 (cut 3 [0 1] raw))
            =+  hih=(con 64 (dis 127 (cut 3 [31 1] raw)))
            =+  mid=(cut 3 [1 30] raw)
            (can 3 [[1 low] [30 mid] [1 hih] ~])
          ::                                            ::  ++sqr:curt:number
          ++  sqr                                       ::
            |=(a=@ (mul a a))
          ::                                            ::  ++inv:curt:number
          ++  inv                                       ::
            |=(a=@ (~(exp fo q) (sub q 2) a))
          ::                                            ::  ++cad:curt:number
          ++  cad                                       ::
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
          ::                                            ::  ++cub:curt:number
          ++  cub                                       ::
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
          --  ::
        ==
    =+  one=[b 1]
    =+  i=253
    =+  r=one
    =+  s=(cub one)
    |-
    ?:  =(i 0)
      =+  x=(cub r)
      (sit.fq (mul -.x (inv +.x)))
    =+  m=(rsh [0 i] a)
    ?:  =(0 (mod m 2))
       $(i (dec i), s (cad r s one), r (cub r))
    $(i (dec i), r (cad r s one), s (cub s))
  ::                                                    ::  ++ga:number
  ++  ga                                                ::  GF (bex p.a)
    |=  a=[p=@ q=@ r=@]                                 ::  dim poly gen
    =+  si=(bex p.a)
    =+  ma=(dec si)
    =>  |%
        ::                                              ::  ++dif:ga:number
        ++  dif                                         ::  add and sub
          |=  [b=@ c=@]
          ~|  [%dif-ga a]
          ?>  &((lth b si) (lth c si))
          (mix b c)
        ::                                              ::  ++dub:ga:number
        ++  dub                                         ::  mul by x
          |=  b=@
          ~|  [%dub-ga a]
          ?>  (lth b si)
          ?:  =(1 (cut 0 [(dec p.a) 1] b))
            (dif (sit q.a) (sit (lsh 0 b)))
          (lsh 0 b)
        ::                                              ::  ++pro:ga:number
        ++  pro                                         ::  slow multiply
          |=  [b=@ c=@]
          ?:  =(0 b)
            0
          ?:  =(1 (dis 1 b))
            (dif c $(b (rsh 0 b), c (dub c)))
          $(b (rsh 0 b), c (dub c))
        ::                                              ::  ++toe:ga:number
        ++  toe                                         ::  exp+log tables
          =+  ^=  nu
              |=  [b=@ c=@]
              ^-  (map @ @)
              =+  d=*(map @ @)
              |-
              ?:  =(0 c)
                d
              %=  $
                c  (dec c)
                d  (~(put by d) c b)
              ==
          =+  [p=(nu 0 (bex p.a)) q=(nu ma ma)]
          =+  [b=1 c=0]
          |-  ^-  [p=(map @ @) q=(map @ @)]
          ?:  =(ma c)
            [(~(put by p) c b) q]
          %=  $
            b  (pro r.a b)
            c  +(c)
            p  (~(put by p) c b)
            q  (~(put by q) b c)
          ==
        ::                                              ::  ++sit:ga:number
        ++  sit                                         ::  reduce
          |=  b=@
          (mod b (bex p.a))
        --  ::
    =+  toe
    |%
    ::                                                  ::  ++fra:ga:number
    ++  fra                                             ::  divide
      |=  [b=@ c=@]
      (pro b (inv c))
    ::                                                  ::  ++inv:ga:number
    ++  inv                                             ::  invert
      |=  b=@
      ~|  [%inv-ga a]
      =+  c=(~(get by q) b)
      ?~  c  !!
      =+  d=(~(get by p) (sub ma u.c))
      (need d)
    ::                                                  ::  ++pow:ga:number
    ++  pow                                             ::  exponent
      |=  [b=@ c=@]
      =+  [d=1 e=c f=0]
      |-
      ?:  =(p.a f)
        d
      ?:  =(1 (cut 0 [f 1] b))
        $(d (pro d e), e (pro e e), f +(f))
      $(e (pro e e), f +(f))
    ::                                                  ::  ++pro:ga:number
    ++  pro                                             ::  multiply
      |=  [b=@ c=@]
      ~|  [%pro-ga a]
      =+  d=(~(get by q) b)
      ?~  d  0
      =+  e=(~(get by q) c)
      ?~  e  0
      =+  f=(~(get by p) (mod (add u.d u.e) ma))
      (need f)
    --  ::ga
  --  ::number
::                                                      ::::
::::                      ++crypto                      ::  (2b) cryptography
  ::                                                    ::::
++  crypto  ^?
  =,  ames
  =,  number
  |%
  ::                                                    ::
  ::::                    ++aes:crypto                  ::  (2b1) aes, all sizes
    ::                                                  ::::
  ++  aes    !.
    ~%  %aes  ..part  ~
    |%
    ::                                                  ::  ++ahem:aes:crypto
    ++  ahem                                            ::  kernel state
      |=  [nnk=@ nnb=@ nnr=@]
      =>
        =+  =>  [gr=(ga 8 0x11b 3) few==>(fe .(a 5))]
            [pro=pro.gr dif=dif.gr pow=pow.gr ror=ror.few]
        =>  |%                                          ::
            ++  cipa  $_  ^?                            ::  AES params
              |%
              ++  co  *[p=@ q=@ r=@ s=@]                ::  column coefficients
              ++  ix  |~(a=@ *@)                        ::  key index
              ++  ro  *[p=@ q=@ r=@ s=@]                ::  row shifts
              ++  su  *@                                ::  s-box
              --  ::cipa
            --  ::
        |%
        ::                                              ::  ++pen:ahem:aes:
        ++  pen                                         ::  encrypt
          ^-  cipa
          |%
          ::                                            ::  ++co:pen:ahem:aes:
          ++  co                                        ::  column coefficients
            [0x2 0x3 1 1]
          ::                                            ::  ++ix:pen:ahem:aes:
          ++  ix                                        ::  key index
            |~(a=@ a)
          ::                                            ::  ++ro:pen:ahem:aes:
          ++  ro                                        ::  row shifts
            [0 1 2 3]
          ::                                            ::  ++su:pen:ahem:aes:
          ++  su                                        ::  s-box
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
          --
        ::                                              ::  ++pin:ahem:aes:
        ++  pin                                         ::  decrypt
          ^-  cipa
          |%
          ::                                            ::  ++co:pin:ahem:aes:
          ++  co                                        ::  column coefficients
            [0xe 0xb 0xd 0x9]
          ::                                            ::  ++ix:pin:ahem:aes:
          ++  ix                                        ::  key index
            |~(a=@ (sub nnr a))
          ::                                            ::  ++ro:pin:ahem:aes:
          ++  ro                                        ::  row shifts
            [0 3 2 1]
          ::                                            ::  ++su:pin:ahem:aes:
          ++  su                                        ::  s-box
            0x7d0c.2155.6314.69e1.26d6.77ba.7e04.2b17.
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
        ::                                              ::  ++mcol:ahem:aes:
        ++  mcol                                        ::
          |=  [a=(list @) b=[p=@ q=@ r=@ s=@]]
          ^-  (list @)
          =+  c=[p=*@ q=*@ r=*@ s=*@]
          |-  ^-  (list @)
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
        ::                                              ::  ++pode:ahem:aes:
        ++  pode                                        ::  explode to block
          |=  [a=bloq b=@ c=@]  ^-  (list @)
          =+  d=(rip a c)
          =+  m=(met a c)
          |-
          ?:  =(m b)
            d
          $(m +(m), d (weld d (limo [0 ~])))
        ::                                              ::  ++sube:ahem:aes:
        ++  sube                                        ::  s-box word
          |=  [a=@ b=@]  ^-  @
          (rep 3 (turn (pode 3 4 a) |=(c=@ (cut 3 [c 1] b))))
        --  ::
      |%
      ::                                                ::  ++be:ahem:aes:crypto
      ++  be                                            ::  block cipher
        |=  [a=? b=@ c=@H]  ^-  @uxH
        ~|  %be-aesc
        =>  %=    .
                +
              =>  +
              |%
              ::                                        ::  ++ankh:be:ahem:aes:
              ++  ankh                                  ::
                |=  [a=cipa b=@ c=@]
                (pode 5 nnb (cut 5 [(mul (ix.a b) nnb) nnb] c))
              ::                                        ::  ++sark:be:ahem:aes:
              ++  sark                                  ::
                |=  [c=(list @) d=(list @)]
                ^-  (list @)
                ?~  c  ~
                ?~  d  !!
                [(mix i.c i.d) $(c t.c, d t.d)]
              ::                                        ::  ++srow:be:ahem:aes:
              ++  srow                                  ::
                |=  [a=cipa b=(list @)]  ^-  (list @)
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
              ::                                        ::  ++subs:be:ahem:aes:
              ++  subs                                  ::
                |=  [a=cipa b=(list @)]  ^-  (list @)
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
      ::                                                ::  ++ex:ahem:aes:crypto
      ++  ex                                            ::  key expand
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
      ::                                                ::  ++ix:ahem:aes:crypto
      ++  ix                                            ::  key expand, inv
        |=  a=@  ^-  @
        =+  [i=1 j=*@ b=*@ c=co:pin]
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
    ::                                                  ::  ++ecba:aes:crypto
    ++  ecba                                            ::  AES-128 ECB
      ~%  %ecba  +>  ~
      |_  key=@H
      ::                                                ::  ++en:ecba:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  blk=@H  ^-  @uxH
        =+  (ahem 4 4 10)
        =:
          key  (~(net fe 7) key)
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be & (ex key) blk)
      ::                                                ::  ++de:ecba:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  blk=@H  ^-  @uxH
        =+  (ahem 4 4 10)
        =:
          key  (~(net fe 7) key)
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be | (ix (ex key)) blk)
      --  ::ecba
    ::                                                  ::  ++ecbb:aes:crypto
    ++  ecbb                                            ::  AES-192 ECB
      ~%  %ecbb  +>  ~
      |_  key=@I
      ::                                                ::  ++en:ecbb:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  blk=@H  ^-  @uxH
        =+  (ahem 6 4 12)
        =:
          key  (rsh 6 (~(net fe 8) key))
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be & (ex key) blk)
      ::                                                ::  ++de:ecbb:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  blk=@H  ^-  @uxH
        =+  (ahem 6 4 12)
        =:
          key  (rsh 6 (~(net fe 8) key))
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be | (ix (ex key)) blk)
      --  ::ecbb
    ::                                                  ::  ++ecbc:aes:crypto
    ++  ecbc                                            ::  AES-256 ECB
      ~%  %ecbc  +>  ~
      |_  key=@I
      ::                                                ::  ++en:ecbc:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  blk=@H  ^-  @uxH
        =+  (ahem 8 4 14)
        =:
          key  (~(net fe 8) key)
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be & (ex key) blk)
      ::                                                ::  ++de:ecbc:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  blk=@H  ^-  @uxH
        =+  (ahem 8 4 14)
        =:
          key  (~(net fe 8) key)
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be | (ix (ex key)) blk)
      --  ::ecbc
    ::                                                  ::  ++cbca:aes:crypto
    ++  cbca                                            ::  AES-128 CBC
      ~%  %cbca  +>  ~
      |_  [key=@H prv=@H]
      ::                                                ::  ++en:cbca:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt=@  ^-  @ux
        =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  cts=(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  pts
          cts
        =+  cph=(~(en ecba key) (mix prv i.pts))
        %=  $
          cts  [cph cts]
          pts  t.pts
          prv  cph
        ==
      ::                                                ::  ++de:cbca:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  txt=@  ^-  @ux
        =+  cts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  pts=(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  cts
          pts
        =+  pln=(mix prv (~(de ecba key) i.cts))
        %=  $
          pts  [pln pts]
          cts  t.cts
          prv  i.cts
        ==
      --  ::cbca
    ::                                                  ::  ++cbcb:aes:crypto
    ++  cbcb                                            ::  AES-192 CBC
      ~%  %cbcb  +>  ~
      |_  [key=@I prv=@H]
      ::                                                ::  ++en:cbcb:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt=@  ^-  @ux
        =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  cts=(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  pts
          cts
        =+  cph=(~(en ecbb key) (mix prv i.pts))
        %=  $
          cts  [cph cts]
          pts  t.pts
          prv  cph
        ==
      ::                                                ::  ++de:cbcb:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  txt=@  ^-  @ux
        =+  cts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  pts=(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  cts
          pts
        =+  pln=(mix prv (~(de ecbb key) i.cts))
        %=  $
          pts  [pln pts]
          cts  t.cts
          prv  i.cts
        ==
      --  ::cbcb
    ::                                                  ::  ++cbcc:aes:crypto
    ++  cbcc                                            ::  AES-256 CBC
      ~%  %cbcc  +>  ~
      |_  [key=@I prv=@H]
      ::                                                ::  ++en:cbcc:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt=@  ^-  @ux
        =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  cts=(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  pts
          cts
        =+  cph=(~(en ecbc key) (mix prv i.pts))
        %=  $
          cts  [cph cts]
          pts  t.pts
          prv  cph
        ==
      ::                                                ::  ++de:cbcc:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  txt=@  ^-  @ux
        =+  cts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  pts=(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  cts
          pts
        =+  pln=(mix prv (~(de ecbc key) i.cts))
        %=  $
          pts  [pln pts]
          cts  t.cts
          prv  i.cts
        ==
      --  ::cbcc
    ::                                                  ::  ++inc:aes:crypto
    ++  inc                                             ::  inc. low bloq
      |=  [mod=bloq ctr=@H]
      ^-  @uxH
      =+  bqs=(rip mod ctr)
      ?~  bqs  0x1
      %+  rep  mod
      [(~(sum fe mod) i.bqs 1) t.bqs]
    ::                                                  ::  ++ctra:aes:crypto
    ++  ctra                                            ::  AES-128 CTR
      ~%  %ctra  +>  ~
      |_  [key=@H mod=bloq len=@ ctr=@H]
      ::                                                ::  ++en:ctra:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt=@
        ^-  @ux
        =/  encrypt  ~(en ecba key)
        =/  blocks  (add (div len 16) ?:(=((^mod len 16) 0) 0 1))
        ?>  (gte len (met 3 txt))
        %+  mix  txt
        %+  rsh  [3 (sub (mul 16 blocks) len)]
        %+  rep  7
        =|  seed=(list @ux)
        |-  ^+  seed
        ?:  =(blocks 0)  seed
        %=  $
          seed    [(encrypt ctr) seed]
          ctr     (inc mod ctr)
          blocks  (dec blocks)
        ==
      ::                                                ::  ++de:ctra:aes:crypto
      ++  de                                            ::  decrypt
        en
      --  ::ctra
    ::                                                  ::  ++ctrb:aes:crypto
    ++  ctrb                                            ::  AES-192 CTR
      ~%  %ctrb  +>  ~
      |_  [key=@I mod=bloq len=@ ctr=@H]
      ::                                                ::  ++en:ctrb:aes:crypto
      ++  en
        ~/  %en
        |=  txt=@
        ^-  @ux
        =/  encrypt  ~(en ecbb key)
        =/  blocks  (add (div len 16) ?:(=((^mod len 16) 0) 0 1))
        ?>  (gte len (met 3 txt))
        %+  mix  txt
        %+  rsh  [3 (sub (mul 16 blocks) len)]
        %+  rep  7
        =|  seed=(list @ux)
        |-  ^+  seed
        ?:  =(blocks 0)  seed
        %=  $
          seed    [(encrypt ctr) seed]
          ctr     (inc mod ctr)
          blocks  (dec blocks)
        ==
      ::                                                ::  ++de:ctrb:aes:crypto
      ++  de                                            ::  decrypt
        en
      --  ::ctrb
    ::                                                  ::  ++ctrc:aes:crypto
    ++  ctrc                                            ::  AES-256 CTR
      ~%  %ctrc  +>  ~
      |_  [key=@I mod=bloq len=@ ctr=@H]
      ::                                                ::  ++en:ctrc:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt=@
        ^-  @ux
        =/  encrypt  ~(en ecbc key)
        =/  blocks  (add (div len 16) ?:(=((^mod len 16) 0) 0 1))
        ?>  (gte len (met 3 txt))
        %+  mix  txt
        %+  rsh  [3 (sub (mul 16 blocks) len)]
        %+  rep  7
        =|  seed=(list @ux)
        |-  ^+  seed
        ?:  =(blocks 0)  seed
        %=  $
          seed    [(encrypt ctr) seed]
          ctr     (inc mod ctr)
          blocks  (dec blocks)
        ==
      ::                                                ::  ++de:ctrc:aes:crypto
      ++  de                                            ::  decrypt
        en
      --  ::ctrc
    ::                                                  ::  ++doub:aes:crypto
    ++  doub                                            ::  double 128-bit
      |=  ::  string mod finite
          ::
          str=@H
      ::
      ::  field (see spec)
      ::
      ^-  @uxH
      %-  ~(sit fe 7)
      ?.  =((xeb str) 128)
        (lsh 0 str)
      (mix 0x87 (lsh 0 str))
    ::                                                  ::  ++mpad:aes:crypto
    ++  mpad                                            ::
      |=  [oct=@ txt=@]
      ::
      ::  pad message to multiple of 128 bits
      ::  by appending 1, then 0s
      ::  the spec is unclear, but it must be octet based
      ::  to match the test vectors
      ::
      ^-  @ux
      =+  pad=(mod oct 16)
      ?:  =(pad 0)  0x8000.0000.0000.0000.0000.0000.0000.0000
      (lsh [3 (sub 15 pad)] (mix 0x80 (lsh 3 txt)))
    ::                                                  ::  ++suba:aes:crypto
    ++  suba                                            ::  AES-128 subkeys
      |=  key=@H
      =+  l=(~(en ecba key) 0)
      =+  k1=(doub l)
      =+  k2=(doub k1)
      ^-  [@ux @ux]
      [k1 k2]
    ::                                                  ::  ++subb:aes:crypto
    ++  subb                                            ::  AES-192 subkeys
      |=  key=@I
      =+  l=(~(en ecbb key) 0)
      =+  k1=(doub l)
      =+  k2=(doub k1)
      ^-  [@ux @ux]
      [k1 k2]
    ::                                                  ::  ++subc:aes:crypto
    ++  subc                                            ::  AES-256 subkeys
      |=  key=@I
      =+  l=(~(en ecbc key) 0)
      =+  k1=(doub l)
      =+  k2=(doub k1)
      ^-  [@ux @ux]
      [k1 k2]
    ::                                                  ::  ++maca:aes:crypto
    ++  maca                                            ::  AES-128 CMAC
      ~/  %maca
      |=  [key=@H oct=(unit @) txt=@]
      ^-  @ux
      =+  [sub=(suba key) len=?~(oct (met 3 txt) u.oct)]
      =+  ^=  pdt
        ?:  &(=((mod len 16) 0) !=(len 0))
          [& txt]
        [| (mpad len txt)]
      =+  ^=  mac
        %-  ~(en cbca key 0)
        %+  mix  +.pdt
        ?-  -.pdt
          %&  -.sub
          %|  +.sub
        ==
      ::  spec says MSBs, LSBs match test vectors
      ::
      (~(sit fe 7) mac)
    ::                                                  ::  ++macb:aes:crypto
    ++  macb                                            ::  AES-192 CMAC
      ~/  %macb
      |=  [key=@I oct=(unit @) txt=@]
      ^-  @ux
      =+  [sub=(subb key) len=?~(oct (met 3 txt) u.oct)]
      =+  ^=  pdt
        ?:  &(=((mod len 16) 0) !=(len 0))
          [& txt]
        [| (mpad len txt)]
      =+  ^=  mac
        %-  ~(en cbcb key 0)
        %+  mix  +.pdt
        ?-  -.pdt
          %&  -.sub
          %|  +.sub
        ==
      ::  spec says MSBs, LSBs match test vectors
      ::
      (~(sit fe 7) mac)
    ::                                                  ::  ++macc:aes:crypto
    ++  macc                                            :: AES-256 CMAC
      ~/  %macc
      |=  [key=@I oct=(unit @) txt=@]
      ^-  @ux
      =+  [sub=(subc key) len=?~(oct (met 3 txt) u.oct)]
      =+  ^=  pdt
        ?:  &(=((mod len 16) 0) !=(len 0))
          [& txt]
        [| (mpad len txt)]
      =+  ^=  mac
        %-  ~(en cbcc key 0)
        %+  mix  +.pdt
        ?-  -.pdt
          %&  -.sub
          %|  +.sub
        ==
      ::  spec says MSBs, LSBs match test vectors
      ::
      (~(sit fe 7) mac)
    ::                                                  ::  ++s2va:aes:crypto
    ++  s2va                                            ::  AES-128 S2V
      ~/  %s2va
      |=  [key=@H ads=(list @)]
      ?~  ads  (maca key `16 0x1)
      =/  res  (maca key `16 0x0)
      %+  maca  key
      |-  ^-  [[~ @ud] @uxH]
      ?~  t.ads
        =/  wyt  (met 3 i.ads)
        ?:  (gte wyt 16)
          [`wyt (mix i.ads res)]
        [`16 (mix (doub res) (mpad wyt i.ads))]
      %=  $
        ads  t.ads
        res  (mix (doub res) (maca key ~ i.ads))
      ==
    ::                                                  ::  ++s2vb:aes:crypto
    ++  s2vb                                            ::  AES-192 S2V
      ~/  %s2vb
      |=  [key=@I ads=(list @)]
      ?~  ads  (macb key `16 0x1)
      =/  res  (macb key `16 0x0)
      %+  macb  key
      |-  ^-  [[~ @ud] @uxH]
      ?~  t.ads
        =/  wyt  (met 3 i.ads)
        ?:  (gte wyt 16)
          [`wyt (mix i.ads res)]
        [`16 (mix (doub res) (mpad wyt i.ads))]
      %=  $
        ads  t.ads
        res  (mix (doub res) (macb key ~ i.ads))
      ==
    ::                                                  ::  ++s2vc:aes:crypto
    ++  s2vc                                            ::  AES-256 S2V
      ~/  %s2vc
      |=  [key=@I ads=(list @)]
      ?~  ads  (macc key `16 0x1)
      =/  res  (macc key `16 0x0)
      %+  macc  key
      |-  ^-  [[~ @ud] @uxH]
      ?~  t.ads
        =/  wyt  (met 3 i.ads)
        ?:  (gte wyt 16)
          [`wyt (mix i.ads res)]
        [`16 (mix (doub res) (mpad wyt i.ads))]
      %=  $
        ads  t.ads
        res  (mix (doub res) (macc key ~ i.ads))
      ==
    ::                                                  ::  ++siva:aes:crypto
    ++  siva                                            ::  AES-128 SIV
      ~%  %siva  +>  ~
      |_  [key=@I vec=(list @)]
      ::                                                ::  ++en:siva:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt=@
        ^-  (trel @uxH @ud @ux)
        =+  [k1=(rsh 7 key) k2=(end 7 key)]
        =+  iv=(s2va k1 (weld vec (limo ~[txt])))
        =+  len=(met 3 txt)
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        :+
          iv
          len
        (~(en ctra k2 7 len hib) txt)
      ::                                                ::  ++de:siva:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  [iv=@H len=@ txt=@]
        ^-  (unit @ux)
        =+  [k1=(rsh 7 key) k2=(end 7 key)]
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  ^=  pln
          (~(de ctra k2 7 len hib) txt)
        ?.  =((s2va k1 (weld vec (limo ~[pln]))) iv)
          ~
        `pln
      --  ::siva
    ::                                                  ::  ++sivb:aes:crypto
    ++  sivb                                            ::  AES-192 SIV
      ~%  %sivb  +>  ~
      |_  [key=@J vec=(list @)]
      ::                                                ::  ++en:sivb:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt=@
        ^-  (trel @uxH @ud @ux)
        =+  [k1=(rsh [6 3] key) k2=(end [6 3] key)]
        =+  iv=(s2vb k1 (weld vec (limo ~[txt])))
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  len=(met 3 txt)
        :+  iv
          len
        (~(en ctrb k2 7 len hib) txt)
      ::                                                ::  ++de:sivb:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  [iv=@H len=@ txt=@]
        ^-  (unit @ux)
        =+  [k1=(rsh [6 3] key) k2=(end [6 3] key)]
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  ^=  pln
          (~(de ctrb k2 7 len hib) txt)
        ?.  =((s2vb k1 (weld vec (limo ~[pln]))) iv)
          ~
        `pln
      --  ::sivb
    ::                                                  ::  ++sivc:aes:crypto
    ++  sivc                                            ::  AES-256 SIV
      ~%  %sivc  +>  ~
      |_  [key=@J vec=(list @)]
      ::                                                ::  ++en:sivc:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt=@
        ^-  (trel @uxH @ud @ux)
        =+  [k1=(rsh 8 key) k2=(end 8 key)]
        =+  iv=(s2vc k1 (weld vec (limo ~[txt])))
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  len=(met 3 txt)
        :+
          iv
          len
        (~(en ctrc k2 7 len hib) txt)
      ::                                                ::  ++de:sivc:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  [iv=@H len=@ txt=@]
        ^-  (unit @ux)
        =+  [k1=(rsh 8 key) k2=(end 8 key)]
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  ^=  pln
          (~(de ctrc k2 7 len hib) txt)
        ?.  =((s2vc k1 (weld vec (limo ~[pln]))) iv)
          ~
        `pln
      --  ::sivc
    --
  ::                                                    ::
  ::::                    ++ed:crypto                   ::  ed25519
    ::                                                  ::::
  ++  ed
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
      ~%  %coed  ..part  ~
      |%
      ::                                                ::  ++norm:ed:crypto
      ++  norm                                          ::
        |=(x=@ ?:(=(0 (mod x 2)) x (sub q x)))
      ::                                                ::  ++xrec:ed:crypto
      ++  xrec                                          ::  recover x-coord
        |=  y=@  ^-  @
        =+  ^=  xx
            %+  mul  (dif.fq (mul y y) 1)
                     (inv.fq +(:(mul d y y)))
        =+  x=(exp.fq (div (add 3 q) 8) xx)
        ?:  !=(0 (dif.fq (mul x x) (sit.fq xx)))
          (norm (pro.fq x ii))
        (norm x)
      ::                                                ::  ++ward:ed:crypto
      ++  ward                                          ::  edwards multiply
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
      ::                                                ::  ++scam:ed:crypto
      ++  scam                                          ::  scalar multiply
        |=  [pp=[@ @] e=@]  ^-  [@ @]
        ?:  =(0 e)
          [0 1]
        =+  qq=$(e (div e 2))
        =>  .(qq (ward qq qq))
        ?:  =(1 (dis 1 e))
          (ward qq pp)
        qq
      ::                                                ::  ++etch:ed:crypto
      ++  etch                                          ::  encode point
        |=  pp=[@ @]  ^-  @
        (can 0 ~[[(sub b 1) +.pp] [1 (dis 1 -.pp)]])
      ::                                                ::  ++curv:ed:crypto
      ++  curv                                          ::  point on curve?
        |=  [x=@ y=@]  ^-  ?
        .=  0
            %+  dif.fq
              %+  sum.fq
                (pro.fq (sub q (sit.fq x)) x)
              (pro.fq y y)
            (sum.fq 1 :(pro.fq d x x y y))
      ::                                                ::  ++deco:ed:crypto
      ++  deco                                          ::  decode point
        |=  s=@  ^-  (unit [@ @])
        =+  y=(cut 0 [0 (dec b)] s)
        =+  si=(cut 0 [(dec b) 1] s)
        =+  x=(xrec y)
        =>  .(x ?:(!=(si (dis 1 x)) (sub q x) x))
        =+  pp=[x y]
        ?.  (curv pp)
          ~
        [~ pp]
      ::                                                ::  ++bb:ed:crypto
      ++  bb                                            ::
        =+  bby=(pro.fq 4 (inv.fq 5))
        [(xrec bby) bby]
      --  ::
    ~%  %ed  +  ~
    |%
    ::
    ++  point-add
      ~/  %point-add
      |=  [a-point=@udpoint b-point=@udpoint]
      ^-  @udpoint
      ::
      =/  a-point-decoded=[@ @]  (need (deco a-point))
      =/  b-point-decoded=[@ @]  (need (deco b-point))
      ::
      %-  etch
      (ward a-point-decoded b-point-decoded)
    ::
    ++  scalarmult
      ~/  %scalarmult
      |=  [a=@udscalar a-point=@udpoint]
      ^-  @udpoint
      ::
      =/  a-point-decoded=[@ @]  (need (deco a-point))
      ::
      %-  etch
      (scam a-point-decoded a)
    ::
    ++  scalarmult-base
      ~/  %scalarmult-base
      |=  scalar=@udscalar
      ^-  @udpoint
      %-  etch
      (scam bb scalar)
    ::
    ++  add-scalarmult-scalarmult-base
      ~/  %add-scalarmult-scalarmult-base
      |=  [a=@udscalar a-point=@udpoint b=@udscalar]
      ^-  @udpoint
      ::
      =/  a-point-decoded=[@ @]  (need (deco a-point))
      ::
      %-  etch
      %+  ward
        (scam bb b)
      (scam a-point-decoded a)
    ::
    ++  add-double-scalarmult
      ~/  %add-double-scalarmult
      |=  [a=@udscalar a-point=@udpoint b=@udscalar b-point=@udpoint]
      ^-  @udpoint
      ::
      =/  a-point-decoded=[@ @]  (need (deco a-point))
      =/  b-point-decoded=[@ @]  (need (deco b-point))
      ::
      %-  etch
      %+  ward
        (scam a-point-decoded a)
      (scam b-point-decoded b)
    ::                                                  ::  ++puck:ed:crypto
    ++  puck                                            ::  public key
      ~/  %puck
      |=  sk=@I  ^-  @
      ?:  (gth (met 3 sk) 32)  !!
      =+  h=(shal (rsh [0 3] b) sk)
      =+  ^=  a
          %+  add
            (bex (sub b 2))
          (lsh [0 3] (cut 0 [3 (sub b 5)] h))
      =+  aa=(scam bb a)
      (etch aa)
    ::                                                  ::  ++suck:ed:crypto
    ++  suck                                            ::  keypair from seed
      |=  se=@I  ^-  @uJ
      =+  pu=(puck se)
      (can 0 ~[[b se] [b pu]])
    ::                                                  ::  ++shar:ed:crypto
    ++  shar                                            ::  curve25519 secret
      ~/  %shar
      |=  [pub=@ sek=@]
      ^-  @ux
      =+  exp=(shal (rsh [0 3] b) (suck sek))
      =.  exp  (dis exp (can 0 ~[[3 0] [251 (fil 0 251 1)]]))
      =.  exp  (con exp (lsh [3 31] 0b100.0000))
      =+  prv=(end 8 exp)
      =+  crv=(fra.fq (sum.fq 1 pub) (dif.fq 1 pub))
      (curt prv crv)
    ::                                                  ::  ++sign:ed:crypto
    ++  sign                                            ::  certify
      ~/  %sign
      |=  [m=@ se=@]  ^-  @
      =+  sk=(suck se)
      =+  pk=(cut 0 [b b] sk)
      =+  h=(shal (rsh [0 3] b) sk)
      =+  ^=  a
          %+  add
            (bex (sub b 2))
          (lsh [0 3] (cut 0 [3 (sub b 5)] h))
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
    ::                                                  ::  ++veri:ed:crypto
    ++  veri                                            ::  validate
      ~/  %veri
      |=  [s=@ m=@ pk=@]  ^-  ?
      ?:  (gth (div b 4) (met 3 s))  |
      ?:  (gth (div b 8) (met 3 pk))  |
      =+  cb=(rsh [0 3] b)
      =+  rr=(deco (cut 0 [0 b] s))
      ?~  rr  |
      =+  aa=(deco pk)
      ?~  aa  |
      =+  ss=(cut 0 [b b] s)
      =+  ha=(can 3 ~[[cb (etch u.rr)] [cb pk] [(met 3 m) m]])
      =+  h=(shaz ha)
      =((scam bb ss) (ward u.rr (scam u.aa h)))
    --  ::ed
  ::                                                    ::
  ::::                    ++scr:crypto                  ::  (2b3) scrypt
    ::                                                  ::::
  ++  scr
    ~%  %scr  ..part  ~
    |%
    ::                                                  ::  ++sal:scr:crypto
    ++  sal                                             ::  salsa20 hash
      |=  [x=@ r=@]                                     ::  with r rounds
      ?>  =((mod r 2) 0)                                ::
      =+  few==>(fe .(a 5))
      =+  ^=  rot
        |=  [a=@ b=@]
        (mix (end 5 (lsh [0 a] b)) (rsh [0 (sub 32 a)] b))
      =+  ^=  lea
        |=  [a=@ b=@]
        (net:few (sum:few (net:few a) (net:few b)))
      =>  |%
          ::                                            ::  ++qr:sal:scr:crypto
          ++  qr                                        ::  quarterround
            |=  y=[@ @ @ @ ~]
            =+  zb=(mix &2.y (rot 7 (sum:few &1.y &4.y)))
            =+  zc=(mix &3.y (rot 9 (sum:few zb &1.y)))
            =+  zd=(mix &4.y (rot 13 (sum:few zc zb)))
            =+  za=(mix &1.y (rot 18 (sum:few zd zc)))
            ~[za zb zc zd]
          ::                                            ::  ++rr:sal:scr:crypto
          ++  rr                                        ::  rowround
            |=  [y=(list @)]
            =+  za=(qr ~[&1.y &2.y &3.y &4.y])
            =+  zb=(qr ~[&6.y &7.y &8.y &5.y])
            =+  zc=(qr ~[&11.y &12.y &9.y &10.y])
            =+  zd=(qr ~[&16.y &13.y &14.y &15.y])
            ^-  (list @)  :~
              &1.za  &2.za  &3.za  &4.za
              &4.zb  &1.zb  &2.zb  &3.zb
              &3.zc  &4.zc  &1.zc  &2.zc
              &2.zd  &3.zd  &4.zd  &1.zd  ==
          ::                                            ::  ++cr:sal:scr:crypto
          ++  cr                                        ::  columnround
            |=  [x=(list @)]
            =+  ya=(qr ~[&1.x &5.x &9.x &13.x])
            =+  yb=(qr ~[&6.x &10.x &14.x &2.x])
            =+  yc=(qr ~[&11.x &15.x &3.x &7.x])
            =+  yd=(qr ~[&16.x &4.x &8.x &12.x])
            ^-  (list @)  :~
              &1.ya  &4.yb  &3.yc  &2.yd
              &2.ya  &1.yb  &4.yc  &3.yd
              &3.ya  &2.yb  &1.yc  &4.yd
              &4.ya  &3.yb  &2.yc  &1.yd  ==
          ::                                            ::  ++dr:sal:scr:crypto
          ++  dr                                        ::  doubleround
            |=  [x=(list @)]
            (rr (cr x))
          ::                                            ::  ++al:sal:scr:crypto
          ++  al                                        ::  add two lists
            |=  [a=(list @) b=(list @)]
            |-  ^-  (list @)
            ?~  a  ~  ?~  b  ~
            [i=(sum:few -.a -.b) t=$(a +.a, b +.b)]
          --  ::
      =+  xw=(rpp 5 16 x)
      =+  ^=  ow  |-  ^-  (list @)
                  ?~  r  xw
                  $(xw (dr xw), r (sub r 2))
      (rep 5 (al xw ow))
    ::                                                  ::  ++rpp:scr:crypto
    ++  rpp                                             ::  rip+filler blocks
      |=  [a=bloq b=@ c=@]
      =+  q=(rip a c)
      =+  w=(lent q)
      ?.  =(w b)
        ?.  (lth w b)  (slag (sub w b) q)
        ^+  q  (weld q (reap (sub b (lent q)) 0))
      q
    ::                                                  ::  ++bls:scr:crypto
    ++  bls                                             ::  split to sublists
      |=  [a=@ b=(list @)]
      ?>  =((mod (lent b) a) 0)
      |-  ^-  (list (list @))
      ?~  b  ~
      [i=(scag a `(list @)`b) t=$(b (slag a `(list @)`b))]
    ::                                                  ::  ++slb:scr:crypto
    ++  slb                                             ::
      |=  [a=(list (list @))]
      |-  ^-  (list @)
      ?~  a  ~
      (weld `(list @)`-.a $(a +.a))
    ::                                                  ::  ++sbm:scr:crypto
    ++  sbm                                             ::  scryptBlockMix
      |=  [r=@ b=(list @)]
      ?>  =((lent b) (mul 2 r))
      =+  [x=(snag (dec (mul 2 r)) b) c=0]
      =|  [ya=(list @) yb=(list @)]
      |-  ^-  (list @)
      ?~  b  (flop (weld yb ya))
      =.  x  (sal (mix x -.b) 8)
      ?~  (mod c 2)
        $(c +(c), b +.b, ya [i=x t=ya])
      $(c +(c), b +.b, yb [i=x t=yb])
    ::                                                  ::  ++srm:scr:crypto
    ++  srm                                             ::  scryptROMix
      |=  [r=@ b=(list @) n=@]
      ?>  ?&  =((lent b) (mul 2 r))
              =(n (bex (dec (xeb n))))
              (lth n (bex (mul r 16)))
          ==
      =+  [v=*(list (list @)) c=0]
      =.  v
        |-  ^-  (list (list @))
        =+  w=(sbm r b)
        ?:  =(c n)  (flop v)
        $(c +(c), v [i=[b] t=v], b w)
      =+  x=(sbm r (snag (dec n) v))
      |-  ^-  (list @)
      ?:  =(c n)  x
      =+  q=(snag (dec (mul r 2)) x)
      =+  z=`(list @)`(snag (mod q n) v)
      =+  ^=  w  |-  ^-  (list @)
                 ?~  x  ~  ?~  z  ~
                 [i=(mix -.x -.z) t=$(x +.x, z +.z)]
      $(x (sbm r w), c +(c))
    ::                                                  ::  ++hmc:scr:crypto
    ++  hmc                                             ::  HMAC-SHA-256
      |=  [k=@ t=@]
      (hml k (met 3 k) t (met 3 t))
    ::                                                  ::  ++hml:scr:crypto
    ++  hml                                             ::  w+length
      |=  [k=@ kl=@ t=@ tl=@]
      =>  .(k (end [3 kl] k), t (end [3 tl] t))
      =+  b=64
      =?  k  (gth kl b)  (shay kl k)
      =+  ^=  q  %+  shay  (add b tl)
       (add (lsh [3 b] t) (mix k (fil 3 b 0x36)))
      %+  shay  (add b 32)
      (add (lsh [3 b] q) (mix k (fil 3 b 0x5c)))
    ::                                                  ::  ++pbk:scr:crypto
    ++  pbk                                             :: PBKDF2-HMAC-SHA256
      ~/  %pbk
      |=  [p=@ s=@ c=@ d=@]
      (pbl p (met 3 p) s (met 3 s) c d)
    ::                                                  ::  ++pbl:scr:crypto
    ++  pbl                                             ::  w+length
      ~/  %pbl
      |=  [p=@ pl=@ s=@ sl=@ c=@ d=@]
      =>  .(p (end [3 pl] p), s (end [3 sl] s))
      =+  h=32
      ::
      ::  max key length 1GB
      ::  max iterations 2^28
      ::
      ?>  ?&  (lte d (bex 30))
              (lte c (bex 28))
              !=(c 0)
          ==
      =+  ^=  l  ?~  (mod d h)
          (div d h)
        +((div d h))
      =+  r=(sub d (mul h (dec l)))
      =+  [t=0 j=1 k=1]
      =.  t  |-  ^-  @
        ?:  (gth j l)  t
        =+  u=(add s (lsh [3 sl] (rep 3 (flop (rpp 3 4 j)))))
        =+  f=0  =.  f  |-  ^-  @
          ?:  (gth k c)  f
          =+  q=(hml p pl u ?:(=(k 1) (add sl 4) h))
          $(u q, f (mix f q), k +(k))
        $(t (add t (lsh [3 (mul (dec j) h)] f)), j +(j))
      (end [3 d] t)
    ::                                                  ::  ++hsh:scr:crypto
    ++  hsh                                             ::  scrypt
      ~/  %hsh
      |=  [p=@ s=@ n=@ r=@ z=@ d=@]
      (hsl p (met 3 p) s (met 3 s) n r z d)
    ::                                                  ::  ++hsl:scr:crypto
    ++  hsl                                             ::  w+length
      ~/  %hsl
      |=  [p=@ pl=@ s=@ sl=@ n=@ r=@ z=@ d=@]
      =|  v=(list (list @))
      =>  .(p (end [3 pl] p), s (end [3 sl] s))
      =+  u=(mul (mul 128 r) z)
      ::
      ::  n is power of 2; max 1GB memory
      ::
      ?>  ?&  =(n (bex (dec (xeb n))))
              !=(r 0)  !=(z 0)
              %+  lte
                  (mul (mul 128 r) (dec (add n z)))
                (bex 30)
              (lth pl (bex 31))
              (lth sl (bex 31))
          ==
      =+  ^=  b  =+  (rpp 3 u (pbl p pl s sl 1 u))
        %+  turn  (bls (mul 128 r) -)
        |=(a=(list @) (rpp 9 (mul 2 r) (rep 3 a)))
      ?>  =((lent b) z)
      =+  ^=  q
        =+  |-  ?~  b  (flop v)
            $(b +.b, v [i=(srm r -.b n) t=v])
        %+  turn  `(list (list @))`-
        |=(a=(list @) (rpp 3 (mul 128 r) (rep 9 a)))
      (pbl p pl (rep 3 (slb q)) u 1 d)
    ::                                                  ::  ++ypt:scr:crypto
    ++  ypt                                             ::  256bit {salt pass}
      |=  [s=@ p=@]
      ^-  @
      (hsh p s 16.384 8 1 256)
    --  ::scr
  ::                                                    ::
  ::::                    ++crub:crypto                 ::  (2b4) suite B, Ed
    ::                                                  ::::
  ++  crub  !:
    ^-  acru
    =|  [pub=[cry=@ sgn=@] sek=(unit [cry=@ sgn=@])]
    |%
    ::                                                  ::  ++as:crub:crypto
    ++  as                                              ::
      |%
      ::                                                ::  ++sign:as:crub:
      ++  sign                                          ::
        |=  msg=@
        ^-  @ux
        (jam [(sigh msg) msg])
      ::                                                ::  ++sigh:as:crub:
      ++  sigh                                          ::
        |=  msg=@
        ^-  @ux
        ?~  sek  ~|  %pubkey-only  !!
        (sign:ed msg sgn.u.sek)
      ::                                                ::  ++sure:as:crub:
      ++  sure                                          ::
        |=  txt=@
        ^-  (unit @ux)
        =+  ;;([sig=@ msg=@] (cue txt))
        ?.  (safe sig msg)  ~
        (some msg)
      ::                                                ::  ++safe:as:crub:
      ++  safe
        |=  [sig=@ msg=@]
        ^-  ?
        (veri:ed sig msg sgn.pub)
      ::                                                ::  ++seal:as:crub:
      ++  seal                                          ::
        |=  [bpk=pass msg=@]
        ^-  @ux
        ?~  sek  ~|  %pubkey-only  !!
        ?>  =('b' (end 3 bpk))
        =+  pk=(rsh 8 (rsh 3 bpk))
        =+  shar=(shax (shar:ed pk cry.u.sek))
        =+  smsg=(sign msg)
        (jam (~(en siva:aes shar ~) smsg))
      ::                                                ::  ++tear:as:crub:
      ++  tear                                          ::
        |=  [bpk=pass txt=@]
        ^-  (unit @ux)
        ?~  sek  ~|  %pubkey-only  !!
        ?>  =('b' (end 3 bpk))
        =+  pk=(rsh 8 (rsh 3 bpk))
        =+  shar=(shax (shar:ed pk cry.u.sek))
        =+  ;;([iv=@ len=@ cph=@] (cue txt))
        =+  try=(~(de siva:aes shar ~) iv len cph)
        ?~  try  ~
        (sure:as:(com:nu:crub bpk) u.try)
      --  ::as
    ::                                                  ::  ++de:crub:crypto
    ++  de                                              ::  decrypt
      |=  [key=@J txt=@]
      ^-  (unit @ux)
      =+  ;;([iv=@ len=@ cph=@] (cue txt))
      %^    ~(de sivc:aes (shaz key) ~)
          iv
        len
      cph
    ::                                                  ::  ++dy:crub:crypto
    ++  dy                                              ::  need decrypt
      |=  [key=@J cph=@]
      (need (de key cph))
    ::                                                  ::  ++en:crub:crypto
    ++  en                                              ::  encrypt
      |=  [key=@J msg=@]
      ^-  @ux
      (jam (~(en sivc:aes (shaz key) ~) msg))
    ::                                                  ::  ++ex:crub:crypto
    ++  ex                                              ::  extract
      |%
      ::                                                ::  ++fig:ex:crub:crypto
      ++  fig                                           ::  fingerprint
        ^-  @uvH
        (shaf %bfig pub)
      ::                                                ::  ++pac:ex:crub:crypto
      ++  pac                                           ::  private fingerprint
        ^-  @uvG
        ?~  sek  ~|  %pubkey-only  !!
        (end 6 (shaf %bcod sec))
      ::                                                ::  ++pub:ex:crub:crypto
      ++  pub                                           ::  public key
        ^-  pass
        (cat 3 'b' (cat 8 sgn.^pub cry.^pub))
      ::                                                ::  ++sec:ex:crub:crypto
      ++  sec                                           ::  private key
        ^-  ring
        ?~  sek  ~|  %pubkey-only  !!
        (cat 3 'B' (cat 8 sgn.u.sek cry.u.sek))
      --  ::ex
    ::                                                  ::  ++nu:crub:crypto
    ++  nu                                              ::
      |%
      ::                                                ::  ++pit:nu:crub:crypto
      ++  pit                                           ::  create keypair
        |=  [w=@ seed=@]
        =+  wid=(add (div w 8) ?:(=((mod w 8) 0) 0 1))
        =+  bits=(shal wid seed)
        =+  [c=(rsh 8 bits) s=(end 8 bits)]
        ..nu(pub [cry=(puck:ed c) sgn=(puck:ed s)], sek `[cry=c sgn=s])
      ::                                                ::  ++nol:nu:crub:crypto
      ++  nol                                           ::  activate secret
        |=  a=ring
        =+  [mag=(end 3 a) bod=(rsh 3 a)]
        ~|  %not-crub-seckey  ?>  =('B' mag)
        =+  [c=(rsh 8 bod) s=(end 8 bod)]
        ..nu(pub [cry=(puck:ed c) sgn=(puck:ed s)], sek `[cry=c sgn=s])
      ::                                                ::  ++com:nu:crub:crypto
      ++  com                                           ::  activate public
        |=  a=pass
        =+  [mag=(end 3 a) bod=(rsh 3 a)]
        ~|  %not-crub-pubkey  ?>  =('b' mag)
        ..nu(pub [cry=(rsh 8 bod) sgn=(end 8 bod)], sek ~)
      --  ::nu
    --  ::crub
  ::                                                    ::
  ::::                    ++crua:crypto                 ::  (2b5) suite B, RSA
    ::                                                  ::::
  ++  crua  !!
  ::                                                    ::
  ::::                    ++test:crypto                 ::  (2b6) test crypto
    ::                                                  ::::
  ++  test  ^?
    |%
    ::                                                  ::  ++trub:test:crypto
    ++  trub                                            ::  test crub
      |=  msg=@t
      ::
      ::  make acru cores
      ::
      =/  ali      (pit:nu:crub 512 (shaz 'Alice'))
      =/  ali-pub  (com:nu:crub pub:ex.ali)
      =/  bob      (pit:nu:crub 512 (shaz 'Robert'))
      =/  bob-pub  (com:nu:crub pub:ex.bob)
      ::
      ::  alice signs and encrypts a symmetric key to bob
      ::
      =/  secret-key  %-  shaz
          'Let there be no duplicity when taking a stand against him.'
      =/  signed-key   (sign:as.ali secret-key)
      =/  crypted-key  (seal:as.ali pub:ex.bob-pub signed-key)
      ::  bob decrypts and verifies
      =/  decrypt-key-attempt  (tear:as.bob pub:ex.ali-pub crypted-key)
      =/  decrypted-key    ~|  %decrypt-fail  (need decrypt-key-attempt)
      =/  verify-key-attempt   (sure:as.ali-pub decrypted-key)
      =/  verified-key     ~|  %verify-fail  (need verify-key-attempt)
      ::  bob encrypts with symmetric key
      =/  crypted-msg  (en.bob verified-key msg)
      ::  alice decrypts with same key
      `@t`(dy.ali secret-key crypted-msg)
    --  ::test
  ::                                                    ::
  ::::                    ++keccak:crypto               ::  (2b7) keccak family
    ::                                                  ::::
  ++  keccak
    ~%  %kecc  ..part  ~
    |%
    ::
    ::  keccak
    ::
    ++  keccak-224  ~/  %k224  |=(a=octs (keccak 1.152 448 224 a))
    ++  keccak-256  ~/  %k256  |=(a=octs (keccak 1.088 512 256 a))
    ++  keccak-384  ~/  %k384  |=(a=octs (keccak 832 768 384 a))
    ++  keccak-512  ~/  %k512  |=(a=octs (keccak 576 1.024 512 a))
    ::
    ++  keccak  (cury (cury hash keccak-f) padding-keccak)
    ::
    ++  padding-keccak  (multirate-padding 0x1)
    ::
    ::  sha3
    ::
    ++  sha3-224  |=(a=octs (sha3 1.152 448 224 a))
    ++  sha3-256  |=(a=octs (sha3 1.088 512 256 a))
    ++  sha3-384  |=(a=octs (sha3 832 768 384 a))
    ++  sha3-512  |=(a=octs (sha3 576 1.024 512 a))
    ::
    ++  sha3  (cury (cury hash keccak-f) padding-sha3)
    ::
    ++  padding-sha3  (multirate-padding 0x6)
    ::
    ::  shake
    ::
    ++  shake-128  |=([o=@ud i=octs] (shake 1.344 256 o i))
    ++  shake-256  |=([o=@ud i=octs] (shake 1.088 512 o i))
    ::
    ++  shake  (cury (cury hash keccak-f) padding-shake)
    ::
    ++  padding-shake  (multirate-padding 0x1f)
    ::
    ::  rawshake
    ::
    ++  rawshake-128  |=([o=@ud i=octs] (rawshake 1.344 256 o i))
    ++  rawshake-256  |=([o=@ud i=octs] (rawshake 1.088 512 o i))
    ::
    ++  rawshake  (cury (cury hash keccak-f) padding-rawshake)
    ::
    ++  padding-rawshake  (multirate-padding 0x7)
    ::
    ::  core
    ::
    ++  hash
      ::  per:  permutation function with configurable width.
      ::  pad:  padding function.
      ::  rat:  bitrate, size in bits of blocks to operate on.
      ::  cap:  capacity, bits of sponge padding.
      ::  out:  length of desired output, in bits.
      ::  inp:  input to hash.
      |=  $:  per=$-(@ud $-(@ @))
              pad=$-([octs @ud] octs)
              rat=@ud
              cap=@ud
              out=@ud
              inp=octs
          ==
      ^-  @
      ::  urbit's little-endian to keccak's big-endian.
      =.  q.inp  (rev 3 inp)
      %.  [inp out]
      (sponge per pad rat cap)
    ::
    ::NOTE  if ++keccak ever needs to be made to operate
    ::      on bits rather than bytes, all that needs to
    ::      be done is updating the way this padding
    ::      function works. (and also "octs" -> "bits")
    ++  multirate-padding
      ::  dsb:  domain separation byte, reverse bit order.
      |=  dsb=@ux
      ?>  (lte dsb 0xff)
      |=  [inp=octs mut=@ud]
      ^-  octs
      =.  mut  (div mut 8)
      =+  pal=(sub mut (mod p.inp mut))
      =?  pal  =(pal 0)  mut
      =.  pal  (dec pal)
      :-  (add p.inp +(pal))
      ::  padding is provided in lane bit ordering,
      ::  ie, LSB = left.
      (cat 3 (con (lsh [3 pal] dsb) 0x80) q.inp)
    ::
    ++  sponge
      ::  sponge construction
      ::
      ::  preperm:  permutation function with configurable width.
      ::  padding:  padding function.
      ::  bitrate:  size of blocks to operate on.
      ::  capacity:  sponge padding.
      |=  $:  preperm=$-(@ud $-(@ @))
              padding=$-([octs @ud] octs)
              bitrate=@ud
              capacity=@ud
          ==
      ::
      ::  preparing
      =+  bitrate-bytes=(div bitrate 8)
      =+  blockwidth=(add bitrate capacity)
      =+  permute=(preperm blockwidth)
      ::
      |=  [input=octs output=@ud]
      |^  ^-  @
        ::
        ::  padding
        =.  input  (padding input bitrate)
        ::
        ::  absorbing
        =/  pieces=(list @)
          ::  amount of bitrate-sized blocks.
          ?>  =(0 (mod p.input bitrate-bytes))
          =+  i=(div p.input bitrate-bytes)
          |-
          ?:  =(i 0)  ~
          :_  $(i (dec i))
          ::  get the bitrate-sized block of bytes
          ::  that ends with the byte at -.
          =-  (cut 3 [- bitrate-bytes] q.input)
          (mul (dec i) bitrate-bytes)
        =/  state=@
          ::  for every piece,
          %+  roll  pieces
          |=  [p=@ s=@]
          ::  pad with capacity,
          =.  p  (lsh [0 capacity] p)
          ::  xor it into the state and permute it.
          (permute (mix s (bytes-to-lanes p)))
        ::
        ::  squeezing
        =|  res=@
        =|  len=@ud
        |-
        ::  append a bitrate-sized head of state to the
        ::  result.
        =.  res
          %+  con  (lsh [0 bitrate] res)
          (rsh [0 capacity] (lanes-to-bytes state))
        =.  len  (add len bitrate)
        ?:  (gte len output)
          ::  produce the requested bits of output.
          (rsh [0 (sub len output)] res)
        $(res res, state (permute state))
      ::
      ++  bytes-to-lanes
        ::  flip byte order in blocks of 8 bytes.
        |=  a=@
        %^  run  6  a
        |=(b=@ (lsh [3 (sub 8 (met 3 b))] (swp 3 b)))
      ::
      ++  lanes-to-bytes
        ::  unflip byte order in blocks of 8 bytes.
        |=  a=@
        %+  can  6
        %+  turn
          =+  (rip 6 a)
          (weld - (reap (sub 25 (lent -)) 0x0))
        |=  a=@
        :-  1
        %+  can  3
        =-  (turn - |=(a=@ [1 a]))
        =+  (flop (rip 3 a))
        (weld (reap (sub 8 (lent -)) 0x0) -)
      --
    ::
    ++  keccak-f
      ::  keccak permutation function
      |=  [width=@ud]
      ::  assert valid blockwidth.
      ?>  =-  (~(has in -) width)
          (sy 25 50 100 200 400 800 1.600 ~)
      ::  assumes 5x5 lanes state, as is the keccak
      ::  standard.
      =+  size=5
      =+  lanes=(mul size size)
      =+  lane-bloq=(dec (xeb (div width lanes)))
      =+  lane-size=(bex lane-bloq)
      =+  rounds=(add 12 (mul 2 lane-bloq))
      |=  [input=@]
      ^-  @
      =*  a  input
      =+  round=0
      |^
        ?:  =(round rounds)  a
        ::
        ::  theta
        =/  c=@
          %+  roll  (gulf 0 (dec size))
          |=  [x=@ud c=@]
          %+  con  (lsh [lane-bloq 1] c)
          %+  roll  (gulf 0 (dec size))
          |=  [y=@ud c=@]
          (mix c (get-lane x y a))
        =/  d=@
          %+  roll  (gulf 0 (dec size))
          |=  [x=@ud d=@]
          %+  con  (lsh [lane-bloq 1] d)
          %+  mix
            =-  (get-word - size c)
            ?:(=(x 0) (dec size) (dec x))
          %^  ~(rol fe lane-bloq)  0  1
          (get-word (mod +(x) size) size c)
        =.  a
          %+  roll  (gulf 0 (dec lanes))
          |=  [i=@ud a=_a]
          %+  mix  a
          %+  lsh
            [lane-bloq (sub lanes +(i))]
          (get-word i size d)
        ::
        ::  rho and pi
        =/  b=@
          %+  roll  (gulf 0 (dec lanes))
          |=  [i=@ b=@]
          =+  x=(mod i 5)
          =+  y=(div i 5)
          %+  con  b
          %+  lsh
            :-  lane-bloq
            %+  sub  lanes
            %+  add  +(y)
            %+  mul  size
            (mod (add (mul 2 x) (mul 3 y)) size)
          %^  ~(rol fe lane-bloq)  0
            (rotation-offset i)
          (get-word i lanes a)
        ::
        ::  chi
        =.  a
          %+  roll  (gulf 0 (dec lanes))
          |=  [i=@ud a=@]
          %+  con  (lsh lane-bloq a)
          =+  x=(mod i 5)
          =+  y=(div i 5)
          %+  mix  (get-lane x y b)
          %+  dis
            =-  (get-lane - y b)
            (mod (add x 2) size)
          %^  not  lane-bloq  1
          (get-lane (mod +(x) size) y b)
        ::
        ::  iota
        =.  a
          =+  (round-constant round)
          (mix a (lsh [lane-bloq (dec lanes)] -))
        ::
        ::  next round
        $(round +(round))
      ::
      ++  get-lane
        ::  get the lane with coordinates
        |=  [x=@ud y=@ud a=@]
        =+  i=(add x (mul size y))
        (get-word i lanes a)
      ::
      ++  get-word
        ::  get word {n} from atom {a} of {m} words.
        |=  [n=@ud m=@ud a=@]
        (cut lane-bloq [(sub m +((mod n m))) 1] a)
      ::
      ++  round-constant
        |=  c=@ud
        =-  (snag (mod c 24) -)
        ^-  (list @ux)
        :~  0x1
            0x8082
            0x8000.0000.0000.808a
            0x8000.0000.8000.8000
            0x808b
            0x8000.0001
            0x8000.0000.8000.8081
            0x8000.0000.0000.8009
            0x8a
            0x88
            0x8000.8009
            0x8000.000a
            0x8000.808b
            0x8000.0000.0000.008b
            0x8000.0000.0000.8089
            0x8000.0000.0000.8003
            0x8000.0000.0000.8002
            0x8000.0000.0000.0080
            0x800a
            0x8000.0000.8000.000a
            0x8000.0000.8000.8081
            0x8000.0000.0000.8080
            0x8000.0001
            0x8000.0000.8000.8008
        ==
      ::
      ++  rotation-offset
        |=  x=@ud
        =-  (snag x -)
        ^-  (list @ud)
        :~   0   1  62  28  27
            36  44   6  55  20
             3  10  43  25  39
            41  45  15  21   8
            18   2  61  56  14
        ==
      --
    --  ::keccak
  ::                                                    ::
  ::::                    ++hmac:crypto                 ::  (2b8) hmac family
    ::                                                  ::::
  ++  hmac
    ~%  %hmac  ..part  ~
    =,  sha
    =>  |%
        ++  meet  |=([k=@ m=@] [[(met 3 k) k] [(met 3 m) m]])
        ++  flip  |=([k=@ m=@] [(swp 3 k) (swp 3 m)])
        --
    |%
    ::
    ::  use with @
    ::
    ++  hmac-sha1     (cork meet hmac-sha1l)
    ++  hmac-sha256   (cork meet hmac-sha256l)
    ++  hmac-sha512   (cork meet hmac-sha512l)
    ::
    ::  use with @t
    ::
    ++  hmac-sha1t    (cork flip hmac-sha1)
    ++  hmac-sha256t  (cork flip hmac-sha256)
    ++  hmac-sha512t  (cork flip hmac-sha512)
    ::
    ::  use with byts
    ::
    ++  hmac-sha1l    (cury hmac sha-1l 64 20)
    ++  hmac-sha256l  (cury hmac sha-256l 64 32)
    ++  hmac-sha512l  (cury hmac sha-512l 128 64)
    ::
    ::  main logic
    ::
    ++  hmac
      ~/  %hmac
      ::  boq: block size in bytes used by haj
      ::  out: bytes output by haj
      |*  [[haj=$-([@u @] @) boq=@u out=@u] key=byts msg=byts]
      ::  ensure key and message fit signaled lengths
      =.  dat.key  (end [3 wid.key] dat.key)
      =.  dat.msg  (end [3 wid.msg] dat.msg)
      ::  keys longer than block size are shortened by hashing
      =?  dat.key  (gth wid.key boq)  (haj wid.key dat.key)
      =?  wid.key  (gth wid.key boq)  out
      ::  keys shorter than block size are right-padded
      =?  dat.key  (lth wid.key boq)  (lsh [3 (sub boq wid.key)] dat.key)
      ::  pad key, inner and outer
      =+  kip=(mix dat.key (fil 3 boq 0x36))
      =+  kop=(mix dat.key (fil 3 boq 0x5c))
      ::  append inner padding to message, then hash
      =+  (haj (add wid.msg boq) (add (lsh [3 wid.msg] kip) dat.msg))
      ::  prepend outer padding to result, hash again
      (haj (add out boq) (add (lsh [3 out] kop) -))
    --  ::  hmac
  ::                                                    ::
  ::::                    ++secp:crypto                 ::  (2b9) secp family
    ::                                                  ::::
  ++  secp  !.
    ::  TODO: as-octs and hmc are outside of jet parent
    =>  :+  ..part
          hmc=hmac-sha256l:hmac:crypto
        as-octs=as-octs:mimes:html
    ~%  %secp  +<  ~
    |%
    +$  jacobian   [x=@ y=@ z=@]                    ::  jacobian point
    +$  point      [x=@ y=@]                        ::  curve point
    +$  domain
      $:  p=@                                       ::  prime modulo
          a=@                                       ::  y^2=x^3+ax+b
          b=@                                       ::
          g=point                                   ::  base point
          n=@                                       ::  prime order of g
      ==
    ++  secp
      |_  [bytes=@ =domain]
      ++  field-p  ~(. fo p.domain)
      ++  field-n  ~(. fo n.domain)
      ++  compress-point
        |=  =point
        ^-  @
        %+  can  3
        :~  [bytes x.point]
            [1 (add 2 (cut 0 [0 1] y.point))]
        ==
      ::
      ++  serialize-point
        |=  =point
        ^-  @
        %+  can  3
        :~  [bytes y.point]
            [bytes x.point]
            [1 4]
        ==
      ::
      ++  decompress-point
        |=  compressed=@
        ^-  point
        =/  x=@  (end [3 bytes] compressed)
        ?>  =(3 (mod p.domain 4))
        =/  fop  field-p
        =+  [fadd fmul fpow]=[sum.fop pro.fop exp.fop]
        =/  y=@  %+  fpow  (rsh [0 2] +(p.domain))
                 %+  fadd  b.domain
                 %+  fadd  (fpow 3 x)
                (fmul a.domain x)
        =/  s=@  (rsh [3 bytes] compressed)
        ~|  [`@ux`s `@ux`compressed]
        ?>  |(=(2 s) =(3 s))
        ::  check parity
        ::
        =?  y  !=((sub s 2) (mod y 2))
          (sub p.domain y)
        [x y]
      ::
      ++  jc                                        ::  jacobian math
        |%
        ++  from
          |=  a=jacobian
          ^-  point
          =/  fop   field-p
          =+  [fmul fpow finv]=[pro.fop exp.fop inv.fop]
          =/  z  (finv z.a)
          :-  (fmul x.a (fpow 2 z))
          (fmul y.a (fpow 3 z))
        ::
        ++  into
          |=  point
          ^-  jacobian
          [x y 1]
        ::
        ++  double
          |=  jacobian
          ^-  jacobian
          ?:  =(0 y)  [0 0 0]
          =/  fop  field-p
          =+  [fadd fsub fmul fpow]=[sum.fop dif.fop pro.fop exp.fop]
          =/  s    :(fmul 4 x (fpow 2 y))
          =/  m    %+  fadd
                     (fmul 3 (fpow 2 x))
                   (fmul a.domain (fpow 4 z))
          =/  nx   %+  fsub
                     (fpow 2 m)
                   (fmul 2 s)
          =/  ny  %+  fsub
                    (fmul m (fsub s nx))
                  (fmul 8 (fpow 4 y))
          =/  nz  :(fmul 2 y z)
          [nx ny nz]
        ::
        ++  add
          |=  [a=jacobian b=jacobian]
          ^-  jacobian
          ?:  =(0 y.a)  b
          ?:  =(0 y.b)  a
          =/  fop  field-p
          =+  [fadd fsub fmul fpow]=[sum.fop dif.fop pro.fop exp.fop]
          =/  u1  :(fmul x.a z.b z.b)
          =/  u2  :(fmul x.b z.a z.a)
          =/  s1  :(fmul y.a z.b z.b z.b)
          =/  s2  :(fmul y.b z.a z.a z.a)
          ?:  =(u1 u2)
            ?.  =(s1 s2)
              [0 0 1]
            (double a)
          =/  h     (fsub u2 u1)
          =/  r     (fsub s2 s1)
          =/  h2    (fmul h h)
          =/  h3    (fmul h2 h)
          =/  u1h2  (fmul u1 h2)
          =/  nx    %+  fsub
                      (fmul r r)
                    :(fadd h3 u1h2 u1h2)
          =/  ny    %+  fsub
                      (fmul r (fsub u1h2 nx))
                    (fmul s1 h3)
          =/  nz    :(fmul h z.a z.b)
          [nx ny nz]
        ::
        ++  mul
          |=  [a=jacobian scalar=@]
          ^-  jacobian
          ?:  =(0 y.a)
            [0 0 1]
          ?:  =(0 scalar)
            [0 0 1]
          ?:  =(1 scalar)
            a
          ?:  (gte scalar n.domain)
            $(scalar (mod scalar n.domain))
          ?:  =(0 (mod scalar 2))
            (double $(scalar (rsh 0 scalar)))
          (add a (double $(scalar (rsh 0 scalar))))
        --
      ++  add-points
        |=  [a=point b=point]
        ^-  point
        =/  j  jc
        (from.j (add.j (into.j a) (into.j b)))
      ++  mul-point-scalar
        |=  [p=point scalar=@]
        ^-  point
        =/  j  jc
        %-  from.j
        %+  mul.j
          (into.j p)
        scalar
      ::
      ++  valid-hash
        |=  has=@
        (lte (met 3 has) bytes)
      ::
      ++  in-order
        |=  i=@
        ?&  (gth i 0)
            (lth i n.domain)
        ==
      ++  priv-to-pub
        |=  private-key=@
        ^-  point
        ?>  (in-order private-key)
        (mul-point-scalar g.domain private-key)
      ::
      ++  make-k
        |=  [hash=@ private-key=@]
        ^-  @
        ?>  (in-order private-key)
        ?>  (valid-hash hash)
        =/  v  (fil 3 bytes 1)
        =/  k  0
        =.  k  %+  hmc  [bytes k]
               %-  as-octs
               %+  can  3
               :~  [bytes hash]
                   [bytes private-key]
                   [1 0]
                   [bytes v]
               ==
        =.  v  (hmc bytes^k bytes^v)
        =.  k  %+  hmc  [bytes k]
               %-  as-octs
               %+  can  3
               :~  [bytes hash]
                   [bytes private-key]
                   [1 1]
                   [bytes v]
               ==
        =.  v  (hmc bytes^k bytes^v)
        (hmc bytes^k bytes^v)
      ::
      ++  ecdsa-raw-sign
        |=  [hash=@ private-key=@]
        ^-  [r=@ s=@ y=@]
        ::  make-k and priv-to pub will validate inputs
        =/  k   (make-k hash private-key)
        =/  rp  (priv-to-pub k)
        =*  r   x.rp
        ?<  =(0 r)
        =/  fon  field-n
        =+  [fadd fmul finv]=[sum.fon pro.fon inv.fon]
        =/  s  %+  fmul  (finv k)
               %+  fadd  hash
               %+  fmul  r
               private-key
        ?<  =(0 s)
        [r s y.rp]
      ::  general recovery omitted, but possible
      --
    ++  secp256k1
      ~%  %secp256k1  +  ~
      |%
      ++  t  :: in the battery for jet matching
        ^-  domain
        :*  0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
            ffff.ffff.ffff.ffff.ffff.fffe.ffff.fc2f
            0
            7
            :-  0x79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.
                  029b.fcdb.2dce.28d9.59f2.815b.16f8.1798
                0x483a.da77.26a3.c465.5da4.fbfc.0e11.08a8.
                  fd17.b448.a685.5419.9c47.d08f.fb10.d4b8
            0xffff.ffff.ffff.ffff.ffff.ffff.ffff.fffe.
              baae.dce6.af48.a03b.bfd2.5e8c.d036.4141
        ==
      ::
      ++  curve             ~(. secp 32 t)
      ++  serialize-point   serialize-point:curve
      ++  compress-point    compress-point:curve
      ++  decompress-point  decompress-point:curve
      ++  add-points        add-points:curve
      ++  mul-point-scalar  mul-point-scalar:curve
      ++  make-k
        ~/  %make
        |=  [hash=@uvI private-key=@]
        ::  checks sizes
        (make-k:curve hash private-key)
      ++  priv-to-pub
        |=  private-key=@
        ::  checks sizes
        (priv-to-pub:curve private-key)
      ::
      ++  ecdsa-raw-sign
        ~/  %sign
        |=  [hash=@uvI private-key=@]
        ^-  [v=@ r=@ s=@]
        =/  c  curve
        ::  raw-sign checks sizes
        =+  (ecdsa-raw-sign.c hash private-key)
        =/  rp=point  [r y]
        =/  s-high  (gte (mul 2 s) n.domain.c)
        =?  s   s-high
          (sub n.domain.c s)
        =?  rp  s-high
          [x.rp (sub p.domain.c y.rp)]
        =/  v   (end 0 y.rp)
        =?  v   (gte x.rp n.domain.c)
          (add v 2)
        [v x.rp s]
      ::
      ++  ecdsa-raw-recover
        ~/  %reco
        |=  [hash=@ sig=[v=@ r=@ s=@]]
        ^-  point
        ?>  (lte v.sig 3)
        =/  c   curve
        ?>  (valid-hash.c hash)
        ?>  (in-order.c r.sig)
        ?>  (in-order.c s.sig)
        =/  x  ?:  (gte v.sig 2)
                 (add r.sig n.domain.c)
               r.sig
        =/  fop  field-p.c
        =+  [fadd fmul fpow]=[sum.fop pro.fop exp.fop]
        =/  ysq   (fadd (fpow 3 x) b.domain.c)
        =/  beta  (fpow (rsh [0 2] +(p.domain.c)) ysq)
        =/  y  ?:  =((end 0 v.sig) (end 0 beta))
                 beta
               (sub p.domain.c beta)
        ?>  =(0 (dif.fop ysq (fmul y y)))
        =/  nz   (sub n.domain.c hash)
        =/  j    jc.c
        =/  gz   (mul.j (into.j g.domain.c) nz)
        =/  xy   (mul.j (into.j x y) s.sig)
        =/  qr   (add.j gz xy)
        =/  qj   (mul.j qr (inv:field-n.c x))
        =/  pub  (from.j qj)
        ?<  =([0 0] pub)
        pub
      ++  schnorr
        ~%  %schnorr  ..schnorr  ~
        =>  |%
            ++  tagged-hash
              |=  [tag=@ [l=@ x=@]]
              =+  hat=(sha-256:sha (swp 3 tag))
              %-  sha-256l:sha
              :-  (add 64 l)
              (can 3 ~[[l x] [32 hat] [32 hat]])
            ++  lift-x
              |=  x=@I
              ^-  (unit point)
              =/  c  curve
              ?.  (lth x p.domain.c)
                ~
              =/  fop  field-p.c
              =+  [fadd fpow]=[sum.fop exp.fop]
              =/  cp  (fadd (fpow 3 x) 7)
              =/  y  (fpow (rsh [0 2] +(p.domain.c)) cp)
              ?.  =(cp (fpow 2 y))
                ~
              %-  some  :-  x
              ?:  =(0 (mod y 2))
                y
              (sub p.domain.c y)
            --
        |%
        ::
        ++  sign                                        ::  schnorr signature
          ~/  %sosi
          |=  [sk=@I m=@I a=@I]
          ^-  @J
          ?>  (gte 32 (met 3 m))
          ?>  (gte 32 (met 3 a))
          =/  c  curve
          ::  implies (gte 32 (met 3 sk))
          ::
          ?<  |(=(0 sk) (gte sk n.domain.c))
          =/  pp
            (mul-point-scalar g.domain.c sk)
          =/  d
            ?:  =(0 (mod y.pp 2))
              sk
            (sub n.domain.c sk)
          =/  t
            %+  mix  d
            (tagged-hash 'BIP0340/aux' [32 a])
          =/  rand
            %+  tagged-hash  'BIP0340/nonce'
            :-  96
            (rep 8 ~[m x.pp t])
          =/  kp  (mod rand n.domain.c)
          ?<  =(0 kp)
          =/  rr  (mul-point-scalar g.domain.c kp)
          =/  k
            ?:  =(0 (mod y.rr 2))
              kp
            (sub n.domain.c kp)
          =/  e
            %-  mod
            :_  n.domain.c
            %+  tagged-hash  'BIP0340/challenge'
            :-  96
            (rep 8 ~[m x.pp x.rr])
          =/  sig
            %^  cat  8
              (mod (add k (mul e d)) n.domain.c)
            x.rr
          ?>  (verify x.pp m sig)
          sig
        ::
        ++  verify                                      ::  schnorr verify
          ~/  %sove
          |=  [pk=@I m=@I sig=@J]
          ^-  ?
          ?>  (gte 32 (met 3 pk))
          ?>  (gte 32 (met 3 m))
          ?>  (gte 64 (met 3 sig))
          =/  c  curve
          =/  pup  (lift-x pk)
          ?~  pup
            %.n
          =/  pp  u.pup
          =/  r  (cut 8 [1 1] sig)
          ?:  (gte r p.domain.c)
            %.n
          =/  s  (end 8 sig)
          ?:  (gte s n.domain.c)
            %.n
          =/  e
            %-  mod
            :_  n.domain.c
            %+  tagged-hash  'BIP0340/challenge'
            :-  96
            (rep 8 ~[m x.pp r])
          =/  aa
            (mul-point-scalar g.domain.c s)
          =/  bb
            (mul-point-scalar pp (sub n.domain.c e))
          ?:  &(=(x.aa x.bb) !=(y.aa y.bb))             ::  infinite?
            %.n
          =/  rr  (add-points aa bb)
          ?.  =(0 (mod y.rr 2))
            %.n
          =(r x.rr)
        --
      --
    --
  ::
  ++  blake
    ~%  %blake  ..part  ~
    |%
    ::TODO  generalize for both blake2 variants
    ++  blake2b
      ~/  %blake2b
      |=  [msg=byts key=byts out=@ud]
      ^-  @
      ::  initialization vector
      =/  iv=@
        0x6a09.e667.f3bc.c908.
          bb67.ae85.84ca.a73b.
          3c6e.f372.fe94.f82b.
          a54f.f53a.5f1d.36f1.
          510e.527f.ade6.82d1.
          9b05.688c.2b3e.6c1f.
          1f83.d9ab.fb41.bd6b.
          5be0.cd19.137e.2179
      ::  per-round constants
      =/  sigma=(list (list @ud))
        :~
          :~   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  ==
          :~  14  10   4   8   9  15  13   6   1  12   0   2  11   7   5   3  ==
          :~  11   8  12   0   5   2  15  13  10  14   3   6   7   1   9   4  ==
          :~   7   9   3   1  13  12  11  14   2   6   5  10   4   0  15   8  ==
          :~   9   0   5   7   2   4  10  15  14   1  11  12   6   8   3  13  ==
          :~   2  12   6  10   0  11   8   3   4  13   7   5  15  14   1   9  ==
          :~  12   5   1  15  14  13   4  10   0   7   6   3   9   2   8  11  ==
          :~  13  11   7  14  12   1   3   9   5   0  15   4   8   6   2  10  ==
          :~   6  15  14   9  11   3   0   8  12   2  13   7   1   4  10   5  ==
          :~  10   2   8   4   7   6   1   5  15  11   9  14   3  12  13   0  ==
          :~   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  ==
          :~  14  10   4   8   9  15  13   6   1  12   0   2  11   7   5   3  ==
        ==
      =>  |%
          ++  get-word-list
            |=  [h=@ w=@ud]
            ^-  (list @)
            %-  flop
            =+  l=(rip 6 h)
            =-  (weld - l)
            (reap (sub w (lent l)) 0)
          ::
          ++  get-word
            |=  [h=@ i=@ud w=@ud]
            ^-  @
            %+  snag  i
            (get-word-list h w)
          ::
          ++  put-word
            |=  [h=@ i=@ud w=@ud d=@]
            ^-  @
            %+  rep  6
            =+  l=(get-word-list h w)
            %-  flop
            %+  weld  (scag i l)
            [d (slag +(i) l)]
          ::
          ++  mod-word
            |*  [h=@ i=@ud w=@ud g=$-(@ @)]
            (put-word h i w (g (get-word h i w)))
          ::
          ++  pad
            |=  [byts len=@ud]
            (lsh [3 (sub len wid)] dat)
          ::
          ++  compress
            |=  [h=@ c=@ t=@ud l=?]
            ^-  @
            ::  set up local work vector
            =+  v=(add (lsh [6 8] h) iv)
            ::  xor the counter t into v
            =.  v
              %-  mod-word
              :^  v  12  16
              (cury mix (end [0 64] t))
            =.  v
              %-  mod-word
              :^  v  13  16
              (cury mix (rsh [0 64] t))
            ::  for the last block, invert v14
            =?  v  l
              %-  mod-word
              :^  v  14  16
              (cury mix 0xffff.ffff.ffff.ffff)
            ::  twelve rounds of message mixing
            =+  i=0
            =|  s=(list @)
            |^
              ?:  =(i 12)
                ::  xor upper and lower halves of v into state h
                =.  h  (mix h (rsh [6 8] v))
                (mix h (end [6 8] v))
              ::  select message mixing schedule and mix v
              =.  s  (snag (mod i 10) sigma)
              =.  v  (do-mix 0 4 8 12 0 1)
              =.  v  (do-mix 1 5 9 13 2 3)
              =.  v  (do-mix 2 6 10 14 4 5)
              =.  v  (do-mix 3 7 11 15 6 7)
              =.  v  (do-mix 0 5 10 15 8 9)
              =.  v  (do-mix 1 6 11 12 10 11)
              =.  v  (do-mix 2 7 8 13 12 13)
              =.  v  (do-mix 3 4 9 14 14 15)
              $(i +(i))
            ::
            ++  do-mix
              |=  [na=@ nb=@ nc=@ nd=@ nx=@ ny=@]
              ^-  @
              =-  =.  v  (put-word v na 16 a)
                  =.  v  (put-word v nb 16 b)
                  =.  v  (put-word v nc 16 c)
                         (put-word v nd 16 d)
              %-  b2mix
              :*  (get-word v na 16)
                  (get-word v nb 16)
                  (get-word v nc 16)
                  (get-word v nd 16)
                  (get-word c (snag nx s) 16)
                  (get-word c (snag ny s) 16)
              ==
            --
          ::
          ++  b2mix
            |=  [a=@ b=@ c=@ d=@ x=@ y=@]
            ^-  [a=@ b=@ c=@ d=@]
            =.  x  (rev 3 8 x)
            =.  y  (rev 3 8 y)
            =+  fed=~(. fe 6)
            =.  a  :(sum:fed a b x)
            =.  d  (ror:fed 0 32 (mix d a))
            =.  c  (sum:fed c d)
            =.  b  (ror:fed 0 24 (mix b c))
            =.  a  :(sum:fed a b y)
            =.  d  (ror:fed 0 16 (mix d a))
            =.  c  (sum:fed c d)
            =.  b  (ror:fed 0 63 (mix b c))
            [a b c d]
          --
      ::  ensure inputs adhere to contraints
      =.  out  (max 1 (min out 64))
      =.  wid.msg  (min wid.msg (bex 128))
      =.  wid.key  (min wid.key 64)
      =.  dat.msg  (end [3 wid.msg] dat.msg)
      =.  dat.key  (end [3 wid.key] dat.key)
      ::  initialize state vector
      =+  h=iv
      ::  mix key length and output length into h0
      =.  h
        %-  mod-word
        :^  h  0  8
        %+  cury  mix
        %+  add  0x101.0000
        (add (lsh 3 wid.key) out)
      ::  keep track of how much we've compressed
      =*  mes  dat.msg
      =+  com=0
      =+  rem=wid.msg
      ::  if we have a key, pad it and prepend to msg
      =?  mes  (gth wid.key 0)
        (can 3 ~[rem^mes 128^(pad key 128)])
      =?  rem  (gth wid.key 0)
        (add rem 128)
      |-
      ::  compress 128-byte chunks of the message
      ?:  (gth rem 128)
        =+  c=(cut 3 [(sub rem 128) 128] mes)
        =.  com   (add com 128)
        %_  $
          rem   (sub rem 128)
          h     (compress h c com |)
        ==
      ::  compress the final bytes of the msg
      =+  c=(cut 3 [0 rem] mes)
      =.  com  (add com rem)
      =.  c  (pad [rem c] 128)
      =.  h  (compress h c com &)
      ::  produce output of desired length
      %+  rsh  [3 (sub 64 out)]
      ::  do some word
      %+  rep  6
      %+  turn  (flop (gulf 0 7))
      |=  a=@
      (rev 3 8 (get-word h a 8))
    --  ::blake
  ::
  ++  argon2
    ~%  %argon  ..part  ~
    |%
    ::
    ::  structures
    ::
    +$  argon-type  ?(%d %i %id %u)
    ::
    ::  shorthands
    ::
    ++  argon2-urbit
      |=  out=@ud
      (argon2 out %u 0x13 4 512.000 1 *byts *byts)
    ::
    ::  argon2 proper
    ::
    ::  main argon2 operation
    ++  argon2
      ::  out:       desired output size in bytes
      ::  typ:       argon2 type
      ::  version:   argon2 version (0x10/v1.0 or 0x13/v1.3)
      ::  threads:   amount of threads/parallelism
      ::  mem-cost:  kb of memory to use
      ::  time-cost: iterations to run
      ::  key:       optional secret
      ::  extra:     optional arbitrary data
      |=  $:  out=@ud
              typ=argon-type
              version=@ux
            ::
              threads=@ud
              mem-cost=@ud
              time-cost=@ud
            ::
              key=byts
              extra=byts
          ==
      ^-  $-([msg=byts sat=byts] @)
      ::
      ::  check configuration sanity
      ::
      ?:  =(0 threads)
        ~|  %parallelism-must-be-above-zero
        !!
      ?:  =(0 time-cost)
        ~|  %time-cost-must-be-above-zero
        !!
      ?:  (lth mem-cost (mul 8 threads))
        ~|  :-  %memory-cost-must-be-at-least-threads
            [threads %times 8 (mul 8 threads)]
        !!
      ?.  |(=(0x10 version) =(0x13 version))
        ~|  [%unsupported-version version %want [0x10 0x13]]
        !!
      ::
      ::  calculate constants and initialize buffer
      ::
      ::  for each thread, there is a row in the buffer.
      ::  the amount of columns depends on the memory-cost.
      ::  columns are split into groups of four.
      ::  a single such quarter section of a row is a segment.
      ::
      ::  blocks:     (m_prime)
      ::  columns:    row length (q)
      ::  seg-length: segment length
      =/  blocks=@ud
        ::  round mem-cost down to the nearest multiple of 4*threads
        =+  (mul 4 threads)
        (mul (div mem-cost -) -)
      =+  columns=(div blocks threads)
      =+  seg-length=(div columns 4)
      ::
      =/  buffer=(list (list @))
        (reap threads (reap columns 0))
      ::
      ::  main function
      ::
      ::  msg: the main input
      ::  sat: optional salt
      ~%  %argon2  ..argon2  ~
      |=  [msg=byts sat=byts]
      ^-  @
      ?:  (lth wid.sat 8)
        ~|  [%min-salt-length-is-8 wid.sat]
        !!
      ::
      ::  h0: initial 64-byte block
      =/  h0=@
        =-  (blake2b:blake - 0^0 64)
        :-  :(add 40 wid.msg wid.sat wid.key wid.extra)
        %+  can  3
        =+  (cury (cury rev 3) 4)
        :~  (prep-wid extra)
            (prep-wid key)
            (prep-wid sat)
            (prep-wid msg)
            4^(- (type-to-num typ))
            4^(- version)
            4^(- time-cost)
            4^(- mem-cost)
            4^(- out)
            4^(- threads)
        ==
      ::
      ::  do time-cost passes over the buffer
      ::
      =+  t=0
      |-
      ?:  (lth t time-cost)
        ::
        ::  process all four segments in the columns...
        ::
        =+  s=0
        |-
        ?.  (lth s 4)  ^$(t +(t))
        ::
        ::  ...of every row/thread
        ::
        =+  r=0
        |-
        ?.  (lth r threads)  ^$(s +(s))
        =;  new=_buffer
          $(buffer new, r +(r))
        %-  fill-segment
        :*  buffer   h0
            t        s          r
            blocks   columns    seg-length
            threads  time-cost  typ         version
        ==
      ::
      ::  mix all rows together and hash the result
      ::
      =+  r=0
      =|  final=@
      |-
      ?:  =(r threads)
        (hash 1.024^final out)
      =-  $(final -, r +(r))
      %+  mix  final
      (snag (dec columns) (snag r buffer))
    ::
    ::  per-segment computation
    ++  fill-segment
      |=  $:  buffer=(list (list @))
              h0=@
            ::
              itn=@ud
              seg=@ud
              row=@ud
            ::
              blocks=@ud
              columns=@ud
              seg-length=@ud
            ::
              threads=@ud
              time-cost=@ud
              typ=argon-type
              version=@ux
          ==
      ::
      ::  fill-segment utilities
      ::
      =>  |%
          ++  put-word
            |=  [rob=(list @) i=@ud d=@]
            %+  weld  (scag i rob)
            [d (slag +(i) rob)]
          --
      ^+  buffer
      ::
      ::  rob:   row buffer to operate on
      ::  do-i:  whether to use prns from input rather than state
      ::  rands: prns generated from input, if we do-i
      =+  rob=(snag row buffer)
      =/  do-i=?
        ?|  ?=(%i typ)
            &(?=(%id typ) =(0 itn) (lte seg 1))
            &(?=(%u typ) =(0 itn) (lte seg 2))
        ==
      =/  rands=(list (pair @ @))
        ?.  do-i  ~
        ::
        ::  keep going until we have a list of :seg-length prn pairs
        ::
        =+  l=0
        =+  counter=1
        |-  ^-  (list (pair @ @))
        ?:  (gte l seg-length)  ~
        =-  (weld - $(counter +(counter), l (add l 128)))
        ::
        ::  generate pseudorandom block by compressing metadata
        ::
        =/  random-block=@
          %+  compress  0
          %+  compress  0
          %+  lsh  [3 968]
          %+  rep  6
          =+  (cury (cury rev 3) 8)
          :~  (- counter)
              (- (type-to-num typ))
              (- time-cost)
              (- blocks)
              (- seg)
              (- row)
              (- itn)
          ==
        ::
        ::  split the random-block into 64-bit sections,
        ::  then extract the first two 4-byte sections from each.
        ::
        %+  turn  (flop (rip 6 random-block))
        |=  a=@
        ^-  (pair @ @)
        :-  (rev 3 4 (rsh 5 a))
        (rev 3 4 (end 5 a))
      ::
      ::  iterate over the entire segment length
      ::
      =+  sin=0
      |-
      ::
      ::  when done, produce the updated buffer
      ::
      ?:  =(sin seg-length)
        %+  weld  (scag row buffer)
        [rob (slag +(row) buffer)]
      ::
      ::  col: current column to process
      =/  col=@ud
        (add (mul seg seg-length) sin)
      ::
      ::  first two columns are generated from h0
      ::
      ?:  &(=(0 itn) (lth col 2))
        =+  (app-num (app-num 64^h0 col) row)
        =+  (hash - 1.024)
        $(rob (put-word rob col -), sin +(sin))
      ::
      ::  c1, c2: prns for picking reference block
      =/  [c1=@ c2=@]
        ?:  do-i  (snag sin rands)
        =+  =-  (snag - rob)
            ?:  =(0 col)  (dec columns)
            (mod (dec col) columns)
        :-  (rev 3 4 (cut 3 [1.020 4] -))
        (rev 3 4 (cut 3 [1.016 4] -))
      ::
      ::  ref-row: reference block row
      =/  ref-row=@ud
        ?:  &(=(0 itn) =(0 seg))  row
        (mod c2 threads)
      ::
      ::  ref-col: reference block column
      =/  ref-col=@ud
        =-  (mod - columns)
        %+  add
          ::  starting index
          ?:  |(=(0 itn) =(3 seg))  0
          (mul +(seg) seg-length)
        ::  pseudorandom offset
        =-  %+  sub  (dec -)
            %+  rsh  [0 32]
            %+  mul  -
            (rsh [0 32] (mul c1 c1))
        ::  reference area size
        ?:  =(0 itn)
          ?:  |(=(0 seg) =(row ref-row))  (dec col)
          ?:  =(0 sin)  (dec (mul seg seg-length))
          (mul seg seg-length)
        =+  sul=(sub columns seg-length)
        ?:  =(ref-row row)   (dec (add sul sin))
        ?:  =(0 sin)  (dec sul)
        sul
      ::
      ::  compress the previous and reference block
      ::  to create the new block
      ::
      =/  new=@
        %+  compress
          =-  (snag - rob)
          ::  previous index, wrap-around
          ?:  =(0 col)  (dec columns)
          (mod (dec col) columns)
        ::  get reference block
        %+  snag  ref-col
        ?:  =(ref-row row)  rob
        (snag ref-row buffer)
      ::
      ::  starting from v1.3, we xor the new block in,
      ::  rather than directly overwriting the old block
      ::
      =?  new  &(!=(0 itn) =(0x13 version))
        (mix new (snag col rob))
      $(rob (put-word rob col new), sin +(sin))
    ::
    ::  compression function (g)
    ++  compress
      ::  x, y: assumed to be 1024 bytes
      |=  [x=@ y=@]
      ^-  @
      ::
      =+  r=(mix x y)
      =|  q=(list @)
      ::
      ::  iterate over rows of r to get q
      ::
      =+  i=0
      |-
      ?:  (lth i 8)
        =;  p=(list @)
          $(q (weld q p), i +(i))
        %-  permute
        =-  (weld (reap (sub 8 (lent -)) 0) -)
        %-  flop
        %+  rip  7
        (cut 10 [(sub 7 i) 1] r)
      ::
      ::  iterate over columns of q to get z
      ::
      =/  z=(list @)  (reap 64 0)
      =.  i  0
      |-
      ::
      ::  when done, assemble z and xor it with r
      ::
      ?.  (lth i 8)
        (mix (rep 7 (flop z)) r)
      ::
      ::  permute the column
      ::
      =/  out=(list @)
        %-  permute
        :~  (snag i q)
            (snag (add i 8) q)
            (snag (add i 16) q)
            (snag (add i 24) q)
            (snag (add i 32) q)
            (snag (add i 40) q)
            (snag (add i 48) q)
            (snag (add i 56) q)
        ==
      ::
      ::  put the result into z per column
      ::
      =+  j=0
      |-
      ?:  =(8 j)  ^$(i +(i))
      =-  $(z -, j +(j))
      =+  (add i (mul j 8))
      %+  weld  (scag - z)
      [(snag j out) (slag +(-) z)]
    ::
    ::  permutation function (p)
    ++  permute
      ::NOTE  this function really just takes and produces
      ::      8 values, but taking and producing them as
      ::      lists helps clean up the code significantly.
      |=  s=(list @)
      ?>  =(8 (lent s))
      ^-  (list @)
      ::
      ::  list inputs as 16 8-byte values
      ::
      =/  v=(list @)
        %-  zing
        ^-  (list (list @))
        %+  turn  s
        |=  a=@
        ::  rev for endianness
        =+  (rip 6 (rev 3 16 a))
        (weld - (reap (sub 2 (lent -)) 0))
      ::
      ::  do permutation rounds
      ::
      =.  v  (do-round v 0 4 8 12)
      =.  v  (do-round v 1 5 9 13)
      =.  v  (do-round v 2 6 10 14)
      =.  v  (do-round v 3 7 11 15)
      =.  v  (do-round v 0 5 10 15)
      =.  v  (do-round v 1 6 11 12)
      =.  v  (do-round v 2 7 8 13)
      =.  v  (do-round v 3 4 9 14)
      ::  rev for endianness
      =.  v  (turn v (cury (cury rev 3) 8))
      ::
      ::  cat v back together into 8 16-byte values
      ::
      %+  turn  (gulf 0 7)
      |=  i=@
      =+  (mul 2 i)
      (cat 6 (snag +(-) v) (snag - v))
    ::
    ::  perform a round and produce updated value list
    ++  do-round
      |=  [v=(list @) na=@ nb=@ nc=@ nd=@]
      ^+  v
      =>  |%
          ++  get-word
            |=  i=@ud
            (snag i v)
          ::
          ++  put-word
            |=  [i=@ud d=@]
            ^+  v
            %+  weld  (scag i v)
            [d (slag +(i) v)]
          --
      =-  =.  v  (put-word na a)
          =.  v  (put-word nb b)
          =.  v  (put-word nc c)
                 (put-word nd d)
      %-  round
      :*  (get-word na)
          (get-word nb)
          (get-word nc)
          (get-word nd)
      ==
    ::
    ::  perform a round (bg) and produce updated values
    ++  round
      |=  [a=@ b=@ c=@ d=@]
      ^-  [a=@ b=@ c=@ d=@]
      ::  operate on 64 bit words
      =+  fed=~(. fe 6)
      =*  sum  sum:fed
      =*  ror  ror:fed
      =+  end=(cury end 5)
      =.  a  :(sum a b :(mul 2 (end a) (end b)))
      =.  d  (ror 0 32 (mix d a))
      =.  c  :(sum c d :(mul 2 (end c) (end d)))
      =.  b  (ror 0 24 (mix b c))
      =.  a  :(sum a b :(mul 2 (end a) (end b)))
      =.  d  (ror 0 16 (mix d a))
      =.  c  :(sum c d :(mul 2 (end c) (end d)))
      =.  b  (ror 0 63 (mix b c))
      [a b c d]
    ::
    ::  argon2 wrapper around blake2b (h')
    ++  hash
      =,  blake
      |=  [byts out=@ud]
      ^-  @
      ::
      ::  msg: input with byte-length prepended
      =+  msg=(prep-num [wid dat] out)
      ::
      ::  if requested size is low enough, hash directly
      ::
      ?:  (lte out 64)
        (blake2b msg 0^0 out)
      ::
      ::  build up the result by hashing and re-hashing
      ::  the input message, adding the first 32 bytes
      ::  of the hash to the result, until we have the
      ::  desired output size.
      ::
      =+  tmp=(blake2b msg 0^0 64)
      =+  res=(rsh [3 32] tmp)
      =.  out  (sub out 32)
      |-
      ?:  (gth out 64)
        =.  tmp  (blake2b 64^tmp 0^0 64)
        =.  res  (add (lsh [3 32] res) (rsh [3 32] tmp))
        $(out (sub out 32))
      %+  add  (lsh [3 out] res)
      (blake2b 64^tmp 0^0 out)
    ::
    ::  utilities
    ::
    ++  type-to-num
      |=  t=argon-type
      ?-  t
        %d    0
        %i    1
        %id   2
        %u   10
      ==
    ::
    ++  app-num
      |=  [byts num=@ud]
      ^-  byts
      :-  (add wid 4)
      %+  can  3
      ~[4^(rev 3 4 num) wid^dat]
    ::
    ++  prep-num
      |=  [byts num=@ud]
      ^-  byts
      :-  (add wid 4)
      %+  can  3
      ~[wid^dat 4^(rev 3 4 num)]
    ::
    ++  prep-wid
      |=  a=byts
      (prep-num a wid.a)
    --
  ::
  ++  ripemd
    ~%  %ripemd  ..part  ~
    |%
    ++  ripemd-160
      ~/  %ripemd160
      |=  byts
      ^-  @
      ::  we operate on bits rather than bytes
      =.  wid  (mul wid 8)
      ::  add padding
      =+  (md5-pad wid dat)
      ::  endianness
      =.  dat  (run 5 dat |=(a=@ (rev 3 4 a)))
      =*  x  dat
      =+  blocks=(div wid 512)
      =+  fev=~(. fe 5)
      ::  initial register values
      =+  h0=0x6745.2301
      =+  h1=0xefcd.ab89
      =+  h2=0x98ba.dcfe
      =+  h3=0x1032.5476
      =+  h4=0xc3d2.e1f0
      ::  i: current block
      =+  [i=0 j=0]
      =+  *[a=@ b=@ c=@ d=@ e=@]       ::  a..e
      =+  *[aa=@ bb=@ cc=@ dd=@ ee=@]  ::  a'..e'
      |^
        ?:  =(i blocks)
          %+  rep  5
          %+  turn  `(list @)`~[h4 h3 h2 h1 h0]
          ::  endianness
          |=(h=@ (rev 3 4 h))
        =:  a  h0     aa  h0
            b  h1     bb  h1
            c  h2     cc  h2
            d  h3     dd  h3
            e  h4     ee  h4
        ==
        ::  j: current word
        =+  j=0
        |-
        ?:  =(j 80)
          %=  ^$
            i   +(i)
            h1  :(sum:fev h2 d ee)
            h2  :(sum:fev h3 e aa)
            h3  :(sum:fev h4 a bb)
            h4  :(sum:fev h0 b cc)
            h0  :(sum:fev h1 c dd)
          ==
        %=  $
          j  +(j)
        ::
          a   e
          b   (fn j a b c d e (get (r j)) (k j) (s j))
          c   b
          d   (rol 10 c)
          e   d
        ::
          aa  ee
          bb  (fn (sub 79 j) aa bb cc dd ee (get (rr j)) (kk j) (ss j))
          cc  bb
          dd  (rol 10 cc)
          ee  dd
        ==
      ::
      ++  get  ::  word from x in block i
        |=  j=@ud
        =+  (add (mul i 16) +(j))
        (cut 5 [(sub (mul blocks 16) -) 1] x)
      ::
      ++  fn
        |=  [j=@ud a=@ b=@ c=@ d=@ e=@ m=@ k=@ s=@]
        =-  (sum:fev (rol s :(sum:fev a m k -)) e)
        =.  j  (div j 16)
        ?:  =(0 j)  (mix (mix b c) d)
        ?:  =(1 j)  (con (dis b c) (dis (not 0 32 b) d))
        ?:  =(2 j)  (mix (con b (not 0 32 c)) d)
        ?:  =(3 j)  (con (dis b d) (dis c (not 0 32 d)))
        ?:  =(4 j)  (mix b (con c (not 0 32 d)))
        !!
      ::
      ++  rol  (cury rol:fev 0)
      ::
      ++  k
        |=  j=@ud
        =.  j  (div j 16)
        ?:  =(0 j)  0x0
        ?:  =(1 j)  0x5a82.7999
        ?:  =(2 j)  0x6ed9.eba1
        ?:  =(3 j)  0x8f1b.bcdc
        ?:  =(4 j)  0xa953.fd4e
        !!
      ::
      ++  kk  ::  k'
        |=  j=@ud
        =.  j  (div j 16)
        ?:  =(0 j)  0x50a2.8be6
        ?:  =(1 j)  0x5c4d.d124
        ?:  =(2 j)  0x6d70.3ef3
        ?:  =(3 j)  0x7a6d.76e9
        ?:  =(4 j)  0x0
        !!
      ::
      ++  r
        |=  j=@ud
        %+  snag  j
        ^-  (list @)
        :~  0  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15
            7  4  13  1  10  6  15  3  12  0  9  5  2  14  11  8
            3  10  14  4  9  15  8  1  2  7  0  6  13  11  5  12
            1  9  11  10  0  8  12  4  13  3  7  15  14  5  6  2
            4  0  5  9  7  12  2  10  14  1  3  8  11  6  15  13
        ==
      ::
      ++  rr  ::  r'
        |=  j=@ud
        %+  snag  j
        ^-  (list @)
        :~  5  14  7  0  9  2  11  4  13  6  15  8  1  10  3  12
            6  11  3  7  0  13  5  10  14  15  8  12  4  9  1  2
            15  5  1  3  7  14  6  9  11  8  12  2  10  0  4  13
            8  6  4  1  3  11  15  0  5  12  2  13  9  7  10  14
            12  15  10  4  1  5  8  7  6  2  13  14  0  3  9  11
        ==
      ::
      ++  s
        |=  j=@ud
        %+  snag  j
        ^-  (list @)
        :~  11  14  15  12  5  8  7  9  11  13  14  15  6  7  9  8
            7  6  8  13  11  9  7  15  7  12  15  9  11  7  13  12
            11  13  6  7  14  9  13  15  14  8  13  6  5  12  7  5
            11  12  14  15  14  15  9  8  9  14  5  6  8  6  5  12
            9  15  5  11  6  8  13  12  5  12  13  14  11  8  5  6
        ==
      ::
      ++  ss  ::  s'
        |=  j=@ud
        %+  snag  j
        ^-  (list @)
        :~  8  9  9  11  13  15  15  5  7  7  8  11  14  14  12  6
            9  13  15  7  12  8  9  11  7  7  12  7  6  15  13  11
            9  7  15  11  8  6  6  14  12  13  5  14  13  13  7  5
            15  5  8  11  14  14  6  14  6  9  12  9  12  5  15  8
            8  5  12  9  12  5  14  6  8  13  6  5  15  13  11  11
        ==
      --
    ::
    ++  md5-pad
      |=  byts
      ^-  byts
      =+  (sub 511 (mod (add wid 64) 512))
      :-  :(add 64 +(-) wid)
      %+  can  0
      ~[64^(rev 3 8 wid) +(-)^(lsh [0 -] 1) wid^dat]
    --
  ::
  ++  pbkdf
    =>  |%
        ++  meet  |=([p=@ s=@ c=@ d=@] [[(met 3 p) p] [(met 3 s) s] c d])
        ++  flip  |=  [p=byts s=byts c=@ d=@]
                  [wid.p^(rev 3 p) wid.s^(rev 3 s) c d]
        --
    |%
    ::
    ::  use with @
    ::
    ++  hmac-sha1     (cork meet hmac-sha1l)
    ++  hmac-sha256   (cork meet hmac-sha256l)
    ++  hmac-sha512   (cork meet hmac-sha512l)
    ::
    ::  use with @t
    ::
    ++  hmac-sha1t    (cork meet hmac-sha1d)
    ++  hmac-sha256t  (cork meet hmac-sha256d)
    ++  hmac-sha512t  (cork meet hmac-sha512d)
    ::
    ::  use with byts
    ::
    ++  hmac-sha1l    (cork flip hmac-sha1d)
    ++  hmac-sha256l  (cork flip hmac-sha256d)
    ++  hmac-sha512l  (cork flip hmac-sha512d)
    ::
    ::  main logic
    ::
    ++  hmac-sha1d    (cury pbkdf hmac-sha1l:hmac 20)
    ++  hmac-sha256d  (cury pbkdf hmac-sha256l:hmac 32)
    ++  hmac-sha512d  (cury pbkdf hmac-sha512l:hmac 64)
    ::
    ++  pbkdf
      ::TODO  jet me! ++hmac:hmac is an example
      |*  [[prf=$-([byts byts] @) out=@u] p=byts s=byts c=@ d=@]
      =>  .(dat.p (end [3 wid.p] dat.p), dat.s (end [3 wid.s] dat.s))
      ::
      ::  max key length 1GB
      ::  max iterations 2^28
      ::
      ~|  [%invalid-pbkdf-params c d]
      ?>  ?&  (lte d (bex 30))
              (lte c (bex 28))
              !=(c 0)
          ==
      =/  l
        ?~  (mod d out)
          (div d out)
        +((div d out))
      =+  r=(sub d (mul out (dec l)))
      =+  [t=0 j=1 k=1]
      =.  t
        |-  ^-  @
        ?:  (gth j l)  t
        =/  u
          %+  add  dat.s
          %+  lsh  [3 wid.s]
          %+  rep  3
          (flop (rpp:scr 3 4 j))
        =+  f=0
        =.  f
          |-  ^-  @
          ?:  (gth k c)  f
          =/  q
            %^  rev  3  out
            =+  ?:(=(k 1) (add wid.s 4) out)
            (prf [wid.p (rev 3 p)] [- (rev 3 - u)])
          $(u q, f (mix f q), k +(k))
        $(t (add t (lsh [3 (mul (dec j) out)] f)), j +(j))
      (rev 3 d (end [3 d] t))
    --
  --  ::crypto
::                                                      ::::
::::                      ++unity                       ::  (2c) unit promotion
  ::                                                    ::::
++  unity  ^?
  |%
  ::                                                    ::  ++drop-list:unity
  ++  drop-list                                         ::  collapse unit list
    |*  lut=(list (unit))
    ?.  |-  ^-  ?
        ?~(lut & ?~(i.lut | $(lut t.lut)))
      ~
    %-  some
    |-
    ?~  lut  ~
    [i=u:+.i.lut t=$(lut t.lut)]
  ::                                                    ::  ++drop-map:unity
  ++  drop-map                                          ::  collapse unit map
    |*  lum=(map term (unit))
    ?:  (~(rep by lum) |=([[@ a=(unit)] b=_|] |(b ?=(~ a))))
      ~
    (some (~(run by lum) need))
  ::                                                    ::  ++drop-pole:unity
  ++  drop-pole                                         ::  collapse to tuple
    |^  |*  pul=(pole (unit))
        ?:  (test-pole pul)  ~
        (some (need-pole pul))
    ::
    ++  test-pole
      |*  pul=(pole (unit))
      ^-  ?
      ?~  pul  &
      ?|  ?=(~ -.pul)
          ?~(+.pul | (test-pole +.pul))
      ==
    ::
    ++  need-pole
      |*  pul=(pole (unit))
      ?~  pul  !!
      ?~  +.pul
        u:->.pul
      [u:->.pul (need-pole +.pul)]
    --
  --
::                                                      ::::
::::                      ++format                      ::  (2d) common formats
  ::                                                    ::::
++  format  ^?
  |%
  ::  0 ending a line (invalid @t) is not preserved     ::  ++to-wain:format
  ++  to-wain                                           ::  cord to line list
    ~%  %leer  ..part  ~
    |=  txt=cord
    ^-  wain
    ?~  txt  ~
    =/  len=@  (met 3 txt)
    =/  cut  =+(cut -(a 3, c 1, d txt))
    =/  sub  sub
    =|  [i=@ out=wain]
    |-  ^+  out
    =+  |-  ^-  j=@
        ?:  ?|  =(i len)
                =(10 (cut(b i)))
            ==
          i
        $(i +(i))
    =.  out  :_  out
      (cut(b i, c (sub j i)))
    ?:  =(j len)
      (flop out)
    $(i +(j))
  ::                                                    ::  ++of-wain:format
  ++  of-wain                                           ::  line list to cord
    |=  tez=wain  ^-  cord
    (rap 3 (join '\0a' tez))
  ::                                                    ::  ++of-wall:format
  ++  of-wall                                           ::  line list to tape
    |=  a=wall  ^-  tape
    ?~(a ~ "{i.a}\0a{$(a t.a)}")
  ::
  ++  json-rn                                           ::  json to rn parser
    %+  knee  *rn  |.
    ;~  plug
      (easy %d)
      ;~(pose (cold | hep) (easy &))
      ;~  plug  dim:ag
        ;~  pose
          ;~  pfix  dot
            %+  sear
              |=  a=tape
              =/  b  (rust a dum:ag)
              ?~  b  ~
              (some [(lent a) u.b])
            (plus (shim '0' '9'))
          ==
          (easy [0 0])
        ==
        ;~  pose
          ;~  pfix
            (mask "eE")
            ;~  plug
              ;~(pose (cold | hep) (cold & lus) (easy &))
              ;~  pose
                ;~(pfix (plus (just '0')) dim:ag)
                dim:ag
              ==
            ==
          ==
          (easy [& 0])
        ==
      ==
    ==
  ::                                                    ::  ++enjs:format
  ++  enjs  ^?                                          ::  json encoders
    |%
    ::                                                  ::  ++frond:enjs:format
    ++  frond                                           ::  object from k-v pair
      |=  [p=@t q=json]
      ^-  json
      [%o [[p q] ~ ~]]
    ::                                                  ::  ++pairs:enjs:format
    ++  pairs                                           ::  object from k-v list
      |=  a=(list [p=@t q=json])
      ^-  json
      [%o (~(gas by *(map @t json)) a)]
    ::                                                  ::  ++tape:enjs:format
    ++  tape                                            ::  string from tape
      |=  a=^tape
      ^-  json
      [%s (crip a)]
    ::                                                  ::  ++wall:enjs:format
    ++  wall                                            ::  string from wall
      |=  a=^wall
      ^-  json
      (tape (of-wall a))
    ::                                                  ::  ++ship:enjs:format
    ++  ship                                            ::  string from ship
      |=  a=^ship
      ^-  json
      [%n (rap 3 '"' (rsh [3 1] (scot %p a)) '"' ~)]
    ::                                                  ::  ++numb:enjs:format
    ++  numb                                            ::  number from unsigned
      |=  a=@u
      ^-  json
      :-  %n
      ?:  =(0 a)  '0'
      %-  crip
      %-  flop
      |-  ^-  ^tape
      ?:(=(0 a) ~ [(add '0' (mod a 10)) $(a (div a 10))])
    ::                                                  ::  ++sect:enjs:format
    ++  sect                                            ::  s timestamp
      |=  a=^time
      (numb (unt:chrono:userlib a))
    ::                                                  ::  ++time:enjs:format
    ++  time                                            ::  ms timestamp
      |=  a=^time
      (numb (unm:chrono:userlib a))
    ::                                                  ::  ++path:enjs:format
    ++  path                                            ::  string from path
      |=  a=^path
      ^-  json
      [%s (spat a)]
    ::                                                  ::  ++tank:enjs:format
    ++  tank                                            ::  tank as string arr
      |=  a=^tank
      ^-  json
      [%a (turn (wash [0 80] a) tape)]
    --  ::enjs
  ::                                                    ::  ++dejs:format
  ++  dejs                                              ::  json reparser
    =>  |%  ++  grub  *                                 ::  result
            ++  fist  $-(json grub)                     ::  reparser instance
        --  ::
    |%
    ::                                                  ::  ++ar:dejs:format
    ++  ar                                              ::  array as list
      |*  wit=fist
      |=  jon=json  ^-  (list _(wit *json))
      ?>  ?=([%a *] jon)
      (turn p.jon wit)
    ::                                                  ::  ++as:dejs:format
    ++  as                                              ::  array as set
      |*  a=fist
      (cu ~(gas in *(set _$:a)) (ar a))
    ::                                                  ::  ++at:dejs:format
    ++  at                                              ::  array as tuple
      |*  wil=(pole fist)
      |=  jon=json
      ?>  ?=([%a *] jon)
      ((at-raw wil) p.jon)
    ::                                                  ::  ++at-raw:dejs:format
    ++  at-raw                                          ::  array as tuple
      |*  wil=(pole fist)
      |=  jol=(list json)
      ?~  jol  !!
      ?-    wil                                         :: mint-vain on empty
          :: [wit=* t=*]
          [* t=*]
        =>  .(wil [wit *]=wil)
        ?~  t.wil  ?^(t.jol !! (wit.wil i.jol))
        [(wit.wil i.jol) ((at-raw t.wil) t.jol)]
      ==
    ::                                                  ::  ++bo:dejs:format
    ++  bo                                              ::  boolean
      |=(jon=json ?>(?=([%b *] jon) p.jon))
    ::                                                  ::  ++bu:dejs:format
    ++  bu                                              ::  boolean not
      |=(jon=json ?>(?=([%b *] jon) !p.jon))
    ::                                                  ::  ++ci:dejs:format
    ++  ci                                              ::  maybe transform
      |*  [poq=gate wit=fist]
      |=  jon=json
      (need (poq (wit jon)))
    ::                                                  ::  ++cu:dejs:format
    ++  cu                                              ::  transform
      |*  [poq=gate wit=fist]
      |=  jon=json
      (poq (wit jon))
    ::                                                  ::  ++di:dejs:format
    ++  di                                              ::  millisecond date
      (cu from-unix-ms:chrono:userlib ni)
    ::                                                  ::  ++du:dejs:format
    ++  du                                              ::  second date
      (cu from-unix:chrono:userlib ni)
    ::                                                  ::  ++mu:dejs:format
    ++  mu                                              ::  true unit
      |*  wit=fist
      |=  jon=json
      ?~(jon ~ (some (wit jon)))
    ::                                                  ::  ++ne:dejs:format
    ++  ne                                              ::  number as real
      |=  jon=json
      ^-  @rd
      ?>  ?=([%n *] jon)
      (rash p.jon (cook ryld (cook royl-cell:^so json-rn)))
    ::                                                  ::  ++ni:dejs:format
    ++  ni                                              ::  number as integer
      |=  jon=json
      ?>  ?=([%n *] jon)
      (rash p.jon dem)
    ::                                                  ::  ++ns:dejs:format
    ++  ns                                              ::  number as signed
      |=  jon=json
      ^-  @s
      ?>  ?=([%n *] jon)
      %+  rash  p.jon
      %+  cook  new:si
      ;~(plug ;~(pose (cold %| (jest '-')) (easy %&)) dem)
    ::                                                  ::  ++no:dejs:format
    ++  no                                              ::  number as cord
      |=(jon=json ?>(?=([%n *] jon) p.jon))
    ::                                                  ::  ++nu:dejs:format
    ++  nu                                              ::  parse number as hex
      |=  jon=json
      ?>  ?=([%s *] jon)
      (rash p.jon hex)
    ::                                                  ::  ++of:dejs:format
    ++  of                                              ::  object as frond
      |*  wer=(pole [cord fist])
      |=  jon=json
      ?>  ?=([%o [@ *] ~ ~] jon)
      |-
      ?-    wer                                         :: mint-vain on empty
          :: [[key=@t wit=*] t=*]
          [[key=@t *] t=*]
        =>  .(wer [[* wit] *]=wer)
        ?:  =(key.wer p.n.p.jon)
          [key.wer ~|(key+key.wer (wit.wer q.n.p.jon))]
        ?~  t.wer  ~|(bad-key+p.n.p.jon !!)
        ((of t.wer) jon)
      ==
    ::                                                  ::  ++ot:dejs:format
    ++  ot                                              ::  object as tuple
      |*  wer=(pole [cord fist])
      |=  jon=json
      ?>  ?=([%o *] jon)
      ((ot-raw wer) p.jon)
    ::                                                  ::  ++ot-raw:dejs:format
    ++  ot-raw                                          ::  object as tuple
      |*  wer=(pole [cord fist])
      |=  jom=(map @t json)
      ?-    wer                                         :: mint-vain on empty
          :: [[key=@t wit=*] t=*]
          [[key=@t *] t=*]
        =>  .(wer [[* wit] *]=wer)
        =/  ten  ~|(key+key.wer (wit.wer (~(got by jom) key.wer)))
        ?~(t.wer ten [ten ((ot-raw t.wer) jom)])
      ==
    ::
    ++  ou                                              ::  object of units
      |*  wer=(pole [cord fist])
      |=  jon=json
      ?>  ?=([%o *] jon)
      ((ou-raw wer) p.jon)
    ::                                                  ::  ++ou-raw:dejs:format
    ++  ou-raw                                          ::  object of units
      |*  wer=(pole [cord fist])
      |=  jom=(map @t json)
      ?-    wer                                         :: mint-vain on empty
          :: [[key=@t wit=*] t=*]
          [[key=@t *] t=*]
        =>  .(wer [[* wit] *]=wer)
        =/  ten  ~|(key+key.wer (wit.wer (~(get by jom) key.wer)))
        ?~(t.wer ten [ten ((ou-raw t.wer) jom)])
      ==
    ::                                                  ::  ++oj:dejs:format
    ++  oj                                              ::  object as jug
      |*  =fist
      ^-  $-(json (jug cord _(fist *json)))
      (om (as fist))
    ::                                                  ::  ++om:dejs:format
    ++  om                                              ::  object as map
      |*  wit=fist
      |=  jon=json
      ?>  ?=([%o *] jon)
      (~(run by p.jon) wit)
    ::                                                  ::  ++op:dejs:format
    ++  op                                              ::  parse keys of map
      |*  [fel=rule wit=fist]
      |=  jon=json  ^-  (map _(wonk *fel) _*wit)
      =/  jom  ((om wit) jon)
      %-  malt
      %+  turn  ~(tap by jom)
      |*  [a=cord b=*]
      =>  .(+< [a b]=+<)
      [(rash a fel) b]
    ::                                                  ::  ++pa:dejs:format
    ++  pa                                              ::  string as path
      (su stap)
    ::                                                  ::  ++pe:dejs:format
    ++  pe                                              ::  prefix
      |*  [pre=* wit=fist]
      (cu |*(* [pre +<]) wit)
    ::                                                  ::  ++sa:dejs:format
    ++  sa                                              ::  string as tape
      |=(jon=json ?>(?=([%s *] jon) (trip p.jon)))
    ::                                                  ::  ++sd:dejs:format
    ++  sd                                              ::  string @ud as date
      |=  jon=json
      ^-  @da
      ?>  ?=(%s -.jon)
      `@da`(rash p.jon dem:ag)
    ::                                                  ::  ++se:dejs:format
    ++  se                                              ::  string as aura
      |=  aur=@tas
      |=  jon=json
      ?>(?=([%s *] jon) (slav aur p.jon))
    ::                                                  ::  ++so:dejs:format
    ++  so                                              ::  string as cord
      |=(jon=json ?>(?=([%s *] jon) p.jon))
    ::                                                  ::  ++su:dejs:format
    ++  su                                              ::  parse string
      |*  sab=rule
      |=  jon=json  ^+  (wonk *sab)
      ?>  ?=([%s *] jon)
      (rash p.jon sab)
    ::                                                  ::  ++uf:dejs:format
    ++  uf                                              ::  unit fall
      |*  [def=* wit=fist]
      |=  jon=(unit json)
      ?~(jon def (wit u.jon))
    ::                                                  ::  ++un:dejs:format
    ++  un                                              ::  unit need
      |*  wit=fist
      |=  jon=(unit json)
      (wit (need jon))
    ::                                                  ::  ++ul:dejs:format
    ++  ul                                              ::  null
      |=(jon=json ?~(jon ~ !!))
    ::
    ++  za                                              ::  full unit pole
      |*  pod=(pole (unit))
      ?~  pod  &
      ?~  -.pod  |
      (za +.pod)
    ::
    ++  zl                                              ::  collapse unit list
      |*  lut=(list (unit))
      ?.  |-  ^-  ?
          ?~(lut & ?~(i.lut | $(lut t.lut)))
        ~
      %-  some
      |-
      ?~  lut  ~
      [i=u:+.i.lut t=$(lut t.lut)]
    ::
    ++  zp                                              ::  unit tuple
      |*  but=(pole (unit))
      ?~  but  !!
      ?~  +.but
        u:->.but
      [u:->.but (zp +.but)]
    ::
    ++  zm                                              ::  collapse unit map
      |*  lum=(map term (unit))
      ?:  (~(rep by lum) |=([[@ a=(unit)] b=_|] |(b ?=(~ a))))
        ~
      (some (~(run by lum) need))
    --  ::dejs
  ::                                                    ::  ++dejs-soft:format
  ++  dejs-soft                                         ::  json reparse to unit
    =,  unity
    =>  |%  ++  grub  (unit *)                          ::  result
            ++  fist  $-(json grub)                     ::  reparser instance
        --  ::
    ::
    ::  XX: this is old code that replaced a rewritten dejs.
    ::      the rewritten dejs rest-looped with ++redo.  the old
    ::      code is still in revision control -- revise and replace.
    ::
    |%
    ++  ar                                              ::  array as list
      |*  wit=fist
      |=  jon=json
      ?.  ?=([%a *] jon)  ~
      %-  zl
      |-
      ?~  p.jon  ~
      [i=(wit i.p.jon) t=$(p.jon t.p.jon)]
    ::
    ++  at                                              ::  array as tuple
      |*  wil=(pole fist)
      |=  jon=json
      ?.  ?=([%a *] jon)  ~
      ?.  =((lent wil) (lent p.jon))  ~
      =+  raw=((at-raw wil) p.jon)
      ?.((za raw) ~ (some (zp raw)))
    ::
    ++  at-raw                                          ::  array as tuple
      |*  wil=(pole fist)
      |=  jol=(list json)
      ?~  wil  ~
      :-  ?~(jol ~ (-.wil i.jol))
      ((at-raw +.wil) ?~(jol ~ t.jol))
    ::
    ++  bo                                              ::  boolean
      |=(jon=json ?.(?=([%b *] jon) ~ [~ u=p.jon]))
    ::
    ++  bu                                              ::  boolean not
      |=(jon=json ?.(?=([%b *] jon) ~ [~ u=!p.jon]))
    ::
    ++  ci                                              ::  maybe transform
      |*  [poq=gate wit=fist]
      |=  jon=json
      (biff (wit jon) poq)
    ::
    ++  cu                                              ::  transform
      |*  [poq=gate wit=fist]
      |=  jon=json
      (bind (wit jon) poq)
    ::
    ++  da                                              ::  UTC date
      |=  jon=json
      ?.  ?=([%s *] jon)  ~
      (bind (stud:chrono:userlib p.jon) |=(a=date (year a)))
    ::
    ++  dank                                            ::  tank
      ^-  $-(json (unit tank))
      %+  re  *tank  |.  ~+
      %-  of  :~
        leaf+sa
        palm+(ot style+(ot mid+sa cap+sa open+sa close+sa ~) lines+(ar dank) ~)
        rose+(ot style+(ot mid+sa open+sa close+sa ~) lines+(ar dank) ~)
      ==
    ::
    ++  di                                              ::  millisecond date
      (cu from-unix-ms:chrono:userlib ni)
    ::
    ++  mu                                              ::  true unit
      |*  wit=fist
      |=  jon=json
      ?~(jon (some ~) (bind (wit jon) some))
    ::
    ++  ne                                              ::  number as real
      |=  jon=json
      ^-  (unit @rd)
      ?.  ?=([%n *] jon)  ~
      (rush p.jon (cook ryld (cook royl-cell:^so json-rn)))
    ::
    ++  ni                                              ::  number as integer
      |=  jon=json
      ?.  ?=([%n *] jon)  ~
      (rush p.jon dem)
    ::
    ++  no                                              ::  number as cord
      |=  jon=json
      ?.  ?=([%n *] jon)  ~
      (some p.jon)
    ::
    ++  of                                              ::  object as frond
      |*  wer=(pole [cord fist])
      |=  jon=json
      ?.  ?=([%o [@ *] ~ ~] jon)  ~
      |-
      ?~  wer  ~
      ?:  =(-.-.wer p.n.p.jon)
        ((pe -.-.wer +.-.wer) q.n.p.jon)
      ((of +.wer) jon)
    ::
    ++  ot                                              ::  object as tuple
      |*  wer=(pole [cord fist])
      |=  jon=json
      ?.  ?=([%o *] jon)  ~
      =+  raw=((ot-raw wer) p.jon)
      ?.((za raw) ~ (some (zp raw)))
    ::
    ++  ot-raw                                          ::  object as tuple
      |*  wer=(pole [cord fist])
      |=  jom=(map @t json)
      ?~  wer  ~
      =+  ten=(~(get by jom) -.-.wer)
      [?~(ten ~ (+.-.wer u.ten)) ((ot-raw +.wer) jom)]
    ::
    ++  om                                              ::  object as map
      |*  wit=fist
      |=  jon=json
      ?.  ?=([%o *] jon)  ~
      (zm (~(run by p.jon) wit))
    ::
    ++  op                                              ::  parse keys of map
      |*  [fel=rule wit=fist]
      %+  cu
        |=  a=(list (pair _(wonk *fel) _(need *wit)))
        (my:nl a)
      %-  ci  :_  (om wit)
      |=  a=(map cord _(need *wit))
      ^-  (unit (list _[(wonk *fel) (need *wit)]))
      %-  zl
      %+  turn  ~(tap by a)
      |=  [a=cord b=_(need *wit)]
      =+  nit=(rush a fel)
      ?~  nit  ~
      (some [u.nit b])
    ::
    ++  pe                                              ::  prefix
      |*  [pre=* wit=fist]
      (cu |*(* [pre +<]) wit)
    ::
    ++  re                                              ::  recursive reparsers
      |*  [gar=* sef=_|.(fist)]
      |=  jon=json
      ^-  (unit _gar)
      ((sef) jon)
    ::
    ++  sa                                              ::  string as tape
      |=  jon=json
      ?.(?=([%s *] jon) ~ (some (trip p.jon)))
    ::
    ++  so                                              ::  string as cord
      |=  jon=json
      ?.(?=([%s *] jon) ~ (some p.jon))
    ::
    ++  su                                              ::  parse string
      |*  sab=rule
      |=  jon=json
      ?.  ?=([%s *] jon)  ~
      (rush p.jon sab)
    ::
    ++  ul  |=(jon=json ?~(jon (some ~) ~))             ::  null
    ++  za                                              ::  full unit pole
      |*  pod=(pole (unit))
      ?~  pod  &
      ?~  -.pod  |
      (za +.pod)
    ::
    ++  zl                                              ::  collapse unit list
      |*  lut=(list (unit))
      ?.  |-  ^-  ?
          ?~(lut & ?~(i.lut | $(lut t.lut)))
        ~
      %-  some
      |-
      ?~  lut  ~
      [i=u:+.i.lut t=$(lut t.lut)]
    ::
    ++  zp                                              ::  unit tuple
      |*  but=(pole (unit))
      ?~  but  !!
      ?~  +.but
        u:->.but
      [u:->.but (zp +.but)]
    ::
    ++  zm                                              ::  collapse unit map
      |*  lum=(map term (unit))
      ?:  (~(rep by lum) |=([[@ a=(unit)] b=_|] |(b ?=(~ a))))
        ~
      (some (~(run by lum) need))
    --  ::dejs-soft
  ::
  ++  klr                                               ::  styx/stub engine
    =,  dill
    |%
    ++  make                                            ::  stub from styx
      |=  a=styx  ^-  stub
      =|  b=stye
      %+  reel
        |-  ^-  stub
        %-  zing  %+  turn  a
        |=  a=$@(@t (pair styl styx))
        ?@  a  [b (tuba (trip a))]~
        ^$(a q.a, b (styd p.a b))
      ::
      |=  [a=(pair stye (list @c)) b=stub]
      ?~  b  [a ~]
      ?.  =(p.a p.i.b)  [a b]
      [[p.a (weld q.a q.i.b)] t.b]
    ::
    ++  styd                                            ::  stye from styl
      |=  [a=styl b=stye]  ^+  b                        ::  with inheritance
      :+  ?~  p.a  p.b
          ?~  u.p.a  ~
          (~(put in p.b) u.p.a)
        (fall p.q.a p.q.b)
      (fall q.q.a q.q.b)
    ::
    ++  lent-char
      |=  a=stub  ^-  @
      (roll (lnts-char a) add)
    ::
    ++  lnts-char                                       ::  stub text lengths
      |=  a=stub  ^-  (list @)
      %+  turn  a
      |=  a=(pair stye (list @c))
      (lent q.a)
    ::
    ++  brek                                            ::  index + incl-len of
      |=  [a=@ b=(list @)]                              ::  stub pair w/ idx a
      =|  [c=@ i=@]
      |-  ^-  (unit (pair @ @))
      ?~  b  ~
      =.  c  (add c i.b)
      ?:  (gte c a)
        `[i c]
      $(i +(i), b t.b)
    ::
    ++  pact                                            ::  condense stub
      |=  a=stub
      ^-  stub
      ?~  a  ~
      ?~  t.a  a
      ?.  =(p.i.a p.i.t.a)  [i.a $(a t.a)]
      =.  q.i.t.a  (weld q.i.a q.i.t.a)
      $(a t.a)
    ::
    ++  slag                                            ::  slag stub
      |=  [a=@ b=stub]
      ^-  stub
      ?:  =(0 a)  b
      ?~  b  ~
      =+  c=(lent q.i.b)
      ?:  =(c a)  t.b
      ?:  (gth c a)
        [[p.i.b (^slag a q.i.b)] t.b]
      $(a (sub a c), b t.b)
    ::
    ++  scag                                            ::  scag stub
      |=  [a=@ b=stub]
      ^-  stub
      ?:  =(0 a)  ~
      ?~  b  ~
      =+  c=(lent q.i.b)
      ?:  (gth c a)
        [p.i.b (^scag a q.i.b)]~
      :-  i.b
      $(a (sub a c), b t.b)
    ::
    ++  swag                                            ::  swag stub
      |=  [[a=@ b=@] c=stub]
      (scag b (slag a c))
    ::
    ++  wail                                            ::  overlay stub
      |=  [a=stub b=@ c=stub d=@c]
      ^-  stub
      ;:  weld
        (scag b a)
      ::
        =+  e=(lent-char a)
        ?:  (lte b e)  ~
        [*stye (reap (sub b e) d)]~
      ::
        c
        (slag (add b (lent-char c)) a)
      ==
    --  ::  klr
  --
::  |cloy: clay helpers
::
++  cloy
  =,  clay
  |%
  ++  new-desk
    |=  [=desk tako=(unit tako) files=(map path page)]
    [%c %park desk &/[(drop tako) (~(run by files) (lead %&))] *rang]
  --
::                                                      ::
::::                      ++differ                      ::  (2d) hunt-mcilroy
  ::                                                    ::::
++  differ  ^?
  =,  clay
  =,  format
  |%
  ::                                                    ::  ++berk:differ
  ++  berk                                              ::  invert diff patch
    |*  bur=(urge)
    |-  ^+  bur
    ?~  bur  ~
    :_  $(bur t.bur)
    ?-  -.i.bur
      %&  i.bur
      %|  [%| q.i.bur p.i.bur]
    ==
  ::                                                    ::  ++loss:differ
  ++  loss                                              ::  longest subsequence
    ~%  %loss  ..part  ~
    |*  [hel=(list) hev=(list)]
    |-  ^+  hev
    =+  ^=  sev
        =+  [inx=0 sev=*(map _i.-.hev (list @ud))]
        |-  ^+  sev
        ?~  hev  sev
        =+  guy=(~(get by sev) i.hev)
        %=  $
          hev  t.hev
          inx  +(inx)
          sev  (~(put by sev) i.hev [inx ?~(guy ~ u.guy)])
        ==
    =|  gox=[p=@ud q=(map @ud [p=@ud q=_hev])]
    =<  abet
    =<  main
    |%
    ::                                                  ::  ++abet:loss:differ
    ++  abet                                            ::  subsequence
      ^+  hev
      ?:  =(0 p.gox)  ~
      (flop q:(need (~(get by q.gox) (dec p.gox))))
    ::                                                  ::  ++hink:loss:differ
    ++  hink                                            ::  extend fits top
      |=  [inx=@ud goy=@ud]  ^-  ?
      ?|  =(p.gox inx)
          (lth goy p:(need (~(get by q.gox) inx)))
      ==
    ::                                                  ::  ++lonk:loss:differ
    ++  lonk                                            ::  extend fits bottom
      |=  [inx=@ud goy=@ud]  ^-  ?
      ?|  =(0 inx)
          (gth goy p:(need (~(get by q.gox) (dec inx))))
      ==
    ::                                                  ::  ++luna:loss:differ
    ++  luna                                            ::  extend
      |=  [inx=@ud goy=@ud]
      ^+  +>
      %_    +>.$
          gox
        :-  ?:(=(inx p.gox) +(p.gox) p.gox)
        %+  ~(put by q.gox)  inx
        :+  goy
          (snag goy hev)
        ?:(=(0 inx) ~ q:(need (~(get by q.gox) (dec inx))))
      ==
    ::                                                  ::  ++merg:loss:differ
    ++  merg                                            ::  merge all matches
      |=  gay=(list @ud)
      ^+  +>
      =+  ^=  zes
          =+  [inx=0 zes=*(list [p=@ud q=@ud])]
          |-  ^+  zes
          ?:  |(?=(~ gay) (gth inx p.gox))  zes
          ?.  (lonk inx i.gay)  $(gay t.gay)
          ?.  (hink inx i.gay)  $(inx +(inx))
          $(inx +(inx), gay t.gay, zes [[inx i.gay] zes])
      |-  ^+  +>.^$
      ?~(zes +>.^$ $(zes t.zes, +>.^$ (luna i.zes)))
    ::                                                  ::  ++main:loss:differ
    ++  main                                            ::
      =+  hol=hel
      |-  ^+  +>
      ?~  hol  +>
      =+  guy=(~(get by sev) i.hol)
      $(hol t.hol, +> (merg (flop `(list @ud)`?~(guy ~ u.guy))))
    --  ::
  ::                                                    ::  ++lurk:differ
  ++  lurk                                              ::  apply list patch
    |*  [hel=(list) rug=(urge)]
    ^+  hel
    =+  war=`_hel`~
    |-  ^+  hel
    ?~  rug  (flop war)
    ?-    -.i.rug
        %&
      %=   $
        rug  t.rug
        hel  (slag p.i.rug hel)
        war  (weld (flop (scag p.i.rug hel)) war)
      ==
    ::
        %|
      %=  $
        rug  t.rug
        hel  =+  gur=(flop p.i.rug)
             |-  ^+  hel
             ?~  gur  hel
             ?>(&(?=(^ hel) =(i.gur i.hel)) $(hel t.hel, gur t.gur))
        war  (weld q.i.rug war)
      ==
    ==
  ::                                                    ::  ++lusk:differ
  ++  lusk                                              ::  lcs to list patch
    |*  [hel=(list) hev=(list) lcs=(list)]
    =+  ^=  rag
        ^-  [$%([%& p=@ud] [%| p=_lcs q=_lcs])]
        [%& 0]
    =>  .(rag [p=rag q=*(list _rag)])
    =<  abet  =<  main
    |%
    ::                                                  ::  ++abet:lusk:differ
    ++  abet                                            ::
      =?  q.rag  !=([& 0] p.rag)  [p.rag q.rag]
      (flop q.rag)
    ::                                                  ::  ++done:lusk:differ
    ++  done                                            ::
      |=  new=_p.rag
      ^+  rag
      ?-  -.p.rag
        %|   ?-  -.new
              %|  [[%| (weld p.new p.p.rag) (weld q.new q.p.rag)] q.rag]
              %&  [new [p.rag q.rag]]
            ==
        %&   ?-  -.new
              %|  [new ?:(=(0 p.p.rag) q.rag [p.rag q.rag])]
              %&  [[%& (add p.p.rag p.new)] q.rag]
            ==
      ==
    ::                                                  ::  ++main:lusk:differ
    ++  main                                            ::
      |-  ^+  +
      ?~  hel
        ?~  hev
          ?>(?=(~ lcs) +)
        $(hev t.hev, rag (done %| ~ [i.hev ~]))
      ?~  hev
        $(hel t.hel, rag (done %| [i.hel ~] ~))
      ?~  lcs
        +(rag (done %| (flop hel) (flop hev)))
      ?:  =(i.hel i.lcs)
        ?:  =(i.hev i.lcs)
          $(lcs t.lcs, hel t.hel, hev t.hev, rag (done %& 1))
        $(hev t.hev, rag (done %| ~ [i.hev ~]))
      ?:  =(i.hev i.lcs)
        $(hel t.hel, rag (done %| [i.hel ~] ~))
      $(hel t.hel, hev t.hev, rag (done %| [i.hel ~] [i.hev ~]))
    --  ::
  --  ::differ
::                                                      ::
::::                      ++html                        ::  (2e) text encodings
  ::                                                    ::::
++  html  ^?  ::  XX rename to web-txt
  =,  eyre
  |%
  ::                                                    ::
  ::::                    ++mimes:html                  ::  (2e1) MIME
    ::                                                  ::::
  ++  mimes  ^?
    ~%  %mimes  ..part  ~
    |%
    ::                                                  ::  ++as-octs:mimes:html
    ++  as-octs                                         ::  atom to octstream
      |=  tam=@  ^-  octs
      [(met 3 tam) tam]
    ::                                                  ::  ++as-octt:mimes:html
    ++  as-octt                                         ::  tape to octstream
      |=  tep=tape  ^-  octs
      (as-octs (rap 3 tep))
    ::                                                  ::  ++en-mite:mimes:html
    ++  en-mite                                         ::  mime type to text
      |=  myn=mite
      %-  crip
      |-  ^-  tape
      ?~  myn  ~
      ?:  =(~ t.myn)  (trip i.myn)
      (weld (trip i.myn) `tape`['/' $(myn t.myn)])
    ::
    ::  |base16: en/decode arbitrary MSB-first hex strings
    ::
    ++  base16
      ~%  %base16  +  ~
      |%
      ++  en
        ~/  %en
        |=  a=octs  ^-  cord
        (crip ((x-co:co (mul p.a 2)) (end [3 p.a] q.a)))
      ::
      ++  de
        ~/  %de
        |=  a=cord  ^-  (unit octs)
        (rush a rule)
      ::
      ++  rule
        %+  cook
          |=  a=(list @)  ^-  octs
          [(add (dvr (lent a) 2)) (rep [0 4] (flop a))]
        (star hit)
      --
    ::  |base64: flexible base64 encoding for little-endian atoms
    ::
    ++  base64
      =>  |%
          +$  byte    @D
          +$  word24  @
          ::
          ++  div-ceil
            ::  divide, rounding up.
            |=  [x=@ y=@]  ^-  @
            ?:  =(0 (mod x y))
              (div x y)
            +((div x y))
          ::
          ++  explode-bytes
            ::  Explode a bytestring into list of bytes. Result is in LSB order.
            |=  =octs  ^-  (list byte)
            =/  atom-byte-width  (met 3 q.octs)
            =/  leading-zeros    (sub p.octs atom-byte-width)
            (weld (reap leading-zeros 0) (rip 3 q.octs))
          ::
          ++  explode-words
            ::  Explode a bytestring to words of bit-width `wid`. Result is in LSW order.
            |=  [wid=@ =octs]
            ^-  (list @)
            =/  atom-bit-width   (met 0 q.octs)
            =/  octs-bit-width   (mul 8 p.octs)
            =/  atom-word-width  (div-ceil atom-bit-width wid)
            =/  rslt-word-width  (div-ceil octs-bit-width wid)
            =/  pad              (sub rslt-word-width atom-word-width)
            =/  x  (rip [0 wid] q.octs)
            %+  weld  x
            (reap pad 0)
          --
      ::
      ::  pad: include padding when encoding, require when decoding
      ::  url: use url-safe characters '-' for '+' and '_' for '/'
      ::
      =+  [pad=& url=|]
      |%
      ::  +en:base64: encode +octs to base64 cord
      ::
      ::  Encode an `octs` into a base64 string.
      ::
      ::  First, we break up the input into a list of 24-bit words. The input
      ::  might not be a multiple of 24-bits, so we add 0-2 padding bytes at
      ::  the end (to the least-significant side, with a left-shift).
      ::
      ::  Then, we encode each block into four base64 characters.
      ::
      ::  Finally we remove the padding that we added at the beginning: for
      ::  each byte that was added, we replace one character with an = (unless
      ::  `pad` is false, in which case we just remove the extra characters).
      ::
      ++  en
        ^-  $-(octs cord)
        ::
        =/  cha
          ?:  url
            'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
          'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
        ::
        |^  |=  bs=octs  ^-  cord
            =/  [padding=@ blocks=(list word24)]
              (octs-to-blocks bs)
            (crip (flop (unpad padding (encode-blocks blocks))))
        ::
        ++  octs-to-blocks
          |=  bs=octs  ^-  [padding=@ud (list word24)]
          =/  padding=@ud  (~(dif fo 3) 0 p.bs)
          =/  padded=octs  [(add padding p.bs) (lsh [3 padding] (rev 3 bs))]
          [padding (explode-words 24 padded)]
        ::
        ++  unpad
          |=  [extra=@ t=tape]  ^-  tape
          =/  without  (slag extra t)
          ?.  pad  without
          (weld (reap extra '=') without)
        ::
        ++  encode-blocks
          |=  ws=(list word24)  ^-  tape
          (zing (turn ws encode-block))
        ::
        ++  encode-block
          |=  w=word24  ^-  tape
          =/  a  (cut 3 [(cut 0 [0 6] w) 1] cha)
          =/  b  (cut 3 [(cut 0 [6 6] w) 1] cha)
          =/  c  (cut 3 [(cut 0 [12 6] w) 1] cha)
          =/  d  (cut 3 [(cut 0 [18 6] w) 1] cha)
          ~[a b c d]
        --
      ::
      ::  +de:base64: decode base64 cord to (unit @)
      ::
      ++  de
        |=  a=cord
        ^-  (unit octs)
        (rush a parse)
      ::  +parse:base64: parse base64 cord to +octs
      ::
      ++  parse
        =<  ^-  $-(nail (like octs))
            %+  sear  reduce
            ;~  plug
              %-  plus  ;~  pose
                (cook |=(a=@ (sub a 'A')) (shim 'A' 'Z'))
                (cook |=(a=@ (sub a 'G')) (shim 'a' 'z'))
                (cook |=(a=@ (add a 4)) (shim '0' '9'))
                (cold 62 (just ?:(url '-' '+')))
                (cold 63 (just ?:(url '_' '/')))
              ==
              (stun 0^2 (cold %0 tis))
            ==
        |%
        ::  +reduce:parse:base64: reduce, measure, and swap base64 digits
        ::
        ++  reduce
          |=  [dat=(list @) dap=(list @)]
          ^-  (unit octs)
          =/  lat  (lent dat)
          =/  lap  (lent dap)
          =/  dif  (~(dif fo 4) 0 lat)
          ?:  &(pad !=(dif lap))
            ::  padding required and incorrect
            ~&(%base-64-padding-err-one ~)
          ?:  &(!pad !=(0 lap))
            ::  padding not required but present
            ~&(%base-64-padding-err-two ~)
          =/  len  (sub (mul 3 (div (add lat dif) 4)) dif)
          :+  ~  len
          =/  res  (rsh [1 dif] (rep [0 6] (flop dat)))
          =/  amt  (met 3 res)
          ::  left shift trailing zeroes in after byte swap
          =/  trl  ?:  (lth len amt)  0  (sub len amt)
          (lsh [3 trl] (swp 3 res))
        --
      --
    ::
    ++  en-base58
      |=  dat=@
      =/  cha
        '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
      %-  flop
      |-  ^-  tape
      ?:  =(0 dat)  ~
      :-  (cut 3 [(mod dat 58) 1] cha)
      $(dat (div dat 58))
    ::
    ++  de-base58
      |=  t=tape
      =-  (scan t (bass 58 (plus -)))
      ;~  pose
        (cook |=(a=@ (sub a 56)) (shim 'A' 'H'))
        (cook |=(a=@ (sub a 57)) (shim 'J' 'N'))
        (cook |=(a=@ (sub a 58)) (shim 'P' 'Z'))
        (cook |=(a=@ (sub a 64)) (shim 'a' 'k'))
        (cook |=(a=@ (sub a 65)) (shim 'm' 'z'))
        (cook |=(a=@ (sub a 49)) (shim '1' '9'))
      ==
    --  ::mimes
  ::                                                    ::
  ::::                    ++json:html                   ::  (2e2) JSON
    ::                                                  ::::
  ++  json  ^?
    ~%  %json  ..part  ~
    |%
    ::                                                  ::  ++en:json:html
    ++  en                                              ::  encode JSON to cord
      ~%  %en  +>+  ~
      |^  |=  jon=^json
          ^-  cord
          (rap 3 (flop (onto jon ~)))
      ::                                                ::  ++onto:en:json:html
      ++  onto
        |=  [val=^json out=(list @t)]
        ^+  out
        ?~  val  ['null' out]
        ?-    -.val
            %a
          ?~  p.val  ['[]' out]
          =.  out    ['[' out]
          !.
          |-  ^+  out
          =.  out  ^$(val i.p.val)
          ?~(t.p.val [']' out] $(p.val t.p.val, out [',' out]))
        ::
            %b
          [?:(p.val 'true' 'false') out]
        ::
            %n
          [p.val out]
        ::
            %s
          [(scap p.val) out]
        ::
            %o
          =/  viz  ~(tap by p.val)
          ?~  viz  ['{}' out]
          =.  out  ['{' out]
          !.
          |-  ^+  out
          =.  out  ^$(val q.i.viz, out [':' [(scap p.i.viz) out]])
          ?~(t.viz ['}' out] $(viz t.viz, out [',' out]))
        ==
      ::                                                ::  ++scap:en:json:html
      ++  scap
        |=  val=@t
        ^-  @t
        =/  out=(list @t)  ['"' ~]
        =/  len  (met 3 val)
        =|  [i=@ud pos=@ud]
        |-  ^-  @t
        ?:  =(len i)
          (rap 3 (flop ['"' (rsh [3 pos] val) out]))
        =/  car  (cut 3 [i 1] val)
        ?:  ?&  (gth car 0x1f)
                !=(car 0x22)
                !=(car 0x5C)
                !=(car 0x7F)
            ==
          $(i +(i))
        =/  cap
          ?+  car  (crip '\\' 'u' ((x-co 4):co car))
            %10    '\\n'
            %'"'   '\\"'
            %'\\'  '\\\\'
          ==
        $(i +(i), pos +(i), out [cap (cut 3 [pos (sub i pos)] val) out])
      --  ::en
    ::                                                  ::  ++de:json:html
    ++  de                                              ::  parse cord to JSON
      ~%  %de  +>+  ~
      |^  |=  txt=cord
          ^-  (unit ^json)
          (rush txt apex)
      ::                                                ::  ++abox:de:json:html
      ++  abox                                          ::  array
        %+  stag  %a
        (ifix [sel (wish ser)] (more (wish com) apex))
      ::                                                ::  ++apex:de:json:html
      ++  apex                                          ::  any value
        %+  knee  *^json  |.  ~+
        %+  ifix  [spac spac]
        ;~  pose
          (cold ~ (jest 'null'))
          (stag %b bool)
          (stag %s stri)
          (cook |=(s=tape [%n p=(rap 3 s)]) numb)
          abox
          obox
        ==
      ::                                                ::  ++bool:de:json:html
      ++  bool                                          ::  boolean
        ;~  pose
          (cold & (jest 'true'))
          (cold | (jest 'false'))
        ==
      ::                                                ::  ++esca:de:json:html
      ++  esca                                          ::  escaped character
        ;~  pfix  bas
          =*  loo
            =*  lip
              ^-  (list (pair @t @))
              [b+8 t+9 n+10 f+12 r+13 ~]
            =*  wow
              ^~
              ^-  (map @t @)
              (malt lip)
            (sear ~(get by wow) low)
          ;~(pose doq fas bas loo unic)
        ==
      ::                                                ::  ++expo:de:json:html
      ++  expo                                          ::  exponent
        ;~  (comp weld)
          (piec (mask "eE"))
          (mayb (piec (mask "+-")))
          (plus nud)
        ==
      ::                                                ::  ++frac:de:json:html
      ++  frac                                          ::  fraction
        ;~(plug dot (plus nud))
      ::                                                ::  ++jcha:de:json:html
      ++  jcha                                          ::  string character
        ;~(pose ;~(less doq bas (shim 32 255)) esca)
      ::                                                ::  ++mayb:de:json:html
      ++  mayb                                          ::  optional
        |*(bus=rule ;~(pose bus (easy ~)))
      ::                                                ::  ++numb:de:json:html
      ++  numb                                          ::  number
        ;~  (comp weld)
          (mayb (piec hep))
          ;~  pose
            (piec (just '0'))
            ;~(plug (shim '1' '9') (star nud))
          ==
          (mayb frac)
          (mayb expo)
        ==
      ::                                                ::  ++obje:de:json:html
      ++  obje                                          ::  object list
        %+  ifix  [(wish kel) (wish ker)]
        (more (wish com) pear)
      ::                                                ::  ++obox:de:json:html
      ++  obox                                          ::  object
        (stag %o (cook malt obje))
      ::                                                ::  ++pear:de:json:html
      ++  pear                                          ::  key-value
        ;~(plug ;~(sfix (wish stri) (wish col)) apex)
      ::                                                ::  ++piec:de:json:html
      ++  piec                                          ::  listify
        |*  bus=rule
        (cook |=(a=@ [a ~]) bus)
      ::                                                ::  ++stri:de:json:html
      ++  stri                                          ::  string
        %+  sear
          |=  a=cord
          ?.  (sune a)  ~
          (some a)
        (cook crip (ifix [doq doq] (star jcha)))
      ::                                                ::  ++spac:de:json:html
      ++  spac                                          ::  whitespace
        (star (mask [`@`9 `@`10 `@`13 ' ' ~]))
      ::                                                ::  ++unic:de:json:html
      ++  unic                                          ::  escaped UTF16
        =*  lob  0x0
        =*  hsb  0xd800
        =*  lsb  0xdc00
        =*  hib  0xe000
        =*  hil  0x1.0000
        |^
          %+  cook
            |=  a=@
            ^-  @t
            (tuft a)
          ;~  pfix  (just 'u')
            ;~(pose solo pair)
          ==
        ++  quad                                        ::  parse num from 4 hex
          (bass 16 (stun [4 4] hit))
        ++  meat                                        ::  gen gate for sear:
          |=  [bot=@ux top=@ux flp=?]                   ::  accept num in range,
          |=  sur=@ux                                   ::  optionally reduce
          ^-  (unit @)
          ?.  &((gte sur bot) (lth sur top))
            ~
          %-  some
          ?.  flp  sur
          (sub sur bot)
        ++  solo                                        ::  single valid UTF16
          ;~  pose
            (sear (meat lob hsb |) quad)
            (sear (meat hib hil |) quad)
          ==
        ++  pair                                        ::  UTF16 surrogate pair
          %+  cook
            |=  [hig=@ low=@]
              ^-  @t
              :(add hil low (lsh [1 5] hig))
          ;~  plug
            (sear (meat hsb lsb &) quad)
            ;~  pfix  (jest '\\u')
              (sear (meat lsb hib &) quad)
            ==
          ==
        --
      ::                                                ::  ++utfe:de:json:html
      ++  utfe                                          ::  UTF-8 sequence
        ;~  less  doq  bas
          =*  qua
            %+  cook
            |=  [a=@ b=@ c=@ d=@]
              (rap 3 a b c d ~)
            ;~  pose
              ;~  plug
                (shim 241 243)
                (shim 128 191)
                (shim 128 191)
                (shim 128 191)
              ==
              ;~  plug
                (just '\F0')
                (shim 144 191)
                (shim 128 191)
                (shim 128 191)
              ==
              ;~  plug
                (just '\F4')
                (shim 128 143)
                (shim 128 191)
                (shim 128 191)
              ==
            ==
          =*  tre
            %+  cook
            |=  [a=@ b=@ c=@]
              (rap 3 a b c ~)
            ;~  pose
              ;~  plug
                ;~  pose
                  (shim 225 236)
                  (shim 238 239)
                ==
                (shim 128 191)
                (shim 128 191)
              ==
              ;~  plug
                (just '\E0')
                (shim 160 191)
                (shim 128 191)
              ==
              ;~  plug
                (just '\ED')
                (shim 128 159)
                (shim 128 191)
              ==
            ==
          =*  dos
            %+  cook
            |=  [a=@ b=@]
              (cat 3 a b)
            ;~  plug
              (shim 194 223)
              (shim 128 191)
            ==
          ;~(pose qua tre dos)
        ==
      ::                                                ::  ++wish:de:json:html
      ++  wish                                          ::  with whitespace
        |*(sef=rule ;~(pfix spac sef))
      ::  XX: These gates should be moved to hoon.hoon
      ::                                                ::  ++sune:de:json:html
      ++  sune                                          ::  cord UTF-8 sanity
        |=  b=@t
        ^-  ?
        ?:  =(0 b)  &
        ?.  (sung b)  |
        $(b (rsh [3 (teff b)] b))
      ::                                                ::  ++sung:de:json:html
      ++  sung                                          ::  char UTF-8 sanity
        |^  |=  b=@t
            ^-  ?
            =+  len=(teff b)
            ?:  =(4 len)  (quad b)
            ?:  =(3 len)  (tres b)
            ?:  =(2 len)  (dos b)
            (lte (end 3 b) 127)
        ::
        ++  dos
          |=  b=@t
          ^-  ?
          =+  :-  one=(cut 3 [0 1] b)
                  two=(cut 3 [1 1] b)
          ?&  (rang one 194 223)
              (cont two)
          ==
        ::
        ++  tres
          |=  b=@t
          ^-  ?
          =+  :+  one=(cut 3 [0 1] b)
                  two=(cut 3 [1 1] b)
                  tre=(cut 3 [2 1] b)
          ?&
            ?|
              ?&  |((rang one 225 236) (rang one 238 239))
                  (cont two)
              ==
              ::
              ?&  =(224 one)
                  (rang two 160 191)
              ==
              ::
              ?&  =(237 one)
                  (rang two 128 159)
              ==
            ==
            ::
            (cont tre)
          ==
        ::
        ++  quad
          |=  b=@t
          ^-  ?
          =+  :^  one=(cut 3 [0 1] b)
                  two=(cut 3 [1 1] b)
                  tre=(cut 3 [2 1] b)
                  for=(cut 3 [3 1] b)
          ?&
            ?|
              ?&  (rang one 241 243)
                  (cont two)
              ==
              ::
              ?&  =(240 one)
                  (rang two 144 191)
              ==
              ::
              ?&  =(244 one)
                  (rang two 128 143)
              ==
            ==
            ::
            (cont tre)
            (cont for)
          ==
        ::
        ++  cont
          |=  a=@
          ^-  ?
          (rang a 128 191)
        ::
        ++  rang
          |=  [a=@ bot=@ top=@]
          ^-  ?
          ?>  (lte bot top)
          &((gte a bot) (lte a top))
        --
      ::  XX: This +teff should overwrite the existing +teff
      ::                                                ::  ++teff:de:json:html
      ++  teff                                          ::  UTF-8 length
        |=  a=@t
        ^-  @
        =+  b=(end 3 a)
        ?:  =(0 b)
          ?>  =(`@`0 a)  0
        ?:  (lte b 127)  1
        ?:  (lte b 223)  2
        ?:  (lte b 239)  3
        4
      --  ::de
    --  ::json
  ::                                                    ::  ++en-xml:html
  ++  en-xml                                            ::  xml printer
    =<  |=(a=manx `tape`(apex a ~))
    |_  _[unq=`?`| cot=`?`|]
    ::                                                  ::  ++apex:en-xml:html
    ++  apex                                            ::  top level
      |=  [mex=manx rez=tape]
      ^-  tape
      ?:  ?=([%$ [[%$ *] ~]] g.mex)
        (escp v.i.a.g.mex rez)
      =+  man=`mane`n.g.mex
      =.  unq  |(unq =(%script man) =(%style man))
      =+  tam=(name man)
      =+  att=`mart`a.g.mex
      :-  '<'
      %+  welp  tam
      =-  ?~(att rez [' ' (attr att rez)])
      ^-  rez=tape
      ?:  &(?=(~ c.mex) |(cot ?^(man | (clot man))))
        [' ' '/' '>' rez]
      :-  '>'
      (many c.mex :(weld "</" tam ">" rez))
    ::                                                  ::  ++attr:en-xml:html
    ++  attr                                            ::  attributes to tape
      |=  [tat=mart rez=tape]
      ^-  tape
      ?~  tat  rez
      =.  rez  $(tat t.tat)
      ;:  weld
        (name n.i.tat)
        "=\""
        (escp(unq |) v.i.tat '"' ?~(t.tat rez [' ' rez]))
      ==
    ::                                                  ::  ++escp:en-xml:html
    ++  escp                                            ::  escape for xml
      |=  [tex=tape rez=tape]
      ?:  unq
        (weld tex rez)
      =+  xet=`tape`(flop tex)
      !.
      |-  ^-  tape
      ?~  xet  rez
      %=    $
        xet  t.xet
        rez  ?-  i.xet
               %34  ['&' 'q' 'u' 'o' 't' ';' rez]
               %38  ['&' 'a' 'm' 'p' ';' rez]
               %39  ['&' '#' '3' '9' ';' rez]
               %60  ['&' 'l' 't' ';' rez]
               %62  ['&' 'g' 't' ';' rez]
               *    [i.xet rez]
             ==
      ==
    ::                                                  ::  ++many:en-xml:html
    ++  many                                            ::  nodelist to tape
      |=  [lix=(list manx) rez=tape]
      |-  ^-  tape
      ?~  lix  rez
      (apex i.lix $(lix t.lix))
    ::                                                  ::  ++name:en-xml:html
    ++  name                                            ::  name to tape
      |=  man=mane  ^-  tape
      ?@  man  (trip man)
      (weld (trip -.man) `tape`[':' (trip +.man)])
    ::                                                  ::  ++clot:en-xml:html
    ++  clot  ~+                                        ::  self-closing tags
      %~  has  in
      %-  silt  ^-  (list term)  :~
        %area  %base  %br  %col  %command  %embed  %hr  %img  %inputt
        %keygen  %link  %meta  %param     %source   %track  %wbr
      ==
    --  ::en-xml
  ::                                                    ::  ++de-xml:html
  ++  de-xml                                            ::  xml parser
    =<  |=(a=cord (rush a apex))
    |_  ent=_`(map term @t)`[[%apos '\''] ~ ~]
    ::                                                  ::  ++apex:de-xml:html
    ++  apex                                            ::  top level
      =+  spa=;~(pose comt whit)
      %+  knee  *manx  |.  ~+
      %+  ifix
        [;~(plug (more spa decl) (star spa)) (star spa)]
      ;~  pose
        %+  sear  |=([a=marx b=marl c=mane] ?.(=(c n.a) ~ (some [a b])))
          ;~(plug head many tail)
        empt
      ==
    ::                                                  ::  ++attr:de-xml:html
    ++  attr                                            ::  attributes
      %+  knee  *mart  |.  ~+
      %-  star
      ;~  plug
        ;~(pfix (plus whit) name)
        ;~  pose
          %+  ifix
            :_  doq
            ;~(plug (ifix [. .]:(star whit) tis) doq)
          (star ;~(less doq escp))
        ::
          %+  ifix
            :_  soq
            ;~(plug (ifix [. .]:(star whit) tis) soq)
          (star ;~(less soq escp))
        ::
          (easy ~)
        ==
      ==
    ::                                                  ::  ++cdat:de-xml:html
    ++  cdat                                            ::  CDATA section
      %+  cook
        |=(a=tape ^-(mars ;/(a)))
      %+  ifix
        [(jest '<![CDATA[') (jest ']]>')]
      %-  star
      ;~(less (jest ']]>') next)
    ::                                                  ::  ++chrd:de-xml:html
    ++  chrd                                            ::  character data
      %+  cook  |=(a=tape ^-(mars ;/(a)))
      (plus ;~(pose (just `@`10) escp))
    ::                                                  ::  ++comt:de-xml:html
    ++  comt                                            ::  comments
      =-  (ifix [(jest '<!--') (jest '-->')] (star -))
      ;~  pose
        ;~(less hep prn)
        whit
        ;~(less (jest '-->') hep)
      ==
    ::
    ++  decl                                            ::  ++decl:de-xml:html
      %+  ifix                                          ::  XML declaration
        [(jest '<?xml') (jest '?>')]
      %-  star
      ;~(less (jest '?>') prn)
    ::                                                  ::  ++escp:de-xml:html
    ++  escp                                            ::
      ;~(pose ;~(less gal gar pam prn) enty)
    ::                                                  ::  ++enty:de-xml:html
    ++  enty                                            ::  entity
      %+  ifix  pam^mic
      ;~  pose
        =+  def=^+(ent (my:nl [%gt '>'] [%lt '<'] [%amp '&'] [%quot '"'] ~))
        %+  sear  ~(get by (~(uni by def) ent))
        (cook crip ;~(plug alf (stun 1^31 aln)))
        %+  cook  |=(a=@c ?:((gth a 0x10.ffff) '�' (tuft a)))
        =<  ;~(pfix hax ;~(pose - +))
        :-  (bass 10 (stun 1^8 dit))
        (bass 16 ;~(pfix (mask "xX") (stun 1^8 hit)))
      ==
    ::                                                  ::  ++empt:de-xml:html
    ++  empt                                            ::  self-closing tag
      %+  ifix  [gal (jest '/>')]
      ;~(plug ;~(plug name attr) (cold ~ (star whit)))
    ::                                                  ::  ++head:de-xml:html
    ++  head                                            ::  opening tag
      (ifix [gal gar] ;~(plug name attr))
    ::                                                  ::  ++many:de-xml:html
    ++  many                                            ::  contents
      ;~(pfix (star comt) (star ;~(sfix ;~(pose apex chrd cdat) (star comt))))
    ::                                                  ::  ++name:de-xml:html
    ++  name                                            ::  tag name
      =+  ^=  chx
          %+  cook  crip
          ;~  plug
              ;~(pose cab alf)
              (star ;~(pose cab dot alp))
          ==
      ;~(pose ;~(plug ;~(sfix chx col) chx) chx)
    ::                                                  ::  ++tail:de-xml:html
    ++  tail                                            ::  closing tag
      (ifix [(jest '</') gar] name)
    ::                                                  ::  ++whit:de-xml:html
    ++  whit                                            ::  whitespace
      (mask ~[' ' `@`0x9 `@`0xa])
    --  ::de-xml
  ::                                                    ::  ++en-urlt:html
  ++  en-urlt                                           ::  url encode
    |=  tep=tape
    ^-  tape
    %-  zing
    %+  turn  tep
    |=  tap=char
    =+  xen=|=(tig=@ ?:((gte tig 10) (add tig 55) (add tig '0')))
    ?:  ?|  &((gte tap 'a') (lte tap 'z'))
            &((gte tap 'A') (lte tap 'Z'))
            &((gte tap '0') (lte tap '9'))
            =('.' tap)
            =('-' tap)
            =('~' tap)
            =('_' tap)
        ==
      [tap ~]
    ['%' (xen (rsh [0 4] tap)) (xen (end [0 4] tap)) ~]
  ::                                                    ::  ++de-urlt:html
  ++  de-urlt                                           ::  url decode
    |=  tep=tape
    ^-  (unit tape)
    ?~  tep  [~ ~]
    ?:  =('%' i.tep)
      ?.  ?=([@ @ *] t.tep)  ~
      =+  nag=(mix i.t.tep (lsh 3 i.t.t.tep))
      =+  val=(rush nag hex:ag)
      ?~  val  ~
      =+  nex=$(tep t.t.t.tep)
      ?~(nex ~ [~ [`@`u.val u.nex]])
    =+  nex=$(tep t.tep)
    ?~(nex ~ [~ i.tep u.nex])
  ::                                                    ::  ++en-purl:html
  ++  en-purl                                           ::  print purl
    =<  |=(pul=purl `tape`(apex %& pul))
    |%
    ::                                                  ::  ++apex:en-purl:html
    ++  apex                                            ::
      |=  qur=quri  ^-  tape
      ?-  -.qur
        %&  (weld (head p.p.qur) `tape`$(qur [%| +.p.qur]))
        %|  ['/' (weld (body p.qur) (tail q.qur))]
      ==
    ::                                                  ::  ++apix:en-purl:html
    ++  apix                                            ::  purf to tape
      |=  purf
      (weld (apex %& p) ?~(q "" `tape`['#' (trip u.q)]))
    ::                                                  ::  ++body:en-purl:html
    ++  body                                            ::
      |=  pok=pork  ^-  tape
      ?~  q.pok  ~
      |-
      =+  seg=(en-urlt (trip i.q.pok))
      ?~  t.q.pok
        ?~(p.pok seg (welp seg '.' (trip u.p.pok)))
      (welp seg '/' $(q.pok t.q.pok))
    ::                                                  ::  ++head:en-purl:html
    ++  head                                            ::
      |=  har=hart
      ^-  tape
      ;:  weld
        ?:(&(p.har !?=(hoke r.har)) "https://" "http://")
      ::
        ?-  -.r.har
          %|  (trip (rsh 3 (scot %if p.r.har)))
          %&  =+  rit=(flop p.r.har)
              |-  ^-  tape
              ?~  rit  ~
              (weld (trip i.rit) ?~(t.rit "" `tape`['.' $(rit t.rit)]))
        ==
      ::
        ?~(q.har ~ `tape`[':' ((d-co:co 1) u.q.har)])
      ==
    ::                                                  ::  ++tail:en-purl:html
    ++  tail                                            ::
      |=  kay=quay
      ^-  tape
      ?:  =(~ kay)  ~
      :-  '?'
      |-  ^-  tape
      ?~  kay  ~
      ;:  welp
        (en-urlt (trip p.i.kay))
        ?~(q.i.kay ~ ['=' (en-urlt (trip q.i.kay))])
        ?~(t.kay ~ `tape`['&' $(kay t.kay)])
      ==
    --  ::
  ::                                                    ::  ++de-purl:html
  ++  de-purl                                           ::  url+header parser
    =<  |=(a=cord `(unit purl)`(rush a auri))
    |%
    ::                                                  ::  ++deft:de-purl:html
    ++  deft                                            ::  parse url extension
      |=  rax=(list @t)
      |-  ^-  pork
      ?~  rax
        [~ ~]
      ?^  t.rax
        [p.pok [ire q.pok]]:[pok=$(rax t.rax) ire=i.rax]
      =/  raf=(like term)
        %-  ;~  sfix
              %+  sear
                |=(a=@ ((sand %ta) (crip (flop (trip a)))))
              (cook |=(a=tape (rap 3 ^-((list @) a))) (star aln))
              dot
            ==
        [1^1 (flop (trip i.rax))]
      ?~  q.raf
        [~ [i.rax ~]]
      =+  `[ext=term [@ @] fyl=tape]`u.q.raf
      :-  `ext
      ?:(=(~ fyl) ~ [(crip (flop fyl)) ~])
    ::                                                  ::  ++apat:de-purl:html
    ++  apat                                            ::  2396 abs_path
      %+  cook  deft
      ;~(pfix fas (more fas smeg))
    ::                                                  ::  ++aurf:de-purl:html
    ++  aurf                                            ::  2396 with fragment
      %+  cook  |~(a=purf a)
      ;~(plug auri (punt ;~(pfix hax (cook crip (star pque)))))
    ::                                                  ::  ++auri:de-purl:html
    ++  auri                                            ::  2396 URL
      ;~  plug
        ;~(plug htts thor)
        ;~(plug ;~(pose apat (easy *pork)) yque)
      ==
    ::                                                  ::  ++auru:de-purl:html
    ++  auru                                            ::  2396 with maybe user
      %+  cook
        |=  $:  a=[p=? q=(unit user) r=[(unit @ud) host]]
                b=[pork quay]
            ==
        ^-  (pair (unit user) purl)
        [q.a [[p.a r.a] b]]
      ::
      ;~  plug
        ;~(plug htts (punt ;~(sfix urt:ab pat)) thor)
        ;~(plug ;~(pose apat (easy *pork)) yque)
      ==
    ::                                                  ::  ++htts:de-purl:html
    ++  htts                                            ::  scheme
      %+  sear  ~(get by (malt `(list (pair term ?))`[http+| https+& ~]))
      ;~(sfix scem ;~(plug col fas fas))
    ::                                                  ::  ++cock:de-purl:html
    ++  cock                                            ::  cookie
      %+  most  ;~(plug mic ace)
      ;~(plug toke ;~(pfix tis tosk))
    ::                                                  ::  ++dlab:de-purl:html
    ++  dlab                                            ::  2396 domainlabel
      %+  sear
        |=  a=@ta
        ?.(=('-' (rsh [3 (dec (met 3 a))] a)) [~ u=a] ~)
      %+  cook  |=(a=tape (crip (cass a)))
      ;~(plug aln (star alp))
    ::                                                  ::  ++fque:de-purl:html
    ++  fque                                            ::  normal query field
      (cook crip (plus pquo))
    ::                                                  ::  ++fquu:de-purl:html
    ++  fquu                                            ::  optional query field
      (cook crip (star pquo))
    ::                                                  ::  ++pcar:de-purl:html
    ++  pcar                                            ::  2396 path char
      ;~(pose pure pesc psub col pat)
    ::                                                  ::  ++pcok:de-purl:html
    ++  pcok                                            ::  cookie char
      ;~(less bas mic com doq prn)
    ::                                                  ::  ++pesc:de-purl:html
    ++  pesc                                            ::  2396 escaped
      ;~(pfix cen mes)
    ::                                                  ::  ++pold:de-purl:html
    ++  pold                                            ::
      (cold ' ' (just '+'))
    ::                                                  ::  ++pque:de-purl:html
    ++  pque                                            ::  3986 query char
      ;~(pose pcar fas wut)
    ::                                                  ::  ++pquo:de-purl:html
    ++  pquo                                            ::  normal query char
      ;~(pose pure pesc pold fas wut col com)
    ::                                                  ::  ++pure:de-purl:html
    ++  pure                                            ::  2396 unreserved
      ;~(pose aln hep cab dot zap sig tar soq pal par)
    ::                                                  ::  ++psub:de-purl:html
    ++  psub                                            ::  3986 sub-delims
      ;~  pose
        zap  buc  pam  soq  pal  par
        tar  lus  com  mic  tis
      ==
    ::                                                  ::  ++ptok:de-purl:html
    ++  ptok                                            ::  2616 token
      ;~  pose
        aln  zap  hax  buc  cen  pam  soq  tar  lus
        hep  dot  ket  cab  tic  bar  sig
      ==
    ::                                                  ::  ++scem:de-purl:html
    ++  scem                                            ::  2396 scheme
      %+  cook  |=(a=tape (crip (cass a)))
      ;~(plug alf (star ;~(pose aln lus hep dot)))
    ::                                                  ::  ++smeg:de-purl:html
    ++  smeg                                            ::  2396 segment
      (cook crip (star pcar))
    ::                                                  ::  ++tock:de-purl:html
    ++  tock                                            ::  6265 raw value
      (cook crip (plus pcok))
    ::                                                  ::  ++tosk:de-purl:html
    ++  tosk                                            ::  6265 quoted value
      ;~(pose tock (ifix [doq doq] tock))
    ::                                                  ::  ++toke:de-purl:html
    ++  toke                                            ::  2616 token
      (cook crip (plus ptok))
    ::                                                  ::  ++thor:de-purl:html
    ++  thor                                            ::  2396 host+port
      %+  cook  |*([* *] [+<+ +<-])
      ;~  plug
        thos
        ;~((bend) (easy ~) ;~(pfix col dim:ag))
      ==
    ::                                                  ::  ++thos:de-purl:html
    ++  thos                                            ::  2396 host, no local
      ;~  plug
        ;~  pose
          %+  stag  %&
          %+  sear                                      ::  LL parser weak here
            |=  a=(list @t)
            =+  b=(flop a)
            ?>  ?=(^ b)
            =+  c=(end 3 i.b)
            ?.(&((gte c 'a') (lte c 'z')) ~ [~ u=b])
          (most dot dlab)
        ::
          %+  stag  %|
          =+  tod=(ape:ag ted:ab)
          %+  bass  256
          ;~(plug tod (stun [3 3] ;~(pfix dot tod)))
        ==
      ==
    ::                                                  ::  ++yque:de-purl:html
    ++  yque                                            ::  query ending
      ;~  pose
        ;~(pfix wut yquy)
        (easy ~)
      ==
    ::                                                  ::  ++yquy:de-purl:html
    ++  yquy                                            ::  query
      ;~  pose
        ::  proper query
        ::
        %+  more
          ;~(pose pam mic)
        ;~(plug fque ;~(pose ;~(pfix tis fquu) (easy '')))
        ::
        ::  funky query
        ::
        %+  cook
          |=(a=tape [[%$ (crip a)] ~])
        (star pque)
      ==
    ::                                                  ::  ++zest:de-purl:html
    ++  zest                                            ::  2616 request-uri
      ;~  pose
        (stag %& (cook |=(a=purl a) auri))
        (stag %| ;~(plug apat yque))
      ==
    --  ::de-purl
  ::  +en-turf: encode +turf as a TLD-last domain string
  ::
  ++  en-turf
    |=  =turf
    ^-  @t
    (rap 3 (flop (join '.' turf)))
  ::  +de-turf: parse a TLD-last domain string into a TLD first +turf
  ::
  ++  de-turf
    |=  host=@t
    ^-  (unit turf)
    %+  rush  host
    %+  sear
      |=  =host:eyre
      ?.(?=(%& -.host) ~ (some p.host))
    thos:de-purl:html
  ::
  ::  MOVEME
  ::                                                    ::  ++fuel:html
  ++  fuel                                              ::  parse urbit fcgi
      |=  [bem=beam ced=noun:cred quy=quer]
      ^-  epic
      =+  qix=|-(`quay`?~(quy quy [[p q]:quy $(quy t.quy)]))
      [(malt qix) ;;(cred ced) bem]
  ::
  ++  hiss-to-request
    |=  =hiss
    ^-  request:http
    ::
    :*  ?-  p.q.hiss
          %conn  %'CONNECT'
          %delt  %'DELETE'
          %get   %'GET'
          %head  %'HEAD'
          %opts  %'OPTIONS'
          %post  %'POST'
          %put   %'PUT'
          %trac  %'TRACE'
        ==
    ::
      (crip (en-purl:html p.hiss))
    ::
      ^-  header-list:http
      ~!  q.q.hiss
      %+  turn  ~(tap by q.q.hiss)
      |=  [a=@t b=(list @t)]
      ^-  [@t @t]
      ?>  ?=(^ b)
      [a i.b]
    ::
      r.q.hiss
    ==
  --  ::  html
::                                                      ::
::::                      ++wired                       ::  wire formatting
  ::                                                    ::::
++  wired  ^?
  |%
  ::                                                    ::  ++dray:wired
  ++  dray                                              ::  load tuple in path
    ::
    ::  .=  ~[p=~.ack q=~.~sarnel r=~..y]
    ::  (dray ~[p=%tas q=%p r=%f] %ack ~sarnel &)
    ::
    =-  |*  [a=[@tas (pole @tas)] b=*]  ^-  (paf a)
        =>  .(b `,(tup -.a +.a)`b)
        ?~  +.a  [(scot -.a b) ~]
        [(scot -.a -.b) `,(paf +.a)`(..$ +.a +.b)]
    :-  paf=|*(a=(pole) ?~(a ,~ ,[(odo:raid ,-.a(. %ta)) ,(..$ +.a)]))
    ^=  tup
    |*  [a=@tas b=(pole @tas)]
    =+  c=(odo:raid a)
    ?~(b c ,[c (..$ ,-.b ,+.b)])
  ::                                                    ::  ++raid:wired
  ++  raid                                              ::  demand path odors
    ::
    ::  .=  [p=%ack q=~sarnel r=&]
    ::  (raid /ack/~sarnel+.y p=%tas q=%p r=%f ~)
    ::
    =-  |*  [a=path b=[@tas (pole @tas)]]
        =*  fog  (odo -.b)
        ?~  +.b  `fog`(slav -.b -.a)
        [`fog`(slav -.b -.a) (..$ +.a +.b)]
    ^=  odo
    |*  a=@tas
    |=  b=*
    =-  a(, (- b))                  ::  preserve face
    ?+  a   @
      %c  @c  %da  @da  %dr  @dr  %f   @f   %if  @if  %is  @is  %p   @p
      %u  @u  %uc  @uc  %ub  @ub  %ui  @ui  %ux  @ux  %uv  @uv  %uw  @uw
      %s  @s  %t   @t   %ta  @ta  %tas  @tas
    ==
::  ::                                                    ::  ++read:wired
::  ++  read                                              ::  parse odored path
::    =<  |*([a=path b=[@tas (pole @tas)]] ((+> b) a))
::    |*  b=[@tas (pole @tas)]
::    |=  a=path
::    ?~  a  ~
::    =+  hed=(slaw -.b i.a)
::    =*  fog  (odo:raid -.b)
::    ?~  +.b
::      ^-  (unit fog)
::      ?^(+.a ~ hed)
::    ^-  (unit [fog _(need *(..^$ +.b))])
::    (both hed ((..^$ +.b) +.a))
  --  ::wired
::                                                      ::
::::                      ++title                       ::  (2j) identity
  ::                                                    ::::
++  title
  ::  deep core: for vane use, with $roof for scrying
  ::
  ::    TODO: refactor to share high-level gates like +saxo
  ::          among the three cores
  ::
  =>  |%
      ++  sein
        |=  [rof=roof pov=path our=ship now=@da who=ship]
        ;;  ship
        =<  q.q  %-  need  %-  need
        (rof ~ pov %j `beam`[[our %sein %da now] /(scot %p who)])
      --
  ::  middle core: stateless queries for default numeric sponsorship
  ::
  =>  |%
      ::                                                ::  ++clan:title
      ++  clan                                          ::  ship to rank
        |=  who=ship
        ^-  rank
        =/  wid  (met 3 who)
        ?:  (lte wid 1)   %czar
        ?:  =(2 wid)      %king
        ?:  (lte wid 4)   %duke
        ?:  (lte wid 8)   %earl
        ?>  (lte wid 16)  %pawn
      ::                                                ::  ++rank:title
      +$  rank  ?(%czar %king %duke %earl %pawn)        ::  ship width class
      ::                                                ::  ++name:title
      ++  name                                          ::  identity
        |=  who=ship
        ^-  ship
        ?.  ?=(%earl (clan who))  who
        (sein who)
      ::                                                ::  ++saxo:title
      ++  saxo                                          ::  autocanon
        |=  who=ship
        ^-  (list ship)
        =/  dad  (sein who)
        [who ?:(=(who dad) ~ $(who dad))]
      ::                                                ::  ++sein:title
      ++  sein                                          ::  autoboss
        |=  who=ship
        ^-  ship
        =/  mir  (clan who)
        ?-  mir
          %czar  who
          %king  (end 3 who)
          %duke  (end 4 who)
          %earl  (end 5 who)
          %pawn  (end 4 who)
        ==
      --
  ::  surface core: for userspace use, with .^
  ::
  |%
  ::                                                    ::  ++cite:title
  ++  cite                                              ::  render ship
    |=  who=@p
    ^-  tape
    =/  wid  (met 4 who)
    ?:  (lte wid 2)  (scow %p who)
    ?:  (lte wid 4)
      =/  nom  (scow %p (end 5 who))
      :(weld (scag 7 nom) "^" (slag 8 nom))
    %-  trip
    %+  rap  3
    :~  '~'
        (tos:po (cut 3 [(dec (mul wid 2)) 1] who))
        (tod:po (cut 3 [(mul (dec wid) 2) 1] who))
        '_'
        (tos:po (cut 3 [1 1] who))
        (tod:po (end 3 who))
    ==
  ::                                                    ::  ++saxo:title
  ++  saxo                                              ::  autocanon
    |=  [our=ship now=@da who=ship]
    .^  (list ship)
        %j
        /(scot %p our)/saxo/(scot %da now)/(scot %p who)
    ==
  ::                                                    ::  ++sein:title
  ++  sein                                              ::  autoboss
    |=  [our=ship now=@da who=ship]
    .^  ship
        %j
        /(scot %p our)/sein/(scot %da now)/(scot %p who)
    ==
  ::  +team was created with two meanings:
  ::    A. her / her moon
  ::    B. whoever should be able to control her ship
  ::
  ::  these two things aren't obviously equal anymore,
  ::  and it's more important for +team to satisfy B than A,
  ::  so now +team just means "her".
  ::
  ::  (ships can definitely be trusted to control themselves)
  ::                                                    ::  ++team:title
  ++  team                                              ::  her
    |=  [her=ship who=ship]
    ^-  ?
    =(her who)
  ::                                                    ::  ++moon:title
  ++  moon                                              ::  her moon
    |=  [her=ship who=ship]
    ^-  ?
    &(=(%earl (clan who)) =(her (^sein who)))
  --  ::title
::                                                      ::
::::                      ++milly                       ::  (2k) milliseconds
  ::                                                    ::::
++  milly  ^|
  |_  now=@da
  ::                                                    ::  ++around:milly
  ++  around                                            ::  relative msec
    |=  wen=@da
    ^-  @tas
    ?:  =(wen now)  %now
    ?:  (gth wen now)
      (cat 3 (scot %ud (msec (sub wen now))) %ms)
    (cat 3 '-' $(now wen, wen now))
  ::
  ++  about                                             ::  ++about:milly
    |=  wun=(unit @da)                                  ::  unit relative msec
    ^-  @tas
    ?~(wun %no (around u.wun))
  ::                                                    ::  ++mill:milly
  ++  mill                                              ::  msec diff
    |=  one=@dr
    ^-  @tas
    ?:  =(`@`0 one)  '0ms'
    (cat 3 (scot %ud (msec one)) %ms)
  ::                                                    ::  ++msec:milly
  ++  msec                                              ::  @dr to @ud ms
    |=(a=@dr `@ud`(div a (div ~s1 1.000)))
  ::                                                    ::  ++mull:milly
  ++  mull                                              ::  unit msec diff
    |=  une=(unit @dr)
    ^-  @tas
    ?~(une %no (mill u.une))
  --
::
::::
  ::
++  contain  ^?
  |%
  ::  +by-clock: interface core for a cache using the clock replacement algorithm
  ::
  ::    Presents an interface for a mapping, but somewhat specialized, and with
  ::    stateful accessors. The clock's :depth parameter is used as the maximum
  ::    freshness that an entry can have. The standard clock algorithm has a depth
  ::    of 1, meaning that a single sweep of the arm will delete the entry. For
  ::    more scan resistance, :depth can be set to a higher number.
  ::
  ::    Internally, :clock maintains a :lookup of type
  ::    `(map key-type [val=val-type fresh=@ud])`, where :depth.clock is the
  ::    maximum value of :fresh. Looking up a key increments its freshness, and a
  ::    sweep of the clock arm decrements its freshness.
  ::
  ::    The clock arm is stored as :queue, which is a `(qeu key-type)`. The head
  ::    of the queue represents the position of the clock arm. New entries are
  ::    inserted at the tail of the queue. When the clock arm sweeps, it
  ::    pops the head off the queue. If the :fresh of the head's entry in :lookup
  ::    is 0, remove the entry from the mapping and replace it with the new entry.
  ::    Otherwise, decrement the entry's freshness, put it back at the tail of
  ::    the queue, and pop the next head off the queue and try again.
  ::
  ::    Cache entries must be immutable: a key cannot be overwritten with a new
  ::    value. This property is enforced for entries currently stored in the
  ::    cache, but it is not enforced for previously deleted entries, since we
  ::    no longer remember what that key's value was supposed to be.
  ::
  ++  by-clock
    |*  [key-type=mold val-type=mold]
    |_  clock=(clock key-type val-type)
    ::  +get: looks up a key, marking it as fresh
    ::
    ++  get
      |=  key=key-type
      ^-  [(unit val-type) _clock]
      ::
      =+  maybe-got=(~(get by lookup.clock) key)
      ?~  maybe-got
        [~ clock]
      ::
      =.  clock  (freshen key)
      ::
      [`val.u.maybe-got clock]
    ::  +put: add a new cache entry, possibly removing an old one
    ::
    ++  put
      |=  [key=key-type val=val-type]
      ^+  clock
      ::  do nothing if our size is 0 so we don't decrement-underflow
      ::
      ?:  =(0 max-size.clock)
        clock
      ::  no overwrite allowed, but allow duplicate puts
      ::
      ?^  existing=(~(get by lookup.clock) key)
        ::  val must not change
        ::
        ?>  =(val val.u.existing)
        ::
        (freshen key)
      ::
      =?  clock  =(max-size.clock size.clock)
        evict
      ::
      %_  clock
        size    +(size.clock)
        lookup  (~(put by lookup.clock) key [val 1])
        queue   (~(put to queue.clock) key)
      ==
    ::  +freshen: increment the protection level on an entry
    ::
    ++  freshen
      |=  key=key-type
      ^+  clock
      %_    clock
          lookup
        %+  ~(jab by lookup.clock)  key
        |=  entry=[val=val-type fresh=@ud]
        entry(fresh (min +(fresh.entry) depth.clock))
      ==
    ::  +resize: changes the maximum size, removing entries if needed
    ::
    ++  resize
      |=  new-max=@ud
      ^+  clock
      ::
      =.  max-size.clock  new-max
      ::
      ?:  (gte new-max size.clock)
        clock
      ::
      (trim (sub size.clock new-max))
    ::  +evict: remove an entry from the cache
    ::
    ++  evict
      ^+  clock
      ::
      =.  size.clock  (dec size.clock)
      ::
      |-
      ^+  clock
      ::
      =^  old-key  queue.clock  ~(get to queue.clock)
      =/  old-entry  (~(got by lookup.clock) old-key)
      ::
      ?:  =(0 fresh.old-entry)
        clock(lookup (~(del by lookup.clock) old-key))
      ::
      %_    $
          lookup.clock
        (~(put by lookup.clock) old-key old-entry(fresh (dec fresh.old-entry)))
      ::
          queue.clock
        (~(put to queue.clock) old-key)
      ==
    ::  +trim: remove :count entries from the cache
    ::
    ++  trim
      |=  count=@ud
      ^+  clock
      ?:  =(0 count)
        clock
      $(count (dec count), clock evict)
    ::  +purge: removes all cache entries
    ::
    ++  purge
      ^+  clock
      %_  clock
        lookup  ~
        queue   ~
        size    0
      ==
    --
  ::  +to-capped-queue: interface door for +capped-queue
  ::
  ::    Provides a queue of a limited size where pushing additional items will
  ::    force pop the items at the front of the queue.
  ::
  ++  to-capped-queue
    |*  item-type=mold
    |_  queue=(capped-queue item-type)
    ::  +put: enqueue :item, possibly popping and producing an old item
    ::
    ++  put
      |=  item=item-type
      ^-  [(unit item-type) _queue]
      ::   are we already at max capacity?
      ::
      ?.  =(size.queue max-size.queue)
        ::  we're below max capacity, so push and increment size
        ::
        =.  queue.queue  (~(put to queue.queue) item)
        =.  size.queue   +(size.queue)
        ::
        [~ queue]
      ::  max is zero, the oldest item to return is the one which just went in.
      ::
      ?:  =(~ queue.queue)
        [`item queue]
      ::  we're at max capacity, so pop before pushing; size is unchanged
      ::
      =^  oldest  queue.queue  ~(get to queue.queue)
      =.  queue.queue          (~(put to queue.queue) item)
      ::
      [`oldest queue]
    ::  +get: pop an item off the queue, adjusting size
    ::
    ++  get
      ^-  [item-type _queue]
      ::
      =.  size.queue           (dec size.queue)
      =^  oldest  queue.queue  ~(get to queue.queue)
      ::
      [oldest queue]
    ::  change the :max-size of the queue, popping items if necessary
    ::
    ++  resize
      =|  pops=(list item-type)
      |=  new-max=@ud
      ^+  [pops queue]
      ::  we're not overfull, so no need to pop off more items
      ::
      ?:  (gte new-max size.queue)
        [(flop pops) queue(max-size new-max)]
      ::  we're above capacity; pop an item off and recurse
      ::
      =^  oldest  queue  get
      ::
      $(pops [oldest pops])
    --
  --
::                                                      ::
::::                      ++userlib                     ::  (2u) non-vane utils
  ::                                                    ::::
++  userlib  ^?
  |%
  ::                                                    ::
  ::::                    ++chrono:userlib              ::  (2uB) time
    ::                                                  ::::
  ++  chrono  ^?
    |%
    ::  +from-unix: unix seconds to @da
    ::
    ++  from-unix
      |=  timestamp=@ud
      ^-  @da
      %+  add  ~1970.1.1
      (mul timestamp ~s1)
    ::  +from-unix-ms: unix milliseconds to @da
    ::
    ++  from-unix-ms
      |=  timestamp=@ud
      ^-  @da
      %+  add  ~1970.1.1
      (div (mul ~s1 timestamp) 1.000)
    ::                                                  ::  ++dawn:chrono:
    ++  dawn                                            ::  Jan 1 weekday
      |=  yer=@ud
      =+  yet=(sub yer 1)
      %-  mod  :_  7
      ;:  add
        1
        (mul 5 (mod yet 4))
        (mul 4 (mod yet 100))
        (mul 6 (mod yet 400))
      ==
    ::                                                  ::  ++daws:chrono:
    ++  daws                                            ::  date weekday
      |=  yed=date
      %-  mod  :_  7
      %+  add
        (dawn y.yed)
      (sub (yawn [y.yed m.yed d.t.yed]) (yawn y.yed 1 1))
    ::                                                  ::  ++deal:chrono:
    ++  deal                                            ::  to leap sec time
      |=  yer=@da
      =+  n=0
      =+  yud=(yore yer)
      |-  ^-  date
      ?:  (gte yer (add (snag n lef:yu) ~s1))
        (yore (year yud(s.t (add n s.t.yud))))
      ?:  &((gte yer (snag n lef:yu)) (lth yer (add (snag n lef:yu) ~s1)))
        yud(s.t (add +(n) s.t.yud))
      ?:  =(+(n) (lent lef:yu))
        (yore (year yud(s.t (add +(n) s.t.yud))))
      $(n +(n))
    ::                                                  ::  ++lead:chrono:
    ++  lead                                            ::  from leap sec time
      |=  ley=date
      =+  ler=(year ley)
      =+  n=0
      |-  ^-  @da
      =+  led=(sub ler (mul n ~s1))
      ?:  (gte ler (add (snag n les:yu) ~s1))
        led
      ?:  &((gte ler (snag n les:yu)) (lth ler (add (snag n les:yu) ~s1)))
        ?:  =(s.t.ley 60)
          (sub led ~s1)
        led
      ?:  =(+(n) (lent les:yu))
        (sub led ~s1)
      $(n +(n))
    ::                                                  ::  ++dust:chrono:
    ++  dust                                            ::  print UTC format
      |=  yed=date
      ^-  tape
      =+  wey=(daws yed)
      =/  num  (d-co:co 1)  :: print as decimal without dots
      =/  pik  |=([n=@u t=wall] `tape`(scag 3 (snag n t)))
      ::
      "{(pik wey wik:yu)}, ".
      "{(num d.t.yed)} {(pik (dec m.yed) mon:yu)} {(num y.yed)} ".
      "{(num h.t.yed)}:{(num m.t.yed)}:{(num s.t.yed)} +0000"
    ::                                                  ::  ++stud:chrono:
    ++  stud                                            ::  parse UTC format
      =<  |=  a=cord                                    ::  expose parsers
          %+  biff  (rush a (more sepa elem))
          |=  b=(list _(wonk *elem))  ^-  (unit date)
          =-  ?.((za:dejs:format -) ~ (some (zp:dejs:format -)))
          ^+  =+  [*date u=unit]
              *[(u _[a y]) (u _m) (u _d.t) (u _+.t) ~]
          :~
              |-(?~(b ~ ?.(?=(%y -.i.b) $(b t.b) `+.i.b)))
              |-(?~(b ~ ?.(?=(%m -.i.b) $(b t.b) `+.i.b)))
              |-(?~(b ~ ?.(?=(%d -.i.b) $(b t.b) `+.i.b)))
              |-(?~(b ~ ?.(?=(%t -.i.b) $(b t.b) `+.i.b)))
          ==
      |%
      ::                                                ::  ++snug:stud:chrono:
      ++  snug                                          ::  position in list
        |=  a=(list tape)
        |=  b=tape
        =+  [pos=1 len=(lent b)]
        |-  ^-  (unit @u)
        ?~  a  ~
        ?:  =(b (scag len i.a))
          `pos
        $(pos +(pos), a t.a)
      ::                                                ::  ++sepa:stud:chrono:
      ++  sepa                                          ::  separator
        ;~(pose ;~(plug com (star ace)) (plus ace))
      ::                                                ::  ++elem:stud:chrono:
      ++  elem                                          ::  date element
        ;~  pose
          (stag %t t)  (stag %y y)  (stag %m m)  (stag %d d)
          (stag %w w)  (stag %z z)
        ==
      ::                                                ::  ++y:stud:chrono:
      ++  y                                             ::  year
        (stag %& (bass 10 (stun 3^4 dit)))
      ::                                                ::  ++m:stud:chrono:
      ++  m                                             ::  month
        (sear (snug mon:yu) (plus alf))
      ::                                                ::  ++d:stud:chrono:
      ++  d                                             ::  day
        (bass 10 (stun 1^2 dit))
      ::                                                ::  ++t:stud:chrono:
      ++  t                                             ::  hours:minutes:secs
        %+  cook  |=([h=@u @ m=@u @ s=@u] ~[h m s])
        ;~(plug d col d col d)
      ::
      ::  XX day of week is currently unchecked, and
      ::  timezone outright ignored.
      ::                                                ::  ++w:stud:chrono:
      ++  w                                             ::  day of week
        (sear (snug wik:yu) (plus alf))
      ::                                                ::  ++z:stud:chrono:
      ++  z                                             ::  time zone
        ;~(plug (mask "-+") dd dd)
      ::                                                ::  ++dd:stud:chrono:
      ++  dd                                            ::  two digits
        (bass 10 (stun 2^2 dit))
      --  ::
    ::                                                  ::  ++unm:chrono:userlib
    ++  unm                                             ::  Urbit to Unix ms
      |=  a=@da
      =-  (div (mul - 1.000) ~s1)
      (sub (add a (div ~s1 2.000)) ~1970.1.1)
    ::                                                  ::  ++unt:chrono:userlib
    ++  unt                                             ::  Urbit to Unix time
      |=  a=@da
      (div (sub a ~1970.1.1) ~s1)
    ::                                                  ::  ++yu:chrono:userlib
    ++  yu                                              ::  UTC format constants
      |%
      ::                                                ::  ++mon:yu:chrono:
      ++  mon                                           ::  months
        ^-  (list tape)
        :~  "January"  "February"  "March"  "April"  "May"  "June"  "July"
            "August"  "September"  "October"  "November"  "December"
        ==
      ::                                                ::  ++wik:yu:chrono:
      ++  wik                                           ::  weeks
        ^-  (list tape)
        :~  "Sunday"  "Monday"  "Tuesday"  "Wednesday"  "Thursday"
            "Friday"  "Saturday"
        ==
      ::                                                ::  ++lef:yu:chrono:
      ++  lef                                           ::  leapsecond dates
        ^-  (list @da)
        :~  ~2016.12.31..23.59.59   ~2015.6.30..23.59.59
            ~2012.6.30..23.59.59    ~2008.12.31..23.59.58
            ~2005.12.31..23.59.57   ~1998.12.31..23.59.56
            ~1997.6.30..23.59.55    ~1995.12.31..23.59.54
            ~1994.6.30..23.59.53    ~1993.6.30..23.59.52
            ~1992.6.30..23.59.51    ~1990.12.31..23.59.50
            ~1989.12.31..23.59.49   ~1987.12.31..23.59.48
            ~1985.6.30..23.59.47    ~1983.6.30..23.59.46
            ~1982.6.30..23.59.45    ~1981.6.30..23.59.44
            ~1979.12.31..23.59.43   ~1978.12.31..23.59.42
            ~1977.12.31..23.59.41   ~1976.12.31..23.59.40
            ~1975.12.31..23.59.39   ~1974.12.31..23.59.38
            ~1973.12.31..23.59.37   ~1972.12.31..23.59.36
            ~1972.6.30..23.59.35
        ==
      ::
      ::  +les:yu:chrono: leapsecond days
      ::
      ::    https://www.ietf.org/timezones/data/leap-seconds.list
      ::
      ++  les
        ^-  (list @da)
        :~  ~2017.1.1  ~2015.7.1  ~2012.7.1  ~2009.1.1  ~2006.1.1  ~1999.1.1
            ~1997.7.1  ~1996.1.1  ~1994.7.1  ~1993.7.1  ~1992.7.1  ~1991.1.1
            ~1990.1.1  ~1988.1.1  ~1985.7.1  ~1983.7.1  ~1982.7.1  ~1981.7.1
            ~1980.1.1  ~1979.1.1  ~1978.1.1  ~1977.1.1  ~1976.1.1  ~1975.1.1
            ~1974.1.1  ~1973.1.1  ~1972.7.1
        ==
      --  ::yu
    --  ::chrono
  ::                                                    ::
  ::::                    ++space:userlib               ::  (2uC) file utils
    ::                                                  ::::
  ++  space  ^?
    =,  clay
    |%
    ::                                                  ::  ++feel:space:userlib
    ++  feel                                            ::  simple file write
      |=  [pax=path val=cage]
      ^-  miso
      =+  dir=.^(arch %cy pax)
      ?~  fil.dir  [%ins val]
      [%mut val]
    ::                                                  ::  ++file:space:userlib
    ++  file                                            ::  simple file load
      |=  pax=path
      ^-  (unit)
      =+  dir=.^(arch %cy pax)
      ?~(fil.dir ~ [~ .^(* %cx pax)])
    ::                                                  ::  ++foal:space:userlib
    ++  foal                                            ::  high-level write
      |=  [pax=path val=cage]
      ^-  toro
      ?>  ?=([* * * *] pax)
      [i.t.pax [%& [[[t.t.t.pax (feel pax val)] ~]]]]
    ::                                                  ::  ++fray:space:userlib
    ++  fray                                            ::  high-level delete
      |=  pax=path
      ^-  toro
      ?>  ?=([* * * *] pax)
      [i.t.pax [%& [[[t.t.t.pax [%del ~]] ~]]]]
    ::                                                  ::  ++furl:space:userlib
    ++  furl                                            ::  unify changes
      |=  [one=toro two=toro]
      ^-  toro
      ~|  %furl
      ?>  ?&  =(p.one p.two)                            ::  same path
              &(?=(%& -.q.one) ?=(%& -.q.two))          ::  both deltas
          ==
      [p.one [%& (weld p.q.one p.q.two)]]
    --  ::space
  ::                                                    ::
  ::::                  ++unix:userlib                  ::  (2uD) unix line-list
    ::                                                  ::::
  ++  unix  ^?
    |%
    ::                                                  ::  ++lune:unix:userlib
    ++  lune                                            ::  cord by unix line
      ~%  %lune  ..part  ~
      |=  txt=@t
      ?~  txt
        ^-  (list @t)  ~
      =+  [byt=(rip 3 txt) len=(met 3 txt)]
      =|  [lin=(list @t) off=@]
      ^-  (list @t)
      %-  flop
      |-  ^+  lin
      ?:  =(off len)
        ~|  %noeol  !!
      ?:  =((snag off byt) 10)
        ?:  =(+(off) len)
          [(rep 3 (scag off byt)) lin]
        %=  $
          lin  [(rep 3 (scag off byt)) lin]
          byt  (slag +(off) byt)
          len  (sub len +(off))
          off  0
        ==
      $(off +(off))
    ::                                                  ::  ++nule:unix:userlib
    ++  nule                                            ::  lines to unix cord
      ~%  %nule  ..part  ~
      |=  lin=(list @t)
      ^-  @t
      %+  can  3
      %+  turn  lin
      |=  t=@t
      [+((met 3 t)) (cat 3 t 10)]
    --
  ::                                                    ::
  ::::                    ++scanf:userlib               ::  (2uF) exterpolation
    ::                                                  ::::
  ++  scanf
    =<  |*  [tape (pole _;/(*[$^(rule tape)]))]         ::  formatted scan
        =>  .(+< [a b]=+<)
        (scan a (parsf b))
    |%
    ::                                                  ::  ++parsf:scanf:
    ++  parsf                                           ::  make parser from:
      |*  a=(pole _;/(*[$^(rule tape)]))                ::  ;"chars{rule}chars"
      =-  (cook - (boil (norm a)))
      |*  (list)
      ?~  +<  ~
      ?~  t  i
      [i $(+< t)]
    ::
    ::  .=  (boil ~[[& dim] [| ", "] [& dim]]:ag)
    ::  ;~(plug dim ;~(pfix com ace ;~(plug dim (easy)))):ag
    ::
    ::                                                  ::  ++boil:scanf:userlib
    ++  boil                                            ::
      |*  (list (each rule tape))
      ?~  +<  (easy ~)
      ?:  ?=(%| -.i)  ;~(pfix (jest (crip p.i)) $(+< t))
      %+  cook  |*([* *] [i t]=+<)
      ;~(plug p.i $(+< t))
    ::
    ::  .=  (norm [;"{n}, {n}"]:n=dim:ag)  ~[[& dim] [| ", "] [& dim]]:ag
    ::
    ::                                                  ::  ++norm:scanf:userlib
    ++  norm                                            ::
      |*  (pole _;/(*[$^(rule tape)]))
      ?~  +<  ~
      =>  .(+< [i=+<- t=+<+])
      :_  t=$(+< t)
      =+  rul=->->.i
      ^=  i
      ?~  rul     [%| p=rul]
      ?~  +.rul   [%| p=rul]
      ?@  &2.rul  [%| p=;;(tape rul)]
      [%& p=rul]
    --  ::scanf
  --
::  +harden: coerce %soft $hobo or pass-through
::
++  harden
  |*  task=mold
  |=  wrapped=(hobo task)
  ^-  task
  ?.  ?=(%soft -.wrapped)
    wrapped
  ;;(task +.wrapped)
::
::
++  balk
  =<  bulk
  !:
  |%
  +$  bulk
    $:  [her=ship rif=rift lyf=life]
        [van=@ta car=@ta cas=case]
        spr=spur
    ==
  ::
  ++  de-part
    |=  [=ship =rift =life =(pole knot)]
    ^-  (unit bulk)
    ?.  ?=([van=@ car=@ cas=@ spr=*] pole)  ~
    ?~  cas=(de-case cas.pole)   ~
    :-  ~
    :*  [ship rift life]
        [van.pole car.pole u.cas]
        spr.pole
    ==
  ::
  ++  de-path-soft
    |=  =(pole knot)
    ^-  (unit bulk)
    ::  [ship rift life vane care case path]
    ?.  ?=([her=@ rif=@ lyf=@ van=@ car=@ cas=@ spr=*] pole)
      ~
    ?~  her=(slaw %p her.pole)   ~
    ?~  rif=(slaw %ud rif.pole)  ~
    ?~  lyf=(slaw %ud lyf.pole)  ~
    ?~  cas=(de-case cas.pole)   ~
    :-  ~
    :*  [u.her u.rif u.lyf]
        [van.pole car.pole u.cas]
        spr.pole
    ==
  ::
  ++  de-path
    |=  =path
    ^-  bulk
    (need (de-path-soft +<))
  ::
  ++  en-path
    |=  =bulk
    ^-  path
    :*  (scot %p her.bulk)
        (scot %ud rif.bulk)
        (scot %ud lyf.bulk)
        van.bulk
        car.bulk
        (scot cas.bulk)
        spr.bulk
    ==
  ::
  ++  as-omen
    |=  =bulk
    ^-  omen
    =/  [des=desk pax=path]
      ?^  spr.bulk  spr.bulk
      [%$ ~]
    =/  bem=beam  =,(bulk [[her des cas] pax])
    =+  vis=(cat 3 van.bulk car.bulk)
    [vis bem]
  --
--
