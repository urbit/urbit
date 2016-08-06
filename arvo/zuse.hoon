:: 
::  zuse (3), standard library (tang)   
::
~%  %zuse  +  ~
!:
|%
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eP, diff (move me)           ::
::
::
++  udal                                                ::  atomic change (%b)
          $:  p/@ud                                     ::  blockwidth
              q/(list {p/@ud q/(unit {p/@ q/@})})       ::  indels
          ==                                            ::
++  udon                                                ::  abstract delta
          $:  p/umph                                    ::  preprocessor
              $=  q                                     ::  patch
              $%  {$a p/* q/*}                          ::  trivial replace
                  {$b p/udal}                           ::  atomic indel
                  {$c p/(urge)}                         ::  list indel
                  {$d p/upas q/upas}                    ::  tree edit
              ==                                        ::
          ==                                            ::
++  umph                                                ::  change filter
          $@  $?  $a                                    ::  no filter
                  $b                                    ::  jamfile
                  $c                                    ::  LF text
              ==                                        ::
          $%  {$d p/@ud}                                ::  blocklist
          ==                                            ::
++  unce  |*  a/mold                                    ::  change part
          $%  {$& p/@ud}                                ::  skip[copy]
              {$| p/(list a) q/(list a)}                ::  p -> q[chunk]
          ==                                            ::
++  upas                                                ::  tree change (%d)
          $^  {p/upas q/upas}                           ::  cell
          $%  {$0 p/axis}                               ::  copy old
              {$1 p/*}                                  ::  insert new
              {$2 p/axis q/udon}                        ::  mutate!
          ==                                            ::
++  urge  |*(a/mold (list (unce a)))                    ::  list change
::
++  berk                                                ::  invert diff patch
  |*  bur/(urge)
  |-  ^+  bur
  ?~  bur  ~
  :_  $(bur t.bur)
  ?-  -.i.bur
    $&  i.bur
    $|  [%| q.i.bur p.i.bur]
  ==
::
++  diff                                                ::  generate patch
  |=  pum/umph
  |=  {old/* new/*}  ^-  udon
  :-  pum
  ?+  pum  ~|(%unsupported !!)
    $a  [%d (nude old new)]
    $b  =+  [hel=(cue ((hard @) old)) hev=(cue ((hard @) new))]
        [%d (nude hel hev)]
    $c  =+  [hel=(lore ((hard @) old)) hev=(lore ((hard @) new))]
        [%c (lusk hel hev (loss hel hev))]
  ==
::
++  loss                                                ::  longest subsequence
  ~%  %loss  ..is  ~
  |*  {hel/(list) hev/(list)}
  |-  ^+  hev
  =+  ^=  sev
      =+  [inx=0 sev=*(map _i.-.hev (list @ud))]
      |-  ^+  sev
      ?~  hev  sev
      =+  guy=(~(get by sev) i.hev)
      $(hev t.hev, inx +(inx), sev (~(put by sev) i.hev [inx ?~(guy ~ u.guy)]))
  =|  gox/{p/@ud q/(map @ud {p/@ud q/_hev})}
  =<  abet
  =<  main
  |%
  ++  abet                                              ::  subsequence
    ^+  hev
    ?:  =(0 p.gox)  ~
    (flop q:(need (~(get by q.gox) (dec p.gox))))
  ::
  ++  hink                                              ::  extend fits top
    |=  {inx/@ud goy/@ud}  ^-  ?
    |(=(p.gox inx) (lth goy p:(need (~(get by q.gox) inx))))
  ::
  ++  lonk                                              ::  extend fits bottom
    |=  {inx/@ud goy/@ud}  ^-  ?
    |(=(0 inx) (gth goy p:(need (~(get by q.gox) (dec inx)))))
  ::
  ++  luna                                              ::  extend
    |=  {inx/@ud goy/@ud}
    ^+  +>
    %_    +>.$
        gox
      :-  ?:(=(inx p.gox) +(p.gox) p.gox)
      %+  ~(put by q.gox)  inx
      [goy (snag goy hev) ?:(=(0 inx) ~ q:(need (~(get by q.gox) (dec inx))))]
    ==
  ::
  ++  merg                                              ::  merge all matches
    |=  gay/(list @ud)
    ^+  +>
    =+  ^=  zes
        =+  [inx=0 zes=*(list {p/@ud q/@ud})]
        |-  ^+  zes
        ?:  |(?=($~ gay) (gth inx p.gox))  zes
        ?.  (lonk inx i.gay)  $(gay t.gay)
        ?.  (hink inx i.gay)  $(inx +(inx))
        $(inx +(inx), gay t.gay, zes [[inx i.gay] zes])
    |-  ^+  +>.^$
    ?~(zes +>.^$ $(zes t.zes, +>.^$ (luna i.zes)))
  ::
  ++  main
    =+  hol=hel
    |-  ^+  +>
    ?~  hol  +>
    =+  guy=(~(get by sev) i.hol)
    $(hol t.hol, +> (merg (flop `(list @ud)`?~(guy ~ u.guy))))
  --
::
++  lore                                                ::  atom to line list
  ~%  %lore  ..is  ~
  |=  lub/@
  =|  tez/(list @t)
  |-  ^+  tez
  =+  ^=  wor
    =+  [meg=0 i=0]
    |-  ^-  {meg/@ i/@ end/@f}
    =+  gam=(cut 3 [i 1] lub)
    ?:  =(0 gam)
      [meg i %.y]
    ?:  =(10 gam)
      [meg i %.n]
    $(meg (cat 3 meg gam), i +(i))
  ?:  end.wor
    (flop ^+(tez [meg.wor tez]))
  ?:  =(0 lub)  (flop tez)
  $(lub (rsh 3 +(i.wor) lub), tez [meg.wor tez])
::
++  role                                                ::  line list to atom
  |=  tez/(list @t)
  =|  {our/@ i/@ud}
  |-  ^-  @
    ?~  tez
      our
    ?:  =(%$ i.tez)
      $(i +(i), tez t.tez, our (cat 3 our 10))
    ?:  =(0 i)
      $(i +(i), tez t.tez, our i.tez)
    $(i +(i), tez t.tez, our (cat 3 (cat 3 our 10) i.tez))
::
++  lune                                                ::  cord by unix line
  ~%  %lune  ..is  ~
  |=  txt/@t
  ?~  txt
    ^-  (list @t)  ~
  =+  [byt=(rip 3 txt) len=(met 3 txt)]
  =|  {lin/(list @t) off/@}
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
::
++  nule                                                ::  lines to unix cord
  ~%  %nule  ..is  ~
  |=  lin/(list @t)
  ^-  @t
  %+  can  3
  %+  turn  lin
  |=  t/@t
  [+((met 3 t)) (cat 3 t 10)]
::
++  lump                                                ::  apply patch
  |=  {don/udon src/*}
  ^-  *
  ?+    p.don  ~|(%unsupported !!)
      $a
    ?+  -.q.don  ~|(%unsupported !!)
      $a  q.q.don
      $c  (lurk ((hard (list)) src) p.q.don)
      $d  (lure src p.q.don)
    ==
  ::
      $c
    =+  dst=(lore ((hard @) src))
    %-  role
    ?+  -.q.don  ~|(%unsupported !!)
      ::
      ::  XX  these hards should not be needed; udon needs parameterized
      ::
      $a  ((hard (list @t)) q.q.don)
      $c  ((hard (list @t)) (lurk `(list *)`dst p.q.don))
    ==
  ==
::
++  lure                                                ::  apply tree diff
  |=  {a/* b/upas}
  ^-  *
  ?^  -.b
    [$(b -.b) $(b +.b)]
  ?+  -.b  ~|(%unsupported !!)
    $0  .*(a [0 p.b])
    $1  .*(a [1 p.b])
  ==
++  limp                                                ::  invert patch
  |=  don/udon  ^-  udon
  :-  p.don
  ?+  -.q.don  ~|(%unsupported !!)
    $a  [%a q.q.don p.q.don]
    $c  [%c (berk p.q.don)]
    $d  [%d q.q.don p.q.don]
  ==
::
++  hump                                                ::  general prepatch
  |=  {pum/umph src/*}  ^-  *
  ?+  pum  ~|(%unsupported !!)
    $a  src
    $b  (cue ((hard @) src))
    $c  (lore ((hard @) src))
  ==
::
++  husk                                                ::  unprepatch
  |=  {pum/umph dst/*}  ^-  *
  ?+  pum  ~|(%unsupported !!)
    $a  dst
    $b  (jam dst)
    $c  (role ((hard (list @)) dst))
  ==
::
++  lurk                                                ::  apply list patch
  |*  {hel/(list) rug/(urge)}
  ^+  hel
  =+  war=`_hel`~
  |-  ^+  hel
  ?~  rug  (flop war)
  ?-    -.i.rug
      $&
    %=   $
      rug  t.rug
      hel  (slag p.i.rug hel)
      war  (weld (flop (scag p.i.rug hel)) war)
    ==
  ::
      $|
    %=  $
      rug  t.rug
      hel  =+  gur=(flop p.i.rug)
           |-  ^+  hel
           ?~  gur  hel
           ?>(&(?=(^ hel) =(i.gur i.hel)) $(hel t.hel, gur t.gur))
      war  (weld q.i.rug war)
    ==
  ==
::
++  lusk                                                ::  lcs to list patch
  |*  {hel/(list) hev/(list) lcs/(list)}
  =+  ^=  rag
      ^-  {$%({$& p/@ud} {$| p/_lcs q/_lcs})}      ::  XX translation
      [%& 0]
  =>  .(rag [p=rag q=*(list _rag)])
  =<  abet  =<  main
  |%
  ++  abet  =.(q.rag ?:(=([& 0] p.rag) q.rag [p.rag q.rag]) (flop q.rag))
  ++  done
    |=  new/_p.rag
    ^+  rag
    ?-  -.p.rag
      $|   ?-  -.new
            $|  [[%| (weld p.new p.p.rag) (weld q.new q.p.rag)] q.rag]
            $&  [new [p.rag q.rag]]
          ==
      $&   ?-  -.new
            $|  [new ?:(=(0 p.p.rag) q.rag [p.rag q.rag])]
            $&  [[%& (add p.p.rag p.new)] q.rag]
          ==
    ==
  ::
  ++  main
    |-  ^+  +
    ?~  hel
      ?~  hev
        ?>(?=($~ lcs) +)
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
  --
++  nude                                                ::  tree change
  =<  |=  {a/* b/*}  ^-  {p/upas q/upas}
      [p=(tred a b) q=(tred b a)]
  |%
  ++  axes                                              ::  locs of nouns
    |=  {a/@ b/*}  ^-  (map * axis)
    =+  c=*(map * axis)
    |-  ^-  (map * axis)
    =>  .(c (~(put by c) b a))
    ?@  b
      c
    %-  ~(uni by c)
    %-  ~(uni by $(a (mul 2 a), b -.b))
    $(a +((mul 2 a)), b +.b)
  ::
  ++  tred                                              ::  diff a->b
    |=  {a/* b/*}  ^-  upas
    =|  c/(unit *)
    =+  d=(axes 1 a)
    |-  ^-  upas
    =>  .(c (~(get by d) b))
    ?~  c
      ?@  b
        [%1 b]
      =+  e=^-(upas [$(b -.b) $(b +.b)])
      ?-  e
        {{$1 *} {$1 *}}  [%1 [p.p.e p.q.e]]
        *  e
      ==
    [%0 u.c]
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 3b, Arvo libraries            ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bA, lite number theory       ::
::
++  dope
  ~%  %dope  ..is   ~  
  |=  a/@
  ~&  [%dope-zuse (mug +>)]
  :(mul a a a)
::
++  fu                                                  ::  modulo (mul p q)
  |=  a/{p/@ q/@}
  =+  b=?:(=([0 0] a) 0 (~(inv fo p.a) (~(sit fo p.a) q.a)))
  |%
  ++  dif
    |=  {c/{@ @} d/{@ @}}
    [(~(dif fo p.a) -.c -.d) (~(dif fo q.a) +.c +.d)]
  ::
  ++  exp
    |=  {c/@ d/{@ @}}
    :-  (~(exp fo p.a) (mod c (dec p.a)) -.d)
    (~(exp fo q.a) (mod c (dec q.a)) +.d)
  ::
  ++  out                                               ::  garner's formula
    |=  c/{@ @}
    %+  add
      +.c
    (mul q.a (~(pro fo p.a) b (~(dif fo p.a) -.c (~(sit fo p.a) +.c))))
  ::
  ++  pro
    |=  {c/{@ @} d/{@ @}}
    [(~(pro fo p.a) -.c -.d) (~(pro fo q.a) +.c +.d)]
  ::
  ++  sum
    |=  {c/{@ @} d/{@ @}}
    [(~(sum fo p.a) -.c -.d) (~(sum fo q.a) +.c +.d)]
  ::
  ++  sit
    |=  c/@
    [(mod c p.a) (mod c q.a)]
  --
::
++  pram                                                ::  rabin-miller
  |=  a/@  ^-  ?
  ?:  ?|  =(0 (end 0 1 a))
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
      |-  ^-  {s/@ t/@}
      ?:  =(0 (end 0 1 s))
        $(s (rsh 0 1 s), t +(t))
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
::
++  ramp                                                ::  make r-m prime
  |=  {a/@ b/(list @) c/@}  ^-  @ux                     ::  {bits snags seed}
  =>  .(c (shas %ramp c))
  =+  d=*@
  |-
  ?:  =((mul 100 a) d)
    ~|(%ar-ramp !!)
  =+  e=(~(raw og c) a)
  ?:  &((levy b |=(f/@ !=(1 (mod e f)))) (pram e))
    e
  $(c +(c), d (shax d))
::
++  curt                                                ::  curve25519
  |=  {a/@ b/@}
  =>  %=    .
          +
        =>  +
        =+  =+  [p=486.662 q=(sub (bex 255) 19)]
            =+  fq=~(. fo q)
            [p=p q=q fq=fq]
        |%
        ++  cla
          |=  raw/@
          =+  low=(dis 248 (cut 3 [0 1] raw))
          =+  hih=(con 64 (dis 127 (cut 3 [31 1] raw)))
          =+  mid=(cut 3 [1 30] raw)
          (can 3 [[1 low] [30 mid] [1 hih] ~])
        ++  sqr  |=(a/@ (mul a a))
        ++  inv  |=(a/@ (~(exp fo q) (sub q 2) a))
        ++  cad
          |=  {n/{x/@ z/@} m/{x/@ z/@} d/{x/@ z/@}}
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
        ++  cub
          |=  {x/@ z/@}
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
    ++  norm  |=(x/@ ?:(=(0 (mod x 2)) x (sub q x)))
    ::
    ++  xrec                                            ::  recover x-coord
      |=  y/@  ^-  @
      =+  ^=  xx
          %+  mul  (dif.fq (mul y y) 1)
                   (inv.fq +(:(mul d y y)))
      =+  x=(exp.fq (div (add 3 q) 8) xx)
      ?:  !=(0 (dif.fq (mul x x) (sit.fq xx)))
        (norm (pro.fq x ii))
      (norm x)
    ::
    ++  ward                                            ::  edwards multiply
      |=  {pp/{@ @} qq/{@ @}}  ^-  {@ @}
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
      |=  {pp/{@ @} e/@}  ^-  {@ @}
      ?:  =(0 e)
        [0 1]
      =+  qq=$(e (div e 2))
      =>  .(qq (ward qq qq))
      ?:  =(1 (dis 1 e))
        (ward qq pp)
      qq
    ::
    ++  etch                                            ::  encode point
      |=  pp/{@ @}  ^-  @
      (can 0 ~[[(sub b 1) +.pp] [1 (dis 1 -.pp)]])
    ::
    ++  curv                                            ::  point on curve?
      |=  {x/@ y/@}  ^-  ?
      .=  0
          %+  dif.fq
            %+  sum.fq
              (pro.fq (sub q (sit.fq x)) x)
            (pro.fq y y)
          (sum.fq 1 :(pro.fq d x x y y))
    ::
    ++  deco                                            ::  decode point
      |=  s/@  ^-  (unit {@ @})
      =+  y=(cut 0 [0 (dec b)] s)
      =+  si=(cut 0 [(dec b) 1] s)
      =+  x=(xrec y)
      =>  .(x ?:(!=(si (dis 1 x)) (sub q x) x))
      =+  pp=[x y]
      ?.  (curv pp)
        ~
      [~ pp]
    ::
    ++  bb
      =+  bby=(pro.fq 4 (inv.fq 5))
      [(xrec bby) bby]
    ::
    --
  ~%  %ed  +  ~
  |%
  ++  puck                                              ::  public key
    ~/  %puck
    |=  sk/@I  ^-  @
    ?:  (gth (met 3 sk) 32)  !!
    =+  h=(shal (rsh 0 3 b) sk)
    =+  ^=  a
        %+  add
          (bex (sub b 2))
        (lsh 0 3 (cut 0 [3 (sub b 5)] h))
    =+  aa=(scam bb a)
    (etch aa)
  ++  suck                                              ::  keypair from seed
    |=  se/@I  ^-  @uJ
    =+  pu=(puck se)
    (can 0 ~[[b se] [b pu]])
  ::
  ++  shar                                              ::  curve25519 secret
    ~/  %shar
    |=  {pub/@ sek/@}
    ^-  @ux
    =+  exp=(shal (rsh 0 3 b) sek)
    =.  exp  (dis exp (can 0 ~[[3 0] [251 (fil 0 251 1)]]))
    =.  exp  (con exp (lsh 3 31 0b100.0000))
    =+  prv=(end 8 1 exp)
    =+  crv=(fra.fq (sum.fq 1 pub) (dif.fq 1 pub))
    (curt prv crv)
  ::
  ++  sign                                              ::  certify
    ~/  %sign
    |=  {m/@ se/@}  ^-  @
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
  ++  veri                                              ::  validate
    ~/  %veri
    |=  {s/@ m/@ pk/@}  ^-  ?
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
::
++  ga                                                  ::  GF (bex p.a)
  |=  a/{p/@ q/@ r/@}                                   ::  dim poly gen
  =+  si=(bex p.a)
  =+  ma=(dec si)
  =>  |%
      ++  dif                                           ::  add and sub
        |=  {b/@ c/@}
        ~|  [%dif-ga a]
        ?>  &((lth b si) (lth c si))
        (mix b c)
      ::
      ++  dub                                           ::  mul by x
        |=  b/@
        ~|  [%dub-ga a]
        ?>  (lth b si)
        ?:  =(1 (cut 0 [(dec p.a) 1] b))
          (dif (sit q.a) (sit (lsh 0 1 b)))
        (lsh 0 1 b)
      ::
      ++  pro                                           ::  slow multiply
        |=  {b/@ c/@}
        ?:  =(0 b)
          0
        ?:  =(1 (dis 1 b))
          (dif c $(b (rsh 0 1 b), c (dub c)))
        $(b (rsh 0 1 b), c (dub c))
      ::
      ++  toe                                           ::  exp+log tables
        =+  ^=  nu
            |=  {b/@ c/@}
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
        |-  ^-  {p/(map @ @) q/(map @ @)}
        ?:  =(ma c)
          [(~(put by p) c b) q]
        %=  $
          b  (pro r.a b)
          c  +(c)
          p  (~(put by p) c b)
          q  (~(put by q) b c)
        ==
      ::
      ++  sit                                           ::  reduce
        |=  b/@
        (mod b (bex p.a))
      --
  =+  toe
  |%
  ++  fra                                               ::  divide
    |=  {b/@ c/@}
    (pro b (inv c))
  ::
  ++  inv                                               ::  invert
    |=  b/@
    ~|  [%inv-ga a]
    =+  c=(~(get by q) b)
    ?~  c  !!
    =+  d=(~(get by p) (sub ma u.c))
    (need d)
  ::
  ++  pow                                               ::  exponent
    |=  {b/@ c/@}
    =+  [d=1 e=c f=0]
    |-
    ?:  =(p.a f)
      d
    ?:  =(1 (cut 0 [f 1] b))
      $(d (pro d e), e (pro e e), f +(f))
    $(e (pro e e), f +(f))
  ::
  ++  pro                                               ::  multiply
    |=  {b/@ c/@}
    ~|  [%pro-ga a]
    =+  d=(~(get by q) b)
    ?~  d  0
    =+  e=(~(get by q) c)
    ?~  e  0
    =+  f=(~(get by p) (mod (add u.d u.e) ma))
    (need f)
  --
::
++  scr                                                 ::  scrypt
  ~%  %scr  +  ~
  |%
  ++  sal
    |=  {x/@ r/@}                                       ::  salsa20 hash
    ?>  =((mod r 2) 0)                                  ::  with r rounds
    =+  few==>(fe .(a 5))
    =+  ^=  rot
      |=  {a/@ b/@}
      (mix (end 5 1 (lsh 0 a b)) (rsh 0 (sub 32 a) b))
    =+  ^=  lea
      |=  {a/@ b/@}
      (net:few (sum:few (net:few a) (net:few b)))
    =>  |%
        ++  qr                                          ::  quarterround
          |=  y/{@ @ @ @ $~}
          =+  zb=(mix &2.y (rot 7 (sum:few &1.y &4.y)))
          =+  zc=(mix &3.y (rot 9 (sum:few zb &1.y)))
          =+  zd=(mix &4.y (rot 13 (sum:few zc zb)))
          =+  za=(mix &1.y (rot 18 (sum:few zd zc)))
          ~[za zb zc zd]
        ++  rr                                          ::  rowround
          |=  {y/(list @)}
          =+  za=(qr ~[&1.y &2.y &3.y &4.y])
          =+  zb=(qr ~[&6.y &7.y &8.y &5.y])
          =+  zc=(qr ~[&11.y &12.y &9.y &10.y])
          =+  zd=(qr ~[&16.y &13.y &14.y &15.y])
          ^-  (list @)  :~
            &1.za  &2.za  &3.za  &4.za
            &4.zb  &1.zb  &2.zb  &3.zb 
            &3.zc  &4.zc  &1.zc  &2.zc
            &2.zd  &3.zd  &4.zd  &1.zd  ==
        ++  cr                                          ::  columnround
          |=  {x/(list @)}
          =+  ya=(qr ~[&1.x &5.x &9.x &13.x])
          =+  yb=(qr ~[&6.x &10.x &14.x &2.x])
          =+  yc=(qr ~[&11.x &15.x &3.x &7.x])
          =+  yd=(qr ~[&16.x &4.x &8.x &12.x])
          ^-  (list @)  :~
            &1.ya  &4.yb  &3.yc  &2.yd
            &2.ya  &1.yb  &4.yc  &3.yd
            &3.ya  &2.yb  &1.yc  &4.yd
            &4.ya  &3.yb  &2.yc  &1.yd  ==
        ++  dr                                          ::  doubleround
          |=  {x/(list @)}
          (rr (cr x))
        ++  al                                          ::  add two lists
          |=  {a/(list @) b/(list @)}
          |-  ^-  (list @)
          ?~  a  ~  ?~  b  ~
          [i=(sum:few -.a -.b) t=$(a +.a, b +.b)]
        --
    =+  xw=(rpp 5 16 x)
    =+  ^=  ow  |-  ^-  (list @)
                ?~  r  xw
                $(xw (dr xw), r (sub r 2))
    (rep 5 (al xw ow))
  ::
  ++  rpp
    |=  {a/bloq b/@ c/@}                                ::  rip w+filler blocks
    =+  q=(rip a c)
    =+  w=(lent q)
    ?.  =(w b)
      ?.  (lth w b)  (slag (sub w b) q)
      ^+  q  (weld q (reap (sub b (lent q)) 0))
    q
  ::
  ++  bls
    |=  {a/@ b/(list @)}                                ::  split to sublists
    ?>  =((mod (lent b) a) 0)
    |-  ^-  (list (list @))
    ?~  b  ~
    [i=(scag a `(list @)`b) t=$(b (slag a `(list @)`b))]
  ::
  ++  slb
    |=  {a/(list (list @))}
    |-  ^-  (list @)
    ?~  a  ~
    (weld `(list @)`-.a $(a +.a))
  ::
  ++  sbm
    |=  {r/@ b/(list @)}                                ::  scryptBlockMix
    ?>  =((lent b) (mul 2 r))
    =+  [x=(snag (dec (mul 2 r)) b) c=0]
    =|  {ya/(list @) yb/(list @)}
    |-  ^-  (list @)
    ?~  b  (flop (weld yb ya))
    =.  x  (sal (mix x -.b) 8)
    ?~  (mod c 2)
      $(c +(c), b +.b, ya [i=x t=ya])
    $(c +(c), b +.b, yb [i=x t=yb])
  ::
  ++  srm
    |=  {r/@ b/(list @) n/@}                            ::  scryptROMix
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
  ::
  ++  hmc
    |=  {k/@ t/@}                                       ::  HMAC-SHA-256
    (hml k (met 3 k) t (met 3 t))
  ::
  ++  hml
    |=  {k/@ kl/@ t/@ tl/@}                             ::  w+length
    =>  .(k (end 3 kl k), t (end 3 tl t))
    =+  b=64
    =.  k  ?.  (gth kl b)  k  (shay kl k)
    =+  ^=  q  %+  shay  (add b tl)
     (add (lsh 3 b t) (mix k (fil 3 b 0x36)))
    %+  shay  (add b 32)
    (add (lsh 3 b q) (mix k (fil 3 b 0x5c)))
  ::
  ++  pbk                                               :: PBKDF2-HMAC-SHA256
    ~/  %pbk
    |=  {p/@ s/@ c/@ d/@}
    (pbl p (met 3 p) s (met 3 s) c d)
  ::
  ++  pbl                                               :: w+length
    ~/  %pbl
    |=  {p/@ pl/@ s/@ sl/@ c/@ d/@}
    =>  .(p (end 3 pl p), s (end 3 sl s))
    =+  h=32
    ?>  ?&  (lte d (bex 30))                            :: max key length 1GB
            (lte c (bex 28))                            :: max iterations 2^28
            !=(c 0)
        ==
    =+  ^=  l  ?~  (mod d h)
        (div d h)
      +((div d h))
    =+  r=(sub d (mul h (dec l)))
    =+  [t=0 j=1 k=1]
    =.  t  |-  ^-  @
      ?:  (gth j l)  t
      =+  u=(add s (lsh 3 sl (rep 3 (flop (rpp 3 4 j)))))
      =+  f=0  =.  f  |-  ^-  @
        ?:  (gth k c)  f
        =+  q=(hml p pl u ?:(=(k 1) (add sl 4) h))
        $(u q, f (mix f q), k +(k))
      $(t (add t (lsh 3 (mul (dec j) h) f)), j +(j))
    (end 3 d t)
  ::
  ++  hsh                                               ::  scrypt
    ~/  %hsh
    |=  {p/@ s/@ n/@ r/@ z/@ d/@}
    (hsl p (met 3 p) s (met 3 s) n r z d)
  ::
  ++  hsl                                               ::  w+length
    ~/  %hsl
    |=  {p/@ pl/@ s/@ sl/@ n/@ r/@ z/@ d/@}
    =|  v/(list (list @))
    =>  .(p (end 3 pl p), s (end 3 sl s))
    =+  u=(mul (mul 128 r) z)
    ?>  ?&  =(n (bex (dec (xeb n))))                    ::  n is power of 2
            !=(r 0)  !=(z 0)
            %+  lte                                     ::  max 1GB memory
                (mul (mul 128 r) (dec (add n z)))
              (bex 30)
            (lth pl (bex 31))
            (lth sl (bex 31))
        ==
    =+  ^=  b  =+  (rpp 3 u (pbl p pl s sl 1 u))
      %+  turn  (bls (mul 128 r) -)
      |=(a/(list @) (rpp 9 (mul 2 r) (rep 3 a)))
    ?>  =((lent b) z)
    =+  ^=  q
      =+  |-  ?~  b  (flop v)
          $(b +.b, v [i=(srm r -.b n) t=v])
      %+  turn  `(list (list @))`-
      |=(a/(list @) (rpp 3 (mul 128 r) (rep 9 a)))
    (pbl p pl (rep 3 (slb q)) u 1 d)
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bB, crypto                   ::
::
++  aes
  ~%  %aes  ..is  ~
  |%
  ++  ahem                                              ::  AES engine
    |=  {nnk/@ nnb/@ nnr/@}
    =>
      =+  =>  [gr=(ga 8 0x11b 3) few==>(fe .(a 5))]
          [pro=pro.gr dif=dif.gr pow=pow.gr ror=ror.few]
      =>  |%
          ++  cipa                                      ::  AES params
            $_  ^?  |%
            ++  co  *{p/@ q/@ r/@ s/@}                  ::  col coefs
            ++  ix  |~(a/@ *@)                          ::  key index
            ++  ro  *{p/@ q/@ r/@ s/@}                  ::  row shifts
            ++  su  *@                                  ::  s-box
            --
          --
      |%
      ++  pen                                           ::  encrypt
        ^-  cipa
        |%
        ++  co  [0x2 0x3 1 1]
        ++  ix  |~(a/@ a)
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
      ++  pin                                           :: decrypt
        ^-  cipa
        |%
        ++  co  [0xe 0xb 0xd 0x9]
        ++  ix  |~(a/@ (sub nnr a))
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
        |=  {a/(list @) b/{p/@ q/@ r/@ s/@}}  ^-  (list @)
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
        |=  {a/{@ @} b/{@ @} c/{@ @} d/{@ @}}
        :(dif (pro a) (pro b) (pro c) (pro d))
      ::
      ++  pode                                          ::  explode to block
        |=  {a/bloq b/@ c/@}  ^-  (list @)
        =+  d=(rip a c)
        =+  m=(met a c)
        |-
        ?:  =(m b)
          d
        $(m +(m), d (weld d (limo [0 ~])))
      ++  sube                                          ::  s-box word
        |=  {a/@ b/@}  ^-  @
        (rep 3 (turn (pode 3 4 a) |=(c/@ (cut 3 [c 1] b))))
      --
    |%
    ++  be                                              ::  block cipher
      |=  {a/? b/@ c/@H}  ^-  @uxH
      ~|  %be-aesc
      =>  %=    .
              +
            =>  +
            |%
            ++  ankh
              |=  {a/cipa b/@ c/@}
              (pode 5 nnb (cut 5 [(mul (ix.a b) nnb) nnb] c))
            ++  sark
              |=  {c/(list @) d/(list @)}  ^-  (list @)
              ?~  c  ~
              ?~  d  !!
              [(mix i.c i.d) $(c t.c, d t.d)]
            ++  srow
              |=  {a/cipa b/(list @)}  ^-  (list @)
              =+  [c=0 d=~ e=ro.a]
              |-
              ?:  =(c nnb)
                d
              :_  $(c +(c))
              %+  rep  3
              %+  turn
                (limo [0 p.e] [1 q.e] [2 r.e] [3 s.e] ~)
              |=  {f/@ g/@}
              (cut 3 [f 1] (snag (mod (add g c) nnb) b))
            ++  subs
              |=  {a/cipa b/(list @)}  ^-  (list @)
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
    ::
    ++  ex                                              ::  key expand
      |=  a/@I  ^-  @
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
    ::
    ++  ix                                              ::  key expand, inv
      |=  a/@  ^-  @
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
  ::
  ++  ecba                                              ::  AES-128 ECB
    ~%  %ecba  +>+  ~
    =+  (ahem 4 4 10)
    |_  key/@H
    ++  en
      ~/  %en
      |=  blk/@H  ^-  @uxH
      =:
        key  (~(net fe 7) key)
        blk  (~(net fe 7) blk)
      ==
      %-  ~(net fe 7)
      (be & (ex key) blk)
    ++  de
      ~/  %de
      |=  blk/@H  ^-  @uxH
      =:
        key  (~(net fe 7) key)
        blk  (~(net fe 7) blk)
      ==
      %-  ~(net fe 7)
      (be | (ix (ex key)) blk)
    --
  ::
  ++  ecbb                                              ::  AES-192 ECB
    ~%  %ecbb  +>+  ~
    =+  (ahem 6 4 12)
    |_  key/@I
    ++  en
      ~/  %en
      |=  blk/@H  ^-  @uxH
      =:
        key  (rsh 6 1 (~(net fe 8) key))
        blk  (~(net fe 7) blk)
      ==
      %-  ~(net fe 7)
      (be & (ex key) blk)
    ++  de
      ~/  %de
      |=  blk/@H  ^-  @uxH
      =:
        key  (rsh 6 1 (~(net fe 8) key))
        blk  (~(net fe 7) blk)
      ==
      %-  ~(net fe 7)
      (be | (ix (ex key)) blk)
    --
  ::
  ++  ecbc                                              ::  AES-256 ECB
    ~%  %ecbc  +>+  ~
    =+  (ahem 8 4 14)
    |_  key/@I
    ++  en
      ~/  %en
      |=  blk/@H  ^-  @uxH
      =:
        key  (~(net fe 8) key)
        blk  (~(net fe 7) blk)
      ==
      %-  ~(net fe 7)
      (be & (ex key) blk)
    ++  de
      ~/  %de
      |=  blk/@H  ^-  @uxH
      =:
        key  (~(net fe 8) key)
        blk  (~(net fe 7) blk)
      ==
      %-  ~(net fe 7)
      (be | (ix (ex key)) blk)
    --
  ::
  ++  cbca                                              ::  AES-128 CBC
    ~%  %cbca  +>  ~
    |_  {key/@H prv/@H}
    ++  en
      ~/  %en
      |=  txt/@  ^-  @ux
      =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
      =|  cts/(list @)
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
    ++  de
      ~/  %de
      |=  txt/@  ^-  @ux
      =+  cts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
      =|  pts/(list @)
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
    --
  ::
  ++  cbcb                                              ::  AES-192 CBC
    ~%  %cbcb  +>  ~
    |_  {key/@I prv/@H}
    ++  en
      ~/  %en
      |=  txt/@  ^-  @ux
      =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
      =|  cts/(list @)
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
    ++  de
      ~/  %de
      |=  txt/@  ^-  @ux
      =+  cts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
      =|  pts/(list @)
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
    --
  ::
  ++  cbcc                                              ::  AES-256 CBC
    ~%  %cbcc  +>  ~
    |_  {key/@I prv/@H}
    ++  en
      ~/  %en
      |=  txt/@  ^-  @ux
      =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
      =|  cts/(list @)
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
    ++  de
      ~/  %de
      |=  txt/@  ^-  @ux
      =+  cts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
      =|  pts/(list @)
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
    --
  ::
  ++  inc                                               ::  inc. low bloq
    |=  {mod/bloq ctr/@H}
    ^-  @uxH
    =+  bqs=(rip mod ctr)
    ?~  bqs  0x1
    %+  rep  mod
    [(~(sum fe mod) i.bqs 1) t.bqs]
  ::
  ++  ctra                                              ::  AES-128 CTR
    ~%  %ctra  +>  ~
    |_  {key/@H mod/bloq ctr/@H}
    ++  en
      ~/  %en
      |=  txt/@  ^-  @ux
      =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 3 txt)))
      =|  cts/(list @)
      =+  str=(flop (rip 3 (~(en ecba key) ctr)))
      %+  rep  3
      ::  logically, flop twice here
      |-  ^-  (list @)
      ?~  pts
        cts
      ?~  str
        =+  nctr=(inc mod ctr)
        $(str (flop (rip 3 (~(en ecba key) nctr))), ctr nctr)
      %=  $
        cts  :_  cts
             (mix i.str i.pts)
        str  t.str
        pts  t.pts
      ==
    ++  de  en
    --
  ::
  ++  ctrb                                              ::  AES-192 CTR
    ~%  %ctrb  +>  ~
    |_  {key/@I mod/bloq ctr/@H}
    ++  en
      ~/  %en
      |=  txt/@  ^-  @ux
      =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 3 txt)))
      =|  cts/(list @)
      =+  str=(flop (rip 3 (~(en ecbb key) ctr)))
      %+  rep  3
      ::  logically, flop twice here
      |-  ^-  (list @)
      ?~  pts
        cts
      ?~  str
        =+  nctr=(inc mod ctr)
        $(str (flop (rip 3 (~(en ecbb key) nctr))), ctr nctr)
      %=  $
        cts  :_  cts
             (mix i.str i.pts)
        str  t.str
        pts  t.pts
      ==
    ++  de  en
    --
  ::
  ++  ctrc                                              ::  AES-256 CTR
    ~%  %ctrc  +>  ~
    |_  {key/@I mod/bloq ctr/@H}
    ++  en
      ~/  %en
      |=  txt/@  ^-  @ux
      =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 3 txt)))
      =|  cts/(list @)
      =+  str=(flop (rip 3 (~(en ecbc key) ctr)))
      %+  rep  3
      ::  logically, flop twice here
      |-  ^-  (list @)
      ?~  pts
        cts
      ?~  str
        =+  nctr=(inc mod ctr)
        $(str (flop (rip 3 (~(en ecbc key) nctr))), ctr nctr)
      %=  $
        cts  :_  cts
             (mix i.str i.pts)
        str  t.str
        pts  t.pts
      ==
    ++  de  en
    --
  ::
  ++  doub                                              ::  double 128-bit
    |=  str/@H                                          ::  string mod finite
    ^-  @uxH                                            ::  field (see spec)
    %-  ~(sit fe 7)
    ?.  =((xeb str) 128)
      (lsh 0 1 str)
    (mix 0x87 (lsh 0 1 str))
  ::
  ++  mpad                ::  pad message to multiple of 128 bits
    |=  {oct/@ txt/@}     ::  by appending 1, then 0s
    ^-  @ux               ::  the spec is unclear, but it must be octet based
    =+  pad=(mod oct 16)  ::  to match the test vectors
    ?:  =(pad 0)  0x8000.0000.0000.0000.0000.0000.0000.0000
    (lsh 3 (sub 15 pad) (mix 0x80 (lsh 3 1 txt)))
  ::
  ++  suba                                              ::  AES-128 subkeys
    |=  key/@H
    =+  l=(~(en ecba key) 0)
    =+  k1=(doub l)
    =+  k2=(doub k1)
    ^-  {@ux @ux}
    [k1 k2]
  ::
  ++  subb                                              ::  AES-192 subkeys
    |=  key/@I
    =+  l=(~(en ecbb key) 0)
    =+  k1=(doub l)
    =+  k2=(doub k1)
    ^-  {@ux @ux}
    [k1 k2]
  ::
  ++  subc                                              ::  AES-256 subkeys
    |=  key/@I
    =+  l=(~(en ecbc key) 0)
    =+  k1=(doub l)
    =+  k2=(doub k1)
    ^-  {@ux @ux}
    [k1 k2]
  ::
  ++  maca                                              :: AES-128 CMAC
    ~/  %maca
    |=  {key/@H oct/(unit @) txt/@}
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
        $&  -.sub
        $|  +.sub
      ==
    (~(sit fe 7) mac)  ::  spec says MSBs, LSBs match test vectors
  ::
  ++  macb                                              :: AES-192 CMAC
    ~/  %macb
    |=  {key/@I oct/(unit @) txt/@}
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
        $&  -.sub
        $|  +.sub
      ==
    (~(sit fe 7) mac)  ::  spec says MSBs, LSBs match test vectors
  ::
  ++  macc                                              :: AES-256 CMAC
    ~/  %macc
    |=  {key/@I oct/(unit @) txt/@}
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
        $&  -.sub
        $|  +.sub
      ==
    (~(sit fe 7) mac)  ::  spec says MSBs, LSBs match test vectors
  ::
  ++  s2va                                              ::  AES-128 S2V
    ~/  %s2va
    |=  {key/@H ads/(list @)}
    =+  res=(maca key `16 0x0)
    %^  maca  key  ~
    |-  ^-  @uxH
    ?~  ads  (maca key `16 0x1)
    ?~  t.ads
      ?:  (gte (xeb i.ads) 128)
        (mix i.ads res)
      %+  mix
        (doub res)
        (mpad (met 3 i.ads) i.ads)
    %=  $
      res  %+  mix
             (doub res)
             (maca key ~ i.ads)
      ads  t.ads
    ==
  ::
  ++  s2vb                                              ::  AES-192 S2V
    ~/  %s2vb
    |=  {key/@I ads/(list @)}
    =+  res=(macb key `16 0x0)
    %^  macb  key  ~
    |-  ^-  @uxH
    ?~  ads  (macb key `16 0x1)
    ?~  t.ads
      ?:  (gte (xeb i.ads) 128)
        (mix i.ads res)
      %+  mix
        (doub res)
        (mpad (met 3 i.ads) i.ads)
    %=  $
      res  %+  mix
             (doub res)
             (macb key ~ i.ads)
      ads  t.ads
    ==
  ::
  ++  s2vc                                              ::  AES-256 S2V
    ~/  %s2vc
    |=  {key/@I ads/(list @)}
    =+  res=(macc key `16 0x0)
    %^  macc  key  ~
    |-  ^-  @uxH
    ?~  ads  (macc key `16 0x1)
    ?~  t.ads
      ?:  (gte (xeb i.ads) 128)
        (mix i.ads res)
      %+  mix
        (doub res)
        (mpad (met 3 i.ads) i.ads)
    %=  $
      res  %+  mix
             (doub res)
             (macc key ~ i.ads)
      ads  t.ads
    ==
  ::
  ++  siva                                              ::  AES-128 SIV
    ~%  %siva  +>  ~
    |_  {key/@I vec/(list @)}
    ++  en
      ~/  %en
      |=  txt/@
      ^-  {@uxH @ux}
      =+  [k1=(rsh 7 1 key) k2=(end 7 1 key)]
      =+  iv=(s2va k1 (weld vec (limo ~[txt])))
      :-
        iv
      (~(en ctra k2 7 (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)) txt)
    ++  de
      ~/  %de
      |=  {iv/@H txt/@}
      ^-  (unit @ux)
      =+  [k1=(rsh 7 1 key) k2=(end 7 1 key)]
      =+  ^=  pln
        (~(de ctra k2 7 (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)) txt)
      ?.  =((s2va k1 (weld vec (limo ~[pln]))) iv)
        ~
      `pln
    --
  ::
  ++  sivb                                              ::  AES-192 SIV
    ~%  %sivb  +>  ~
    |_  {key/@J vec/(list @)}
    ++  en
      ~/  %en
      |=  txt/@
      ^-  {@uxH @ux}
      =+  [k1=(rsh 5 3 key) k2=(end 5 3 key)]
      =+  iv=(s2vb k1 (weld vec (limo ~[txt])))
      :-
        iv
      (~(en ctrb k2 7 (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)) txt)
    ++  de
      ~/  %de
      |=  {iv/@H txt/@}
      ^-  (unit @ux)
      =+  [k1=(rsh 5 3 key) k2=(end 5 3 key)]
      =+  ^=  pln
        (~(de ctrb k2 7 (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)) txt)
      ?.  =((s2vb k1 (weld vec (limo ~[pln]))) iv)
        ~
      `pln
    --
  ::
  ++  sivc                                              ::  AES-256 SIV
    ~%  %sivc  +>  ~
    |_  {key/@J vec/(list @)}
    ++  en
      ~/  %en
      |=  txt/@
      ^-  {@uxH @ux}
      =+  [k1=(rsh 8 1 key) k2=(end 8 1 key)]
      =+  iv=(s2vc k1 (weld vec (limo ~[txt])))
      :-
        iv
      (~(en ctrc k2 7 (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)) txt)
    ++  de
      ~/  %de
      |=  {iv/@H txt/@}
      ^-  (unit @ux)
      =+  [k1=(rsh 8 1 key) k2=(end 8 1 key)]
      =+  ^=  pln
        (~(de ctrc k2 7 (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)) txt)
      ?.  =((s2vc k1 (weld vec (limo ~[pln]))) iv)
        ~
      `pln
    --
  --
++  crua  !:                                            ::  cryptosuite A (RSA)
  ^-  acru
  =|  {mos/@ pon/(unit {p/@ q/@ r/{p/@ q/@} s/_*fu})}
  =>  |%
      ++  mx  (dec (met 0 mos))                         ::  bit length
      ++  dap                                           ::  OEAP decode
        |=  {wid/@ xar/@ dog/@}  ^-  {p/@ q/@}
        =+  pav=(sub wid xar)
        =+  qoy=(cut 0 [xar pav] dog)
        =+  dez=(mix (end 0 xar dog) (shaw %pad-b xar qoy))
        [dez (mix qoy (shaw %pad-a pav dez))]
      ::
      ++  pad                                           ::  OEAP encode
        |=  {wid/@ rax/{p/@ q/@} meg/@}  ^-  @
        =+  pav=(sub wid p.rax)
        ?>  (gte pav (met 0 meg))
        ^-  @
        =+  qoy=(mix meg (shaw %pad-a pav q.rax))
        =+  dez=(mix q.rax (shaw %pad-b p.rax qoy))
        (can 0 [p.rax dez] [pav qoy] ~)
      ::
      ++  pull  |=(a/@ (~(exp fo mos) 3 a))
      ++  push  |=(a/@ (~(exp fo mos) 5 a))
      ++  pump
        |=  a/@  ^-  @
        ?~  pon  !!
        (out.s.u.pon (exp.s.u.pon p.r.u.pon (sit.s.u.pon a)))
      ::
      ++  punt
        |=  a/@  ^-  @
        ?~  pon  !!
        (out.s.u.pon (exp.s.u.pon q.r.u.pon (sit.s.u.pon a)))
      --
  |%                                                    ::  opaque object
  ++  as
    =>  |%
        ++  haul                                        ::  revealing haul
          |=  a/pass
          =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
          ?>  =('a' mag)
          ..as(mos bod, pon ~)
        --
    ^?
    |%  ++  seal
          |=  {a/pass b/@ c/@}
          ^-  @
          =>  .(c (sign b c))
          =+  her=(haul a)
          =+  det=(lte (add 256 (met 0 c)) mx.her)
          =+  lip=?:(det c 0)
          =-  (add ?:(p.mav 0 1) (lsh 0 1 q.mav))
          ^=  mav  ^-  {p/? q/@}
          :-  det
          =+  dog=(pad mx.her [256 b] lip)
          =+  hog=(push.her dog)
          =+  ben=(en b c)
          ?:(det hog (jam hog ben))
        ++  sign
          |=  {a/@ b/@}  ^-  @
          =-  (add ?:(p.mav 0 1) (lsh 0 1 q.mav))
          ^=  mav  ^-  {p/? q/@}
          =+  det=(lte (add 128 (met 0 b)) mx)
          :-  det
          =+  hec=(shaf (mix %agis a) b)
          =+  dog=(pad mx [128 hec] ?:(det b 0))
          =+  hog=(pump dog)
          ?:(det hog (jam hog b))
        ++  sure
          |=  {a/@ b/@}
          ^-  (unit @)
          =+  [det==(0 (end 0 1 b)) bod=(rsh 0 1 b)]
          =+  gox=?:(det [p=bod q=0] ((hard {p/@ q/@}) (cue bod)))
          =+  dog=(pull p.gox)
          =+  pig=(dap mx 128 dog)
          =+  log=?:(det q.pig q.gox)
          ?.(=(p.pig (shaf (mix %agis a) log)) ~ [~ log])
        ++  tear
          |=  {a/pass b/@}
          ^-  (unit {p/@ q/@})
          =+  her=(haul a)
          =+  [det==(0 (end 0 1 b)) bod=(rsh 0 1 b)]
          =+  gox=?:(det [p=bod q=0] ((hard {p/@ q/@}) (cue bod)))
          =+  dog=(punt p.gox)
          =+  pig=(dap mx 256 dog)
          =+  ^=  cow
              ^-  (unit @)
              ?:  det
                [~ q.pig]
              (de p.pig q.gox)
          ?~  cow  ~
          =>  .(cow (sure:as.her p.pig u.cow))
          ?~  cow  ~
          [~ p.pig u.cow]
    --
  ::
  ++  de
    |~  {key/@ cep/@}  ^-  (unit @)
    =+  toh=(met 8 cep)
    ?:  (lth toh 2)
      ~
    =+  adj=(dec toh)
    =+  [hax=(end 8 1 cep) bod=(rsh 8 1 cep)]
    =+  msg=(mix (~(raw og (mix hax key)) (mul 256 adj)) bod)
    ?.  =(hax (shax (mix key (shax (mix adj msg)))))
      ~
    [~ msg]
  ::
  ++  dy  |~({a/@ b/@} (need (de a b)))
  ++  en
    |~  {key/@ msg/@}  ^-  @ux
    =+  len=(met 8 msg)
    =+  adj=?:(=(0 len) 1 len)
    =+  hax=(shax (mix key (shax (mix adj msg))))
    (rap 8 hax (mix msg (~(raw og (mix hax key)) (mul 256 adj))) ~)
  ::
  ++  ex  ^?
    |%  ++  fig  ^-  @uvH  (shaf %afig mos)
        ++  pac  ^-  @uvG  (end 6 1 (shaf %acod sec))
        ++  pub  ^-  pass  (cat 3 'a' mos)
        ++  sec  ^-  ring  ?~(pon !! (cat 3 'A' (jam p.u.pon q.u.pon)))
    --
  ::
  ++  nu
    =>  |%
        ++  elcm
          |=  {a/@ b/@}
          (div (mul a b) d:(egcd a b))
        ::
        ++  eldm
          |=  {a/@ b/@ c/@}
          (~(inv fo (elcm (dec b) (dec c))) a)
        ::
        ++  ersa
          |=  {a/@ b/@}
          [a b [(eldm 3 a b) (eldm 5 a b)] (fu a b)]
        --
    ^?
    |%  ++  com
          |=  a/@
          ^+  ^?(..nu)
          ..nu(mos a, pon ~)
        ::
        ++  pit
          |=  {a/@ b/@}
          =+  c=(rsh 0 1 a)
          =+  [d=(ramp c [3 5 ~] b) e=(ramp c [3 5 ~] +(b))]
          ^+  ^?(..nu)
          ..nu(mos (mul d e), pon [~ (ersa d e)])
        ::
        ++  nol
          |=  a/@
          ^+  ^?(..nu)
          =+  b=((hard {p/@ q/@}) (cue a))
          ..nu(mos (mul p.b q.b), pon [~ (ersa p.b q.b)])
    --
  --                                                    ::
:: 
++  bruw                                                ::  create keypair
  |=  {a/@ b/@}                                         ::  width seed
  ^-  acru
  (pit:nu:crua a b)
::
++  haul                                                ::  activate public key
  |=  a/pass
  ^-  acru
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('a' mag)
  (com:nu:crua bod)
::
++  weur                                                ::  activate secret key
  |=  a/ring
  ^-  acru
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('A' mag)
  (nol:nu:crua bod)
::
++  trua                                                ::  test rsa
  |=  msg/@tas
  ^-  @
  =+  ali=(bruw 1.024 (shax 'ali'))
  =+  bob=(bruw 1.024 (shax 'bob'))
  =+  tef=(sign:as.ali [0 msg])
  =+  lov=(sure:as.ali [0 tef])
  ?.  &(?=(^ lov) =(msg u.lov))
    ~|(%test-fail-sign !!)
  =+  key=(shax (shax (shax msg)))
  =+  sax=(seal:as.ali pub:ex.bob key msg)
  =+  tin=(tear:as.bob pub:ex.ali sax)
  ?.  &(?=(^ tin) =(key p.u.tin) =(msg q.u.tin))
    ~|(%test-fail-seal !!)
  msg
::
++  crub                                                ::  cryptosuite B (Ed)
  ^-  acru
  =|  {pub/{cry/@ sgn/@} sek/(unit {cry/@ sgn/@})}
  |%
  ++  as
    |%
    ++  sign
      |=  {nonc/@ msg/@}
      ^-  @ux
      ?~  sek  ~|  %pubkey-only  !!
      =+  nms=(jam [nonc msg])
      (jam [(sign:ed nms sgn.u.sek) nms])
    ++  sure
      |=  {nonc/@ txt/@}
      ^-  (unit @ux)
      =+  ((hard {sig/@ nms/@}) (cue txt))
      ?.  (veri:ed sig nms sgn.pub)  ~
      =+  ((hard {n/@ msg/@}) (cue nms))
      ?.  =(nonc n)  ~
      (some msg)
    ++  seal
      |=  {bpk/pass nonc/@ msg/@}
      ^-  @ux
      ?~  sek  ~|  %pubkey-only  !!
      ?>  =('b' (end 3 1 bpk))
      =+  pk=(rsh 8 1 (rsh 3 1 bpk))
      =+  shar=(shax (shar:ed pk cry.u.sek))
      (jam [nonc (~(en siva:aes shar ~[nonc]) msg)])
    ++  tear
      |=  {bpk/pass txt/@}
      ^-  (unit {@ux @ux})
      ?~  sek  ~|  %pubkey-only  !!
      ?>  =('b' (end 3 1 bpk))
      =+  pk=(rsh 8 1 (rsh 3 1 bpk))
      =+  shar=(shax (shar:ed pk cry.u.sek))
      =+  ((hard {nonc/@ iv/@ cph/@}) (cue txt))
      %+  both  (some nonc)
      (~(de siva:aes shar ~[nonc]) iv cph)
    --
  ++  de
    |=  {key/@I cph/@}
    ^-  (unit @ux)
    %+  ~(de siva:aes key ~)
      (end 7 1 cph)
      (rsh 7 1 cph)
  ++  dy  |=({key/@I cph/@} (need (de key cph)))
  ++  en
    |=  {key/@I msg/@}
    ^-  @ux
    (cat 7 (~(en siva:aes key ~) msg))
  ++  ex
    |%
    ++  fig  ^-  @uvH  (shaf %bfig sgn.^pub)
    ++  pac  ^-  @uvG  ?~  sek  ~|  %pubkey-only  !!
                       (end 6 1 (shaf %bcod sgn.u.sek))
    ++  pub  ^-  pass  (cat 3 'b' (cat 8 sgn.^pub cry.^pub))
    ++  sec  ^-  ring  ?~  sek  ~|  %pubkey-only  !!
                       (cat 3 'B' (cat 8 sgn.u.sek cry.u.sek))
    --
  ++  nu
    |%
    ++  pit
      |=  {w/@ seed/@}
      =+  bits=(shaz seed)  ::  need 512 bits
      =+  [c=(rsh 8 1 seed) s=(end 8 1 seed)]
      ..nu(pub [cry=(puck:ed c) sgn=(puck:ed s)], sek `[cry=c sgn=s])
    ++  nol
      |=  a/ring
      =+  [c=(rsh 8 1 a) s=(end 8 1 a)]
      ..nu(pub [cry=(puck:ed c) sgn=(puck:ed s)], sek `[cry=c sgn=s])
    ++  com
      |=  a/pass
      ..nu(pub [cry=(rsh 8 1 a) sgn=(end 8 1 a)], sek ~)
    --
  --
::
++  brew                                                ::  create keypair
  |=  {a/@ b/@}                                         ::  width seed
  ^-  acru
  (pit:nu:crub a b)
::
++  hail                                                ::  activate public key
  |=  a/pass
  ^-  acru
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('b' mag)
  (com:nu:crub bod)
::
++  wear                                                ::  activate secret key
  |=  a/ring
  ^-  acru
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('B' mag)
  (nol:nu:crub bod)
::
++  trub                                                ::  test ed
  |=  msg/@tas
  ^-  @
  =+  ali=(brew 1.024 (cat 8 (shax 'ali') (shad 'ali')))
  =+  bob=(brew 1.024 (cat 8 (shax 'bob') (shad 'bob')))
  =+  tef=(sign:as.ali [0 msg])
  =+  lov=(sure:as.ali [0 tef])
  ?.  &(?=(^ lov) =(msg u.lov))
    ~|(%test-fail-sign !!)
  =+  key=(shax (shax (shax msg)))
  =+  sax=(seal:as.ali pub:ex.bob key msg)
  =+  tin=(tear:as.bob pub:ex.ali sax)
  ?.  &(?=(^ tin) =(key p.u.tin) =(msg q.u.tin))
    ~|(%test-fail-seal !!)
  msg
::
++  hmac                                                ::  HMAC-SHA1
  |=  {key/@ mes/@}
  =+  ip=(fil 3 64 0x36)
  =+  op=(fil 3 64 0x5c)
  =+  ^=  kex
      ?:  (gth (met 3 key) 64)
        (lsh 3 44 (shan key))
      (lsh 3 (sub 64 (met 3 key)) (swp 3 key))
  =+  inn=(shan (swp 3 (cat 3 (swp 3 mes) (mix ip kex))))
  (shan (swp 3 (cat 3 inn (mix op kex))))
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bC, UTC                      ::  Gregorian only
::
++  dawn                                                ::  Jan 1 weekday
  |=  yer/@ud
  =+  yet=(sub yer 1)
  %-  mod  :_  7
  :(add 1 (mul 5 (mod yet 4)) (mul 4 (mod yet 100)) (mul 6 (mod yet 400)))
::
++  daws                                                ::  date weekday
  |=  yed/date
  %-  mod  :_  7
  (add (dawn y.yed) (sub (yawn [y.yed m.yed d.t.yed]) (yawn y.yed 1 1)))
::
++  deal                                                ::  to leap sec time
  |=  yer/@da
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
::
++  lead                                                ::  from leap sec time
  |=  ley/date
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
::
++  dust                                                ::  print UTC format
  |=  yed/date
  ^-  tape
  =+  wey=(daws yed)
  ;:  weld
      `tape`(snag wey (turn wik:yu |=(a/tape (scag 3 a))))
      ", "  ~(rud at d.t.yed)  " "
      `tape`(snag (dec m.yed) (turn mon:yu |=(a/tape (scag 3 a))))
      " "  (scag 1 ~(rud at y.yed))  (slag 2 ~(rud at y.yed))  " "
      ~(rud at h.t.yed)  ":"  ~(rud at m.t.yed)  ":"  ~(rud at s.t.yed)
      " "  "+0000"
  ==
::
++  stud    !:                                          ::  parse UTC format
  =<  |=  a/cord                                        ::  expose parsers
      %+  biff  (rush a (more sepa elem))
      |=  b/(list _(wonk *elem))  ^-  (unit date)
      =-  ?.((za:jo -) ~ (some (zp:jo -)))
      ^+  =+  [*date u=unit]
          *{(u _[a y]) (u _m) (u _d.t) (u _+.t) $~}
      :~                                                ::  XX  types
          |-(?~(b ~ ?.(?=($y -.i.b) $(b t.b) `+.i.b)))
          |-(?~(b ~ ?.(?=($m -.i.b) $(b t.b) `+.i.b)))
          |-(?~(b ~ ?.(?=($d -.i.b) $(b t.b) `+.i.b)))
          |-(?~(b ~ ?.(?=($t -.i.b) $(b t.b) `+.i.b)))
      ==
  |%
  ::
  ++  snug
    |=  a/wall
    |=  b/tape
    =+  [pos=1 len=(lent b)]
    |-  ^-  (unit @u)
    ?~  a  ~
    ?:  =(b (scag len i.a))
      `pos
    $(pos +(pos), a t.a)
  ::
  ::
  ++  sepa  ;~(pose ;~(plug com (star ace)) (plus ace))
  ++  elem
    ;~  pose 
      (stag %t t)  (stag %y y)  (stag %m m)  (stag %d d)
      (stag %w w)  (stag %z z)
    == 
  ::
  ++  y  (stag %& (bass 10 (stun 3^4 dit)))
  ++  m  (sear (snug mon:yu) (plus alf))
  ++  d  (bass 10 (stun 1^2 dit))
  ++  t  [;~(plug - - + (easy ~))]:[;~(sfix d col) d]
  ++  w  (sear (snug wik:yu) (plus alf))
  ++  z  [;~(plug (mask "-+") . .)]:(bass 10 (stun 2^2 dit))
  --
::
++  unt                                                 ::  UGT to UTC time
  |=  a/@
  (div (sub a ~1970.1.1) ~s1)
::
++  yu                                                  ::  UTC format constants
  |%
  ++  mon  ^-  (list tape)
    :~  "January"  "February"  "March"  "April"  "May"  "June"  "July"
        "August"  "September"  "October"  "November"  "December"
    ==
  ::
  ++  wik  ^-  (list tape)
    :~  "Sunday"  "Monday"  "Tuesday"  "Wednesday"  "Thursday"
        "Friday"  "Saturday"
    ==
  ::
  ++  les  ^-  (list @da)
    :~  ~2015.7.1  ~2012.7.1  ~2009.1.1  ~2006.1.1  ~1999.1.1  ~1997.7.1
        ~1996.1.1  ~1994.7.1  ~1993.7.1  ~1992.7.1  ~1991.1.1  ~1990.1.1
        ~1988.1.1  ~1985.7.1  ~1983.7.1  ~1982.7.1  ~1981.7.1  ~1980.1.1
        ~1979.1.1  ~1978.1.1  ~1977.1.1  ~1976.1.1  ~1975.1.1  ~1974.1.1
        ~1973.1.1  ~1972.7.1
    ==
  ++  lef  ^-  (list @da)
    :~  ~2015.6.30..23.59.59   ~2012.6.30..23.59.59
        ~2008.12.31..23.59.58  ~2005.12.31..23.59.57
        ~1998.12.31..23.59.56  ~1997.6.30..23.59.55
        ~1995.12.31..23.59.54  ~1994.6.30..23.59.53
        ~1993.6.30..23.59.52   ~1992.6.30..23.59.51
        ~1990.12.31..23.59.50  ~1989.12.31..23.59.49
        ~1987.12.31..23.59.48  ~1985.6.30..23.59.47
        ~1983.6.30..23.59.46   ~1982.6.30..23.59.45
        ~1981.6.30..23.59.44   ~1979.12.31..23.59.43
        ~1978.12.31..23.59.42  ~1977.12.31..23.59.41
        ~1976.12.31..23.59.40  ~1975.12.31..23.59.39
        ~1974.12.31..23.59.38  ~1973.12.31..23.59.37
        ~1972.12.31..23.59.36  ~1972.6.30..23.59.35
    ==
  --
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bD, JSON and XML             ::
::
++  moon                                                ::  mime type to text
  |=  myn/mite
  %-  crip
  |-  ^-  tape
  ?~  myn  ~
  ?:  =(~ t.myn)  (trip i.myn)
  (weld (trip i.myn) `tape`['/' $(myn t.myn)])
::
++  perk                                                ::  parse cube fork 
  |*  a/(pole @tas)
  ?~  a  fail
  ;~  pose 
    (cold -.a (jest -.a))
    $(a +.a)
  ==
::
++  poja                                                ::  parse JSON
  =<  |=(a/cord `(unit json)`(rush a apex))
  |%
  ++  apex                                              ::  JSON value
    %+  knee  *json  |.  ~+
    %+  ifix  [spac spac]
    ;~  pose
      (cold ~ (jest 'null'))
      (stag %b bool)
      (stag %s stri)
      (cook |=(s/tape [%n p=(rap 3 s)]) numb)
      abox
      obox
    ==
  ++  tops  ;~(pose abox obox)                          ::  JSON strict
  ::  JSON arrays
  ++  abox  (stag %a (ifix [sel (ws ser)] (more (ws com) apex)))
  ::  JSON objects
  ++  pear  ;~(plug ;~(sfix (ws stri) (ws col)) apex)
  ++  obje  (ifix [(ws kel) (ws ker)] (more (ws com) pear))
  ++  obox  (stag %o (cook malt obje))
  ::  JSON booleans
  ++  bool  ;~(pose (cold & (jest 'true')) (cold | (jest 'false')))
  ::  JSON strings
  ++  stri  (cook crip (ifix [doq doq] (star jcha)))
  ++  jcha  ;~(pose ;~(less doq bas prn) esca)           :: character in string
  ++  esca                                               :: Escaped character
    ;~  pfix  bas
      ;~  pose
        doq  fas  soq  bas
        (sear ~(get by `(map @t @)`(malt `(list (pair @t @))`[b+8 t+9 n+10 f+12 r+13 ~])) low)
        ;~(pfix (just 'u') (cook tuft qix:ab))           :: 4-digit hex to UTF-8
      ==
    ==
  ::  JSON numbers
  ++  numb
    ;~  (comp twel)
      (mayb (piec hep))
      ;~  pose
        (piec (just '0'))
        ;~(plug (shim '1' '9') digs)
      ==
      (mayb frac)
      (mayb expo)
    ==
  ++  digs  (star (shim '0' '9'))
  ++  expo                                              :: Exponent part
    ;~  (comp twel)
      (piec (mask "eE"))
      (mayb (piec (mask "+-")))
      digs
    ==
  ++  frac   ;~(plug dot digs)                          :: Fractional part
  ::  whitespace
  ++  spac  (star (mask [`@`9 `@`10 `@`13 ' ' ~]))
  ++  ws  |*(sef/rule ;~(pfix spac sef))
  ::  plumbing
  ++  mayb  |*(bus/rule ;~(pose bus (easy "")))
  ++  twel  |=({a/tape b/tape} (weld a b))
  ++  piec
    |*  bus/rule
    (cook |=(a/@ [a ~]) bus)
  --
::
++  pojo                                                ::  print json
  =|  rez/tape
  |=  val/json
  ^-  tape
  ?~  val  (weld "null" rez)
  ?-    -.val
      $a
    :-  '['
    =.  rez  [']' rez]
    !.
    ?~  p.val  rez
    |-
    ?~  t.p.val  ^$(val i.p.val)
    ^$(val i.p.val, rez [',' $(p.val t.p.val)])
 ::
      $b  (weld ?:(p.val "true" "false") rez)
      $n  (weld (trip p.val) rez)
      $s
    :-  '"'
    =.  rez  ['"' rez]
    =+  viz=(trip p.val)
    !.
    |-  ^-  tape
    ?~  viz  rez
    =+  hed=(jesc i.viz)
    ?:  ?=({@ $~} hed)                 :: common case
      [i.hed $(viz t.viz)]            :: cons-and-tail
    (weld hed $(viz t.viz))
 ::
      $o
    :-  '{'
    =.  rez  ['}' rez]
    =+  viz=(~(tap by p.val))
    ?~  viz  rez
    !.
    |-  ^+  rez
    ?~  t.viz  ^$(val [%s p.i.viz], rez [':' ^$(val q.i.viz)])
    =.  rez  [',' $(viz t.viz)]
    ^$(val [%s p.i.viz], rez [':' ^$(val q.i.viz)])
  ==
::
++  poxo                                                ::  node to tape
  =<  |=(a/manx `tape`(apex a ~))
  |_  _[unq=`?`| cot=`?`|]                           ::  self-close all tags
  ++  apex                                              ::  top level
    |=  {mex/manx rez/tape}
    ^-  tape
    ?:  ?=({$$ {{$$ *} $~}} g.mex)
      (escp v.i.a.g.mex rez)
    =+  man=`mane`n.g.mex
    =.  unq  |(unq =(%script man) =(%style man))
    =+  tam=(name man)
    =+  att=`mart`a.g.mex
    :-  '<'
    %+  welp  tam
    =-  ?~(att rez [' ' (attr att rez)])
    ^-  rez/tape
    ?:  &(?=($~ c.mex) |(cot (clot man)))
      [' ' '/' '>' rez]
    :-  '>'
    (many c.mex :(weld "</" tam ">" rez))
  ::
  ++  attr                                              ::  attributes to tape
    |=  {tat/mart rez/tape}
    ^-  tape
    ?~  tat  rez
    =.  rez  $(tat t.tat)
    ;:  weld 
      (name n.i.tat)
      "=\"" 
      (escp(unq |) v.i.tat '"' ?~(t.tat rez [' ' rez]))
    ==
  ::
  ++  escp                                              ::  escape for xml
    |=  {tex/tape rez/tape}
    ?:  unq
      (weld tex rez)
    =+  xet=`tape`(flop tex)
    !.
    |-  ^-  tape
    ?~  xet  rez
    %=    $
      xet  t.xet
      rez  ?-  i.xet
             $34  ['&' 'q' 'u' 'o' 't' ';' rez]
             $38  ['&' 'a' 'm' 'p' ';' rez]
             $39  ['&' '#' '3' '9' ';' rez]
             $60  ['&' 'l' 't' ';' rez]
             $62  ['&' 'g' 't' ';' rez]
             *    [i.xet rez]
           ==
    ==
  ::
  ++  many                                              ::  nodelist to tape
    |=  {lix/(list manx) rez/tape}
    |-  ^-  tape
    ?~  lix  rez
    (apex i.lix $(lix t.lix))
  ::
  ++  name                                              ::  name to tape
    |=  man/mane  ^-  tape
    ?@  man  (trip man)
    (weld (trip -.man) `tape`[':' (trip +.man)])
  ::
  ++  clot  ~+                                          ::  self-closing tags
    %~  has  in
        %-  silt  ^-  (list term)  :~
          %area  %base  %br  %col  %command  %embed  %hr  %img  %input  %keygen
          %link  %meta  %param     %source   %track  %wbr 
    ==  ==
  --
::
++  foo
    ;~(plug fas fas fas)
++  poxa                                                ::  xml parser
  =<  |=(a/cord (rush a apex))
  |_  ent/_`(map term @t)`[[%apos '\''] ~ ~]
  ++  apex
    =+  spa=;~(pose comt whit)
    %+  knee  *manx  |.  ~+
    %+  ifix  [(star spa) (star spa)]
    ;~  pose
      %+  sear  |=({a/marx b/marl c/mane} ?.(=(c n.a) ~ (some [a b])))
        ;~(plug head many tail)
      empt
    == 
  ::
  ++  attr                                              ::  attributes
    %+  knee  *mart  |.  ~+ 
    %-  star
    ;~  plug  
      ;~(pfix (plus whit) name)
      ;~  pose
        (ifix [;~(plug tis doq) doq] (star ;~(less doq escp)))
        (ifix [;~(plug tis soq) soq] (star ;~(less soq escp)))
        (easy ~)
      ==
    ==
  ::
  ++  chrd                                              ::  character data
    %+  cook  |=(a/tape ^-(mars ;/(a)))
    (plus ;~(less doq ;~(pose (just `@`10) escp)))
  ::
  ++  comt                                              ::  comments 
    =-  (ifix [(jest '<!--') (jest '-->')] (star -))
    ;~  pose
      ;~(less hep prn)
      whit
      ;~(less (jest '-->') hep)
    ==
  :: 
  ++  escp  ;~(pose ;~(less gal gar pam prn) enty)
  ++  enty                                              ::  entity
    %+  ifix  pam^sem
    ;~  pose
      =+  def=^+(ent (my [%gt '>'] [%lt '<'] [%amp '&'] [%quot '"'] ~))
      %+  sear  ~(get by (~(uni by def) ent))
      (cook crip ;~(plug alf (stun 1^31 aln)))
      %+  cook  |=(a/@c ?:((gth a 0x10.ffff) '�' (tuft a)))
      =<  ;~(pfix hax ;~(pose - +))
      :-  (bass 10 (stun 1^8 dit))
      (bass 16 ;~(pfix (mask "xX") (stun 1^8 hit)))
    ==
  ::
  ++  empt                                              ::  self-closing tag
    %+  ifix  [gal (jest '/>')]
    ;~(plug ;~(plug name attr) (cold ~ (star whit)))
  ::
  ++  head                                              ::  opening tag
    (ifix [gal gar] ;~(plug name attr))
  ::
  ++  many
    (more (star comt) ;~(pose apex chrd))
  ::
  ++  name                                              ::  tag name 
    =+  ^=  chx
        %+  cook  crip 
        ;~  plug 
            ;~(pose cab alf) 
            (star ;~(pose cab dot alp))
        ==
    ;~(pose ;~(plug ;~(sfix chx col) chx) chx)
  ::
  ++  tail  (ifix [(jest '</') gar] name)               ::  closing tag
  ++  whit  (mask ~[' ' `@`0x9 `@`0xa])                 ::  whitespace
  --
::
++  jo                                                  ::  json reparser
  =>  |%  ++  grub  (unit *) 
          ++  fist  $-(json grub)
      --
  |%
  ++  ar                                                ::  array as list
    |*  wit/fist
    |=  jon/json
    ?.  ?=({$a *} jon)  ~
    %-  zl
    |-  
    ?~  p.jon  ~
    [i=(wit i.p.jon) t=$(p.jon t.p.jon)]
  ::
  ++  at                                                ::  array as tuple
    |*  wil/(pole fist)
    |=  jon/json
    ?.  ?=({$a *} jon)  ~
    =+  raw=((at-raw wil) p.jon)
    ?.((za raw) ~ (some (zp raw)))
  ::
  ++  at-raw                                            ::  array as tuple
    |*  wil/(pole fist)
    |=  jol/(list json)
    ?~  wil  ~
    :-  ?~(jol ~ (-.wil i.jol))
    ((at-raw +.wil) ?~(jol ~ t.jol))
  ::
  ++  bo                                                ::  boolean
    |=(jon/json ?.(?=({$b *} jon) ~ [~ u=p.jon]))
  ::
  ++  bu                                                ::  boolean not
    |=(jon/json ?.(?=({$b *} jon) ~ [~ u=!p.jon]))
  ::
  ++  ci                                                ::  maybe transform
    |*  {poq/gate wit/fist}
    |=  jon/json
    (biff (wit jon) poq)
  ::
  ++  cu                                                ::  transform
    |*  {poq/gate wit/fist}
    |=  jon/json
    (bind (wit jon) poq)
  ::
  ++  da                                                ::  UTC date
    |=  jon/json
    ?.  ?=({$s *} jon)  ~
    (bind (stud p.jon) |=(a/date (year a)))
  ::
  ++  di                                                ::  millisecond date
    %+  cu
      |=  a/@u  ^-  @da
      (add ~1970.1.1 (div (mul ~s1 a) 1.000))
    ni
  ::
  ++  mu                                                ::  true unit
    |*  wit/fist
    |=  jon/json
    ?~(jon (some ~) (bind (wit jon) some))
  ::
  ++  ne                                                ::  number as real
    |=  jon/json
    ^-  (unit @rd)
    !!
  ::
  ++  ni                                                ::  number as integer
    |=  jon/json 
    ?.  ?=({$n *} jon)  ~
    (rush p.jon dem)
  ::
  ++  no                                                ::  number as cord
    |=  jon/json
    ?.  ?=({$n *} jon)  ~
    (some p.jon)
  ::
  ++  of                                                ::  object as frond
    |*  wer/(pole {cord fist})
    |=  jon/json
    ?.  ?=({$o {@ *} $~ $~} jon)  ~
    |-
    ?~  wer  ~
    ?:  =(-.-.wer p.n.p.jon)  
      ((pe -.-.wer +.-.wer) q.n.p.jon)
    ((of +.wer) jon)
  ::
  ++  ot                                                ::  object as tuple
    |*  wer/(pole {cord fist})
    |=  jon/json
    ?.  ?=({$o *} jon)  ~
    =+  raw=((ot-raw wer) p.jon)
    ?.((za raw) ~ (some (zp raw)))
  ::
  ++  ot-raw                                            ::  object as tuple
    |*  wer/(pole {cord fist})
    |=  jom/(map @t json)
    ?~  wer  ~
    =+  ten=(~(get by jom) -.-.wer)
    [?~(ten ~ (+.-.wer u.ten)) ((ot-raw +.wer) jom)]
  ::
  ++  om                                                ::  object as map
    |*  wit/fist
    |=  jon/json
    ?.  ?=({$o *} jon)  ~
    (zm (~(run by p.jon) wit))
  ::
  ++  op                                                ::  parse keys of map
    |*  {fel/rule wit/fist}
    %+  cu  my
    %-  ci  :_  (om wit)
    |=  a/(map cord _(need *wit))
    ^-  (unit (list _[(wonk *fel) (need *wit)]))
    =-  (zl (turn (~(tap by a)) -))
    |*  {a/cord b/*}
    =+  nit=(rush a fel) 
    ?~  nit  ~
    (some [u.nit b])
  ::
  ++  pe                                                ::  prefix
    |*  {pre/* wit/fist}
    (cu |*(* [pre +<]) wit)
  ::
  ++  sa                                                ::  string as tape
    |=  jon/json
    ?.(?=({$s *} jon) ~ (some (trip p.jon)))
  ::
  ++  so                                                ::  string as cord
    |=  jon/json
    ?.(?=({$s *} jon) ~ (some p.jon))
  ::
  ++  su                                                ::  parse string
    |*  sab/rule
    |=  jon/json
    ?.  ?=({$s *} jon)  ~
    (rush p.jon sab)
  ::
  ++  ul  |=(jon/json ?~(jon (some ~) ~))               ::  null
  ++  za                                                ::  full unit pole
    |*  pod/(pole (unit))
    ?~  pod  &
    ?~  -.pod  |
    (za +.pod)
  ::
  ++  zl                                                ::  collapse unit list
    |*  lut/(list (unit))
    ?.  |-  ^-  ?
        ?~(lut & ?~(i.lut | $(lut t.lut)))
      ~
    %-  some
    |-
    ?~  lut  ~
    [i=u:+.i.lut t=$(lut t.lut)]
  ::
  ++  zp                                                ::  unit tuple
    |*  but/(pole (unit))
    ?~  but  !!
    ?~  +.but  
      u:->.but
    [u:->.but (zp +.but)]
  ::
  ++  zm                                                ::  collapse unit map
    |*  lum/(map term (unit))
    ?:  (~(rep by lum) |=({{@ a/(unit)} b/_|} |(b ?=($~ a))))
      ~
    (some (~(run by lum) need))
  --
::
++  joba                                                ::  object from k-v pair
  |=  {p/@t q/json}
  ^-  json
  [%o [[p q] ~ ~]]
::
++  jobe                                                ::  object from k-v list
  |=  a/(list {p/@t q/json})
  ^-  json
  [%o (~(gas by *(map @t json)) a)]
::
++  jape                                                ::  string from tape
  |=  a/tape
  ^-  json
  [%s (crip a)]
::
++  jone                                                ::  number from unsigned
  |=  a/@u
  ^-  json
  :-  %n
  ?:  =(0 a)  '0'
  (crip (flop |-(^-(tape ?:(=(0 a) ~ [(add '0' (mod a 10)) $(a (div a 10))])))))
::
++  jode                                                ::  ms timestamp
  |=  a/time 
  =-  (jone (div (mul - 1.000) ~s1))
  (add (div ~s1 2.000) (sub a ~1970.1.1))
::
++  jesc
  =+  utf=|=(a/@ ['\\' 'u' ((x-co 4):co a)]) 
  |=  a/@  ^-  tape
  ?+  a  ?:((gth a 0x1f) [a ~] (utf a))
    $10  "\\n"
    $34  "\\\""
    $92  "\\\\"
  ==
::
++  scanf                                              ::  formatted scan
  |*  {tape (pole _;/(*{$^(rule tape)}))}
  =>  .(+< [a b]=+<)
  (scan a (parsf b))
++  parsf                                              ::  make parser from:
  |^  |*  a/(pole _;/(*{$^(rule tape)}))            ::  ;"chars{rule}chars"
      =-  (cook - (bill (norm a)))
      |*  (list)
      ?~  +<  ~
      ?~  t  i
      [i $(+< t)]
  ::
  ::  .=  (norm [;"{n}, {n}"]:n=dim:ag)  ~[[& dim] [| ", "] [& dim]]:ag
  ++  norm                                             
    |*  (pole _;/(*{$^(rule tape)}))
    ?~  +<  ~
    =>  .(+< [i=+<- t=+<+])
    :_  t=$(+< t)
    =+  rul=->->.i
    ^=  i
    ?~  rul     [%| p=rul]
    ?~  +.rul   [%| p=rul]
    ?@  &2.rul  [%| p=;;(tape rul)]
    [%& p=rul]
  ::
  ::  .=  (bill ~[[& dim] [| ", "] [& dim]]:ag)
  ::  ;~(plug dim ;~(pfix com ace ;~(plug dim (easy)))):ag
  ++  bill
    |*  (list (each rule tape))
    ?~  +<  (easy ~)
    ?:  ?=($| -.i)  ;~(pfix (jest (crip p.i)) $(+< t))
    %+  cook  |*({* *} [i t]=+<)
    ;~(plug p.i $(+< t))
  --
::
++  taco                                                ::  atom to octstream
  |=  tam/@  ^-  octs
  [(met 3 tam) tam]
::
++  tact                                                ::  tape to octstream
  |=  tep/tape  ^-  octs
  (taco (rap 3 tep))
::
++  tell                                                ::  wall to octstream
  |=  wol/wall  ^-  octs
  =+  buf=(rap 3 (turn wol |=(a/tape (crip (weld a `tape`[`@`10 ~])))))
  [(met 3 buf) buf]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bF, filesystem interface     ::
::
++  feel                                                ::  simple file write
  |=  {pax/path val/cage}
  ^-  miso
  =+  dir=.^(arch %cy pax)
  ?~  fil.dir  [%ins val]
  [%mut val]
::
++  file                                                ::  simple file load
  |=  pax/path
  ^-  (unit)
  =+  dir=.^(arch %cy pax)
  ?~(fil.dir ~ [~ .^(* %cx pax)])
::
++  foal                                                ::  high-level write
  |=  {pax/path val/cage}
  ^-  toro
  ?>  ?=({* * * *} pax)
  [i.t.pax [%& [[[t.t.t.pax (feel pax val)] ~]]]]
::
++  fray                                                ::  high-level delete
  |=  pax/path
  ^-  toro
  ?>  ?=({* * * *} pax)
  [i.t.pax [%& [[[t.t.t.pax [%del ~]] ~]]]]
::
++  furl                                                ::  unify changes
  |=  {one/toro two/toro}
  ^-  toro
  ~|  %furl
  ?>  ?&  =(p.one p.two)                                ::  same path
          &(?=($& -.q.one) ?=($& -.q.two))              ::  both deltas
      ==
  [p.one [%& (weld p.q.one p.q.two)]]
::
++  tame                                                ::  parse kite path
  |=  hap/path
  ^-  (unit kite)
  ?.  ?=({@ @ @ @ *} hap)  ~
  =+  :*  hyr=(slay i.hap)
          fal=(slay i.t.hap)
          dyc=(slay i.t.t.hap)
          ved=(slay i.t.t.t.hap)
          ::  ved=(slay i.t.hap)
          ::  fal=(slay i.t.t.hap)
          ::  dyc=(slay i.t.t.t.hap)
          tyl=t.t.t.t.hap
      ==
  ?.  ?=({$~ $$ $tas @} hyr)  ~
  ?.  ?=({$~ $$ $p @} fal)  ~
  ?.  ?=({$~ $$ $tas @} dyc)  ~
  ?.  ?=({$~ $$ case} ved)  ~
  =+  his=`@p`q.p.u.fal
  =+  [dis=(end 3 1 q.p.u.hyr) rem=(rsh 3 1 q.p.u.hyr)]
  ?.  ?&(?=($c dis) ?=(?($v $w $x $y $z) rem))  ~
  [~ rem p.u.ved q.p.u.fal q.p.u.dyc tyl]
::
++  tome                                                ::  parse path to beam
  |=  pax/path
  ^-  (unit beam)
  ?.  ?=({* * * *} pax)  ~
  %+  biff  (slaw %p i.pax)
  |=  who/ship
  %+  biff  (slaw %tas i.t.pax)
  |=  dex/desk
  %+  biff  (slay i.t.t.pax)
  |=  cis/coin
  ?.  ?=({$$ case} cis)  ~
  `(unit beam)`[~ [who dex `case`p.cis] (flop t.t.t.pax)]
::
++  tope                                                ::  beam to path
  |=  bem/beam
  ^-  path
  [(scot %p p.bem) q.bem (scot r.bem) (flop s.bem)]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bG, URL handling             ::
::
++  deft                                                ::  parse url extension
  |=  rax/(list @t)
  |-  ^-  pork
  ?~  rax
    [~ ~]
  ?^  t.rax
    [p.pok [ire q.pok]]:[pok=$(rax t.rax) ire=i.rax]
  =+  ^-  raf/(like term)
      =>  |=(a/@ ((sand %tas) (crip (flop (trip a)))))
      (;~(sfix (sear . sym) dot) [1^1 (flop (trip i.rax))])
  ?~  q.raf
    [~ [i.rax ~]]
  =+  `{ext/term {@ @} fyl/tape}`u.q.raf
  :-  `ext
  ?:(=(~ fyl) ~ [(crip (flop fyl)) ~])
::
++  fuel                                                ::  parse fcgi
  |=  {bem/beam ced/noun:cred quy/|-($@($~ {p/@t q/@t t/$}))}
  ^-  epic
  =+  qix=|-(`quay`?~(quy quy [[p q]:quy $(quy t.quy)]))
  [(malt qix) ((hard cred) ced) bem /]
::
++  sifo                                                ::  64-bit encode
  |=  tig/@
  ^-  tape
  =+  poc=(~(dif fo 3) 0 (met 3 tig))
  =+  pad=(lsh 3 poc (swp 3 tig))
  =+  ^=  cha
  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  =+  ^=  sif
      |-  ^-  tape
      ?~  pad
        ~
      =+  d=(end 0 6 pad)
      [(cut 3 [d 1] cha) $(pad (rsh 0 6 pad))]
  (weld (flop (slag poc sif)) (reap poc '='))
::
++  ofis  ::  XX broken
  =-  |=(a/cord (rash a fel))
  =<  fel=(cook |~(a/@ `@t`(swp 3 a)) (bass 64 .))
  =-  (cook welp ;~(plug (plus siw) (stun 0^2 (cold %0 tis))))
  ^=  siw
  ;~  pose
     (cook |=(a/@ (sub a 'A')) (shim 'A' 'Z'))
     (cook |=(a/@ (sub a 'G')) (shim 'a' 'z'))
     (cook |=(a/@ (add a 4)) (shim '0' '9'))
     (cold 62 (just '+'))
     (cold 63 (just '/'))
   ==
::
::  .=  ~[p=~.ack q=~.~sarnel r=~..y]
::  (dray ~[p=%tas q=%p r=%f] %ack ~sarnel &)
++  dray                                                ::  load tuple into path
  =-  |*  {a/{@tas (pole @tas)} b/*}  ^-  (paf a)
      =>  .(b `(tup -.a +.a)`b)
      ?~  +.a  [(scot -.a b) ~]
      [(scot -.a -.b) `(paf +.a)`(..$ +.a +.b)]
  :-  paf=|*(a/(pole) ?~(a $~ {(odo:raid -.a(. %ta)) (..$ +.a)}))
  tup=|*({a/@tas b/(pole @tas)} =+(c=(odo:raid a) ?~(b c {c (..$ -.b +.b)})))
::
::  .=  [p=%ack q=~sarnel r=&]
::  (raid /ack/~sarnel+.y p=%tas q=%p r=%f ~)
++  raid                                                ::  demand path odors
  =-  |*  {a/path b/{@tas (pole @tas)}}
      ?~  +.b  `(odo -.b)`(slav -.b -.a)
      [`(odo -.b)`(slav -.b -.a) (..$ +.a +.b)]
  ^=  odo
  |*  a/@tas
  |=  b/*
  =<  a(, (. b))                  ::  preserve face
  ?+  a   @
    $c  @c  $da  @da  $dr  @dr  $f   @f   $if  @if  $is  @is  $p   @p
    $u  @u  $uc  @uc  $ub  @ub  $ui  @ui  $ux  @ux  $uv  @uv  $uw  @uw
    $s  @s  $t   @t   $ta  @ta  $tas  @tas
  ==
::
++  read                                                ::  parse odored path
  =<  |*({a/path b/{@tas (pole @tas)}} ((+> b) a))
  |*  b/{@tas (pole @tas)}
  |=  a/path
  ?~  a  ~
  =+  hed=(slaw -.b i.a)
  ?~  +.b
    ^-  (unit (odo:raid -.b))
    ?^(+.a ~ hed)
  ^-  (unit {(odo:raid -.b) _(need *(..^$ +.b))})
  (both hed ((..^$ +.b) +.a))
::
++  urle                                                ::  URL encode
  |=  tep/tape
  ^-  tape
  %-  zing
  %+  turn  tep
  |=  tap/char
  =+  xen=|=(tig/@ ?:((gte tig 10) (add tig 55) (add tig '0')))
  ?:  ?|  &((gte tap 'a') (lte tap 'z'))
          &((gte tap 'A') (lte tap 'Z'))
          &((gte tap '0') (lte tap '9'))
          =('.' tap)
          =('-' tap)
          =('~' tap)
          =('_' tap)
      ==
    [tap ~]
  ['%' (xen (rsh 0 4 tap)) (xen (end 0 4 tap)) ~]
::
++  urld                                                ::  URL decode
  |=  tep/tape
  ^-  (unit tape)
  ?~  tep  [~ ~]
  ?:  =('%' i.tep)
    ?.  ?=({@ @ *} t.tep)  ~
    =+  nag=(mix i.t.tep (lsh 3 1 i.t.t.tep))
    =+  val=(rush nag hex:ag)
    ?~  val  ~
    =+  nex=$(tep t.t.t.tep)
    ?~(nex ~ [~ [`@`u.val u.nex]])
  =+  nex=$(tep t.tep)
  ?~(nex ~ [~ i.tep u.nex])
::
++  earf                                                ::  purf to tape
  |=  purf
  (weld (earn p) ?~(q "" `tape`['#' (trip u.q)]))
::
++  earl                                                ::  localize purl
  |=  {who/@p pul/purl}
  ^-  purl
  pul(q.q [(rsh 3 1 (scot %p who)) q.q.pul])
::
++  earn                                                ::  purl to tape
  =<  |=(pul/purl `tape`(apex %& pul))
  |%
  ++  apex
    |=  qur/quri  ^-  tape
    ?-  -.qur
      $&  (weld (head p.p.qur) `tape`$(qur [%| +.p.qur]))
      $|  ['/' (weld (body p.qur) (tail q.qur))]
    ==
  ::
  ++  body
    |=  pok/pork  ^-  tape
    ?~  q.pok  ~
    |-
    =+  seg=(urle (trip i.q.pok))
    ?~  t.q.pok
      ?~(p.pok seg (welp seg '.' (trip u.p.pok)))
    (welp seg '/' $(q.pok t.q.pok))
  ::
  ++  head
    |=  har/hart
    ^-  tape
    ;:  weld
      ?:(&(p.har !?=(hoke r.har)) "https://" "http://")
    ::
      ?-  -.r.har
        $|  (trip (rsh 3 1 (scot %if p.r.har)))
        $&  =+  rit=(flop p.r.har)
            |-  ^-  tape
            ?~(rit ~ (weld (trip i.rit) ?~(t.rit "" `tape`['.' $(rit t.rit)])))
      ==
    ::
      ?~(q.har ~ `tape`[':' ((d-co:co 1) u.q.har)])
    ==
  ::
  ++  tail
    |=  kay/quay
    ^-  tape
    ?:  =(~ kay)  ~
    :-  '?'
    |-  ^-  tape
    ?~  kay  ~
    ;:  welp
      (urle (trip p.i.kay))
      ?~(q.i.kay ~ ['=' (urle (trip q.i.kay))])
      ?~(t.kay ~ `tape`['&' $(kay t.kay)])
    ==
  --
::
++  urlp                                                ::  url+header parser
  |%
  ++  apat                                              ::  2396 abs_path
    %+  cook  deft
    (ifix [fas ;~(pose fas (easy ~))] (more fas smeg))
  ++  aurf                                              ::  2396 with fragment
    %+  cook  |~(a/purf a)
    ;~(plug auri (punt ;~(pfix hax (cook crip (star pque)))))
  ++  auru                                              ::  2396 with maybe user
    %+  cook
      |=  $:  a/{p/? q/(unit iden) r/{(unit @ud) host}}
              b/{pork quay}
          ==
      ^-  (pair (unit iden) purl)
      [q.a [[p.a r.a] b]]
    ::
    ;~  plug
      ;~(plug htts (punt ;~(sfix urt:ab pat)) thor)
      ;~(plug ;~(pose apat (easy *pork)) yque)
    ==
  ++  auri                                              ::  2396 URL
    %+  cook
      |=  a/purl
      ?.(?=(hoke r.p.a) a a(p.p &))
    ;~  plug
      ;~(plug htts thor)
      ;~(plug ;~(pose apat (easy *pork)) yque)
    ==
  ++  htts  
    %+  sear  ~(get by (malt `(list (pair term ?))`[http+| https+& ~]))
    ;~(sfix scem ;~(plug col fas fas))
  ::
  ++  cock                                              ::  cookie
    (most ;~(plug sem ace) ;~(plug toke ;~(pfix tis tosk)))
  ++  dlab                                              ::  2396 domainlabel
    %+  sear
      |=  a/@ta
      ?.(=('-' (rsh 3 (dec (met 3 a)) a)) [~ u=a] ~)
    %+  cook  cass
    ;~(plug aln (star alp))
  ::
  ++  fque  (cook crip (plus pquo))                     ::  normal query field
  ++  fquu  (cook crip (star pquo))                     ::  optional field
  ++  pcar  ;~(pose pure pesc psub col pat)             ::  2396 path char
  ++  pcok  ;~(less bas sem com doq prn)                ::  cookie char
  ++  pesc  ;~(pfix cen mes)                            ::  2396 escaped
  ++  pold  (cold ' ' (just '+'))                       ::  old space code
  ++  pque  ;~(pose pcar fas wut)                       ::  3986 query char
  ++  pquo  ;~(pose pure pesc pold fas wut)             ::  normal query char
  ++  pure  ;~(pose aln hep dot cab sig)                ::  2396 unreserved
  ++  psub  ;~  pose                                    ::  3986 sub-delims
              zap  buc  pam  soq  pel  per
              tar  lus  com  sem  tis
            ==
  ++  ptok  ;~  pose                                    ::  2616 token
              aln  zap  hax  buc  cen  pam  soq  tar  lus
              hep  dot  ket  cab  tec  bar  sig
            ==
  ++  scem                                              ::  2396 scheme
    %+  cook  cass
    ;~(plug alf (star ;~(pose aln lus hep dot)))
  ::
  ++  smeg  (cook crip (plus pcar))                     ::  2396 segment
  ++  tock  (cook crip (plus pcok))                     ::  6265 cookie-value
  ++  tosk  ;~(pose tock (ifix [doq doq] tock))         ::  6265 cookie-value
  ++  toke  (cook crip (plus ptok))                     ::  2616 token
  ++  thor                                              ::  2396 host+port
    %+  cook  |*({* *} [+<+ +<-])
    ;~  plug
      thos
      ;~((bend) (easy ~) ;~(pfix col dim:ag))
    ==
  ++  thos                                              ::  2396 host, no local
    ;~  plug
      ;~  pose
        %+  stag  %&
        %+  sear                                        ::  LL parser weak here
          |=  a/(list @t)
          =+  b=(flop a)
          ?>  ?=(^ b)
          =+  c=(end 3 1 i.b)
          ?.(&((gte c 'a') (lte c 'z')) ~ [~ u=b])
        (most dot dlab)
      ::
        %+  stag  %|
        =+  tod=(ape:ag ted:ab)
        %+  bass  256
        ;~(plug tod (stun [3 3] ;~(pfix dot tod)))
      ==
    ==
  ++  yque                                              ::  query ending
    ;~  pose
      ;~(pfix wut yquy)
      (easy ~)
    ==
  ++  yquy                                              ::  query
    ;~  pose                                            ::  proper query
      %+  more
        ;~(pose pam sem)
      ;~(plug fque ;~(pose ;~(pfix tis fquu) (easy '')))
    ::
      %+  cook                                          ::  funky query
        |=(a/tape [[%$ (crip a)] ~])
      (star pque)
    ==
  ++  zest                                              ::  2616 request-uri
    ;~  pose
      (stag %& (cook |=(a/purl a) auri))
      (stag %| ;~(plug apat yque))
    ==
  --
++  epur                                                ::  url+header parser
  =>(urlp |=(a/cord `(unit purl)`(rush a auri)))
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bH, names etc                ::
::
++  clan                                                ::  ship to rank
  |=  who/ship  ^-  rank
  =+  wid=(met 3 who)
  ?:  (lte wid 1)   %czar
  ?:  =(2 wid)      %king
  ?:  (lte wid 4)   %duke
  ?:  (lte wid 8)   %earl
  ?>  (lte wid 16)  %pawn
::
++  glam                                                ::  carrier names
  |=  zar/@pD  ^-  tape
  (weld "galaxy " (scow %p zar))
::
++  gnom                                                ::  ship display name
  |=  {{our/@p now/@da} him/@p}  ^-  @t
  =+  yow=(scot %p him)
  =+  pax=[(scot %p our) %name (scot %da now) yow ~]
  =+  woy=.^(@t %a pax)
  ?:  =(%$ woy)  yow
  (rap 3 yow ' ' woy ~)
::
++  gnow
  |=  {who/@p gos/gcos}  ^-  @t
  ?-    -.gos
      $czar                 (rap 3 '|' (rap 3 (glam who)) '|' ~)
      $king                 (rap 3 '_' p.gos '_' ~)
      $earl                 (rap 3 ':' p.gos ':' ~)
      $pawn                 ?~(p.gos %$ (rap 3 '.' u.p.gos '.' ~))
      $duke
    ?:  ?=($anon -.p.gos)  %$
    %+  rap  3
    ^-  (list @)
    ?-    -.p.gos
        $punk  ~['"' q.p.gos '"']
        ?($lord $lady)
      =+  ^=  nad
          =+  nam=`name`s.p.p.gos
          %+  rap  3
          :~  p.nam
              ?~(q.nam 0 (cat 3 ' ' u.q.nam))
              ?~(r.nam 0 (rap 3 ' (' u.r.nam ')' ~))
              ' '
              s.nam
          ==
      ?:(=(%lord -.p.gos) ~['[' nad ']'] ~['(' nad ')'])
    ==
  ==
++  pale                                                ::  filter peers
  |=  {hid/bowl fun/$-(sink ?)}
  (skim (~(tap by sup.hid)) fun)
::
++  prix                                                ::  filter gate
  |=  pax/path  |=  sink  ^-  ?
  ?~  pax  &  ?~  r.+<  | 
  &(=(i.pax i.r.+<) $(pax t.pax, r.+< t.r.+<))
::
++  prey  |=({pax/path hid/bowl} (pale hid (prix pax))) ::  prefix 
++  hunt                                                ::  first of unit dates
  |=  {one/(unit @da) two/(unit @da)}
  ^-  (unit @da)
  ?~  one  two
  ?~  two  one
  ?:((lth u.one u.two) one two)
::
++  mole                                                ::  new to old sky
  |=  ska/$-(* (unit (unit)))
  |=  a/*
  ^-  (unit)
  =+  b=(ska a)
  ?~  b  ~
  ?~  u.b  ~
  [~ u.u.b]
::
++  myle                                                ::  new to old sky
  |=  ska/$-({* *} (unit (unit)))
  ^-  $-({* *} (unit))
  |=  a/{* *}
  ^-  (unit)
  =+  b=(ska a)
  ?~  b  ~
  ?~  u.b  ~
  [~ u.u.b]
::
++  pack                                                ::  light path encoding
  |=  {a/term b/path}  ^-  knot
  %+  rap  3  :-  (wack a)
  (turn b |=(c/knot (cat 3 '_' (wack c))))
::
++  puck                                                ::  light path decoding
  =+  fel=(most cab (sear wick urt:ab))
  |=(a/knot `(unit {p/term q/path})`(rush a fel))
::
++  saxo                                                ::  autocanon
  |=  who/ship
  ^-  (list ship)
  ?:  (lth who 256)  [who ~]
  [who $(who (sein who))]
::
++  sein                                                ::  autoboss
  |=  who/ship  ^-  ship
  =+  mir=(clan who)
  ?-  mir
    $czar  who
    $king  (end 3 1 who)
    $duke  (end 4 1 who)
    $earl  (end 5 1 who)
    $pawn  `@p`0
  ==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bI, Arvo structures          ::
::
++  acru                                                ::  asym cryptosuite
          $_  ^?  |%                                    ::  opaque object
          ++  as  ^?                                    ::  asym ops
            |%  ++  seal  |~({a/pass b/@ c/@} *@)       ::  encrypt to a
                ++  sign  |~({a/@ b/@} *@)              ::  certify as us
                ++  sure  |~({a/@ b/@} *(unit @))       ::  authenticate from us
                ++  tear  |~  {a/pass b/@}              ::  accept from a 
                          *(unit {p/@ q/@})             ::
            --                                          ::
          ++  de  |~({a/@ b/@} *(unit @))               ::  symmetric de, soft
          ++  dy  |~({a/@ b/@} *@)                      ::  symmetric de, hard
          ++  en  |~({a/@ b/@} *@)                      ::  symmetric en
          ++  ex  ^?                                    ::  export
            |%  ++  fig  *@uvH                          ::  fingerprint
                ++  pac  *@uvG                          ::  default passcode
                ++  pub  *pass                          ::  public key
                ++  sec  *ring                          ::  private key
            --                                          ::
          ++  nu  ^?                                    ::  reconstructors
             |%  ++  pit  |~({a/@ b/@} ^?(..nu))        ::  from [width seed]
                 ++  nol  |~(a/@ ^?(..nu))              ::  from naked ring
                 ++  com  |~(a/@ ^?(..nu))              ::  from naked pass
             --                                         ::
          --                                            ::
++  aeon  @ud                                          ::
++  agon  (map {p/ship q/desk} {p/@ud q/@ud r/waks})    ::  mergepts
++  ankh                                                ::  fs node (new)
          $:  fil/(unit {p/lobe q/cage})                ::  file
              dir/(map @ta ankh)                        ::  folders
          ==                                            ::
++  apex  {p/@uvI q/(map @ta @uvI) r/(map @ta $~)}      ::  node report (old)
++  ares  (unit {p/term q/(list tank)})                 ::  possible error
++  bale                                                ::  driver state
  |*  a/_*                                              ::  %jael keys type
  $:  {our/ship now/@da eny/@uvI byk/beak}              ::  base info
      {usr/iden dom/(list @t)}                          ::  req user, domain
      key/a                                             ::  secrets from %jael
  ==                                                    ::
++  iden  knot:?($~ @ta)                                ::  username
++  sec-move                                            ::  driver effect
  $%  {$send p/hiss}                                    ::  http out
      {$show p/purl}                                    ::  direct user to url
      {$give p/httr}                                    ::  respond immediately
      {$redo $~}                                         ::  restart request qeu
  ==                                                    ::
++  ball  @uw                                           ::  statement payload
++  bait  {p/skin q/@ud r/dove}                         ::  fmt nrecvd spec
++  beam  {{p/ship q/desk r/case} s/path}               ::  global name
++  beak  {p/ship q/desk r/case}                        ::  garnish with beak
++  bird                                                ::  packet in travel
          $:  gom/soap                                  ::  message identity
              mup/@ud                                   ::  pktno in msg
              nux/@ud                                   ::  xmission count
              lys/@da                                   ::  last sent
              pac/rock                                  ::  packet data
          ==                                            ::
++  bitt  (map bone (pair ship path))                   ::  incoming subs
++  blob                                                ::  fs blob
          $%  {$delta p/lobe q/{p/mark q/lobe} r/page}  ::  delta on q
              {$direct p/lobe q/page}                   ::  immediate
          ==                                            ::
++  boat  (map (pair bone wire) (trel bean ship path))  ::  outgoing subs
++  boon                                                ::  fort output
          $%  {$beer p/ship}                            ::  gained ownership
              {$bier p/ship q/deed}                     ::  unsigned deed
              {$cake p/sock q/soap r/coop s/duct}       ::  e2e message result
              {$coke p/sock q/soap r/cape s/duct}       ::  message result
              {$mead p/lane q/rock}                     ::  accept packet
              {$milk p/sock q/soap r/*}                 ::  accept message
              {$mulk p/sock q/soap r/*}                 ::  e2e pass message
              {$ouzo p/lane q/rock}                     ::  transmit packet
              {$wine p/sock q/tape}                     ::  notify user
          ==                                            ::
++  bowl                                                ::  standard app state
        $:  $:  our/ship                                ::  host
                src/ship                                ::  guest
                dap/term                                ::  agent
            ==                                          ::  
            $:  wex/boat                                ::  outgoing subs
                sup/bitt                                ::  incoming subs
            ==                                          ::
            $:  ost/bone                                ::  opaque cause
                act/@ud                                 ::  change number
                eny/@uvI                                ::  entropy
                now/@da                                 ::  current time
                byk/beak                                ::  load source
        ==  ==                                          ::
++  bray  {p/life q/(unit life) r/ship s/@da}           ::  our parent us now
++  brow  {p/@da q/@tas}                                ::  browser version
++  buck  {p/mace q/will}                               ::  all security data
++  cake  {p/sock q/skin r/@}                           ::  top level packet
++  cape                                                ::  end-to-end result
          $?  $good                                     ::  delivered
              $dead                                     ::  rejected
          ==                                            ::
++  care  ?($u $v $w $x $y $z)                          ::  clay submode
++  case                                                ::  ship desk case spur
          $%  {$da p/@da}                               ::  date
              {$tas p/@tas}                             ::  label
              {$ud p/@ud}                               ::  number
          ==                                            ::
++  chum  @uvI                                          ::  hashed passcode
++  clot                                                ::  symmetric record
          $:  yed/(unit {p/hand q/code})                ::  outbound
              heg/(map hand code)                       ::  proposed
              qim/(map hand code)                       ::  inbound
          ==                                            ::
++  coal  *                                             ::  untyped vase
++  code  @uvI                                          ::  symmetric key
++  cone                                                ::  reconfiguration
          $%  {$& p/twig}                               ::  transform
              {$| p/(list @tas)}                        ::  alter
          ==                                            ::
++  chum  @uvI                                          ::  hashed passcode
++  claw                                                ::  startup chain
          $:  joy/(unit coal)                           ::  local context
              ran/(unit coal)                           ::  arguments
              pux/(unit path)                           ::  execution path
              jiv/(unit coal)                           ::  app configuration
              kyq/(unit coal)                           ::  app customization
              gam/(unit coal)                           ::  app image
          ==                                            ::
++  clip  (each @if @is)                                ::  client IP
++  coal  *                                             ::  untyped vase
++  code  @uvI                                          ::  symmetric key
++  cone                                                ::  reconfiguration
          $%  {$& p/twig}                                ::  transform
              {$| p/(list @tas)}                        ::  alter
          ==                                            ::
++  coop  (unit ares)                                   ::  e2e ack
++  cred                                                ::  credential
          $:  hut/hart                                  ::  client host
              aut/(jug @tas @t)                         ::  client identities
              orx/oryx                                  ::  CSRF secret
              acl/(unit @t)                             ::  accept-language
              cip/(each @if @is)                        ::  client IP
              cum/(map @tas *)                          ::  custom dirt
          ==                                            ::
++  deed  {p/@ q/step r/?}                              ::  sig stage fake?
++  dome                                                ::  project state
          $:  ank/ankh                                  ::  state
              let/@ud                                   ::  top id
              hit/(map @ud tako)                        ::  changes by id
              lab/(map @tas @ud)                        ::  labels
          ==                                            ::
++  dore                                                ::  foreign contact
          $:  wod/road                                  ::  connection to
              wyl/will                                  ::  inferred mirror
              caq/clot                                  ::  symmetric key state
          ==                                            ::
++  dove  {p/@ud q/(map @ud @)}                         ::  count hash 13-blocks
++  epic                                                ::  FCGI parameters
          $:  qix/(map @t @t)                           ::  query
              ced/cred                                  ::  client credentials
              bem/beam                                  ::  original path
              but/path                                  ::  ending
          ==                                            ::
++  flap  @uvH                                          ::  network packet id
++  flow                                                ::  packet connection
          $:  rtt/@dr                                   ::  decaying avg rtt
              wid/@ud                                   ::  logical wdow msgs
          ==                                            ::
++  gank  (each vase (list tank))                       ::  abstract result
++  gilt  {@tas *}                                      ::  presumed gift
++  gens  {p/lang q/gcos}                               ::  general identity
++  germ                                                ::  merge style
          $?  $init                                     ::  new desk
              $this                                     ::  ours with parents
              $that                                     ::  hers with parents
              $fine                                     ::  fast forward
              $meet                                     ::  orthogonal files
              $mate                                     ::  orthogonal changes
              $meld                                     ::  force merge
          ==                                            ::
++  gcos                                                ::  id description
          $%  {$czar $~}                                ::  8-bit ship
              {$duke p/what}                            ::  32-bit ship
              {$earl p/@t}                              ::  64-bit ship
              {$king p/@t}                              ::  16-bit ship
              {$pawn p/(unit @t)}                       ::  128-bit ship
          ==                                            ::
++  govt  path                                          ::  country+postcode
++  hand  @uvH                                          ::  hash of code
++  hart  {p/? q/(unit @ud) r/host}                     ::  http sec+port+host
++  hate  {p/purl q/@p r/moth}                          ::  semi-cooked request
++  heir  {p/@ud q/mess r/(unit love)}                  ::  status+headers+data
++  hiss  {p/purl q/moth}                               ::  outbound request
++  hist  {p/@ud q/(list @t)}                           ::  depth texts
++  hole  @t                                            ::  session identity
++  hort  {p/(unit @ud) q/host}                         ::  http port+host
++  host  (each (list @t) @if)                          ::  http host
++  hoke  %+  each   {$localhost $~}                    ::  local host
          ?($.0.0.0.0 $.127.0.0.1)                      ::
++  httq                                                ::  raw http request
          $:  p/meth                                    ::  method
              q/@t                                      ::  unparsed url
              r/(list {p/@t q/@t})                      ::  headers
              s/(unit octs)                             ::  body
          ==                                            ::
++  httr  {p/@ud q/mess r/(unit octs)}                  ::  raw http response
++  httx                                                ::  encapsulated http
          $:  p/?                                       ::  https?
              q/clip                                    ::  source IP
              r/httq                                    ::
          ==                                            ::
++  kite  {p/care q/case r/ship s/desk t/spur}          ::  parsed global name
++  json                                                ::  normal json value
          $@  $~                                        ::  null
          $%  {$a p/(list json)}                        ::  array
              {$b p/?}                                  ::  boolean
              {$o p/(map @t json)}                      ::  object
              {$n p/@ta}                                ::  number
              {$s p/@t}                                 ::  string
          ==                                            ::
++  lamb                                                ::  short path
          $%  {$& p/@tas}                               ::  auto
              {$| p/twig}                               ::  manual
          ==                                            ::
++  lane                                                ::  packet route
          $%  {$if p/@da q/@ud r/@if}                   ::  IP4/public UDP/addr
              {$is p/@ud q/(unit lane) r/@is}           ::  IPv6 w+alternates
              {$ix p/@da q/@ud r/@if}                   ::  IPv4 provisional
          ==                                            ::
++  lang  @ta                                           ::  IETF lang as code
++  lark  {p/(unit @tas) q/lawn}                        ::  parsed command
++  lass  ?($0 $1 $2)                                   ::  power increment
++  lath  $%                                            ::  pipeline stage
              {$0 p/lass q/lamb r/(list cone) s/twig}   ::  command
              {$1 p/twig}                               ::  generator
              {$2 p/twig}                               ::  filter
          ==                                            ::
++  lawn  (list lath)                                   ::
++  lice  {p/ship q/buck}                               ::  full license
++  life  @ud                                           ::  regime number
++  lint  (list rock)                                   ::  fragment array
++  lobe  @uvI                                          ::  blob ref
++  love  $%                                            ::  http response
              {$ham p/manx}                             ::  html node
              {$mid p/mite q/octs}                      ::  mime-typed data
              {$raw p/httr}                             ::  raw http response
              {$wan p/wain}                             ::  text lines
              {$zap p/@ud q/(list tank)}                ::  status+error
          ==                                            ::
++  maki  {p/@ta q/@ta r/@ta s/path}                    ::
++  mace  (list {p/life q/ring})                        ::  private secrets
++  math  (map @t (list @t))                            ::  semiparsed headers
++  meal                                                ::  payload
          $%  {$back p/cape q/flap r/@dr}               ::  acknowledgment
              {$buck p/coop q/flap r/@dr}               ::  e2e ack
              {$bond p/life q/path r/@ud s/*}           ::  message
              {$bund p/life q/path r/@ud s/*}           ::  e2e message
              {$carp p/@ q/@ud r/@ud s/flap t/@}        ::  skin+inx+cnt+hash
              {$fore p/ship q/(unit lane) r/@}          ::  forwarded packet
          ==                                            ::
++  mess  (list {p/@t q/@t})                            ::  raw http headers
++  meta                                                ::  path metadata
          $%  {$& q/@uvI}                               ::  hash
              {$| q/(list @ta)}                         ::  dir
          ==                                            ::
++  meth                                                ::  http methods
          $?  $conn                                     ::  CONNECT
              $delt                                     ::  DELETE
              $get                                      ::  GET
              $head                                     ::  HEAD
              $opts                                     ::  OPTIONS
              $post                                     ::  POST
              $put                                      ::  PUT
              $trac                                     ::  TRACE
          ==                                            ::
++  mime  {p/mite q/octs}                               ::  mimetyped data
++  mite  (list @ta)                                    ::  mime type
++  miso                                                ::  ankh delta
          $%  {$del $~}                                 ::  delete
              {$ins p/cage}                             ::  insert
              {$dif p/cage}                             ::  mutate from diff
              {$mut p/cage}                             ::  mutate from raw
          ==                                            ::
++  misu                                                ::  computed delta
          $%  {$del $~}                                 ::  delete
              {$ins p/cage}                             ::  insert
              {$dif p/lobe q/cage}                      ::  mutate from diff
          ==                                            ::
++  mizu  {p/@u q/(map @ud tako) r/rang}                ::  new state
++  moar  {p/@ud q/@ud}                                 ::  normal change range
++  moat  {p/case q/case r/path}                        ::  change range
++  mood  {p/care q/case r/path}                        ::  request in desk
++  moth  {p/meth q/math r/(unit octs)}                 ::  http operation
++  name  {p/@t q/(unit @t) r/(unit @t) s/@t}           ::  first mid+nick last
++  newt  ?($boot $kick $mess $slay $wake)              ::  lifecycle events
++  nori                                                ::  repository action
          $%  {$& p/soba}                               ::  delta
              {$| p/@tas}                               ::  label
          ==                                            ::
++  nuri                                                ::  repository action
          $%  {$& p/suba}                               ::  delta
              {$| p/@tas}                               ::  label
          ==                                            ::
++  octs  {p/@ud q/@t}                                  ::  octet-stream
++  oryx  @t                                            ::  CSRF secret
++  page  (cask *)                                      ::  untyped cage
++  pail  ?($none $warm $cold)                          ::  connection status
++  plan  (trel view (pair @da (unit @dr)) path)        ::  subscription
++  plea  {p/@ud q/{p/? q/@t}}                          ::  live prompt
++  plop  blob                                          ::  unvalidated blob
++  pork  {p/(unit @ta) q/(list @t)}                    ::  fully parsed url
++  pred  {p/@ta q/@tas r/@ta $~}                       ::  proto-path
++  prod  {p/prom q/tape r/tape}                        ::  prompt
++  prom  ?($text $pass $none)                          ::  format type
++  purf  (pair purl (unit @t))                         ::  url with fragment
++  purl  {p/hart q/pork r/quay}                        ::  parsed url
++  putt                                                ::  outgoing message
          $:  ski/snow                                  ::  sequence acked+sent
              wyv/(list rock)                           ::  packet list XX gear
          ==                                            ::
++  pyre                                                ::  cascade stash
          $:  p/(map {p/path q/path r/coal} coal)       ::  by path
              q/(map {p/path q/@uvI r/coal} coal)       ::  by source hash
              r/(map {p/* q/coal} coal)                 ::  by (soft) twig
          ==                                            ::
++  quay  (list {p/@t q/@t})                            ::  parsed url query
++  quri                                                ::  request-uri
          $%  {$& p/purl}                               ::  absolute
              {$| p/pork q/quay}                        ::  relative
          ==                                            ::
++  race                                                ::  inbound stream
          $:  did/@ud                                   ::  filled sequence
              dod/?                                     ::  not processing
              bum/(map @ud ares)                        ::  nacks
              mis/(map @ud {p/cape q/lane r/flap s/(unit)}) ::  misordered
          ==                                            ::
++  rank  ?($czar $king $duke $earl $pawn)              ::  ship width class
++  rang  $:  hut/(map tako yaki)                       ::
              lat/(map lobe blob)                       ::
          ==                                            ::
++  rand                                                ::  unvalidated rant
          $:  p/{p/care q/case r/@tas}                  ::  clade release book
              q/path                                    ::  spur
              r/page                                    ::  data
          ==                                            ::
++  rant                                                ::  namespace binding
          $:  p/{p/care q/case r/@tas}                  ::  clade release book
              q/path                                    ::  spur
              r/cage                                    ::  data
          ==                                            ::
++  rave                                                ::  general request
          $%  {$sing p/mood}                            ::  single request
              {$next p/mood}                            ::  next version
              {$many p/? q/moat}                        ::  change range
          ==                                            ::
++  rill                                                ::  outbound stream
          $:  sed/@ud                                   ::  sent
              san/(map @ud duct)                        ::  outstanding
          ==                                            ::
++  riot  (unit rant)                                   ::  response+complete
++  road                                                ::  secured oneway route
          $:  exp/@da                                   ::  expiration date
              lun/(unit lane)                           ::  route to friend
              lew/will                                  ::  will of friend
          ==                                            ::
++  rock  @uvO                                          ::  packet
++  rout  {p/(list host) q/path r/oryx s/path}          ::  http route (new)
++  rump  {p/care q/case r/@tas s/path}                 ::  relative path
++  saba  {p/ship q/@tas r/moar s/dome}                 ::  patch+merge
++  sack  {p/ship q/ship}                               ::  incoming [our his}
++  scar                                                ::  opaque duct
          $:  p/@ud                                     ::  bone sequence
              q/(map duct bone)                         ::  by duct
              r/(map bone duct)                         ::  by bone
          ==                                            ::
++  sufi                                                ::  domestic host
          $:  hoy/(list ship)                           ::  hierarchy
              val/wund                                  ::  private keys
              law/will                                  ::  server will
              seh/(map hand {p/ship q/@da})             ::  key cache
              hoc/(map ship dore)                       ::  neighborhood
          ==                                            ::
++  salt  @uv                                           ::  entropy
++  seal                                                ::  auth conversation
          $:  whu/(unit ship)                           ::  client identity
              pul/purl                                  ::  destination url
              wit/?                                     ::  wait for partner
              foy/(unit {p/ship q/hole})                ::  partner to notify
              pus/(unit @ta)                            ::  password
          ==                                            ::
++  sect  ?($black $blue $red $orange $white)           ::  banner
++  shed                                                ::  packet flow
          $:  $:  rtt/@dr                               ::  smoothed rtt
                  rto/@dr                               ::  retransmit timeout
                  rtn/(unit @da)                        ::  next timeout
                  rue/(unit @da)                        ::  last heard from
              ==                                        ::
              $:  nus/@ud                               ::  number sent
                  nif/@ud                               ::  number live
                  nep/@ud                               ::  next expected
                  caw/@ud                               ::  logical window
                  cag/@ud                               ::  congest thresh
              ==                                        ::
              $:  diq/(map flap @ud)                    ::  packets sent
                  pyz/(map soup @ud)                    ::  message+unacked
                  puq/(qeu {p/@ud q/soul})              ::  packet queue
              ==                                        ::
          ==                                            ::
++  skit  {p/(unit @ta) q/(list @ta) r/(list @ta)}      ::  tracking path
++  skin  ?($none $open $fast $full)                    ::  encoding stem
++  snow  {p/@ud q/@ud r/(set @ud)}                     ::  window exceptions
++  soap  {p/{p/life q/life} q/path r/@ud}              ::  statement id
++  soup  {p/path q/@ud}                                ::  new statement id
++  soul                                                ::  packet in travel
          $:  gom/soup                                  ::  message identity
              nux/@ud                                   ::  xmission count
              liv/?                                     ::  deemed live
              lys/@da                                   ::  last sent
              pac/rock                                  ::  packet data
          ==                                            ::
++  soba  (list {p/path q/miso})                        ::  delta
++  sock  {p/ship q/ship}                               ::  outgoing [from to]
++  spur  path                                          ::  ship desk case spur
++  step  {p/bray q/gens r/pass}                        ::  identity stage
++  suba  (list {p/path q/misu})                        ::  delta
++  tako  @                                             ::  yaki ref
++  tick  @ud                                           ::  process id
++  toro  {p/@ta q/nori}                                ::  general change
++  town                                                ::  all security state
          $:  lit/@ud                                   ::  imperial modulus
              any/@                                     ::  entropy
              urb/(map ship sufi)                       ::  all keys and routes
              fak/?                                     ::
          ==                                            ::
++  tube  {p/@ta q/@ta r/@ta s/path}                    ::  canonical path
++  tutu  *                                             ::  presumed type
++  yaki  {p/(list tako) q/(map path lobe) r/tako t/@da} ::  commit
++  view  ?($u $v $w $x $y $z)                          ::  view mode
++  waks  (map path woof)                               ::  list file states
++  what                                                ::  logical identity
          $%  {$anon $~}                                ::  anonymous
              {$lady p/whom}                            ::  female person ()
              {$lord p/whom}                            ::  male person []
              {$punk p/sect q/@t}                       ::  opaque handle ""
          ==                                            ::
++  whom  {p/@ud q/govt r/sect s/name}                  ::  year+govt+id
++  woof  $@  $know                                     ::  udon transform
              {$chan (list {$@(@ud {p/@ud q/@ud})})}    ::
++  wund  (list {p/life q/ring r/acru})                 ::  mace in action
++  will  (list deed)                                   ::  certificate
++  zuse  %310                                          ::  hoon+zuse kelvin
::          ::
::::        ::::  this will become `%york`, vane structures.
  ::          ::
++  gift-ames                                           ::  out result <-$
          $%  {$drop $~}                                ::  drop packet
              {$hear p/lane q/@}                        ::  receive packet
              {$east p/sock q/*}                        ::  network response
              {$init p/@p}                              ::  report install
              {$mass p/mass}                            ::  memory usage
              {$send p/lane q/@}                        ::  transmit packet
              {$waft p/ship q/path r/*}                 ::  response message
              {$wart p/sock q/@tas r/path s/*}          ::  network request
              {$went p/ship q/cape}                     ::  reaction message
              {$woot p/ship q/path r/coop}              ::  e2e reaction message
          ==                                            ::
++  kiss-ames                                           ::  in request ->$
          $%  ::  {$born p/@p q/@pG r/?}                ::  ticket birth
              {$barn $~}                                ::  new unix process
              {$crud p/@tas q/(list tank)}              ::  error with trace
              {$cash p/@p q/buck}                       ::  civil license
              ::  {$funk p/@p q/@p r/@}                 ::  symtix from/to/key
              {$hear p/lane q/@}                        ::  receive packet
              {$hole p/lane q/@}                        ::  packet failed
              {$junk p/@}                               ::  entropy
              {$kick p/@da}                             ::  wake up
              {$make p/(unit @t) q/@ud r/@ s/?}         ::  wild license
              {$sith p/@p q/@uw r/?}                    ::  imperial generator
              {$wake $~}                                ::  timer activate
              {$want p/sock q/path r/*}                 ::  send message
              {$wegh $~}                                ::  report memory
              {$wont p/sock q/path r/*}                 ::  e2e send message
          ==                                            ::
++  card-ames                                           ::  out cards
          $%  {$went p/sack q/path r/@ud s/coop}        ::  response confirm 
              {$west p/sack q/path r/@ud s/*}           ::  network request
          ==                                            ::
++  note-ames                                           ::  out request $->
          $%  {$c card-ames}                            ::  to %clay
              {$e card-ames}                            ::  to %eyre
              {$g card-ames}                            ::  to %gall
          ==                                            ::  
::
::::    %behn
  ::
++  gift-behn                                           ::  out result <-$
          $%  {$mass p/mass}                            ::  memory usage
              {$wake $~}                                ::  wakey-wakey
          ==                                            ::
++  kiss-behn                                           ::  in request ->$
          $%  {$rest p/@da}                             ::  cancel alarm
              {$wait p/@da}                             ::  set alarm
              {$wake $~}                                ::  timer activate
              {$wegh $~}                                ::  report memory
          ==                                            ::
::
::::    %clay
  ::
++  khan                                                ::
          $:  fil/(unit (unit cage))                    ::  XX see khan-to-soba
              dir/(unit (map @ta (unit khan)))          ::
          ==                                            ::
++  mode  (list {path (unit mime)})                     ::
++  riff  {p/desk q/(unit rave)}                        ::  request+desist
::::                                                    ::
++  gift-clay                                           ::  out result <-$
          $%  {$ergo p/@tas q/mode}                     ::  version update
              {$hill p/(list @tas)}                     ::  mount points
              {$mack p/(unit tang)}                     ::  ack
              {$mass p/mass}                            ::  memory usage
              {$mere p/(each (set path) (pair term tang))}  ::  merge result
              {$note p/@tD q/tank}                      ::  debug message
              {$ogre p/@tas}                            ::  delete mount point
              {$writ p/riot}                            ::  response
          ==                                            ::
++  kiss-clay                                           ::  in request ->$
          $%  {$boat $~}                                ::  pier rebooted
              {$drop p/@p q/@tas}                       ::  cancel pending merge
              {$info p/@p q/@tas r/nori}                ::  internal edit
              {$init p/@p}                              ::  report install
              {$into p/@tas q/? r/mode}                 ::  external edit
              {$merg p/@p q/@tas r/@p s/@tas t/case u/germ}  ::  merge desks
              {$mont p/@tas q/@p r/@tas s/path}         ::  mount to unix
              {$ogre p/$@(@tas beam)}                   ::  delete mount point
              {$warp p/sock q/riff}                     ::  file request
              {$wegh $~}                                ::  report memory
              {$went p/sack q/path r/@ud s/coop}        ::  response confirm 
              {$west p/sack q/path r/@ud s/*}           ::  network request
          ==                                            ::
::
::::
  ::
++  blew  {p/@ud q/@ud}                                 ::  columns rows
++  belt                                                ::  old belt
  $%  {$aro p/?($d $l $r $u)}                           ::  arrow key
      {$bac $~}                                         ::  true backspace
      {$ctl p/@c}                                       ::  control-key
      {$del $~}                                         ::  true delete
      {$met p/@c}                                       ::  meta-key
      {$ret $~}                                         ::  return
      {$txt p/(list @c)}                                ::  utf32 text
  ==                                                    ::
++  blit                                                ::  old blit
  $%  {$bel $~}                                         ::  make a noise
      {$clr $~}                                         ::  clear the screen
      {$hop p/@ud}                                      ::  set cursor position
      {$lin p/(list @c)}                                ::  set current line
      {$mor $~}                                         ::  newline
      {$sag p/path q/*}                                 ::  save to jamfile
      {$sav p/path q/@}                                 ::  save to file
      {$url p/@t}                                       ::  activate url
  ==                                                    ::
++  dill-belt                                           ::  new belt
  $%  {$aro p/?($d $l $r $u)}                           ::  arrow key
      {$bac $~}                                         ::  true backspace
      {$cru p/@tas q/(list tank)}                       ::  echo error
      {$ctl p/@}                                        ::  control-key
      {$del $~}                                         ::  true delete
      {$hey $~}                                         ::  refresh
      {$met p/@}                                        ::  meta-key
      {$ret $~}                                         ::  return
      {$rez p/@ud q/@ud}                                ::  resize, cols, rows
      {$txt p/(list @c)}                                ::  utf32 text
      {$yow p/gill}                                     ::  connect to app
  ==                                                    ::
++  dill-blit                                           ::  new blit
  $%  {$bel $~}                                         ::  make a noise
      {$clr $~}                                         ::  clear the screen
      {$hop p/@ud}                                      ::  set cursor position
      {$mor p/(list dill-blit)}                         ::  multiple blits
      {$pro p/(list @c)}                                ::  show as cursor+line
      {$qit $~}                                         ::  close console
      {$out p/(list @c)}                                ::  send output line
      {$sag p/path q/*}                                 ::  save to jamfile
      {$sav p/path q/@}                                 ::  save to file
      {$url p/@t}                                       ::  activate url
  ==                                                    ::
++  flog                                                ::  sent to %dill
  $%  {$crud p/@tas q/(list tank)}                      ::
      {$heft $~}                                        ::
      {$text p/tape}                                    ::
      {$veer p/@ta q/path r/@t}                         ::  install vane
      {$vega p/path}                                    ::  reboot by path
      {$verb $~}                                        ::  verbose mode
  ==                                                    ::
++  gill  (pair ship term)                              ::  general contact
::::
++  gift-dill                                           ::  out result <-$
  $%  {$bbye $~}                                        ::  reset prompt
      {$blit p/(list blit)}                             ::  terminal output
      {$burl p/@t}                                      ::  activate url
      {$init p/@p}                                      ::  set owner
      {$logo $~}                                        ::  logout
      {$mass p/mass}                                    ::  memory usage
      {$veer p/@ta q/path r/@t}                         ::  install vane
      {$vega p/path}                                    ::  reboot by path
      {$verb $~}                                        ::  verbose mode
  ==                                                    ::
++  kiss-dill                                           ::  in request ->$
  $%  {$belt p/belt}                                    ::  terminal input
      {$blew p/blew}                                    ::  terminal config
      {$boot p/*}                                       ::  weird %dill boot
      {$crud p/@tas q/(list tank)}                      ::  error with trace
      {$flog p/flog}                                    ::  wrapped error
      {$flow p/@tas q/(list gill)}                      ::  terminal config
      {$hail $~}                                        ::  terminal refresh
      {$heft $~}                                        ::  memory report
      {$hook $~}                                        ::  this term hung up
      {$harm $~}                                        ::  all terms hung up
      {$init p/ship}                                    ::  after gall ready
      {$tick p/@p q/@p}                                 ::  initial ticket
      {$noop $~}                                        ::  no operation
      {$talk p/tank}                                    ::
      {$text p/tape}                                    ::
      {$veer p/@ta q/path r/@t}                         ::  install vane
      {$vega p/path}                                    ::  reboot by path
      {$verb $~}                                        ::  verbose mode
  ==                                                    ::
::
::::    %eyre
  ::
++  gram                                                ::  inter-ship message
  =+  fuz={path @ud *}                                  ::  ames format
  $?  {{$lon $~} p/hole}                                ::  login request
      {{$aut $~} p/hole}                                ::  login reply
      {{$hat $~} p/hole q/hart}                         ::  login redirect
      {{$get $~} p/@uvH q/{? clip httq}}                ::  remote request
      {{$got $~} p/@uvH q/httr}                         ::  remote response
      {{$gib $~} p/@uvH}                                ::  remote cancel
  ==                                                    ::
::::                                                    ::
++  kiss-eyre                                           ::  in request ->$
          $%  {$born $~}                                ::  new unix process
              {$crud p/@tas q/(list tank)}              ::  XX rethink
              {$hiss p/(unit iden) q/mark r/cage}       ::  outbound user req
              {$init p/@p}                              ::  report install
              {$serv p/$@(desk beam)}                   ::  set serving root
              {$them p/(unit hiss)}                     ::  outbound request
              {$they p/@ud q/httr}                      ::  inbound response
              {$chis p/? q/clip r/httq}                 ::  IPC inbound request
              {$this p/? q/clip r/httq}                 ::  inbound request
              {$thud $~}                                ::  inbound cancel
              {$wegh $~}                                ::  report memory
              {$went p/sack q/path r/@ud s/coop}        ::  response confirm 
              {$west p/sack q/fuz:gram}                 ::  network request
          ==                                            ::
++  gift-eyre                                           ::  out result <-$
          $%  {$mass p/mass}                            ::  memory usage
              {$mack p/(unit tang)}                     ::  message ack
              {$sigh p/cage}                            ::  marked http response
              {$thou p/httr}                            ::  raw http response
              {$thus p/@ud q/(unit hiss)}               ::  http request+cancel
              {$veer p/@ta q/path r/@t}                 ::  drop-through
              {$vega p/path}                            ::  drop-through
          ==                                            ::
::
::::    %ford
  ::
++  hood                                                ::  assembly plan
  $:  zus/@ud                                           ::  zuse kelvin
      sur/(list hoof)                                   ::  structures
      lib/(list hoof)                                   ::  libraries
      fan/(list horn)                                   ::  resources
      src/(list hoop)                                   ::  program
  ==                                                    ::
++  hoof  (pair term (unit (pair case ship)))           ::  resource reference
++  hoop                                                ::  source in hood
  $%  {$& p/twig}                                       ::  direct twig
      {$| p/beam}                                       ::  resource location   
  ==                                                    ::
++  hops   {pre/(unit tyke) pof/(unit {p/@ud q/tyke})}  ::  XX late-bound path
++  horn                                                ::  resource tree
  $%  {$ape p/twig}                                     ::  /~  twig by hand
      {$arg p/twig}                                     ::  /$  argument
      {$alt p/(list horn)}                              ::  /|  options
      {$dep p/horn}                                     ::  /#  insert dephash
      {$dub p/term q/horn}                              ::  /=  apply face
      {$fan p/(list horn)}                              ::  /.  list
      {$for p/(list (pair spur horn))}                  ::  /,  switch by path
      {$hel p/horn}                                     ::  /%  propagate args
      {$lin p/(list mark) q/horn}                       ::  /&  translates
      {$man p/(map knot horn)}                          ::  /*  hetero map
      {$nap p/horn}                                     ::  /_  homo map
      {$now p/horn}                                     ::  DEPRECATED
      {$nod p/term q/horn}                              ::  /_  @  list by odor
      {$saw p/twig q/horn}                              ::  /;  operate on
      {$see p/hops q/horn}                              ::  /:  relative to
      {$sic p/twig q/horn}                              ::  /^  cast
      {$toy p/? q/mark}                                 ::  /mark/  static/hook
  ==                                                    ::
++  milk  (trel ship desk silk)                         ::  sourced silk
++  silk                                                ::  construction layer
  $^  {p/silk q/silk}                                   ::  cons
  $%  {$$ p/cage}                                       ::  literal
      {$alts p/(list silk)}                             ::  options
      {$bake p/mark q/coin r/beam}                      ::  local synthesis
      {$bunt p/mark}                                    ::  example of mark
      {$call p/silk q/silk}                             ::  slam
      {$cast p/mark q/silk}                             ::  translate
      {$core p/beam}                                    ::  build program
      {$diff p/silk q/silk}                             ::  diff
      {$dude p/(trap tank) q/silk}                      ::  error wrap
      {$file p/beam}                                    ::  from clay
      {$flag p/(set $@(@uvH beam)) q/silk}              ::  add dependencies
      {$join p/mark q/silk r/silk}                      ::  merge
      {$mash p/mark q/milk r/milk}                      ::  annotate
      {$mute p/silk q/(list (pair wing silk))}          ::  mutant
      {$pact p/silk q/silk}                             ::  patch
      {$plan p/beam q/coin r/hood}                      ::  structured assembly
      {$reef $~}                                        ::  kernel reef
      {$ride p/twig q/silk}                             ::  silk thru twig
      {$tabl p/(list (pair silk silk))}                 ::  list
      {$vale p/mark q/*}                                ::  validate
      {$volt p/(cask *)}                                ::  unsafe add type
  ==                                                    ::
::::
++  bilk  (pair beak silk)                              ::  sourced request
++  gage                                                ::  recursive cage+tang
  $%  {$& p/cage}                                       ::  success
      {$| p/tang}                                       ::  error
      {$tabl p/(list (pair gage gage))}                 ::  table of results
  ==
++  gift-ford                                           ::  out result <-$
          $%  {$made p/@uvH q/gage}                     ::  computed result
              {$mass p/mass}                            ::  memory usage
              {$news p/@uvH}                            ::  fresh depends
          ==                                            ::
++  kiss-ford                                           ::  in request ->$
          $%  {$exec p/@p q/(unit bilk)}                ::  make / kill
              {$wasp p/@p q/{@uvH ?}}                   ::  depends ask / kill
              {$wegh $~}                                ::  report memory
              {$wipe p/@p $~}                           ::  clear cache
          ==                                            ::
::
::::    %gall
  ::
++  club                                                ::  agent action
  $%  {$peel p/mark q/path}                             ::  translated peer
      {$peer p/path}                                    ::  subscribe
      {$poke p/cage}                                    ::  apply
      {$puff p/mark q/noun}                             ::  unchecked poke
      {$pull $~}                                        ::  unsubscribe
      {$punk p/mark q/cage}                             ::  translated poke
      {$pump $~}                                        ::  pump yes+no
  ==                                                    ::
++  cuft                                                ::  internal gift
  $%  {$coup p/(unit tang)}                             ::  poke result
      {$diff p/cage}                                    ::  subscription output
      {$doff p/mark q/noun}                             ::  untyped diff
      {$quit $~}                                        ::  close subscription
      {$reap p/(unit tang)}                             ::  peer result
  ==                                                    ::
++  culm                                                ::  config action
  $%  {$load p/scup}                                    ::  load+reload
  ::  {$kick $~}                                        ::  restart everything
  ::  {$stop $~}                                        ::  toggle suspend
  ::  {$wipe $~}                                        ::  destroy all state
  ==                                                    ::
++  cush  (pair term club)                              ::  internal kiss
++  dude  term                                          ::  server identity
++  scup  (pair ship desk)                              ::  autoupdate
++  well  (pair desk term)                              ::
++  suss  (trel dude @tas @da)                          ::  config report
::::                                                    ::
++  kiss-gall                                           ::  incoming request
  $%  {$conf p/dock q/culm}                             ::  configure app
      {$init p/ship}                                    ::  set owner
      {$deal p/sock q/cush}                             ::  full transmission
      {$went p/sack q/path r/@ud s/coop}                ::  response confirm 
      {$west p/sack q/path r/@ud s/*}                   ::  network request
      {$wegh $~}                                        ::  report memory
  ==                                                    ::
++  gift-gall                                           ::  outgoing result
  $%  {$mass p/mass}                                    ::  memory usage
      {$onto p/(each suss tang)}                        ::  about agent
      {$rend p/path q/*}                                ::  network request
      {$unto p/cuft}
      {$mack p/(unit tang)}                             ::  message ack
  ==                                                    ::
::
::::    %arvo
  ::
++  gift-arvo                                           ::  out result <-$
  $?  gift-ames
      gift-clay
      gift-dill
      gift-eyre
      gift-ford
      gift-gall
      gift-behn
  ==
++  kiss-arvo                                           ::  in request ->$
  $?  kiss-ames
      kiss-clay
      kiss-dill
      kiss-eyre
      kiss-ford
      kiss-gall
      kiss-behn
  ==
++  note-arvo                                           ::  out request $->
  $?  {@tas $meta vase}
  $%  {$a kiss-ames}
      {$b kiss-behn}
      {$c kiss-clay}
      {$d kiss-dill}
      {$e kiss-eyre}
      {$f kiss-ford}
      {$g kiss-gall}
  ==  ==
++  sign-arvo                                           ::  in result $<-
  $%  {$a gift-ames}
      {$b gift-behn}
      {$c gift-clay}
      {$d gift-dill}
      {$e gift-eyre}
      {$f gift-ford}
      {$g gift-gall}
  ==
--
