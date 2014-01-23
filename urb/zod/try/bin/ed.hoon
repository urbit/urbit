!:
::  /=try=/bin/ed/hoon
::
::
=>  %=    .
        +
      =>  +
      |%
      ++  b  256
      ++  q  (sub (bex 255) 19)
      ++  l
        %+  add
          (bex 252)
        27.742.317.777.372.353.535.851.937.790.883.648.493
      ++  d  (~(dif fo q) 0 (~(fra fo q) 121.665 121.666))
      ++  ii  (~(exp fo q) (div (dec q) 4) 2)
      ::
      ++  norm  |=(x=@ ?:(=(0 (mod x 2)) x (sub q x)))
      ++  xrec
        |=  y=@  ^-  @
        =+  ^=  xx
            %+  mul  (~(dif fo q) (mul y y) 1)
                     (~(inv fo q) +(:(mul d y y)))
        =+  x=(~(exp fo q) (div (add 3 q) 8) xx)
        ?:  !=(0 (~(dif fo q) (mul x x) (~(sit fo q) xx)))
          (norm (~(pro fo q) x ii))
        (norm x)
      ::
      ++  bby  (~(pro fo q) 4 (~(inv fo q) 5))
      ++  bb  [(xrec bby) bby]
      ::
      ++  ward
        |=  [pp=[@ @] qq=[@ @]]  ^-  [@ @]
        =+  dp=:(~(pro fo q) d -.pp -.qq +.pp +.qq)
        =+  ^=  xt
            %+  ~(pro fo q)
              %+  ~(sum fo q)
                (~(pro fo q) -.pp +.qq)
              (~(pro fo q) -.qq +.pp)
            (~(inv fo q) (~(sum fo q) 1 dp))
        =+  ^=  yt
            %+  ~(pro fo q)
              %+  ~(sum fo q)
                (~(pro fo q) +.pp +.qq)
              (~(pro fo q) -.pp -.qq)
            (~(inv fo q) (~(dif fo q) 1 dp))
        [xt yt]
      ::
      ++  scam
        |=  [pp=[@ @] e=@]  ^-  [@ @]
        ?:  =(0 e)
          [0 1]
        =+  qq=$(e (div e 2))
        =>  .(qq (ward qq qq))
        ?:  =(1 (dis 1 e))
          (ward qq pp)
        qq
      ::
      ++  etch
        |=  pp=[@ @]  ^-  @
        (can 0 ~[[(sub b 1) +.pp] [1 (dis 1 -.pp)]])
      ::
      ++  puck
        |=  sk=@  ^-  @
        =+  h=(shal 32 sk)
        =+  ^=  a
            %+  add
              (bex (sub b 2))
            (lsh 0 3 (cut 0 [3 (sub b 5)] h))
        =+  aa=(scam bb a)
        (etch aa)
      ::
      ++  sign
        |=  [m=@ sk=@ pk=@]  ^-  @
        =+  h=(shal 32 sk)
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
      ++  curv
        |=  [x=@ y=@]  ^-  ?
        .=  0
            %+  ~(dif fo q)
              %+  ~(sum fo q)
                (~(pro fo q) (sub q (~(sit fo q) x)) x)
              (~(pro fo q) y y)
            (~(sum fo q) 1 :(~(pro fo q) d x x y y))
      ++  decp
        |=  s=@  ^-  [@ @]
        =+  y=(cut 0 [0 (dec b)] s)
        =+  si=(cut 0 [(dec b) 1] s)
        =+  x=(xrec y)
        =>  .(x ?:(!=(si (dis 1 x)) (sub q x) x))
        =+  pp=[x y]
        ?.  (curv pp)  !!
        pp
      ++  veri
        |=  [s=@ m=@ pk=@]  ^-  ?
        ?:  (gth (div b 4) (met 3 s))  !!
        ?:  (gth (div b 8) (met 3 pk))  !!
        =+  rr=(decp (cut 0 [0 b] s))
        =+  aa=(decp pk)
        =+  ss=(cut 0 [b b] s)
        =+  ha=(can 0 ~[[b (etch rr)] [b pk] [(met 0 m) m]])
        =+  h=(shaz ha)
        =((scam bb ss) (ward rr (scam aa h)))
      --
    ==
|=  [est=time eny=@uw]
|=  [arg=@ ~]
^-  bowl
=+  sk=(cut 0 [0 b] (shaz eny))
~&  [%sk `@ux`sk]
=+  pk=(puck sk)
~&  [%pk `@ux`pk]
=+  si=(sign arg sk pk)
~&  [%si `@ux`si]
:_  ~  :_  ~
:-  %$
!>
=+  ^=  sis
    ?:  (veri si arg pk)
  'valid sig'
'invalid sig'
=+  ^=  fos
    ?.  (veri si +(arg) pk)
  'detected forgery'
'undetected forgery'
[sis fos]
