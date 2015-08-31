section 2eW, lite number theory
===============================

### `++egcd`

GCD

    ++  egcd                                                ::  schneier's egcd
      |=  [a=@ b=@]
      =+  si
      =+  [c=(sun a) d=(sun b)]
      =+  [u=[c=(sun 1) d=--0] v=[c=--0 d=(sun 1)]]
      |-  ^-  [d=@ u=@ v=@]
      ?:  =(--0 c)
        [(abs d) d.u d.v]
      ::  ?>  ?&  =(c (sum (pro (sun a) c.u) (pro (sun b) c.v)))
      ::          =(d (sum (pro (sun a) d.u) (pro (sun b) d.v)))
      ::      ==
      =+  q=(fra d c)
      %=  $
        c  (dif d (pro q c))
        d  c
        u  [(dif d.u (pro q c.u)) c.u]
        v  [(dif d.v (pro q c.v)) c.v]
      ==
    ::

Greatest common denominator

    ~zod/try=> (egcd 20 15)
    [d=5 u=2 v=1]
    ~zod/try=> (egcd 24 16)
    [d=8 u=2 v=1]
    ~zod/try=> (egcd 7 5)
    [d=1 u=3 v=6]
    ~zod/try=> (egcd (shaf ~ %ham) (shaf ~ %sam))
    [ d=1
      u=59.983.396.314.566.203.239.184.568.129.921.874.787  
      v=38.716.650.351.034.402.960.165.718.823.532.275.722
    ]

------------------------------------------------------------------------

### `++pram`

Probable prime

    ++  pram                                                ::  rabin-miller
      |=  a=@  ^-  ?
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
          |-  ^-  [s=@ t=@]
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

Probable prime test

    ~zod/try=> (pram 31)
    %.y
    ~zod/try=> =+(a=2 |-(?:(=(a 31) ~ [i=(mod 31 a) t=$(a +(a))])))
    ~[1 1 3 1 1 3 7 4 1 9 7 5 3 1 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1]
    ~zod/try=> =+(a=2 |-(?:(=(a 31) ~ [i=(mod 30 a) t=$(a +(a))])))
    ~[0 0 2 0 0 2 6 3 0 8 6 4 2 0 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0]
    ~zod/try=> (pram 256)
    %.n
    ~zod/try=> (pram (dec (bex 127)))
    %.y

------------------------------------------------------------------------

### `++ramp`

`r-m` prime

    ++  ramp                                                ::  make r-m prime
      |=  [a=@ b=(list ,@) c=@]  ^-  @ux                    ::  [bits snags seed]
      =>  .(c (shas %ramp c))
      =+  d=_@
      |-
      ?:  =((mul 100 a) d)
        ~|(%ar-ramp !!)
      =+  e=(~(raw og c) a)
      ?:  &((levy b |=(f=@ !=(1 (mod e f)))) (pram e))
        e
      $(c +(c), d (shax d))
    ::

Random `a` bit prime, which isn't 1 modulo a list of other numbers,
using salt `c`.

    ~zod/try=> (ramp 20 ~ %hamelok)
    0xf.1f0d
    ~zod/try=> (ramp 20 ~ %hameloe)
    0x2.d341
    ~zod/try=> (ramp 5 ~ %kole)
    0x1f
    ~zod/try=> (ramp 7 ~ %kole)
    0x4f
    ~zod/try=> (ramp 7 ~[0x4e] %kole)
    0x43
    ~zod/try=> `@uw`(ramp 128 ~ %late)
    0w3y.irKIL.l-pp1.2CkG4.3lsTF

------------------------------------------------------------------------

### `++fo`

Prime engine

    ++  fo                                                  ::  modulo prime
      |_  a=@

XX DO NOT RERUN GET.LS, THERE EXIST ARM COLLISIONS

Core for performing arithmetic modulo a prime number

    ~zod/try=> ~(. fo 79)
    <7.get [@ud <373.jdd 100.kzl 1.ypj %164>]>

------------------------------------------------------------------------

### `++dif`

Difference

      ++  dif
        |=  [b=@ c=@]
        (sit (sub (add a b) (sit c)))
      ::

Subtract

    ~zod/try=> (~(dif fo 79) 10 5)
    5
    ~zod/try=> (~(dif fo 79) 5 10)
    74

------------------------------------------------------------------------

### `++exp`

Exponent

      ++  exp
        |=  [b=@ c=@]
        ?:  =(0 b)
          1
        =+  d=$(b (rsh 0 1 b))
        =+  e=(pro d d)
        ?:(=(0 (end 0 1 b)) e (pro c e))
      ::

Exponent

    ~zod/try=> (~(exp fo 79) 3 5)
    46

------------------------------------------------------------------------

### `++fra`

Divide

      ++  fra
        |=  [b=@ c=@]
        (pro b (inv c))
      ::

Divide

    ~zod/try=> (~(fra fo 79) 20 4)
    5
    ~zod/try=> (~(fra fo 79) 7 11)
    15

------------------------------------------------------------------------

### `++inv`

Inverse

      ++  inv
        |=  b=@
        =+  c=(dul:si u:(egcd b a) a)
        c
      ::

Multiplicative inverse

    ~zod/try=> (~(inv fo 79) 12)
    33
    ~zod/try=> (~(pro fo 79) 12 33)
    1
    ~zod/try=> (~(inv fo 79) 0)
    0

------------------------------------------------------------------------

### `++pro`

Product

      ++  pro
        |=  [b=@ c=@]
        (sit (mul b c))
      ::

Product

    ~zod/try=> (~(pro fo 79) 5 10)
    50
    ~zod/try=> (~(pro fo 79) 5 20)
    21

------------------------------------------------------------------------

### `++sit`

Bounds

      ++  sit
        |=  b=@
        (mod b a)
      ::

Bounds check

    ~zod/try=> (~(sit fo 79) 9)
    9
    ~zod/try=> (~(sit fo 79) 99)
    20

------------------------------------------------------------------------

### `++sum`

Sum

      ++  sum
        |=  [b=@ c=@]
        (sit (add b c))
      --

Add

    ~zod/try=> (~(sum fo 79) 9 9)
    18
    ~zod/try=> (~(sum fo 79) 70 9)
    0

------------------------------------------------------------------------

### `++ga`

    ++  ga                                                  ::  GF (bex p.a)
      |=  a=[p=@ q=@ r=@]                                   ::  dim poly gen
      =+  si=(bex p.a)
      =+  ma=(dec si)
      =>  |%

RSA internals

XX document

------------------------------------------------------------------------

### `++dif`

          ++  dif                                           ::  add and sub
            |=  [b=@ c=@]
            ~|  [%dif-ga a]
            ?>  &((lth b si) (lth c si))
            (mix b c)
          ::

XX document

------------------------------------------------------------------------

### `++dub`

          ++  dub                                           ::  mul by x
            |=  b=@
            ~|  [%dub-ga a]
            ?>  (lth b si)
            ?:  =(1 (cut 0 [(dec p.a) 1] b))
              (dif (sit q.a) (sit (lsh 0 1 b)))
            (lsh 0 1 b)
          ::

XX document

------------------------------------------------------------------------

### `++pro`

          ++  pro                                           ::  slow multiply
            |=  [b=@ c=@]
            ?:  =(0 b)
              0
            ?:  =(1 (dis 1 b))
              (dif c $(b (rsh 0 1 b), c (dub c)))
            $(b (rsh 0 1 b), c (dub c))
          ::

XX document

------------------------------------------------------------------------

### `++toe`

          ++  toe                                           ::  exp/log tables
            =+  ^=  nu
                |=  [b=@ c=@]
                ^-  (map ,@ ,@)
                =+  d=*(map ,@ ,@)
                |-
                ?:  =(0 c)
                  d
                %=  $
                  c  (dec c)
                  d  (~(put by d) c b)
                ==
            =+  [p=(nu 0 (bex p.a)) q=(nu ma ma)]
            =+  [b=1 c=0]
            |-  ^-  [p=(map ,@ ,@) q=(map ,@ ,@)]
            ?:  =(ma c)
              [(~(put by p) c b) q]
            %=  $
              b  (pro r.a b)
              c  +(c)
              p  (~(put by p) c b)
              q  (~(put by q) b c)
            ==
          ::

XX document

------------------------------------------------------------------------

### `++sit`

          ++  sit                                           ::  reduce
            |=  b=@
            (mod b (bex p.a))
          --

XX document

------------------------------------------------------------------------

### `++fra`

      ++  fra                                               ::  divide
        |=  [b=@ c=@]
        (pro b (inv c))
      ::

XX document

------------------------------------------------------------------------

### `++inv`

      ++  inv                                               ::  invert
        |=  b=@
        ~|  [%inv-ga a]
        =+  c=(~(get by q) b)
        ?~  c  !!
        =+  d=(~(get by p) (sub ma u.c))
        (need d)
      ::

XX document

------------------------------------------------------------------------

### `++pow`

      ++  pow                                               ::  exponent
        |=  [b=@ c=@]
        =+  [d=1 e=c f=0]
        |-
        ?:  =(p.a f)
          d
        ?:  =(1 (cut 0 [f 1] b))
          $(d (pro d e), e (pro e e), f +(f))
        $(e (pro e e), f +(f))
      ::

XX document

------------------------------------------------------------------------

### `++pro`

      ++  pro                                               ::  multiply
        |=  [b=@ c=@]
        ~|  [%pro-ga a]
        =+  d=(~(get by q) b)
        ?~  d  0
        =+  e=(~(get by q) c)
        ?~  e  0
        =+  f=(~(get by p) (mod (add u.d u.e) ma))
        (need f)
      --

XX document

------------------------------------------------------------------------
