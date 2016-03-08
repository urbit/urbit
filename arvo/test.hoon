!:                                                      ::
::::    /hoon/hoon                                      ::
  ::                                                    ::
=>  %151  =>  
::                                                      ::
::::    0: version stub                                 ::
  ::                                                    ::
|%
++  hoon  +
--  =>
::                                                      ::
::::    1: layer one                                    ::
  ::                                                    ::
  ::      1a: basic arithmetic                          ::
  ::      1b: tree addressing                           ::
  ::      1c: molds and mold builders                   ::
  ::
|%
::                                                      ::
::::    1a: unsigned arithmetic and tree addressing     ::
  ::                                                    ::
  ::      add, dec, div, dvr, gte, gth, lte,            ::
  ::      lth, max, min, mod, mul, sub                  ::
  ::
++  add                                                 ::  unsigned addition
  ~/  %add
  |=  {a/@ b/@}
  ^-  @
  ?:  =(0 a)  b
  $(a (dec a), b +(b))
::
++  dec                                                 ::  unsigned decrement
  ~/  %dec
  |=  a/@
  ~>  %mean.[0 %leaf "decrement-underflow"]
  ?<  =(0 a)
  =+  b=0
  |-  ^-  @
  ?:  =(a +(b))  b
  $(b +(b))
::
++  div                                                 ::  unsigned divide
  ~/  %div
  =+  [a=`@`1 b=`@`1]
  |.
  ^-  @
  ~>  %mean.[0 %leaf "divide-by-zero"]
  ?<  =(0 b)
  =+  c=0
  |-
  ?:  (lth a b)  c
  $(a (sub a b), c +(c))
::
++  dvr                                                 ::  divide w/remainder
  ~/  %dvr
  |=  {a/@ b/@}  
  ^-  {p/@ q/@}
  [(div a b) (mod a b)]
::
++  gte                                                 ::  unsigned greater/eq
  ~/  %gte
  |=  {a/@ b/@}
  ^-  ?
  !(lth a b)
::
++  gth                                                 ::  unsigned greater
  ~/  %gth
  |=  {a/@ b/@}
  ^-  ?
  !(lte a b)
::
++  lte                                                 ::  unsigned less/eq
  ~/  %lte
  |=  {a/@ b/@}
  |(=(a b) (lth a b))
::
++  lth                                                 ::  unsigned less
  ~/  %lth
  |=  {a/@ b/@}
  ^-  ?
  ?&  !=(a b)
      |-
      ?|  =(0 a)
          ?&  !=(0 b)
              $(a (dec a), b (dec b))
  ==  ==  ==
::
++  max                                                 ::  unsigned maximum
  ~/  %max
  |=  {a/@ b/@}
  ^-  @
  ?:  (gth a b)  a
  b
::
++  min                                                 ::  unsigned minimum
  ~/  %min
  |=  {a/@ b/@}
  ^-  @
  ?:  (lth a b)  a
  b
::
++  mod                                                 ::  unsigned modulus
  ~/  %mod
  |:  [a=`@`1 b=`@`1]
  ^-  @
  ?<  =(0 b)
  (sub a (mul b (div a b)))
::
++  mul                                                 ::  unsigned multiply
  ~/  %mul
  |:  [a=`@`1 b=`@`1]
  ^-  @
  =+  c=0
  |-
  ?:  =(0 a)  c
  $(a (dec a), c (add b c))
::
++  sub                                                 ::  subtract
  ~/  %sub
  |=  {a/@ b/@}
  ~>  %mean.[0 %leaf "subtract-underflow"]
  ^-  @
  ?:  =(0 b)  a
  $(a (dec a), b (dec b))
::                                                      ::
::::  1b: tree addressing                               ::
  ::                                                    ::
  ::    cap, mas, peg                                   ::
  ::
++  cap                                                 ::  fragment head
  ~/  %cap
  |=  a/@
  ^-  ?($2 $3)
  ?-  a
    $2        %2
    $3        %3
    ?($0 $1)  !!
    *         $(a (div a 2))
  ==
::
++  mas                                                 ::  fragment body
  ~/  %mas
  |=  a/@
  ^-  @
  ?-  a
    $1   !!
    $2   1
    $3   1
    *    (add (mod a 2) (mul $(a (div a 2)) 2))
  ==
::
++  peg                                                 ::  fragment compose
  ~/  %peg
  |=  {a/@ b/@}
  ^-  @
  ?-  b
    $1  a
    $2  (mul a 2)
    $3  +((mul a 2))
    *   (add (mod b 2) (mul $(b (div b 2)) 2))
  ==
::                                                      ::
::::  1c: ideal containers                              ::
  ::                                                    ::
  ::    ache, bloq, each, gate, list, lone, pair, pole  ::
  ::    qual, quid, quip, trap, tree, trel, unit        ::
  ::
++  ache  |*({a/gate b/gate} $%({$| p/b} {$& p/a}))     ::  a or b, b default
++  bloq  @                                             ::  bitblock, eg 3=byte
++  each  |*({a/gate b/gate} $%({$& p/a} {$| p/b}))     ::  a or b, a default
++  gate  $-(* *)                                       ::  generic gate
++  list  |*(a/gate $@($~ {i/a t/(list a)}))            ::  nullterminated list 
++  lone  |*(a/gate p/a)                                ::  1-tuple
++  pair  |*({a/gate b/gate} {p/a q/b})                 ::  2-tuple
++  pole  |*(a/gate $@($~ {a (pole a)}))                ::  faceless list
++  qual  |*  {a/gate b/gate c/gate d/gate}             ::  4-tuple
          {p/a q/b r/c s/d}                             ::
++  quid  |*({a/gate b/*} {a _b})                       ::  mixed for sip
++  quip  |*({a/gate b/*} {(list a) _b})                ::  list-mixed for sip
++  trap  |*(a/gate _|?(*a))                            ::  producer
++  tree  |*(a/gate $@($~ {n/a l/(tree a) r/(tree a)})) ::  binary tree
++  trel  |*({a/gate b/gate c/gate} {p/a q/b r/c})      ::  3-tuple
++  unit  |*(a/gate $@($~ {$~ u/a}))                    ::  maybe
--  =>
::                                                      ::
::::  2: layer two                                      ::
  ::                                                    ::
  ::    2a: unit logic                                  ::
  ::    2b: list logic                                  ::
  ::    2c: bit arithmetic                              ::
  ::    2d: bit logic                                   ::
  ::    2e: insecure hashing                            ::
  ::    2f: noun ordering                               ::
  ::    2g: unsigned powers                             ::
  ::    2h: set logic                                   ::
  ::    2i: map logic                                   ::
  ::    2j: jar and jug logic                           ::
  ::    2k: queue logic                                 ::
  ::    2l: container from container                    ::
  ::    2m: container from noun                         ::
  ::    2n: functional hacks                            ::
  ::    2o: normalizing containers                      ::
  ::    2p: serialization                               ::
  ::    2q: molds
  ::
|%
::                                                      ::
::::  2a: unit logic                                    ::
  ::                                                    ::
  ::    biff, bind, bond, both, clap, drop,             ::
  ::    fall, flit, lift, mate, need, some              ::
  ::
++  biff                                                ::  apply
  |*  {a/(unit) b/$-(* (unit))}
  ?~  a  ~
  (b u.a)
::
++  bind                                                ::  argue
  |*  {a/(unit) b/gate}
  ?~  a  ~
  [~ u=(b u.a)]
::
++  bond                                                ::  replace
  |*  a/(trap)
  |*  b/(unit)
  ?~  b  $:a
  u.b
::
++  both                                                ::  all the above
  |*  {a/(unit) b/(unit)}
  ?~  a  ~
  ?~  b  ~
  [~ u=[u.a u.b]]
::
++  clap                                                ::  combine
  |*  {a/(unit) b/(unit) c/_|=(^ +<-)}
  ?~  a  b
  ?~  b  a
  [~ u=(c u.a u.b)]
::
++  drop                                                ::  enlist
  |*  a/(unit)
  ?~  a  ~
  [i=u.a t=~]
::
++  fall                                                ::  default
  |*  {a/(unit) b/*}
  ?~(a b u.a)
::
++  flit                                                ::  make filter
  |*  a/$-(* ?)
  |*  b/*
  ?.((a b) ~ [~ u=b])
::
++  lift                                                ::  lift gate (fmap)
  |*  a/gate                                            ::  flipped
  |*  b/(unit)                                          ::  curried
  (bind b a)                                            ::  bind
::
++  mate                                                ::  choose
  |*  {a/(unit) b/(unit)}
  ?~  b  a
  ?~  a  b
  ?.(=(u.a u.b) ~>(%mean.[0 %leaf "mate"] !!) a)
::
++  need                                                ::  demand
  |*  a/(unit)
  ?~  a  ~>(%mean.[0 %leaf "need"] !!)
  u.a
::
++  some                                                ::  lift (pure)
  |*  a/*
  [~ u=a]
::
::::  2b: list logic                                    ::
  ::                                                    ::
  ::    flop, homo, lent, levy, lien, limo, murn, reap, ::
  ::    reel, roll, skid, skim, skip, scag, slag, snag, ::
  ::    sort, swag, turn, weld, welp, zing              ::
  ::                                                    ::
++  flop                                                ::  reverse
  ~/  %flop
  |*  a/(list)
  =>  .(a (homo a))
  ^+  a
  =+  b=`_a`~
  |-
  ?~  a  b
  $(a t.a, b [i.a b])
::
++  gulf                                                ::  range inclusive
  |=  {a/@ b/@} 
  ^-  (list @)
  ?:(=(a +(b)) ~ [a $(a +(a))])
::
++  homo                                                ::  homogenize
  |*  a/(list)
  ^+  =<  $
    |%  +-  $  ?:(*? ~ [i=(snag 0 a) t=$])
    --
  a
::
++  lent                                                ::  length
  ~/  %lent
  |=  a/(list)
  ^-  @
  =+  b=0
  |-
  ?~  a  b
  $(a t.a, b +(b))
::
++  levy
  ~/  %levy                                             ::  all of
  |*  {a/(list) b/$-(* ?)}
  |-  ^-  ?
  ?~  a  &
  ?.  (b i.a)  |
  $(a t.a)
::
++  lien                                                ::  some of
  ~/  %lien
  |*  {a/(list) b/$-(* ?)}
  |-  ^-  ?
  ?~  a  |
  ?:  (b i.a)  &
  $(a t.a)
::
++  limo                                                ::  listify
  |*  a/*
  ^+  =<  $
    |%  +-  $  ?~(a ~ ?:(*? [i=-.a t=$] $(a +.a)))
    --
  a
::
++  murn                                                ::  maybe transform
  ~/  %murn
  |*  {a/(list) b/$-(* (unit))}
  |-
  ?~  a  ~
  =+  c=(b i.a)
  ?~  c
    $(a t.a)
  [i=u.c t=$(a t.a)]
::
++  reap                                                ::  replicate
  ~/  %reap
  |*  {a/@ b/*}
  |-  ^-  (list _b)
  ?~  a  ~
  [b $(a (dec a))]
::
++  reel                                                ::  right fold
  ~/  %reel
  |*  {a/(list) b/_|=({* *} +<+)}
  |-  ^+  +<+.b
  ?~  a
    +<+.b
  (b i.a $(a t.a))
::
++  roll                                                ::  left fold
  ~/  %roll
  |*  {a/(list) b/_|=({* *} +<+)}
  |-  ^+  +<+.b
  ?~  a
    +<+.b
  $(a t.a, b b(+<+ (b i.a +<+.b)))
::
++  skid                                                ::  separate
  ~/  %skid
  |*  {a/(list) b/$-(* ?)}
  |-  ^+  [p=a q=a]
  ?~  a  [~ ~]
  =+  c=$(a t.a)
  ?:((b i.a) [[i.a p.c] q.c] [p.c [i.a q.c]])
::
++  skim                                                ::  only
  ~/  %skim
  |*  {a/(list) b/$-(* ?)}
  |-
  ^+  a
  ?~  a  ~
  ?:((b i.a) [i.a $(a t.a)] $(a t.a))
::
++  skip                                                ::  except
  ~/  %skip
  |*  {a/(list) b/$-(* ?)}
  |-
  ^+  a
  ?~  a  ~
  ?:((b i.a) $(a t.a) [i.a $(a t.a)])
::
++  scag                                                ::  prefix
  ~/  %scag
  |*  {a/@ b/(list)}
  |-  ^+  b
  ?:  |(?=($~ b) =(0 a))  ~
  [i.b $(b t.b, a (dec a))]
::
++  slag                                                ::  suffix
  ~/  %slag
  |*  {a/@ b/(list)}
  |-  ^+  b
  ?:  =(0 a)  b
  ?~  b  ~
  $(b t.b, a (dec a))
::
++  snag                                                ::  index
  ~/  %snag
  |*  {a/@ b/(list)}
  |-
  ?~  b
    ~>  %mean.[0 %leaf "snag-fail"]
    !!
  ?:  =(0 a)  i.b
  $(b t.b, a (dec a))
::
++  sort   !.                                           ::  quicksort
  ~/  %sort
  |*  {a/(list) b/$-([* *] ?)}
  =>  .(a ^.(homo a))
  |-  ^+  a
  ?~  a  ~
  %+  weld
    $(a (skim t.a |=(c/_i.a (b c i.a))))
  ^+  t.a
  [i.a $(a (skim t.a |=(c/_i.a !(b c i.a))))]
::
++  swag                                                ::  slice
  |*  {{a/@ b/@} c/(list)}
  (scag +<-> (slag +<-< c))
::
++  turn                                                ::  transform
  ~/  %turn
  |*  {a/(list) b/$-(* *)}
  |-
  ?~  a  ~
  [i=(b i.a) t=$(a t.a)]
::
++  weld                                                ::  concatenate
  ~/  %weld
  |*  {a/(list) b/(list)}
  =>  .(a ^.(homo a), b ^.(homo b))
  |-  ^+  b
  ?~  a  b
  [i.a $(a t.a)]
::
++  welp                                                ::  faceless weld
  =|  {* *}
  |%
  +-  $
    ?~  +<-
      +<-(. +<+)
    +<-(+ $(+<- +<->))
  --
::
++  zing                                                ::  promote
  =|  *
  |%
  +-  $
    ?~  +<
      +<
    (welp +<- $(+< +<+))
  --
::                                                      ::
::::  2c: bit arithmetic                                ::
  ::                                                    ::
  ::    bex, can, cat, cut, end, fil, lsh, met,         ::
  ::    rap, rep, rip, rsh, swp, xeb                    ::
  ::
++  bex                                                 ::  binary exponent
  ~/  %bex
  |=  a/@
  ^-  @
  ?:  =(0 a)  1
  (mul 2 $(a (dec a)))
::
++  can                                                 ::  assemble
  ~/  %can
  |=  {a/bloq b/(list {p/@u q/@})}
  ^-  @
  ?~  b  0
  (add (end a p.i.b q.i.b) (lsh a p.i.b $(b t.b)))
::
++  cat                                                 ::  concatenate
  ~/  %cat
  |=  {a/bloq b/@ c/@}
  (add (lsh a (met a b) c) b)
::
++  cut                                                 ::  slice
  ~/  %cut
  |=  {a/bloq {b/@u c/@u} d/@}
  (end a c (rsh a b d))
::
++  end                                                 ::  tail
  ~/  %end
  |=  {a/bloq b/@u c/@}
  (mod c (bex (mul (bex a) b)))
::
++  fil                                                 ::  fill bloqstream
  |=  {a/bloq b/@u c/@}
  =+  n=0
  =+  d=c
  |-  ^-  @
  ?:  =(n b)
    (rsh a 1 d)
  $(d (add c (lsh a 1 d)), n +(n))
::
++  lsh                                                 ::  left-shift
  ~/  %lsh
  |=  {a/bloq b/@u c/@}
  (mul (bex (mul (bex a) b)) c)
::
++  met                                                 ::  measure
  ~/  %met
  |=  {a/bloq b/@}
  ^-  @
  =+  c=0
  |-
  ?:  =(0 b)  c
  $(b (rsh a 1 b), c +(c))
::
++  rap                                                 ::  assemble nonzero
  ~/  %rap
  |=  {a/bloq b/(list @)}
  ^-  @
  ?~  b  0
  (cat a i.b $(b t.b))
::
++  rep                                                 ::  assemble single
  ~/  %rep
  |=  {a/bloq b/(list @)}
  ^-  @
  =+  c=0
  |-
  ?~  b  0
  (add (lsh a c (end a 1 i.b)) $(c +(c), b t.b))
::
++  rip                                                 ::  disassemble
  ~/  %rip
  |=  {a/bloq b/@}
  ^-  (list @)
  ?:  =(0 b)  ~
  [(end a 1 b) $(b (rsh a 1 b))]
::
++  rsh                                                 ::  right-shift
  ~/  %rsh
  |=  {a/bloq b/@u c/@}
  (div c (bex (mul (bex a) b)))
::
++  swp  |=({a/bloq b/@} (rep a (flop (rip a b))))      ::  reverse bloq order
++  xeb                                                 ::  binary logarithm
  ~/  %xeb
  |=  a/@
  ^-  @
  (met 0 a)
::                                                      ::
::::  2d: bit logic                                     ::
  ::                                                    ::
  ::    con, dis, mix, not                              ::
  ::
++  con                                                 ::  binary or
  ~/  %con
  |=  {a/@ b/@}
  =+  [c=0 d=0]
  |-  ^-  @
  ?:  ?&(=(0 a) =(0 b))  d
  %=  $
    a   (rsh 0 1 a)
    b   (rsh 0 1 b)
    c   +(c)
    d   %+  add  d
          %^  lsh  0  c
          ?&  =(0 (end 0 1 a))
              =(0 (end 0 1 b))
          ==
  ==
::
++  dis                                                 ::  binary and
  ~/  %dis
  |=  {a/@ b/@}
  =|  {c/@ d/@}
  |-  ^-  @
  ?:  ?|(=(0 a) =(0 b))  d
  %=  $
    a   (rsh 0 1 a)
    b   (rsh 0 1 b)
    c   +(c)
    d   %+  add  d
          %^  lsh  0  c
          ?|  =(0 (end 0 1 a))
              =(0 (end 0 1 b))
          ==
  ==
::
++  mix                                                 ::  binary xor
  ~/  %mix
  |=  {a/@ b/@}
  ^-  @
  =+  [c=0 d=0]
  |-
  ?:  ?&(=(0 a) =(0 b))  d
  %=  $
    a   (rsh 0 1 a)
    b   (rsh 0 1 b)
    c   +(c)
    d   (add d (lsh 0 c =((end 0 1 a) (end 0 1 b))))
  ==
::
++  not  |=  {a/bloq b/@ c/@}                           ::  binary not (sized)
  (mix c (dec (bex (mul b (bex a)))))
::                                                      ::
::::  2e: insecure hashing                              ::
  ::                                                    ::
  ::  fnv, mum, mug                                     ::
  ::
++  fnv  |=(a/@ (end 5 1 (mul 16.777.619 a)))           ::  FNV scrambler
++  mum                                                 ::  mug with murmur3
  ~/  %mum
  |=  a/*
  |^  (trim ?@(a a (mix $(a -.a) (mix 0x7fff.ffff $(a +.a)))))
  ++  spec                                              ::  standard murmur3
    |=  {syd/@ key/@}
    ?>  (lte (met 5 syd) 1)
    =+  ^=  row
        |=  {a/@ b/@}
        (con (end 5 1 (lsh 0 a b)) (rsh 0 (sub 32 a) b))
    =+  mow=|=({a/@ b/@} (end 5 1 (mul a b)))
    =+  len=(met 5 key)
    =-  =.  goc  (mix goc len)
        =.  goc  (mix goc (rsh 4 1 goc))
        =.  goc  (mow goc 0x85eb.ca6b)
        =.  goc  (mix goc (rsh 0 13 goc))
        =.  goc  (mow goc 0xc2b2.ae35)
        (mix goc (rsh 4 1 goc))
    ^=  goc
    =+  [inx=0 goc=syd]
    |-  ^-  @
    ?:  =(inx len)  goc
    =+  kop=(cut 5 [inx 1] key)
    =.  kop  (mow kop 0xcc9e.2d51)
    =.  kop  (row 15 kop)
    =.  kop  (mow kop 0x1b87.3593)
    =.  goc  (mix kop goc)
    =.  goc  (row 13 goc)
    =.  goc  (end 5 1 (add 0xe654.6b64 (mul 5 goc)))
    $(inx +(inx))
  ::
  ++  trim                                              ::  31-bit nonzero
    |=  key/@
    =+  syd=0xcafe.babe
    |-  ^-  @
    =+  haz=(spec syd key)
    =+  ham=(mix (rsh 0 31 haz) (end 0 31 haz))
    ?.(=(0 ham) ham $(syd +(syd)))
  --
::
++  mug                                                 ::  31bit nonzero FNV1a
  ~/  %mug
  |=  a/*
  ?^  a
    =+  b=[p=$(a -.a) q=$(a +.a)]
    |-  ^-  @
    =+  c=(fnv (mix p.b (fnv q.b)))
    =+  d=(mix (rsh 0 31 c) (end 0 31 c))
    ?.  =(0 c)  c
    $(q.b +(q.b))
  =+  b=2.166.136.261
  |-  ^-  @
  =+  c=b
  =+  [d=0 e=(met 3 a)]
  |-  ^-  @
  ?:  =(d e)
    =+  f=(mix (rsh 0 31 c) (end 0 31 c))
    ?.  =(0 f)  f
    ^$(b +(b))
  $(c (fnv (mix c (cut 3 [d 1] a))), d +(d))
::                                                      ::
::::  2f: noun ordering                                 ::
  ::                                                    ::
  ::    aor, dor, gor, hor, lor, vor                    ::
  ::
++  aor                                                 ::  a-order
  ~/  %aor
  |=  {a/* b/*}
  ^-  ?
  ?:  =(a b)  &
  ?.  ?=(@ a)
    ?:  ?=(@ b)  |
    ?:  =(-.a -.b)
      $(a +.a, b +.b)
    $(a -.a, b -.b)
  ?.  ?=(@ b)  &
  |-
  =+  [c=(end 3 1 a) d=(end 3 1 b)]
  ?:  =(c d)
    $(a (rsh 3 1 a), b (rsh 3 1 b))
  (lth c d)
::
++  dor                                                 ::  d-order
  ~/  %dor
  |=  {a/* b/*}
  ^-  ?
  ?:  =(a b)  &
  ?.  ?=(@ a)
    ?:  ?=(@ b)  |
    ?:  =(-.a -.b)
      $(a +.a, b +.b)
    $(a -.a, b -.b)
  ?.  ?=(@ b)  &
  (lth a b)
::
++  gor                                                 ::  g-order
  ~/  %gor
  |=  {a/* b/*}
  ^-  ?
  =+  [c=(mug a) d=(mug b)]
  ?:  =(c d)
    (dor a b)
  (lth c d)
::
++  hor                                                 ::  h-order
  ~/  %hor
  |=  {a/* b/*}
  ^-  ?
  ?:  ?=(@ a)
    ?.  ?=(@ b)  &
    (gor a b)
  ?:  ?=(@ b)  |
  ?:  =(-.a -.b)
    (gor +.a +.b)
  (gor -.a -.b)
::
++  lor                                                 ::  l-order
  ~/  %lor
  |=  {a/* b/*}
  ^-  ?
  ?:  =(a b)  &
  ?@  a
    ?^  b  &
    (lth a b)
  ?:  =(-.a -.b)
    $(a +.a, b +.b)
  $(a -.a, b -.b)
::
++  vor                                                 ::  v-order
  ~/  %vor
  |=  {a/* b/*}
  ^-  ?
  =+  [c=(mug (mug a)) d=(mug (mug b))]
  ?:  =(c d)
    (dor a b)
  (lth c d)
::                                                      ::
::::                                                    ::
  ::  2g: unsigned powers                               ::
  ::                                                    ::
  ::    pow, sqt                                        ::
  ::
++  pow                                                 ::  unsigned exponent
  ~/  %pow
  |=  {a/@ b/@}
  ?:  =(b 0)  1
  |-  ?:  =(b 1)  a
  =+  c=$(b (div b 2))
  =+  d=(mul c c)
  ?~  (dis b 1)  d  (mul d a)
::
++  sqt                                                 ::  unsigned sqrt/rem
  ~/  %sqt
  |=  a/@  ^-  {p/@ q/@}
  ?~  a  [0 0]
  =+  [q=(div (dec (xeb a)) 2) r=0]
  =-  [-.b (sub a +.b)]
  ^=  b  |-
  =+  s=(add r (bex q))
  =+  t=(mul s s)
  ?:  =(q 0)
    ?:((lte t a) [s t] [r (mul r r)])
  ?:  (lte t a)  
    $(r s, q (dec q))  
  $(q (dec q))
::                                                      ::
::::                                                    ::
  ::                                                    ::
  ::  2h: set logic                                     ::
  ::                                                    ::
  ::    in                                              ::
  ::
++  in                                                  ::  set engine
  ~/  %in
  |_  a/(tree)
  +-  all                                               ::  logical AND
    ~/  %all
    |*  b/$-(* ?)
    |-  ^-  ?
    ?~  a
      &
    ?&((b n.a) $(a l.a) $(a r.a))
  ::
  +-  any                                               ::  logical OR
    ~/  %any
    |*  b/$-(* ?)
    |-  ^-  ?
    ?~  a
      |
    ?|((b n.a) $(a l.a) $(a r.a))
  ::
  +-  apt                                               ::  check correctness
    |-  ^-  ?
    ?~  a
      &
    ?&  ?~(l.a & ?&((vor n.a n.l.a) (hor n.l.a n.a) $(a l.a)))
        ?~(r.a & ?&((vor n.a n.r.a) (hor n.a n.r.a) $(a r.a)))
    ==
  ::
  +-  bif                                               ::  splits a by b
    ~/  %bif
    |*  b/*
    ^+  [l=a r=a]
    =<  [+< +>]
    |-  ^+  a
    ?~  a
      [b ~ ~]
    ?:  =(b n.a)
      a
    ?:  (hor b n.a)
      =+  c=$(a l.a)
      ?>  ?=(^ c)
      [n.c l.c [n.a r.c r.a]]
    =+  c=$(a r.a)
    ?>  ?=(^ c)
    [n.c [n.a l.a l.c] r.c]
  ::
  +-  del                                               ::  b without any a
    ~/  %del
    |*  b/*
    |-  ^+  a
    ?~  a
      ~
    ?.  =(b n.a)
      ?:  (hor b n.a)
        [n.a $(a l.a) r.a]
      [n.a l.a $(a r.a)]
    |-  ^-  {$?($~ _a)}
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (vor n.l.a n.r.a)
      [n.l.a l.l.a $(l.a r.l.a)]
    [n.r.a $(r.a l.r.a) r.r.a]
  ::
  +-  dif                                               ::  difference
    ~/  %dif
    |*  b/_a
    |-  ^+  a
    ?~  b
      a
    =+  c=(bif n.b)
    ?>  ?=(^ c)
    =+  d=$(a l.c, b l.b)
    =+  e=$(a r.c, b r.b)
    |-  ^-  {$?($~ _a)}
    ?~  d  e
    ?~  e  d
    ?:  (vor n.d n.e)
      [n.d l.d $(d r.d)]
    [n.e $(e l.e) r.e]
  ::
  +-  dig                                               ::  axis of a in b
    |=  b/*
    =+  c=1
    |-  ^-  (unit @)
    ?~  a  ~
    ?:  =(b n.a)  [~ u=(peg c 2)]
    ?:  (hor b n.a)
      $(a l.a, c (peg c 6))
    $(a r.a, c (peg c 7))
  ::
  +-  gas                                               ::  concatenate
    ~/  %gas
    |=  b/(list _?>(?=(^ a) n.a))
    |-  ^+  a
    ?~  b
      a
    $(b t.b, a (put i.b))
  ::
  +-  has                                               ::  b exists in a check
    ~/  %has
    |*  b/*
    |-  ^-  ?
    ?~  a
      |
    ?:  =(b n.a)
      &
    ?:  (hor b n.a)
      $(a l.a)
    $(a r.a)
  ::
  +-  int                                               ::  intersection
    ~/  %int
    |*  b/_a
    |-  ^+  a
    ?~  b
      ~
    ?~  a
      ~
    ?.  (vor n.a n.b)
      $(a b, b a)
    ?:  =(n.b n.a)
      [n.a $(a l.a, b l.b) $(a r.a, b r.b)]
    ?:  (hor n.b n.a)
      %-  uni(a $(a l.a, b [n.b l.b ~]))  $(b r.b)
    %-  uni(a $(a r.a, b [n.b ~ r.b]))  $(b l.b)
  ::
  +-  put                                               ::  puts b in a, sorted
    ~/  %put
    |*  b/*
    |-  ^+  a
    ?~  a
      [b ~ ~]
    ?:  =(b n.a)
      a
    ?:  (hor b n.a)
      =+  c=$(a l.a)
      ?>  ?=(^ c)
      ?:  (vor n.a n.c)
        [n.a c r.a]
      [n.c l.c [n.a r.c r.a]]
    =+  c=$(a r.a)
    ?>  ?=(^ c)
    ?:  (vor n.a n.c)
      [n.a l.a c]
    [n.c [n.a l.a l.c] r.c]
  ::
  +-  rep                                               ::  replace by product
    |*  b/_|=({* *} +<+)
    |-
    ?~  a  +<+.b
    $(a r.a, +<+.b $(a l.a, +<+.b (b n.a +<+.b)))
  ::
  +-  run                                               ::  apply gate to values
    |*  {b/$-(* *) c/*}
    |-
    ?~  a  c
    $(a r.a, c [(b n.a) $(a l.a)])
  ::
  +-  tap                                               ::  convert to list
    ~/  %tap
    |=  b/(list _?>(?=(^ a) n.a))
    ^+  b
    ?~  a
      b
    $(a r.a, b [n.a $(a l.a)])
  ::
  +-  uni                                               ::  union
    ~/  %uni
    |*  b/_a
    ?:  =(a b)  a
    |-  ^+  a
    ?~  b
      a
    ?~  a
      b
    ?:  (vor n.a n.b)
      ?:  =(n.b n.a)
        [n.b $(a l.a, b l.b) $(a r.a, b r.b)]
      ?:  (hor n.b n.a)
        $(a [n.a $(a l.a, b [n.b l.b ~]) r.a], b r.b)
      $(a [n.a l.a $(a r.a, b [n.b ~ r.b])], b l.b)
    ?:  =(n.a n.b)
      [n.b $(b l.b, a l.a) $(b r.b, a r.a)]
    ?:  (hor n.a n.b)
      $(b [n.b $(b l.b, a [n.a l.a ~]) r.b], a r.a)
    $(b [n.b l.b $(b r.b, a [n.a ~ r.a])], a l.a)
  ::
  +-  wyt                                               ::  size of set
    |-  ^-  @
    ?~(a 0 +((add $(a l.a) $(a r.a))))
  --
::                                                      ::
::::  2i: map logic                                     ::
  ::                                                    ::
  ::    by                                              ::
  ::
++  by                                                  ::  map engine
  ~/  %by
  |_  a/(tree (pair))
  +-  all                                               ::  logical AND
    ~/  %all
    |*  b/$-(* ?)
    |-  ^-  ?
    ?~  a
      &
    ?&((b q.n.a) $(a l.a) $(a r.a))
  ::
  +-  any                                               ::  logical OR
    ~/  %any
    |*  b/$-(* ?)
    |-  ^-  ?
    ?~  a
      |
    ?|((b q.n.a) $(a l.a) $(a r.a))
  ::
  +-  apt                                               ::  map invariant
    |-  ^-  ?
    ?~  a
      &
    ?&  ?~(l.a & ?&((vor p.n.a p.n.l.a) (gor p.n.l.a p.n.a) $(a l.a)))
        ?~(r.a & ?&((vor p.n.a p.n.r.a) (gor p.n.a p.n.r.a) $(a l.a)))
    ==
  +-  bif                                               ::  splits a by b
    ~/  %bif
    |*  {b/* c/*}
    ^+  [l=a r=a]
    =<  [+< +>]
    |-  ^+  a
    ?~  a
      [[b c] ~ ~]
    ?:  =(b p.n.a)
      ?:  =(c q.n.a)
        a
      [[b c] l.a r.a]
    ?:  (gor b p.n.a)
      =+  d=$(a l.a)
      ?>  ?=(^ d)
      [n.d l.d [n.a r.d r.a]]
    =+  d=$(a r.a)
    ?>  ?=(^ d)
    [n.d [n.a l.a l.d] r.d]
  ::
  +-  del                                               ::  delete at key b
    ~/  %del
    |*  b/*
    |-  ^+  a
    ?~  a
      ~
    ?.  =(b p.n.a)
      ?:  (gor b p.n.a)
        [n.a $(a l.a) r.a]
      [n.a l.a $(a r.a)]
    |-  ^-  {$?($~ _a)}
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (vor p.n.l.a p.n.r.a)
      [n.l.a l.l.a $(l.a r.l.a)]
    [n.r.a $(r.a l.r.a) r.r.a]
  ::
  +-  dif                                               ::  difference
    ~/  %dif
    |*  b/_a
    |-  ^+  a
    ?~  b
      a
    =+  c=(bif n.b)
    ?>  ?=(^ c)
    =+  d=$(a l.c, b l.b)
    =+  e=$(a r.c, b r.b)
    |-  ^-  {$?($~ _a)}
    ?~  d  e
    ?~  e  d
    ?:  (vor p.n.d p.n.e)
      [n.d l.d $(d r.d)]
    [n.e $(e l.e) r.e]
  ::
  +-  dig                                               ::  axis of b key
    |=  b/*
    =+  c=1
    |-  ^-  (unit @)
    ?~  a  ~
    ?:  =(b p.n.a)  [~ u=(peg c 2)]
    ?:  (gor b p.n.a)
      $(a l.a, c (peg c 6))
    $(a r.a, c (peg c 7))
  ::
  +-  gas                                               ::  concatenate
    ~/  %gas
    |*  b/(list {p/* q/*})
    =>  .(b `(list _?>(?=(^ a) n.a))`b)
    |-  ^+  a
    ?~  b
      a
    $(b t.b, a (put p.i.b q.i.b))
  ::
  +-  get                                               ::  grab value by key
    ~/  %get
    |=  b/*
    ^-  {$@($~ {$~ u/_?>(?=(^ a) q.n.a)})}
    ?~  a
      ~
    ?:  =(b p.n.a)
      [~ u=q.n.a]
    ?:  (gor b p.n.a)
      $(a l.a)
    $(a r.a)
  ::
  +-  got
    |*  b/*
    (need (get b))
  ::
  +-  has                                               ::  key existence check
    ~/  %has
    |*  b/*
    !=(~ (get b))
  ::
  +-  int                                               ::  intersection
    ~/  %int
    |*  b/_a
    |-  ^+  a
    ?~  b
      ~
    ?~  a
      ~
    ?:  (vor p.n.a p.n.b)
      ?:  =(p.n.b p.n.a)
        [n.b $(a l.a, b l.b) $(a r.a, b r.b)]
      ?:  (gor p.n.b p.n.a)
        %-  uni(a $(a l.a, b [n.b l.b ~]))  $(b r.b)
      %-  uni(a $(a r.a, b [n.b ~ r.b]))  $(b l.b)
    ?:  =(p.n.a p.n.b)
      [n.b $(b l.b, a l.a) $(b r.b, a r.a)]
    ?:  (gor p.n.a p.n.b)
      %-  uni(a $(b l.b, a [n.a l.a ~]))  $(a r.a)
    %-  uni(a $(b r.b, a [n.a ~ r.a]))  $(a l.a)
  ::
  +-  mar                                               ::  add with validation
    |*  {b/_?>(?=(^ a) p.n.a) c/(unit _?>(?=(^ a) q.n.a))}
    ?~  c
      (del b)
    (put b u.c)
  ::
  +-  put                                               ::  adds key-value pair
    ~/  %put
    |*  {b/* c/*}
    |-  ^+  a
    ?~  a
      [[b c] ~ ~]
    ?:  =(b p.n.a)
      ?:  =(c q.n.a)
        a
      [[b c] l.a r.a]
    ?:  (gor b p.n.a)
      =+  d=$(a l.a)
      ?>  ?=(^ d)
      ?:  (vor p.n.a p.n.d)
        [n.a d r.a]
      [n.d l.d [n.a r.d r.a]]
    =+  d=$(a r.a)
    ?>  ?=(^ d)
    ?:  (vor p.n.a p.n.d)
      [n.a l.a d]
    [n.d [n.a l.a l.d] r.d]
  ::
  +-  rep                                               ::  replace by product
    |*  b/_|=({* *} +<+)
    |-
    ?~  a  +<+.b
    $(a r.a, +<+.b $(a l.a, +<+.b (b n.a +<+.b)))
  ::
  +-  rib                                               ::  transform + product
    |*  {b/* c/$-(* *)}
    |-  ^+  [b a]
    ?~  a  [b ~]
    =+  d=(c n.a b)
    =.  n.a  +.d
    =+  e=$(a l.a, b -.d)
    =+  f=$(a r.a, b -.e)
    [-.f [n.a +.e +.f]]
  ::
  +-  run                                               ::  apply gate to values
    |*  b/$-(* *)
    |-
    ?~  a  a
    [n=[p=p.n.a q=(b q.n.a)] l=$(a l.a) r=$(a r.a)]
  ::
  +-  tap                                               ::  listify pairs
    ~/  %tap
    |=  b/(list _?>(?=(^ a) n.a))
    ^+  b
    ?~  a
      b
    $(a r.a, b [n.a $(a l.a)])
  ::
  +-  uni                                               ::  union, merge
    ~/  %uni
    |*  b/_a
    |-  ^+  a
    ?~  b
      a
    ?~  a
      b
    ?:  (vor p.n.a p.n.b)
      ?:  =(p.n.b p.n.a)
        [n.b $(a l.a, b l.b) $(a r.a, b r.b)]
      ?:  (gor p.n.b p.n.a)
        $(a [n.a $(a l.a, b [n.b l.b ~]) r.a], b r.b)
      $(a [n.a l.a $(a r.a, b [n.b ~ r.b])], b l.b)
    ?:  =(p.n.a p.n.b)
      [n.b $(b l.b, a l.a) $(b r.b, a r.a)]
    ?:  (gor p.n.a p.n.b)
      $(b [n.b $(b l.b, a [n.a l.a ~]) r.b], a r.a)
    $(b [n.b l.b $(b r.b, a [n.a ~ r.a])], a l.a)
  ::
  +-  urn                                               ::  apply gate to nodes
    |*  b/$-({* *} *)
    |-
    ?~  a  ~
    [n=[p=p.n.a q=(b p.n.a q.n.a)] l=$(a l.a) r=$(a r.a)]
  ::
  +-  wyt                                               ::  depth of map
    |-  ^-  @
    ?~(a 0 +((add $(a l.a) $(a r.a))))
  --
::                                                      ::
::::  2j: jar and jug logic                             ::
  ::                                                    ::
  ::
++  ja                                                  ::  jar engine
  |_  a/(tree (pair * (list)))
  +-  get                                               ::  gets list by key
    |*  b/*
    =+  c=(~(get by a) b)
    ?~(c ~ u.c)
  ::
  +-  add                                               ::  adds key-list pair
    |*  {b/* c/*}
    =+  d=(get b)
    (~(put by a) b [c d])
  --
++  ju                                                  ::  jug engine
  |_  a/(tree (pair * (tree)))
  +-  del                                               ::  del key-set pair
    |*  {b/* c/*}
    ^+  a
    =+  d=(get b)
    =+  e=(~(del in d) c)
    ?~  e
      (~(del by a) b)
    (~(put by a) b e)
  ::
  +-  gas                                               ::  concatenate
    |*  b/(list {p/* q/*})
    =>  .(b `(list _?>(?=({{* ^} ^} a) [p=p q=n.q]:n.a))`b)
    |-  ^+  a
    ?~  b
      a
    $(b t.b, a (put p.i.b q.i.b))
  ::
  +-  get                                               ::  gets set by key
    |*  b/*
    =+  c=(~(get by a) b)
    ?~(c ~ u.c)
  ::
  +-  has                                               ::  existence check
    |*  {b/* c/*}
    ^-  ?
    (~(has in (get b)) c)
  ::
  +-  put                                               ::  add key-set pair
    |*  {b/* c/*}
    ^+  a
    =+  d=(get b)
    (~(put by a) b (~(put in d) c))
  --
::                                                      ::
::::  2k: queue logic                                   ::
  ::                                                    ::
  ::    to                                              ::
  ::
++  to                                                  ::  queue engine
  |_  a/(tree)
  +-  bal
    |-  ^+  a
    ?~  a  ~
    ?.  |(?=($~ l.a) (vor n.a n.l.a))
      $(a [n.l.a l.l.a $(a [n.a r.l.a r.a])])
    ?.  |(?=($~ r.a) (vor n.a n.r.a))
      $(a [n.r.a $(a [n.a l.a l.r.a]) r.r.a])
    a
  ::
  +-  dep                                               ::  max depth of queue
    |-  ^-  @
    ?~  a  0
    +((max $(a l.a) $(a r.a)))
  ::
  +-  gas                                               ::  insert list to que
    |=  b/(list _?>(?=(^ a) n.a))
    |-  ^+  a
    ?~(b a $(b t.b, a (put i.b)))
  ::
  +-  get                                               ::  head-rest pair
    |-  ^+  ?>(?=(^ a) [p=n.a q=*(tree _n.a)])
    ?~  a
      !!
    ?~  r.a
      [n.a l.a]
    =+  b=$(a r.a)
    :-  p.b
    ?:  |(?=($~ q.b) (vor n.a n.q.b))
      [n.a l.a q.b]
    [n.q.b [n.a l.a l.q.b] r.q.b]
  ::
  +-  nap                                               ::  removes head
    ?>  ?=(^ a)
    ?:  =(~ l.a)  r.a
    =+  b=get(a l.a)
    bal(a ^+(a [p.b q.b r.a]))
  ::
  +-  put                                               ::  insert new tail
    |*  b/*
    |-  ^+  a
    ?~  a
      [b ~ ~]
    bal(a a(l $(a l.a)))
  ::
  +-  tap                                               ::  adds list to end
    |=  b/(list _?>(?=(^ a) n.a))
    =+  0                                               ::  breaks tap.in match
    ^+  b
    ?~  a
      b
    $(a r.a, b [n.a $(a l.a)])
  ::
  +-  top                                               ::  produces head
    |-  ^-  (unit _?>(?=(^ a) n.a))
    ?~  a  ~
    ?~(r.a [~ n.a] $(a r.a))
  --
::                                                      ::
::::  2l: container from container                      ::
  ::                                                    ::
  ::    malt, molt, silt                                ::
  ::
++  malt                                                ::  map from list
  |*  a/(list) 
  (molt `(list {p/_-<.a q/_->.a})`a)
::
++  molt                                                ::  map from pair list
  |*  a/(list (pair))
  (~(gas by `(tree {_p.i.-.a _q.i.-.a})`~) a)
::
++  silt                                                ::  set from list
  |*  a/(list)
  =+  b=*(tree _?>(?=(^ a) i.a))
  (~(gas in b) a)
::                                                      ::
::::  2m: container from noun                           ::
  ::                                                    ::
  ::    ly, my, sy                                      ::
  ::
++  ly                                                  ::  list from raw noun
  |*  a/*
  ^+((homo (limo a)) a)
::
++  my                                                  ::  map from raw noun
  |*  a/* 
  (malt ^+((homo (limo a)) a))
::
++  sy                                                  ::  set from raw noun
  |*  a/*
  (silt ^+((homo (limo a)) a))
::                                                      ::
::::  2n: functional hacks                              ::
  ::                                                    ::
  ::    aftr, cork, corl, cury, curr, fore,             ::
  ::    hard, head, same, soft, tail, test              ::
  ::
++  aftr  |*(a/$-(* *) |*(b/$-(* *) (pair b a)))        ::  pair after
++  cork  |*({a/_|=(* **) b/gate} (corl b a))           ::  compose forward
++  corl                                                ::  compose backwards
  |*  {a/gate b/_|=(* **)}
  =<  +:|.((a (b)))      ::  span check
  =+  c=+<.b
  |.((a (b c)))
::
++  cury                                                ::  curry left
  |*  {a/_|=(^ **) b/*}
  =+  c=+<+.a
  |.((a b c))
::
++  curr                                                ::  curry right
  |*  {a/_|=(^ **) b/*}     
  =+  c=+<+.a
  |.((a c b))
::
++  fore  |*(a/$-(* *) |*(b/$-(* *) (pair a b)))        ::  pair before
++  hard                                                ::  force remold
  |*  han/$-(* *)
  |=  fud/*  ^-  han
  ~>  %mean.[0 %leaf "hard"]
  =+  gol=(han fud)
  ?>(=(gol fud) gol)
::
::
++  head  |*(^ +<-)                                     ::  get head
++  same  |*(* +<)                                      ::  identity
++  soft                                                ::  maybe remold
  |*  han/$-(* *)
  |=  fud/*  ^-  (unit han)
  =+  gol=(han fud)
  ?.(=(gol fud) ~ [~ gol])
::
++  tail  |*(^ +<+)                                     ::  get tail
++  test  |=(^ =(+<- +<+))                              ::  equality
::
::                                                      ::
::::  2o: normalizing containers                        ::
  ::                                                    ::
  ::    jar, jug, map, set, qeu                         ::
  ::
++  jar  |*({a/gate b/gate} (map a (list b)))           ::  map of lists
++  jug  |*({a/gate b/gate} (map a (set b)))            ::  map of sets
++  map  |*  {a/gate b/gate}                            ::  table
         $@($~ {n/{p/a q/b} l/(map a b) r/(map a b)})   ::
++  qeu  |*  a/gate                                     ::  queue
         $@($~ {n/a l/(qeu a) r/(qeu a)})               ::
++  set  |*  a/gate                                     ::  set
         $@($~ {n/a l/(set a) r/(set a)})               ::
::
::::  2p: serialization                                 ::
  ::                                                    ::
  ::    cue, jam, mat, rub                              ::
  ::
++  cue                                                 ::  unpack
  ~/  %cue
  |=  a/@
  ^-  *
  =+  b=0
  =+  m=`(map @ *)`~
  =<  q
  |-  ^-  {p/@ q/* r/(map @ *)}
  ?:  =(0 (cut 0 [b 1] a))
    =+  c=(rub +(b) a)
    [+(p.c) q.c (~(put by m) b q.c)]
  =+  c=(add 2 b)
  ?:  =(0 (cut 0 [+(b) 1] a))
    =+  u=$(b c)
    =+  v=$(b (add p.u c), m r.u)
    =+  w=[q.u q.v]
    [(add 2 (add p.u p.v)) w (~(put by r.v) b w)]
  =+  d=(rub c a)
  [(add 2 p.d) (need (~(get by m) q.d)) m]
::
++  jam                                                 ::  pack
  ~/  %jam
  |=  a/*
  ^-  @
  =+  b=0
  =+  m=`(map * @)`~
  =<  q
  |-  ^-  {p/@ q/@ r/(map * @)}
  =+  c=(~(get by m) a)
  ?~  c
    =>  .(m (~(put by m) a b))
    ?:  ?=(@ a)
      =+  d=(mat a)
      [(add 1 p.d) (lsh 0 1 q.d) m]
    =>  .(b (add 2 b))
    =+  d=$(a -.a)
    =+  e=$(a +.a, b (add b p.d), m r.d)
    [(add 2 (add p.d p.e)) (mix 1 (lsh 0 2 (cat 0 q.d q.e))) r.e]
  ?:  ?&(?=(@ a) (lte (met 0 a) (met 0 u.c)))
    =+  d=(mat a)
    [(add 1 p.d) (lsh 0 1 q.d) m]
  =+  d=(mat u.c)
  [(add 2 p.d) (mix 3 (lsh 0 2 q.d)) m]
::
++  mat                                                 ::  length-encode
  ~/  %mat
  |=  a/@
  ^-  {p/@ q/@}
  ?:  =(0 a)
    [1 1]
  =+  b=(met 0 a)
  =+  c=(met 0 b)
  :-  (add (add c c) b)
  (cat 0 (bex c) (mix (end 0 (dec c) b) (lsh 0 (dec c) a)))
::
++  rub                                                 ::  length-decode
  ~/  %rub
  |=  {a/@ b/@}
  ^-  {p/@ q/@}
  =+  ^=  c
      =+  [c=0 m=(met 0 b)]
      |-  ?<  (gth c m)
      ?.  =(0 (cut 0 [(add a c) 1] b))
        c
      $(c +(c))
  ?:  =(0 c)
    [1 0]
  =+  d=(add a +(c))
  =+  e=(add (bex (dec c)) (cut 0 [d (dec c)] b))
  [(add (add c c) e) (cut 0 [(add d (dec c)) e] b)]
::
::::  2q: molds and mold builders                       ::
  ::                                                    ::
  ::    tape                                            ::
  ::
++  char  @t                                            ::  UTF8 byte
++  cord  @t                                            ::  UTF8, LSB first
++  date  {{a/? y/@ud} m/@ud t/tarp}                    ::  parsed date
++  knot  @ta                                           ::  ASCII text
++  tang  (list tank)                                   ::  bottom-first error
++  tank  $%  {$leaf p/tape}                            ::  printing formats
              $:  $palm                                 ::  backstep list
                  p/{p/tape q/tape r/tape s/tape}       ::
                  q/(list tank)                         ::
              ==                                        ::
              $:  $rose                                 ::  flat list
                  p/{p/tape q/tape r/tape}              ::  mid open close
                  q/(list tank)                         ::
              ==                                        ::
          ==                                            ::
++  tanq                                                ::  tomorrow's tank   
          $?  {$~ p/(list tanq)}                        ::  list of printables
              {$~ $~ p/tape}                            ::  simple string
              (pair @tas tanq)                          ::  captioned
          ==                                            ::
++  tape  (list @t)                                     ::  UTF8 string as list
++  tarp  {d/@ud h/@ud m/@ud s/@ud f/(list @ux)}        ::  parsed time
++  term  @tas                                          ::  ascii symbol
++  tone  $%  {$0 p/*}                                  ::  success
              {$1 p/(list)}                             ::  blocks
              {$2 p/(list {@ta *})}                     ::  error report
          ==                                            ::
++  toon  $%  {$0 p/*}                                  ::  success
              {$1 p/(list)}                             ::  blocks
              {$2 p/(list tank)}                        ::  stack trace
          ==                                            ::
--  =>
::                                                      ::
::::  3: layer three                                    ::
  ::                                                    ::
|%
::
::::  3a: signed and modular ints                       ::
  ::                                                    ::
  ::    egcd, fe, fo, si                                ::
  ::
++  egcd  !:                                            ::  schneier's egcd
  |=  {a/@ b/@}
  =+  si
  =+  [c=(sun a) d=(sun b)]
  =+  [u=[c=(sun 1) d=--0] v=[c=--0 d=(sun 1)]]
  |-  ^-  {d/@ u/@s v/@s}
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
++  fe                                                  ::  modulo bloq
  |_  a/bloq
  ++  dif  |=({b/@ c/@} (sit (sub (add out (sit b)) (sit c))))  ::  difference
  ++  inv  |=(b/@ (sub (dec out) (sit b)))              ::  inverse
  ++  net  |=  b/@  ^-  @                               ::  flip byte endianness
           =>  .(b (sit b))
           ?:  (lte a 3)
             b
           =+  c=(dec a)
           %+  con
             (lsh c 1 $(a c, b (cut c [0 1] b)))
           $(a c, b (cut c [1 1] b))
  ++  out  (bex (bex a))                                ::  mod value
  ++  rol  |=  {b/bloq c/@ d/@}  ^-  @                  ::  roll left
           =+  e=(sit d)
           =+  f=(bex (sub a b))
           =+  g=(mod c f)
           (sit (con (lsh b g e) (rsh b (sub f g) e)))
  ++  ror  |=  {b/bloq c/@ d/@}  ^-  @                  ::  roll right
           =+  e=(sit d)
           =+  f=(bex (sub a b))
           =+  g=(mod c f)
           (sit (con (rsh b g e) (lsh b (sub f g) e)))
  ++  sum  |=({b/@ c/@} (sit (add b c)))                ::  wrapping add
  ++  sit  |=(b/@ (end a 1 b))                          ::  enforce modulo
  --
::
++  fo                                                  ::  modulo prime
  |_  a/@
  ++  dif
    |=  {b/@ c/@}
    (sit (sub (add a b) (sit c)))
  ::
  ++  exp
    |=  {b/@ c/@}
    ?:  =(0 b)
      1
    =+  d=$(b (rsh 0 1 b))
    =+  e=(pro d d)
    ?:(=(0 (end 0 1 b)) e (pro c e))
  ::
  ++  fra
    |=  {b/@ c/@}
    (pro b (inv c))
  ::
  ++  inv
    |=  b/@
    =+  c=(dul:si u:(egcd b a) a)
    c
  ::
  ++  pro
    |=  {b/@ c/@}
    (sit (mul b c))
  ::
  ++  sit
    |=  b/@
    (mod b a)
  ::
  ++  sum
    |=  {b/@ c/@}
    (sit (add b c))
  --
::
++  si  !:                                              ::  signed integer
  |%
  ++  abs  |=(a/@s (add (end 0 1 a) (rsh 0 1 a)))       ::  absolute value
  ++  dif  |=  {a/@s b/@s}                              ::  subtraction
           (sum a (new !(syn b) (abs b)))
  ++  dul  |=  {a/@s b/@}                               ::  modulus
           =+(c=(old a) ?:(-.c (mod +.c b) (sub b +.c)))
  ++  fra  |=  {a/@s b/@s}                              ::  divide
           (new =(0 (mix (syn a) (syn b))) (div (abs a) (abs b)))
  ++  new  |=  {a/? b/@}                                ::  [sign value] to @s
           `@s`?:(a (mul 2 b) ?:(=(0 b) 0 +((mul 2 (dec b)))))
  ++  old  |=(a/@s [(syn a) (abs a)])                   ::  [sign value]
  ++  pro  |=  {a/@s b/@s}                              ::  multiplication
           (new =(0 (mix (syn a) (syn b))) (mul (abs a) (abs b)))
  ++  rem  |=({a/@s b/@s} (dif a (pro b (fra a b))))    ::  remainder
  ++  sum  |=  {a/@s b/@s}                              ::  addition
           =+  [c=(old a) d=(old b)]
           ?:  -.c
             ?:  -.d
               (new & (add +.c +.d))
             ?:  (gte +.c +.d)
               (new & (sub +.c +.d))
             (new | (sub +.d +.c))
           ?:  -.d
             ?:  (gte +.c +.d)
               (new | (sub +.c +.d))
             (new & (sub +.d +.c))
           (new | (add +.c +.d))
  ++  sun  |=(a/@u (mul 2 a))                           ::  @u to @s
  ++  syn  |=(a/@s =(0 (end 0 1 a)))                    ::  sign test
  ++  cmp  |=  {a/@s b/@s}                              ::  compare
           ^-  @s
           ?:  =(a b)
             --0
           ?:  (syn a)
             ?:  (syn b)
               ?:  (gth a b)
                 --1
               -1
             --1
          ?:  (syn b)
            -1
          ?:  (gth a b)
            -1
          --1
  --
::                                                      ::
::::  3b: floating point                                ::
  ::                                                    ::
  ::  rd, rh, rs, rq                                    ::
  ::  rlyd, rlys, rlyh, rlyq                            ::
  ::  ryld, ryls, rylh, rylq                            ::
  ::
--  =>
|%
++  fn  ::  float, infinity, or NaN
        ::  s=sign, e=exponent, a=arithmetic form
        ::  (-1)^s * a * 2^e
        $%  {$f s/? e/@s a/@u}
            {$i s/?}
            {$n $~}
        ==
::
++  dn  ::  decimal float, infinity, or NaN
        ::  (-1)^s * a * 10^e
        $%  {$d s/? e/@s a/@u}
            {$i s/?}
            {$n $~}
        ==
::
++  rn  ::  parsed decimal float
        ::
        $%  {$d a/? b/{c/@ {d/@ e/@} f/? i/@}}
            {$i a/?}
            {$n $~}
        == 
::
++  fl                                                  ::  arb. precision fp
  =+  ^-  {{p/@u v/@s w/@u} r/$?($n $u $d $z $a) d/$?($d $f $i)}
    [[113 -16.494 32.765] %n %d]
  ::  p=precision:     number of bits in arithmetic form; must be at least 2
  ::  v=min exponent:  minimum value of e
  ::  w=width:         max - min value of e, 0 is fixed point
  ::  r=rounding mode: nearest (ties to even), up, down, to zero, away from zero
  ::  d=behavior:      return denormals, flush denormals to zero,
  ::                   infinite exponent range
  =>
    ~%  %cofl  +>  ~
    ::  internal functions; mostly operating on {e/@s a/@u}, in other words
    ::  positive numbers. many of these have undefined behavior if a=0.
    |%
    ++  rou
      |=  {a/{e/@s a/@u}}  ^-  fn  (rau a &)
    ::
    ++  rau
      |=  {a/{e/@s a/@u} t/?}  ^-  fn
      ?-  r
        $z  (lug %fl a t)  $d  (lug %fl a t)
        $a  (lug %ce a t)  $u  (lug %ce a t)
        $n  (lug %ne a t)
      ==
    ::
    ++  add                                             ::  add; exact if e
      |=  {a/{e/@s a/@u} b/{e/@s a/@u} e/?}  ^-  fn
      =+  q=(dif:si e.a e.b)
      |-  ?.  (syn:si q)  $(b a, a b, q +(q))           ::  a has larger exp
      ?:  e
        [%f & e.b (^add (lsh 0 (abs:si q) a.a) a.b)]
      =+  [ma=(met 0 a.a) mb=(met 0 a.b)]
      =+  ^=  w  %+  dif:si  e.a  %-  sun:si            ::  expanded exp of a
        ?:  (gth prc ma)  (^sub prc ma)  0
      =+  ^=  x  %+  sum:si  e.b  (sun:si mb)           ::  highest exp for b
      ?:  =((cmp:si w x) --1)                           ::  don't need to add
        ?-  r
          $z  (lug %fl a &)  $d  (lug %fl a &)
          $a  (lug %lg a &)  $u  (lug %lg a &)
          $n  (lug %na a &)
        ==
      (rou [e.b (^add (lsh 0 (abs:si q) a.a) a.b)])
    ::
    ++  sub                                             ::  subtract; exact if e
      |=  {a/{e/@s a/@u} b/{e/@s a/@u} e/?}  ^-  fn
      =+  q=(dif:si e.a e.b)
      |-  ?.  (syn:si q)
        (fli $(b a, a b, q +(q), r swr))
      =+  [ma=(met 0 a.a) mb=(met 0 a.b)]
      =+  ^=  w  %+  dif:si  e.a  %-  sun:si
        ?:  (gth prc ma)  (^sub prc ma)  0
      =+  ^=  x  %+  sum:si  e.b  (sun:si mb)
      ?:  &(!e =((cmp:si w x) --1))
        ?-  r
          $z  (lug %sm a &)  $d  (lug %sm a &)
          $a  (lug %ce a &)  $u  (lug %ce a &)
          $n  (lug %nt a &)
        ==
      =+  j=(lsh 0 (abs:si q) a.a)
      |-  ?.  (gte j a.b)
        (fli $(a.b j, j a.b, r swr))
      =+  i=(^sub j a.b)
      ?~  i  [%f & zer]
      ?:  e  [%f & e.b i]  (rou [e.b i])
    ::
    ++  mul                                             ::  multiply
      |=  {a/{e/@s a/@u} b/{e/@s a/@u}}  ^-  fn
      (rou (sum:si e.a e.b) (^mul a.a a.b))
    ::
    ++  div                                             ::  divide
      |=  {a/{e/@s a/@u} b/{e/@s a/@u}}  ^-  fn
      =+  [ma=(met 0 a.a) mb=(met 0 a.b)]
      =+  v=(dif:si (sun:si ma) (sun:si +((^add mb prc))))
      =.  a  ?:  (syn:si v)  a
      a(e (sum:si v e.a), a (lsh 0 (abs:si v) a.a))
      =+  [j=(dif:si e.a e.b) q=(dvr a.a a.b)]
      (rau [j p.q] =(q.q 0))
    ::
    ++  sqt                                             ::  square root
      |=  {a/{e/@s a/@u}}  ^-  fn
      =.  a
        =+  [w=(met 0 a.a) x=(^mul +(prc) 2)]
        =+  ?:((^lth w x) (^sub x w) 0)
        =+  ?:  =((dis - 1) (dis (abs:si e.a) 1))  -
          (^add - 1)
        a(e (dif:si e.a (sun:si -)), a (lsh 0 - a.a))
      =+  [y=(^sqt a.a) z=(fra:si e.a --2)]
      (rau [z p.y] =(q.y 0))
    ::
    ++  lth                                             ::  less-than
      |=  {a/{e/@s a/@u} b/{e/@s a/@u}}  ^-  ?
      ?:  =(e.a e.b)  (^lth a.a a.b)
      =+  c=(cmp:si (ibl a) (ibl b))
      ?:  =(c -1)  &  ?:  =(c --1)  |
      ?:  =((cmp:si e.a e.b) -1)
        (^lth (rsh 0 (abs:si (dif:si e.a e.b)) a.a) a.b)
      (^lth (lsh 0 (abs:si (dif:si e.a e.b)) a.a) a.b)
    ::
    ++  lte                                             ::  less-equals
      |=  {a/{e/@s a/@u} b/{e/@s a/@u}}  ^-  ?
      ?:  =(e.a e.b)  (^lte a.a a.b)
      =+  c=(cmp:si (ibl a) (ibl b))
      ?:  =(c -1)  &  ?:  =(c --1)  |
      ?:  =((cmp:si e.a e.b) -1)
        (^lte a.a (lsh 0 (abs:si (dif:si e.a e.b)) a.b))
      (^lte (lsh 0 (abs:si (dif:si e.a e.b)) a.a) a.b)
    ::
    ++  equ                                             ::  equals
      |=  {a/{e/@s a/@u} b/{e/@s a/@u}}  ^-  ?
      ?.  =((ibl a) (ibl b))  |
      ?:  =((cmp:si e.a e.b) -1)
        =((lsh 0 (abs:si (dif:si e.a e.b)) a.b) a.a)
      =((lsh 0 (abs:si (dif:si e.a e.b)) a.a) a.b)
    ::
    ::  integer binary logarithm: 2^ibl(a) <= |a| < 2^(ibl(a)+1)
    ++  ibl
      |=  {a/{e/@s a/@u}}  ^-  @s
      (sum:si (sun:si (dec (met 0 a.a))) e.a)
    ::
    ::  change to a representation where a.a is odd
    ::  every fn has a unique representation of this kind
    ++  uni
      |=  {a/{e/@s a/@u}}
      |-  ?:  =((end 0 1 a.a) 1)  a
      $(a.a (rsh 0 1 a.a), e.a (sum:si e.a --1))
    ::
    ::  expands to either full precision or to denormalized
    ++  xpd
      |=  {a/{e/@s a/@u}}
      =+  ma=(met 0 a.a)
      ?:  (gte ma prc)  a
      =+  ?:  =(den %i)  (^sub prc ma)
          =+  ^=  q
            =+  w=(dif:si e.a emn)
            ?:  (syn:si w)  (abs:si w)  0
          (min q (^sub prc ma))
      a(e (dif:si e.a (sun:si -)), a (lsh 0 - a.a))
    ::
    ::  central rounding mechanism
    ::  can perform: floor, ceiling, smaller, larger,
    ::               nearest (round ties to: even, away from 0, toward 0)
    ::  s is sticky bit: represents a value less than ulp(a) = 2^(e.a)
    ::
    ++  lug
      ~/  %lug
      |=  {t/$?($fl $ce $sm $lg $ne $na $nt) a/{e/@s a/@u} s/?}  ^-  fn
      ?<  =(a.a 0)
      =-
        ?.  =(den %f)  -                                ::  flush denormals
        ?.  ?=({$f *} -)  -
        ?:  =((met 0 ->+>) prc)  -  [%f & zer]
      ::
      =+  m=(met 0 a.a)
      ?>  |(s (gth m prc))                              ::  require precision
      =+  ^=  q
        =+  ^=  f                                       ::  reduce precision
          ?:  (gth m prc)  (^sub m prc)  0
        =+  ^=  g  %-  abs:si                           ::  enforce min. exp
          ?:  =(den %i)  --0
          ?:  =((cmp:si e.a emn) -1)  (dif:si emn e.a)  --0
        (max f g)
      =^  b  a  :-  (end 0 q a.a)
        a(e (sum:si e.a (sun:si q)), a (rsh 0 q a.a))
      ::
      ?~  a.a
        ?<  =(den %i)
        ?-  t
          $fl  [%f & zer]  
          $sm  [%f & zer]
          $ce  [%f & spd]  
          $lg  [%f & spd]
          $ne  ?:  s  [%f & ?:((^lte b (bex (dec q))) zer spd)]
               [%f & ?:((^lth b (bex (dec q))) zer spd)]
          $nt  ?:  s  [%f & ?:((^lte b (bex (dec q))) zer spd)]
               [%f & ?:((^lth b (bex (dec q))) zer spd)]
          $na  [%f & ?:((^lth b (bex (dec q))) zer spd)]
        ==
      ::
      =.  a  (xpd a)
      ::
      =.  a
        ?-  t
          $fl  a
          $lg  a(a +(a.a))
          $sm  ?.  &(=(b 0) s)  a
               ?:  &(=(e.a emn) !=(den %i))  a(a (dec a.a))
               =+  y=(dec (^mul a.a 2))
               ?.  (^lte (met 0 y) prc)  a(a (dec a.a))
               [(dif:si e.a --1) y]
          $ce  ?:  &(=(b 0) s)  a  a(a +(a.a))
          $ne  ?~  b  a
               =+  y=(bex (dec q))
               ?:  &(=(b y) s)                          ::  round halfs to even
                 ?~  (dis a.a 1)  a  a(a +(a.a))
               ?:  (^lth b y)  a  a(a +(a.a))
          $na  ?~  b  a
               =+  y=(bex (dec q))
               ?:  (^lth b y)  a  a(a +(a.a))
          $nt  ?~  b  a
               =+  y=(bex (dec q))
               ?:  =(b y)  ?:  s  a  a(a +(a.a))
               ?:  (^lth b y)  a  a(a +(a.a))
        ==
      ::
      =.  a  ?.  =((met 0 a.a) +(prc))  a
        a(a (rsh 0 1 a.a), e (sum:si e.a --1))
      ?~  a.a  [%f & zer]
      ::
      ?:  =(den %i)  [%f & a]
      ?:  =((cmp:si emx e.a) -1)  [%i &]  [%f & a]      ::  enforce max. exp
    ::
    ++  drg                                             ::  dragon4;
      ~/  %drg                                          ::  convert to decimal
      |=  {a/{e/@s a/@u}}  ^-  {@s @u}
      ?<  =(a.a 0)
      =.  a  (xpd a)
      =+  r=(lsh 0 ?:((syn:si e.a) (abs:si e.a) 0) a.a)
      =+  s=(lsh 0 ?.((syn:si e.a) (abs:si e.a) 0) 1)
      =+  m=(lsh 0 ?:((syn:si e.a) (abs:si e.a) 0) 1)
      =+  [k=--0 q=(^div (^add s 9) 10)]
      |-  ?:  (^lth r q)
        %=  $
          k  (dif:si k --1)
          r  (^mul r 10)
          m  (^mul m 10)
        ==
      |-  ?:  (gte (^add (^mul r 2) m) (^mul s 2))
        $(s (^mul s 10), k (sum:si k --1))
      =+  [u=0 o=0]
      |-
      =+  v=(dvr (^mul r 10) s)
      =>  %=  .
          k  (dif:si k --1)
          u  p.v
          r  q.v
          m  (^mul m 10)
        ==
      =+  l=(^lth (^mul r 2) m)
      =+  ^=  h
        ?|  (^lth (^mul s 2) m)
            (gth (^mul r 2) (^sub (^mul s 2) m))
        ==
      ?:  &(!l !h)
        $(o (^add (^mul o 10) u))
      =+  q=&(h |(!l (gte (^mul r 2) s)))
      =.  o  (^add (^mul o 10) ?:(q +(u) u))
      [k o]
    ::
    ++  toj                                             ::  round to integer
      |=  {a/{e/@s a/@u}}  ^-  fn
      ?.  =((cmp:si e.a --0) -1)  [%f & a]
      =+  x=(abs:si e.a)
      =+  y=(rsh 0 x a.a)
      ?:  |(=(r %d) =(r %z))  [%f & --0 y]
      =+  z=(end 0 x a.a)
      ?:  |(=(r %u) =(r %a))  [%f & --0 ?~(z y +(y))]
      =+  i=(bex (dec x))
      ?:  &(=(z i) =((dis y 1) 0))  [%f & --0 y]
      ?:  (^lth z i)  [%f & --0 y]  [%f & --0 +(y)]
    ::
    ++  ned                                             ::  require ?=({$f *} a)
      |=  {a/fn}  ^-  {$f s/? e/@s a/@u}
      ?:  ?=({$f *} a)  a
      ~>  %mean.[0 leaf+"need-float"]
      !!
    ::
    ++  shf                                             ::  a * 2^b; no rounding
      |=  {a/fn b/@s}
      ?:  |(?=({$n *} a) ?=({$i *} a))  a
      a(e (sum:si e.a b))
    ::
    ++  fli                                             ::  flip sign
      |=  {a/fn}  ^-  fn
      ?-(-.a $f a(s !s.a), $i a(s !s.a), $n a)
    ::
    ++  swr  ?+(r r $d %u, $u %d)                       ::  flipped rounding
    ++  prc  ?>((gth p 1) p)                            ::  force >= 2 precision
    ++  den  d                                          ::  denorm+flush+inf exp
    ++  emn  v                                          ::  minimum exponent
    ++  emx  (sum:si emn (sun:si w))                    ::  maximum exponent
    ++  spd  [e=emn a=1]                                ::  smallest denormal
    ++  spn  [e=emn a=(bex (dec prc))]                  ::  smallest normal
    ++  lfn  [e=emx a=(fil 0 prc 1)]                    ::  largest
    ++  lfe  (sum:si emx (sun:si prc))                  ::  2^lfe is > than all
    ++  zer  [e=--0 a=0]
    --
  |%
  ++  rou                                               ::  round
    |=  {a/fn}  ^-  fn
    ?.  ?=({$f *} a)  a
    ?~  a.a  [%f s.a zer]
    ?:  s.a  (^rou +>.a)
    =.(r swr (fli (^rou +>.a)))
  ::
  ++  syn                                               ::  get sign
    |=  {a/fn}  ^-  ?
    ?-(-.a $f s.a, $i s.a, $n &)
  ::
  ++  abs                                               ::  absolute value
    |=  {a/fn}  ^-  fn
    ?:  ?=({$f *} a)  [%f & e.a a.a]
    ?:  ?=({$i *} a)  [%i &]  [%n ~]
  ::
  ++  add                                               ::  add
    |=  {a/fn b/fn}  ^-  fn
    ?:  |(?=({$n *} a) ?=({$n *} b))  [%n ~]
    ?:  |(?=({$i *} a) ?=({$i *} b))
      ?:  &(?=({$i *} a) ?=({$i *} b))
        ?:  =(a b)  a  [%n ~]
      ?:  ?=({$i *} a)  a  b
    ?:  |(=(a.a 0) =(a.b 0))
      ?.  &(=(a.a 0) =(a.b 0))  %-  rou  ?~(a.a b a)
      [%f ?:(=(r %d) &(s.a s.b) |(s.a s.b)) zer]
    %-  |=  {a/fn}
        ?.  ?=({$f *} a)  a
        ?.  =(a.a 0)  a
        [%f !=(r %d) zer]
    ?:  =(s.a s.b)
      ?:  s.a  (^add +>.a +>.b |)
      =.(r swr (fli (^add +>.a +>.b |)))
    ?:  s.a  (^sub +>.a +>.b |)
    (^sub +>.b +>.a |)
  ::
  ++  ead                                               ::  exact add
    |=  {a/fn b/fn}  ^-  fn
    ?:  |(?=({$n *} a) ?=({$n *} b))  [%n ~]
    ?:  |(?=({$i *} a) ?=({$i *} b))
      ?:  &(?=({$i *} a) ?=({$i *} b))
        ?:  =(a b)  a  [%n ~]
      ?:  ?=({$i *} a)  a  b
    ?:  |(=(a.a 0) =(a.b 0))
      ?.  &(=(a.a 0) =(a.b 0))  ?~(a.a b a)
      [%f ?:(=(r %d) &(s.a s.b) |(s.a s.b)) zer]
    %-  |=  {a/fn}
        ?.  ?=({$f *} a)  a
        ?.  =(a.a 0)  a
        [%f !=(r %d) zer]
    ?:  =(s.a s.b)
      ?:  s.a  (^add +>.a +>.b &)
      (fli (^add +>.a +>.b &))
    ?:  s.a  (^sub +>.a +>.b &)
    (^sub +>.b +>.a &)
  ::
  ++  sub                                               ::  subtract
    |=  {a/fn b/fn}  ^-  fn  (add a (fli b))
  ::
  ++  mul                                               ::  multiply
    |=  {a/fn b/fn}  ^-  fn
    ?:  |(?=({$n *} a) ?=({$n *} b))  [%n ~]
    ?:  ?=({$i *} a)
      ?:  ?=({$i *} b)  
        [%i =(s.a s.b)]
      ?:  =(a.b 0)  [%n ~]  [%i =(s.a s.b)]
    ?:  ?=({$i *} b)
      ?:  =(a.a 0)  [%n ~]  [%i =(s.a s.b)]
    ?:  |(=(a.a 0) =(a.b 0))  [%f =(s.a s.b) zer]
    ?:  =(s.a s.b)  (^mul +>.a +>.b)
    =.(r swr (fli (^mul +>.a +>.b)))
  ::
  ++  emu                                               ::  exact multiply
    |=  {a/fn b/fn}  ^-  fn
    ?:  |(?=({$n *} a) ?=({$n *} b))  [%n ~]
    ?:  ?=({$i *} a)
      ?:  ?=({$i *} b)  
        [%i =(s.a s.b)]
      ?:  =(a.b 0)  [%n ~]  [%i =(s.a s.b)]
    ?:  ?=({$i *} b)
      ?:  =(a.a 0)  [%n ~]  [%i =(s.a s.b)]
    ?:  |(=(a.a 0) =(a.b 0))  [%f =(s.a s.b) zer]
    [%f =(s.a s.b) (sum:si e.a e.b) (^^mul a.a a.b)]
  ::
  ++  div                                               ::  divide
    |=  {a/fn b/fn}  ^-  fn
    ?:  |(?=({$n *} a) ?=({$n *} b))  [%n ~]
    ?:  ?=({$i *} a)
      ?:  ?=({$i *} b)  [%n ~]  [%i =(s.a s.b)]
    ?:  ?=({$i *} b)  [%f =(s.a s.b) zer]
    ?:  =(a.a 0)  ?:  =(a.b 0)  [%n ~]  [%f =(s.a s.b) zer]
    ?:  =(a.b 0)  [%i =(s.a s.b)]
    ?:  =(s.a s.b)  (^div +>.a +>.b)
    =.(r swr (fli (^div +>.a +>.b)))
  ::
  ++  fma                                               ::  fused multiply-add
    |=  {a/fn b/fn c/fn}  ^-  fn                        ::  (a * b) + c
    (add (emu a b) c)
  ::
  ++  sqt                                               ::  square root
    |=  {a/fn}  ^-  fn
    ?:  ?=({$n *} a)  [%n ~]
    ?:  ?=({$i *} a)  ?:(s.a a [%n ~])
    ?~  a.a  [%f s.a zer]
    ?:  s.a  (^sqt +>.a)  [%n ~]
  ::
  ++  inv                                               ::  inverse
    |=  {a/fn}  ^-  fn
    (div [%f & --0 1] a)
  ::
  ++  sun                                               ::  uns integer to float
    |=  {a/@u}  ^-  fn
    (rou [%f & --0 a])
  ::
  ++  san                                               ::  sgn integer to float
    |=  {a/@s}  ^-  fn
    =+  b=(old:si a)
    (rou [%f -.b --0 +.b])
  ::
  ::  comparisons return ~ in the event of a NaN
  ++  lth                                               ::  less-than
    |=  {a/fn b/fn}  ^-  (unit ?)
    ?:  |(?=({$n *} a) ?=({$n *} b))  ~  :-  ~
    ?:  =(a b)  |
    ?:  ?=({$i *} a)  !s.a  ?:  ?=({$i *} b)  s.b
    ?:  |(=(a.a 0) =(a.b 0))
      ?:  &(=(a.a 0) =(a.b 0))  |
      ?:  =(a.a 0)  s.b  !s.a
    ?:  !=(s.a s.b)  s.b
    ?:  s.a  (^lth +>.a +>.b)  (^lth +>.b +>.a)
  ::
  ++  lte                                               ::  less-equal
    |=  {a/fn b/fn}  ^-  (unit ?)
    ?:  |(?=({$n *} a) ?=({$n *} b))  ~  :-  ~
    ?:  =(a b)  &
    ?:  ?=({$i *} a)  !s.a  ?:  ?=({$i *} b)  s.b
    ?:  |(=(a.a 0) =(a.b 0))
      ?:  &(=(a.a 0) =(a.b 0))  &
      ?:  =(a.a 0)  s.b  !s.a
    ?:  !=(s.a s.b)  s.b
    ?:  s.a  (^lte +>.a +>.b)  (^lte +>.b +>.a)
  ::
  ++  equ                                               ::  equal
    |=  {a/fn b/fn}  ^-  (unit ?)
    ?:  |(?=({$n *} a) ?=({$n *} b))  ~  :-  ~
    ?:  =(a b)  &
    ?:  |(?=({$i *} a) ?=({$i *} b))  |
    ?:  |(=(a.a 0) =(a.b 0))
      ?:  &(=(a.a 0) =(a.b 0))  &  |
    ?:  |(=(e.a e.b) !=(s.a s.b))  |
    (^equ +>.a +>.b)
  ::
  ++  gte                                               ::  greater-equal
    |=  {a/fn b/fn}  ^-  (unit ?)  (lte b a)
  ::
  ++  gth                                               ::  greater-than
    |=  {a/fn b/fn}  ^-  (unit ?)  (lth b a)
  ::
  ++  drg                                               ::  float to decimal
    |=  {a/fn}  ^-  dn
    ?:  ?=({$n *} a)  [%n ~]
    ?:  ?=({$i *} a)  [%i s.a]
    ?~  a.a  [%d s.a --0 0]
    [%d s.a (^drg +>.a)]
  ::
  ++  grd                                               ::  decimal to float
    |=  {a/dn}  ^-  fn
    ?:  ?=({$n *} a)  [%n ~]
    ?:  ?=({$i *} a)  [%i s.a]
    =>  .(r %n)
    =+  q=(abs:si e.a)
    ?:  (syn:si e.a)
      (mul [%f s.a --0 a.a] [%f & e.a (pow 5 q)])
    (div [%f s.a --0 a.a] [%f & (sun:si q) (pow 5 q)])
  ::
  ++  toi                                               ::  round to integer @s
    |=  {a/fn}  ^-  (unit @s)
    =+  b=(toj a)
    ?.  ?=({$f *} b)  ~  :-  ~
    =+  c=(^^mul (bex (abs:si e.b)) a.b)
    (new:si s.b c)
  ::
  ++  toj                                               ::  round to integer fn
    |=  {a/fn}  ^-  fn
    ?.  ?=({$f *} a)  a
    ?~  a.a  [%f s.a zer]
    ?:  s.a  (^toj +>.a)
    =.(r swr (fli (^toj +>.a)))
  --
::
++  ff                                                  ::  ieee 754 format fp
  |_  {{w/@u p/@u b/@s} r/$?($n $u $d $z $a)}
  ::  this core has no use outside of the functionality
  ::  provided to ++rd, ++rs, ++rq, and ++rh
  ::
  ::  w=width:         bits in exponent field
  ::  p=precision:     bits in fraction field
  ::  w=bias:          added to exponent when storing
  ::  r=rounding mode: same as in ++fl
  ::
  ++  sb  (bex (^add w p))                              ::  sign bit
  ++  me  (dif:si (dif:si --1 b) (sun:si p))            ::  minimum exponent
  ::
  ++  pa
    %*(. fl p +(p), v me, w (^sub (bex w) 3), d %d, r r)
  ::
  ++  sea                                               ::  @r to fn
    |=  {a/@r}  ^-  fn
    =+  [f=(cut 0 [0 p] a) e=(cut 0 [p w] a)]
    =+  s=(sig a)
    ?:  =(e 0)
      ?:  =(f 0)  [%f s --0 0]  [%f s me f]
    ?:  =(e (fil 0 w 1))
      ?:  =(f 0)  [%i s]  [%n ~]
    =+  q=:(sum:si (sun:si e) me -1)
    =+  r=(^add f (bex p))
    [%f s q r]
  ::
  ++  bit  |=  {a/fn}  (bif (rou:pa a))                 ::  fn to @r w+ rounding
  ::
  ++  bif                                               ::  fn to @r no rounding
    |=  {a/fn}  ^-  @r
    ?:  ?=({$i *} a)
      =+  q=(lsh 0 p (fil 0 w 1))
      ?:  s.a  q  (^add q sb)
    ?:  ?=({$n *} a)  (lsh 0 (dec p) (fil 0 +(w) 1))
    ?~  a.a  ?:  s.a  `@r`0  sb
    =+  ma=(met 0 a.a)
    ?.  =(ma +(p))
      ?>  =(e.a me)
      ?>  (^lth ma +(p))
      ?:  s.a  `@r`a.a  (^add a.a sb)
    =+  q=(sum:si (dif:si e.a me) --1)
    =+  r=(^add (lsh 0 p (abs:si q)) (end 0 p a.a))
    ?:  s.a  r  (^add r sb)
  ::
  ++  sig                                               ::  get sign
    |=  {a/@r}  ^-  ?
    =(0 (cut 0 [(^add p w) 1] a))
  ::
  ++  exp                                               ::  get exponent
    |=  {a/@r}  ^-  @s
    (dif:si (sun:si (cut 0 [p w] a)) b)
  ::
  ++  add                                               ::  add
    |=  {a/@r b/@r}
    (bif (add:pa (sea a) (sea b)))
  ::
  ++  sub                                               ::  subtract
    |=  {a/@r b/@r}
    (bif (sub:pa (sea a) (sea b)))
  ::
  ++  mul                                               ::  multiply
    |=  {a/@r b/@r}
    (bif (mul:pa (sea a) (sea b)))
  ::
  ++  div                                               ::  divide
    |=  {a/@r b/@r}
    (bif (div:pa (sea a) (sea b)))
  ::
  ++  fma                                               ::  fused multiply-add
    |=  {a/@r b/@r c/@r}
    (bif (fma:pa (sea a) (sea b) (sea c)))
  ::
  ++  sqt                                               ::  square root
    |=  {a/@r}
    (bif (sqt:pa (sea a)))
  ::
  ++  lth                                               ::  less-than
    |=  {a/@r b/@r}  (fall (lth:pa (sea a) (sea b)) |)
  ++  lte                                               ::  less-equals
    |=  {a/@r b/@r}  (fall (lte:pa (sea a) (sea b)) |)
  ++  equ                                               ::  equals
    |=  {a/@r b/@r}  (fall (equ:pa (sea a) (sea b)) |)
  ++  gte                                               ::  greater-equals
    |=  {a/@r b/@r}  (fall (gte:pa (sea a) (sea b)) |)
  ++  gth                                               ::  greater-than
    |=  {a/@r b/@r}  (fall (gth:pa (sea a) (sea b)) |)
  ++  sun                                               ::  uns integer to @r
    |=  {a/@u}  (bit [%f & --0 a])
  ++  san                                               ::  signed integer to @r
    |=  {a/@s}  (bit [%f (syn:si a) --0 (abs:si a)])
  ++  toi                                               ::  round to integer
    |=  {a/@r}  (toi:pa (sea a))
  ++  drg                                               ::  @r to decimal float
    |=  {a/@r}  (drg:pa (sea a))
  ++  grd                                               ::  decimal float to @r
    |=  {a/dn}  (bif (grd:pa a))
  --
::
++  rlyd  |=  a/@rd  ^-  dn  (drg:rd a)                 ::  prep @rd for print
++  rlys  |=  a/@rs  ^-  dn  (drg:rs a)                 ::  prep @rs for print
++  rlyh  |=  a/@rh  ^-  dn  (drg:rh a)                 ::  prep @rh for print
++  rlyq  |=  a/@rq  ^-  dn  (drg:rq a)                 ::  prep @rq for print
++  ryld  |=  a/dn  ^-  @rd  (grd:rd a)                 ::  finish parsing @rd
++  ryls  |=  a/dn  ^-  @rs  (grd:rs a)                 ::  finish parsing @rs
++  rylh  |=  a/dn  ^-  @rh  (grd:rh a)                 ::  finish parsing @rh
++  rylq  |=  a/dn  ^-  @rq  (grd:rq a)                 ::  finish parsing @rq
::
++  rd                                                  ::  double precision fp
  ~%  %rd  +>  ~
  |_  r/$?($n $u $d $z)
  ::  round to nearest, round up, round down, round to zero
  ::
  ++  ma
    %*(. ff w 11, p 52, b --1.023, r r)
  ::
  ++  sea                                               ::  @rd to fn
    |=  {a/@rd}  (sea:ma a)
  ::
  ++  bit                                               ::  fn to @rd
    |=  {a/fn}  ^-  @rd  (bit:ma a)
  ::
  ++  add  ~/  %add                                     ::  add
    |=  {a/@rd b/@rd}  ^-  @rd  
    ~>  %mean.[0 leaf+"rd-fail"]
    (add:ma a b)
  ::
  ++  sub  ~/  %sub                                     ::  subtract
    |=  {a/@rd b/@rd}  ^-  @rd  
    ~>  %mean.[0 leaf+"rd-fail"]
    (sub:ma a b)
  ::
  ++  mul  ~/  %mul                                     ::  multiply
    |=  {a/@rd b/@rd}  ^-  @rd  
    ~>  %mean.[0 leaf+"rd-fail"]
    (mul:ma a b)
  ::
  ++  div  ~/  %div                                     ::  divide
    |=  {a/@rd b/@rd}  ^-  @rd  
    ~>  %mean.[0 leaf+"rd-fail"]
    (div:ma a b)
  ::
  ++  fma  ~/  %fma                                     ::  fused multiply-add
    |=  {a/@rd b/@rd c/@rd}  ^-  @rd  
    ~>  %mean.[0 leaf+"rd-fail"]
    (fma:ma a b c)
  ::
  ++  sqt  ~/  %sqt                                     ::  square root
    |=  {a/@rd}  ^-  @rd  ~>  %mean.[0 leaf+"rd-fail"]
    (sqt:ma a)
  ::
  ++  lth  ~/  %lth                                     ::  less-than
    |=  {a/@rd b/@rd}  
    ~>  %mean.[0 leaf+"rd-fail"]  
    (lth:ma a b)
  ::
  ++  lte  ~/  %lte                                     ::  less-equals
    |=  {a/@rd b/@rd}  ~>  %mean.[0 leaf+"rd-fail"]  (lte:ma a b)
  ++  equ  ~/  %equ                                     ::  equals
    |=  {a/@rd b/@rd}  ~>  %mean.[0 leaf+"rd-fail"]  (equ:ma a b)
  ++  gte  ~/  %gte                                     ::  greater-equals
    |=  {a/@rd b/@rd}  ~>  %mean.[0 leaf+"rd-fail"]  (gte:ma a b)
  ++  gth  ~/  %gth                                     ::  greater-than
    |=  {a/@rd b/@rd}  ~>  %mean.[0 leaf+"rd-fail"]  (gth:ma a b)
  ::
  ++  sun  |=  {a/@u}  ^-  @rd  (sun:ma a)              ::  uns integer to @rd
  ++  san  |=  {a/@s}  ^-  @rd  (san:ma a)              ::  sgn integer to @rd
  ++  sig  |=  {a/@rd}  ^-  ?  (sig:ma a)               ::  get sign
  ++  exp  |=  {a/@rd}  ^-  @s  (exp:ma a)              ::  get exponent
  ++  toi  |=  {a/@rd}  ^-  (unit @s)  (toi:ma a)       ::  round to integer
  ++  drg  |=  {a/@rd}  ^-  dn  (drg:ma a)              ::  @rd to decimal float
  ++  grd  |=  {a/dn}  ^-  @rd  (grd:ma a)              ::  decimal float to @rd
  --
::
++  rs                                                  ::  single precision fp
  ~%  %rs  +>  ~
  |_  r/$?($n $u $d $z)
  ::  round to nearest, round up, round down, round to zero
  ::
  ++  ma
    %*(. ff w 8, p 23, b --127, r r)
  ::
  ++  sea                                               ::  @rs to fn
    |=  {a/@rs}  (sea:ma a)
  ::
  ++  bit                                               ::  fn to @rs
    |=  {a/fn}  ^-  @rs  (bit:ma a)
  ::
  ++  add  ~/  %add                                     ::  add
    |=  {a/@rs b/@rs}  ^-  @rs  
    ~>  %mean.[0 leaf+"rs-fail"]
    (add:ma a b)
  ::
  ++  sub  ~/  %sub                                     ::  subtract
    |=  {a/@rs b/@rs}  ^-  @rs  
    ~>  %mean.[0 leaf+"rs-fail"]
    (sub:ma a b)
  ::
  ++  mul  ~/  %mul                                     ::  multiply
    |=  {a/@rs b/@rs}  ^-  @rs  
    ~>  %mean.[0 leaf+"rs-fail"]
    (mul:ma a b)
  ::
  ++  div  ~/  %div                                     ::  divide
    |=  {a/@rs b/@rs}  ^-  @rs  
    ~>  %mean.[0 leaf+"rs-fail"]
    (div:ma a b)
  ::
  ++  fma  ~/  %fma                                     ::  fused multiply-add
    |=  {a/@rs b/@rs c/@rs}  ^-  @rs  
    ~>  %mean.[0 leaf+"rs-fail"]
    (fma:ma a b c)
  ::
  ++  sqt  ~/  %sqt                                     ::  square root
    |=  {a/@rs}  ^-  @rs  
    ~>  %mean.[0 leaf+"rs-fail"]
    (sqt:ma a)
  ::
  ++  lth  ~/  %lth                                     ::  less-than
    |=  {a/@rs b/@rs}  
    ~>  %mean.[0 leaf+"rs-fail"]  
    (lth:ma a b)
  ::
  ++  lte  ~/  %lte                                     ::  less-equals
    |=  {a/@rs b/@rs}  
    ~>  %mean.[0 leaf+"rs-fail"]  
    (lte:ma a b)
  ::
  ++  equ  ~/  %equ                                     ::  equals
    |=  {a/@rs b/@rs}  
    ~>  %mean.[0 leaf+"rs-fail"]  
    (equ:ma a b)
  ::
  ++  gte  ~/  %gte                                     ::  greater-equals
    |=  {a/@rs b/@rs}  
    ~>  %mean.[0 leaf+"rs-fail"]  
    (gte:ma a b)
  ::
  ++  gth  ~/  %gth                                     ::  greater-than
    |=  {a/@rs b/@rs}  
    ~>  %mean.[0 leaf+"rs-fail"]  
    (gth:ma a b)
  ::
  ++  sun  |=  {a/@u}  ^-  @rs  (sun:ma a)              ::  uns integer to @rs
  ++  san  |=  {a/@s}  ^-  @rs  (san:ma a)              ::  sgn integer to @rs
  ++  sig  |=  {a/@rs}  ^-  ?  (sig:ma a)               ::  get sign
  ++  exp  |=  {a/@rs}  ^-  @s  (exp:ma a)              ::  get exponent
  ++  toi  |=  {a/@rs}  ^-  (unit @s)  (toi:ma a)       ::  round to integer
  ++  drg  |=  {a/@rs}  ^-  dn  (drg:ma a)              ::  @rs to decimal float
  ++  grd  |=  {a/dn}  ^-  @rs  (grd:ma a)              ::  decimal float to @rs
  --
::
++  rq                                                  ::  quad precision fp
  ~%  %rq  +>  ~
  |_  r/$?($n $u $d $z)
  ::  round to nearest, round up, round down, round to zero
  ::
  ++  ma
    %*(. ff w 15, p 112, b --16.383, r r)
  ::
  ++  sea                                               ::  @rq to fn
    |=  {a/@rq}  (sea:ma a)
  ::
  ++  bit                                               ::  fn to @rq
    |=  {a/fn}  ^-  @rq  (bit:ma a)
  ::
  ++  add  ~/  %add                                     ::  add
    |=  {a/@rq b/@rq}  ^-  @rq  
    ~>  %mean.[0 leaf+"rq-fail"]
    (add:ma a b)
  ::
  ++  sub  ~/  %sub                                     ::  subtract
    |=  {a/@rq b/@rq}  ^-  @rq  
    ~>  %mean.[0 leaf+"rq-fail"]
    (sub:ma a b)
  ::
  ++  mul  ~/  %mul                                     ::  multiply
    |=  {a/@rq b/@rq}  ^-  @rq  
    ~>  %mean.[0 leaf+"rq-fail"]
    (mul:ma a b)
  ::
  ++  div  ~/  %div                                     ::  divide
    |=  {a/@rq b/@rq}  ^-  @rq  
    ~>  %mean.[0 leaf+"rq-fail"]
    (div:ma a b)
  ::
  ++  fma  ~/  %fma                                     ::  fused multiply-add
    |=  {a/@rq b/@rq c/@rq}  ^-  @rq  
    ~>  %mean.[0 leaf+"rq-fail"]
    (fma:ma a b c)
  ::
  ++  sqt  ~/  %sqt                                     ::  square root
    |=  {a/@rq}  ^-  @rq  
    ~>  %mean.[0 leaf+"rq-fail"]
    (sqt:ma a)
  ::
  ++  lth  ~/  %lth                                     ::  less-than
    |=  {a/@rq b/@rq}  
    ~>  %mean.[0 leaf+"rq-fail"]
    (lth:ma a b)
  ::
  ++  lte  ~/  %lte                                     ::  less-equals
    |=  {a/@rq b/@rq}  
    ~>  %mean.[0 leaf+"rq-fail"]  
    (lte:ma a b)
  ::
  ++  equ  ~/  %equ                                     ::  equals
    |=  {a/@rq b/@rq}  
    ~>  %mean.[0 leaf+"rq-fail"]  
    (equ:ma a b)
  ::
  ++  gte  ~/  %gte                                     ::  greater-equals
    |=  {a/@rq b/@rq}  
    ~>  %mean.[0 leaf+"rq-fail"]  
    (gte:ma a b)
  ::
  ++  gth  ~/  %gth                                     ::  greater-than
    |=  {a/@rq b/@rq}  
    ~>  %mean.[0 leaf+"rq-fail"]  
    (gth:ma a b)
  ::
  ++  sun  |=  {a/@u}  ^-  @rq  (sun:ma a)              ::  uns integer to @rq
  ++  san  |=  {a/@s}  ^-  @rq  (san:ma a)              ::  sgn integer to @rq
  ++  sig  |=  {a/@rq}  ^-  ?  (sig:ma a)               ::  get sign
  ++  exp  |=  {a/@rq}  ^-  @s  (exp:ma a)              ::  get exponent
  ++  toi  |=  {a/@rq}  ^-  (unit @s)  (toi:ma a)       ::  round to integer
  ++  drg  |=  {a/@rq}  ^-  dn  (drg:ma a)              ::  @rq to decimal float
  ++  grd  |=  {a/dn}  ^-  @rq  (grd:ma a)              ::  decimal float to @rq
  --
::
++  rh                                                  ::  half precision fp
  |_  r/$?($n $u $d $z)
  ::  round to nearest, round up, round down, round to zero
  ::
  ++  ma
    %*(. ff w 5, p 10, b --15, r r)
  ::
  ++  sea                                               ::  @rh to fn
    |=  {a/@rh}  (sea:ma a)
  ::
  ++  bit                                               ::  fn to @rh
    |=  {a/fn}  ^-  @rh  (bit:ma a)
  ::
  ++  tos                                               ::  @rh to @rs
    |=  {a/@rh}  (bit:rs (sea a))
  ::
  ++  fos                                               ::  @rs to @rh
    |=  {a/@rs}  (bit (sea:rs a))
  ::
  ++  lth  ~/  %lth                                     ::  less-than
    |=  {a/@rh b/@rh}  
    ~>  %mean.[0 leaf+"rh-fail"]  
    (lth:ma a b)
  ::
  ++  lte  ~/  %lte                                     ::  less-equals
    |=  {a/@rh b/@rh}  
    ~>  %mean.[0 leaf+"rh-fail"]  
    (lte:ma a b)
  ::
  ++  equ  ~/  %equ                                     ::  equals
    |=  {a/@rh b/@rh}  
    ~>  %mean.[0 leaf+"rh-fail"]  
    (equ:ma a b)
  ::
  ++  gte  ~/  %gte                                     ::  greater-equals
    |=  {a/@rh b/@rh}  
    ~>  %mean.[0 leaf+"rh-fail"]  
    (gte:ma a b)
  ::
  ++  gth  ~/  %gth                                     ::  greater-than
    |=  {a/@rh b/@rh}  
    ~>  %mean.[0 leaf+"rh-fail"]  
    (gth:ma a b)
  ::
  ++  sun  |=  {a/@u}  ^-  @rh  (sun:ma a)              ::  uns integer to @rh
  ++  san  |=  {a/@s}  ^-  @rh  (san:ma a)              ::  sgn integer to @rh
  ++  sig  |=  {a/@rh}  ^-  ?  (sig:ma a)               ::  get sign
  ++  exp  |=  {a/@rh}  ^-  @s  (exp:ma a)              ::  get exponent
  ++  toi  |=  {a/@rh}  ^-  (unit @s)  (toi:ma a)       ::  round to integer
  ++  drg  |=  {a/@rh}  ^-  dn  (drg:ma a)              ::  @rh to decimal float
  ++  grd  |=  {a/dn}  ^-  @rh  (grd:ma a)              ::  decimal float to @rh
  --
::    3c: urbit time                                    ::
::::                                                    ::
  ::  year, yore, yell, yule, yall, yawn, yelp, yo      ::
  ::
--  =>
|%
++  year                                                ::  date to @d
  |=  det/date
  ^-  @da
  =+  ^=  yer
      ?:  a.det
        (add 292.277.024.400 y.det)
      (sub 292.277.024.400 (dec y.det))
  =+  day=(yawn yer m.det d.t.det)
  (yule day h.t.det m.t.det s.t.det f.t.det)
::
++  yore                                                ::  @d to date
  |=  now/@da
  ^-  date
  =+  rip=(yell now)
  =+  ger=(yall d.rip)
  :-  ?:  (gth y.ger 292.277.024.400)
        [a=& y=(sub y.ger 292.277.024.400)]
      [a=| y=+((sub 292.277.024.400 y.ger))]
  [m.ger d.ger h.rip m.rip s.rip f.rip]
::
++  yell                                                ::  tarp from @d
  |=  now/@d
  ^-  tarp
  =+  sec=(rsh 6 1 now)
  =+  ^=  fan
      =+  [muc=4 raw=(end 6 1 now)]
      |-  ^-  (list @ux)
      ?:  |(=(0 raw) =(0 muc))
        ~
      =>  .(muc (dec muc))
      [(cut 4 [muc 1] raw) $(raw (end 4 muc raw))]
  =+  day=(div sec day:yo)
  =>  .(sec (mod sec day:yo))
  =+  hor=(div sec hor:yo)
  =>  .(sec (mod sec hor:yo))
  =+  mit=(div sec mit:yo)
  =>  .(sec (mod sec mit:yo))
  [day hor mit sec fan]
::
++  yule                                                ::  time atom
  |=  rip/tarp
  ^-  @d
  =+  ^=  sec  ;:  add
                 (mul d.rip day:yo)
                 (mul h.rip hor:yo)
                 (mul m.rip mit:yo)
                 s.rip
               ==
  =+  ^=  fac  =+  muc=4
               |-  ^-  @
               ?~  f.rip
                 0
               =>  .(muc (dec muc))
               (add (lsh 4 muc i.f.rip) $(f.rip t.f.rip))
  (con (lsh 6 1 sec) fac)
::
++  yall                                                ::  day / to day of year
  |=  day/@ud
  ^-  {y/@ud m/@ud d/@ud}
  =+  [era=0 cet=0 lep=*?]
  =>  .(era (div day era:yo), day (mod day era:yo))
  =>  ^+  .
      ?:  (lth day +(cet:yo))
        .(lep &, cet 0)
      =>  .(lep |, cet 1, day (sub day +(cet:yo)))
      .(cet (add cet (div day cet:yo)), day (mod day cet:yo))
  =+  yer=(add (mul 400 era) (mul 100 cet))
  |-  ^-  {y/@ud m/@ud d/@ud}
  =+  dis=?:(lep 366 365)
  ?.  (lth day dis)
    =+  ner=+(yer)
    $(yer ner, day (sub day dis), lep =(0 (end 0 2 ner)))
  |-  ^-  {y/@ud m/@ud d/@ud}
  =+  [mot=0 cah=?:(lep moy:yo moh:yo)]
  |-  ^-  {y/@ud m/@ud d/@ud}
  =+  zis=(snag mot cah)
  ?:  (lth day zis)
    [yer +(mot) +(day)]
  $(mot +(mot), day (sub day zis))
::
++  yawn                                                ::  days since Jesus
  |=  {yer/@ud mot/@ud day/@ud}
  ^-  @ud
  =>  .(mot (dec mot), day (dec day))
  =>  ^+  .
      %=    .
          day
        =+  cah=?:((yelp yer) moy:yo moh:yo)
        |-  ^-  @ud
        ?:  =(0 mot)
          day
        $(mot (dec mot), cah (slag 1 cah), day (add day (snag 0 cah)))
      ==
  |-  ^-  @ud
  ?.  =(0 (mod yer 4))
    =+  ney=(dec yer)
    $(yer ney, day (add day ?:((yelp ney) 366 365)))
  ?.  =(0 (mod yer 100))
    =+  nef=(sub yer 4)
    $(yer nef, day (add day ?:((yelp nef) 1.461 1.460)))
  ?.  =(0 (mod yer 400))
    =+  nec=(sub yer 100)
    $(yer nec, day (add day ?:((yelp nec) 36.525 36.524)))
  (add day (mul (div yer 400) (add 1 (mul 4 36.524))))
::
++  yelp                                                ::  leap year
  |=  yer/@ud  ^-  ?
  &(=(0 (mod yer 4)) |(!=(0 (mod yer 100)) =(0 (mod yer 400))))
::
++  yo                                                  ::  time constants
  |%  ++  cet  36.524                 ::  (add 24 (mul 100 365))
      ++  day  86.400                 ::  (mul 24 hor)
      ++  era  146.097                ::  (add 1 (mul 4 cet))
      ++  hor  3.600                  ::  (mul 60 mit)
      ++  jes  106.751.991.084.417    ::  (mul 730.692.561 era)
      ++  mit  60
      ++  moh  `(list @ud)`[31 28 31 30 31 30 31 31 30 31 30 31 ~]
      ++  moy  `(list @ud)`[31 29 31 30 31 30 31 31 30 31 30 31 ~]
      ++  qad  126.144.001            ::  (add 1 (mul 4 yer))
      ++  yer  31.536.000             ::  (mul 365 day)
  --
::                                                      ::
::::  3d: SHA family                                    ::
  ::                                                    ::
  ::    shad, shaf, sham, shas, shax, shay, shaw,       ::
  ::    shal, shaz, shan, og                            ::
  ::
++  shad  |=(ruz/@ (shax (shax ruz)))                   ::  double sha-256
++  shaf                                                ::  half sha-256
  |=  {sal/@ ruz/@}
  =+  haz=(shas sal ruz)
  (mix (end 7 1 haz) (rsh 7 1 haz))
::
++  sham                                                ::  128bit noun hash
  |=  yux/*  ^-  @uvH  ^-  @
  ?@  yux
    (shaf %mash yux)
  (shaf %sham (jam yux))
::
++  shas                                                ::  salted hash
  |=  {sal/@ ruz/@}
  (shax (mix sal (shax ruz)))
::
++  shax                                                ::  sha-256
  ~/  %shax
  |=  ruz/@  ^-  @
  (shay [(met 3 ruz) ruz])
::
++  shay                                                ::  sha-256 with length
  ~/  %shay
  |=  {len/@u ruz/@}  ^-  @
  =>  .(ruz (cut 3 [0 len] ruz))
  =+  [few==>(fe .(a 5)) wac=|=({a/@ b/@} (cut 5 [a 1] b))]
  =+  [sum=sum.few ror=ror.few net=net.few inv=inv.few]
  =+  ral=(lsh 0 3 len)
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
++  shaw                                                ::  hash to nbits
  |=  {sal/@ len/@ ruz/@}
  (~(raw og (shas sal (mix len ruz))) len)
::
++  shaz                                                ::  sha-512
  |=  ruz/@  ^-  @
  (shal [(met 3 ruz) ruz])
::
++  shal                                                ::  sha-512 with length
  ~/  %shal
  |=  {len/@ ruz/@}  ^-  @
  =>  .(ruz (cut 3 [0 len] ruz))
  =+  [few==>(fe .(a 6)) wac=|=({a/@ b/@} (cut 6 [a 1] b))]
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
++  shan                                                ::  sha-1 (deprecated)
  |=  ruz/@
  =+  [few==>(fe .(a 5)) wac=|=({a/@ b/@} (cut 5 [a 1] b))]
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
::
++  og                                                  ::  shax-powered rng
  ~/  %og
  |_  a/@
  ++  rad                                               ::  random in range
    |=  b/@  ^-  @
    =+  c=(raw (met 0 b))
    ?:((lth c b) c $(a +(a)))
  ::
  ++  rads                                              ::  random continuation
    |=  b/@
    =+  r=(rad b)
    [r +>.$(a (shas %og-s r))]
  ::
  ++  raw                                               ::  random bits
    ~/  %raw
    |=  b/@  ^-  @
    %+  can
      0
    =+  c=(shas %og-a (mix b a))
    |-  ^-  (list {@ @})
    ?:  =(0 b)
      ~
    =+  d=(shas %og-b (mix b (mix a c)))
    ?:  (lth b 256)
      [[b (end 0 b d)] ~]
    [[256 d] $(c d, b (sub b 256))]
  ::
  ++  raws                                              ::  random bits
    |=  b/@                                             ::  continuation
    =+  r=(raw b)
    [r +>.$(a (shas %og-s r))]
  --
::                                                      ::
::::  3e: AES and friends                               ::
  ::                                                    ::
  ::    aesc, ga                                        ::
  ::
--  =>
|%
++  aesc                                                ::  AES-256
  ~%  %aesc  +  ~
  |%
  ++  en                                                ::  ECB enc
    ~/  %en
    |=  {a/@I b/@H}  ^-  @uxH
    =+  ahem
    (be & (ex a) b)
  ++  de                                                ::  ECB dec
    ~/  %de
    |=  {a/@I b/@H}  ^-  @uxH
    =+  ahem
    (be | (ix (ex a)) b)
  --
++  ahem                                                ::  AES helpers
::  XX should be in aesc, isn't for performance reasons
  =>
    =+  =+  [gr=(ga 8 0x11b 3) few==>(fe .(a 5))]
        =+  [pro=pro.gr dif=dif.gr pow=pow.gr ror=ror.few]
        [pro=pro dif=dif pow=pow ror=ror nnk=8 nnb=4 nnr=14]
    =>  |%
        ++  cipa                                        ::  AES params
          $_  ^?  |%
          ++  co  *{p/@ q/@ r/@ s/@}                    ::  col coefs
          ++  ix  |~(a/@ *@)                            ::  key index
          ++  ro  *{p/@ q/@ r/@ s/@}                    ::  row shifts
          ++  su  *@                                    ::  s-box
          --
        --
    |%
    ++  pen                                             ::  encrypt
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
    ++  pin                                             :: decrypt
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
    ++  pode                                            ::  explode to block
      |=  {a/bloq b/@ c/@}  ^-  (list @)
      =+  d=(rip a c)
      =+  m=(met a c)
      |-
      ?:  =(m b)
        d
      $(m +(m), d (weld d (limo [0 ~])))
    ++  sube                                            ::  s-box word
      |=  {a/@ b/@}  ^-  @
      (rep 3 (turn (pode 3 4 a) |=(c/@ (cut 3 [c 1] b))))
    --
  |%
  ++  be                                                ::  block cipher
    |=  {a/? b/@ c/@H}  ^-  @uxH
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
  ++  ex                                                ::  key expand
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
  ++  ix                                                ::  key expand, inv
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
++  ga                                                  ::  GF (bex p.a)
  |=  a/{p/@ q/@ r/@}                                   ::  dim poly gen
  =+  si=(bex p.a)
  =+  ma=(dec si)
  =>  |%
      ++  dif                                           ::  add and sub
        |=  {b/@ c/@}
        ?>  &((lth b si) (lth c si))
        (mix b c)
      ::
      ++  dub                                           ::  mul by x
        |=  b/@
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
    =+  d=(~(get by q) b)
    ?~  d  0
    =+  e=(~(get by q) c)
    ?~  e  0
    =+  f=(~(get by p) (mod (add u.d u.e) ma))
    (need f)
  --
::                                                      ::
::::  3f: scrambling                                    ::
  ::                                                    ::
  ::    un (XX), ga                                     ::
  ::
--  => 
|%
++  un                                                  ::  =(x (wred (wren x)))
  |%
  ++  wren                                              ::  conceal structure
    |=  pyn/@  ^-  @
    =+  len=(met 3 pyn)
    ?:  =(0 len)
      0
    =>  .(len (dec len))
    =+  mig=(zaft (xafo len (cut 3 [len 1] pyn)))
    %+  can  3
    %-  flop  ^-  (list {@ @})
    :-  [1 mig]
    |-  ^-  (list {@ @})
    ?:  =(0 len)
      ~
    =>  .(len (dec len))
    =+  mog=(zyft :(mix mig (end 3 1 len) (cut 3 [len 1] pyn)))
    [[1 mog] $(mig mog)]
  ::
  ++  wred                                              ::  restore structure
    |=  cry/@  ^-  @
    =+  len=(met 3 cry)
    ?:  =(0 len)
      0
    =>  .(len (dec len))
    =+  mig=(cut 3 [len 1] cry)
    %+  can  3
    %-  flop  ^-  (list {@ @})
    :-  [1 (xaro len (zart mig))]
    |-  ^-  (list {@ @})
    ?:  =(0 len)
      ~
    =>  .(len (dec len))
    =+  mog=(cut 3 [len 1] cry)
    [[1 :(mix mig (end 3 1 len) (zyrt mog))] $(mig mog)]
  ::
  ++  xafo  |=({a/@ b/@} +((mod (add (dec b) a) 255)))
  ++  xaro  |=({a/@ b/@} +((mod (add (dec b) (sub 255 (mod a 255))) 255)))
  ::
  ++  zaft                                              ::  forward 255-sbox
    |=  a/@D
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
  ++  zart                                              ::  reverse 255-sbox
    |=  a/@D
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
  ++  zyft                                              ::  forward 256-sbox
    |=  a/@D
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
  ++  zyrt                                              ::  reverse 256-sbox
    |=  a/@D
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
  --
::
++  ob
  |%
  ++  feen                                              ::  conceal structure v2
    |=  pyn/@  ^-  @
    ?:  &((gte pyn 0x1.0000) (lte pyn 0xffff.ffff))
      (add 0x1.0000 (fice (sub pyn 0x1.0000)))
    ?:  &((gte pyn 0x1.0000.0000) (lte pyn 0xffff.ffff.ffff.ffff))
      =+  lo=(dis pyn 0xffff.ffff)
      =+  hi=(dis pyn 0xffff.ffff.0000.0000)
      %+  con  hi
      (add 0x1.0000 (fice (sub lo 0x1.0000)))
    pyn
  ::
  ++  fend                                              ::  restore structure v2
    |=  cry/@  ^-  @
    ?:  &((gte cry 0x1.0000) (lte cry 0xffff.ffff))
      (add 0x1.0000 (teil (sub cry 0x1.0000)))
    ?:  &((gte cry 0x1.0000.0000) (lte cry 0xffff.ffff.ffff.ffff))
      =+  lo=(dis cry 0xffff.ffff)
      =+  hi=(dis cry 0xffff.ffff.0000.0000)
      %+  con  hi
      (add 0x1.0000 (teil (sub lo 0x1.0000)))
    cry
  ::
  ++  fice                                              ::  adapted from
    |=  nor/@                                           ::  black and rogaway
    ^-  @                                               ::  "ciphers with
    =+  ^=  sel                                         ::   arbitrary finite
    %+  rynd  2                                         ::   domains", 2002
    %+  rynd  1
    %+  rynd  0
    [(mod nor 65.535) (div nor 65.535)]
    (add (mul 65.535 -.sel) +.sel)
  ::
  ++  teil                                              ::  reverse ++fice
    |=  vip/@
    ^-  @
    =+  ^=  sel
    %+  rund  0
    %+  rund  1
    %+  rund  2
    [(mod vip 65.535) (div vip 65.535)]
    (add (mul 65.535 -.sel) +.sel)
  ::
  ++  rynd                                              ::  feistel round
    |=  {n/@ l/@ r/@}
    ^-  {@ @}
    :-  r
    ?~  (mod n 2)
      (~(sum fo 65.535) l (en:aesc (snag n raku) r))
    (~(sum fo 65.536) l (en:aesc (snag n raku) r))
  ::
  ++  rund                                              ::  reverse round
    |=  {n/@ l/@ r/@}
    ^-  {@ @}
    :-  r
    ?~  (mod n 2)
      (~(dif fo 65.535) l (en:aesc (snag n raku) r))
    (~(dif fo 65.536) l (en:aesc (snag n raku) r))
  ::
  ++  raku
    ^-  (list @ux)
    :~  0x15f6.25e3.083a.eb3e.7a55.d4db.fb99.32a3.
          43af.2750.219e.8a24.e5f8.fac3.6c36.f968
        0xf2ff.24fe.54d0.1abd.4b2a.d8aa.4402.8e88.
          e82f.19ec.948d.b1bb.ed2e.f791.83a3.8133
        0xa3d8.6a7b.400e.9e91.187d.91a7.6942.f34a.
          6f5f.ab8e.88b9.c089.b2dc.95a6.aed5.e3a4
    ==
  --
--
.
