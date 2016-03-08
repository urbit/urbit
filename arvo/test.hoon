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
  ::      1c: ideal containers                          ::
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
++  sqt                                                 ::  unsigned rem/sqrt
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
++  char  @tD
--  =>
::                                                      ::
::::  3: layer three                                    ::
  ::                                                    ::
|%
::
::::  3a: signed and modular ints                       ::
  ::                                                    ::
  ::    fe, si                                          ::
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
--
.
