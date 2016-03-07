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
--
::                                                      ::
::::  2: layer two                                      ::
  ::                                                    ::
  ::    2a: unit logic                                  ::
  ::    2b: list logic                                  ::
  ::    2c: bit arithmetic                              ::
  ::    2d: bit logic                                   ::
  ::    2e: noun ordering                               ::
  ::    2f: insecure hashing                            ::
  ::    2g: unsigned powers                             ::
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
  (con (lsh a c (end a 1 i.b)) $(c +(c), b t.b))
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
::::  2e: noun ordering                                 ::
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
::::  2f: insecure hashing                              ::
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
  |_  a/(map * (list))
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
  |_  a/(map * (set))
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
    |-  ^+  ?>(?=(^ a) [p=n.a q=*(qeu _n.a)])
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
::::  2o: normalizing container molds                   ::
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
--
