::                                                      ::
::::    /sys/hoon                                       ::
  ::                                                    ::
=<  ride
=>  %143  =>
::                                                      ::
::::    0: version stub                                 ::
  ::                                                    ::
~%  %k.143  ~  ~                                        ::
|%
++  hoon-version  +
--  =>
~%  %one  +  ~
:>  #  %base
:>
:>    basic mathematical operations
|%
:>  #  %math
:>    unsigned arithmetic
+|
++  add
  ~/  %add
  :>  unsigned addition
  :>
  :>  a: augend
  :>  b: addend
  |=  [a=@ b=@]
  :>  sum
  ^-  @
  ?:  =(0 a)  b
  $(a (dec a), b +(b))
::
++  dec
  ~/  %dec
  :>  unsigned decrement by one.
  |=  a=@
  ~_  leaf+"decrement-underflow"
  ?<  =(0 a)
  =+  b=0
  :>  decremented integer
  |-  ^-  @
  ?:  =(a +(b))  b
  $(b +(b))
::
++  div
  ~/  %div
  :>  unsigned divide
  :>
  :>  a: dividend
  :>  b: divisor
  |:  [a=`@`1 b=`@`1]
  :>  quotient
  ^-  @
  ~_  leaf+"divide-by-zero"
  ?<  =(0 b)
  =+  c=0
  |-
  ?:  (lth a b)  c
  $(a (sub a b), c +(c))
::
++  dvr
  ~/  %dvr
  :>  unsigned divide with remainder
  :>
  :>  a: dividend
  :>  b: divisor
  |=  [a=@ b=@]
  :>  p: quotient
  :>  q: remainder
  ^-  [p=@ q=@]
  [(div a b) (mod a b)]
::
++  gte
  ~/  %gte
  :>    unsigned greater than or equals
  :>
  :>  returns whether {a >= b}.
  :>
  :>  a: left hand operand (todo: name)
  :>  b: right hand operand
  |=  [a=@ b=@]
  :>  greater than or equal to?
  ^-  ?
  !(lth a b)
::
++  gth
  ~/  %gth
  :>    unsigned greater than
  :>
  :>  returns whether {a > b}
  :>
  :>  a: left hand operand (todo: name)
  :>  b: right hand operand
  |=  [a=@ b=@]
  :>  greater than?
  ^-  ?
  !(lte a b)
::
++  lte
  ~/  %lte
  :>    unsigned less than or equals
  :>
  :>  returns whether {a >= b}.
  :>
  :>  a: left hand operand (todo: name)
  :>  b: right hand operand
  |=  [a=@ b=@]
  :>  less than or equal to?
  |(=(a b) (lth a b))
::
++  lth
  ~/  %lth
  :>    unsigned less than
  :>
  :>  a: left hand operand (todo: name)
  :>  b: right hand operand
  |=  [a=@ b=@]
  :>  less than?
  ^-  ?
  ?&  !=(a b)
      |-
      ?|  =(0 a)
          ?&  !=(0 b)
              $(a (dec a), b (dec b))
  ==  ==  ==
::
++  max
  ~/  %max
  :>  unsigned maximum
  |=  [a=@ b=@]
  :>  the maximum
  ^-  @
  ?:  (gth a b)  a
  b
::
++  min
  ~/  %min
  :>  unsigned minimum
  |=  [a=@ b=@]
  :>  the minimum
  ^-  @
  ?:  (lth a b)  a
  b
::
++  mod
  ~/  %mod
  :>  unsigned modulus
  :>
  :>  a: dividend
  :>  b: divisor
  |:  [a=`@`1 b=`@`1]
  :>  the remainder
  ^-  @
  ?<  =(0 b)
  (sub a (mul b (div a b)))
::
++  mul
  ~/  %mul
  :>  unsigned multiplication
  :>
  :>  a: multiplicand
  :>  b: multiplier
  |:  [a=`@`1 b=`@`1]
  :>  product
  ^-  @
  =+  c=0
  |-
  ?:  =(0 a)  c
  $(a (dec a), c (add b c))
::
++  sub
  ~/  %sub
  :>  unsigned subtraction
  :>
  :>  a: minuend
  :>  b: subtrahend
  |=  [a=@ b=@]
  ~_  leaf+"subtract-underflow"
  :>  difference
  ^-  @
  ?:  =(0 b)  a
  $(a (dec a), b (dec b))
::
:>  #  %tree
:>
:>    tree addressing
+|
++  cap
  ~/  %cap
  :>    tree head
  :>
  :>  tests whether an `a` is in the head or tail of a noun. produces %2 if it
  :>  is within the head, or %3 if it is within the tail.
  |=  a=@
  ^-  ?($2 $3)
  ?-  a
    $2        %2
    $3        %3
    ?($0 $1)  !!
    *         $(a (div a 2))
  ==
::
++  mas
  ~/  %mas
  :>    axis within head/tail
  :>
  :>  computes the axis of `a` within either the head or tail of a noun
  :>  (depends whether `a` lies within the the head or tail).
  |=  a=@
  ^-  @
  ?-  a
    $1   !!
    $2   1
    $3   1
    *    (add (mod a 2) (mul $(a (div a 2)) 2))
  ==
::
++  peg
  ~/  %peg
  :>    axis within axis
  :>
  :>  computes the axis of {b} within axis {a}.
  |=  [a=@ b=@]
  ?<  =(0 a)
  :>  a composed axis
  ^-  @
  ?-  b
    $1  a
    $2  (mul a 2)
    $3  +((mul a 2))
    *   (add (mod b 2) (mul $(b (div b 2)) 2))
  ==
::
:>  #  %containers
:>
:>    the most basic of data types
+|
++  bloq
  :>    blocksize
  :>
  :>  a blocksize is the power of 2 size of an atom. ie, 3 is a byte as 2^3 is
  :>  8 bits.
  @
::
++  each
  :>    either {a} or {b}, defaulting to {a}.
  :>
  :>  mold generator: produces a discriminated fork between two types,
  :>  defaulting to {a}.
  |*({a/mold b/mold} $%({$| p/b} {$& p/a}))
::
++  gate
  :>    function
  :>
  :>  a core with one arm, `$`--the empty name--which transforms a sample noun
  :>  into a product noun. If used dryly as a type, the subject must have a
  :>  sample type of `*`.
  $-(* *)
::
++  list
  :>    null-terminated list
  :>
  :>  mold generator: produces a mold of a null-terminated list of the
  :>  homogeneous type {a}.
  |*(a/mold $@($~ {i/a t/(list a)}))
::
++  lone
  :>    single item tuple
  :>
  :>  mold generator: puts the face of `p` on the passed in mold.
  |*(a/mold {p/a})
::
++  mold
  :>    normalizing gate
  :>
  :>  a gate that accepts any noun, and validates its shape, producing the
  :>  input if it fits or a default value if it doesn't.
  :>
  :>  examples: * @ud ,[p=time q=?(%a %b)]
  _|~(* +<)
::
++  pair
  :>    dual tuple
  :>
  :>  mold generator: produces a tuple of the two types passed in.
  :>
  :>  a: first type, labeled {p}
  :>  b: second type, labeled {q}
  |*({a/mold b/mold} {p/a q/b})
::
++  pole
  :>    faceless list
  :>
  :>  like ++list, but without the faces {i} and {t}.
  :>
  :>  a: a mold for the item type.
  |*(a/mold $@($~ {a (pole a)}))
::
++  qual
  :>    quadruple tuple
  :>
  :>  mold generator: produces a tuple of the four types passed in.
  :>
  :>  a: first type, labeled {p}
  :>  b: second type, labeled {q}
  :>  c: third type, labeled {r}
  :>  d: fourth type, labeled {s}
  |*  {a/mold b/mold c/mold d/mold}
  {p/a q/b r/c s/d}
::
++  quip
  :>    pair of list of first and second
  :>
  :>  a common pattern in hoon code is to return a ++list of changes, along with
  :>  a new state.
  :>
  :>  a: type of list item
  :>  b: type of returned state
  |*({a/mold b/mold} {(list a) b})
::
++  trap
  :>    a core with one arm `$`
  :>
  :>  a: return type of the `$` arm.
  |*(a/mold _|?(*a))
::
++  tree
  :>    tree mold generator
  :>
  :>  a `++tree` can be empty, or contain a node of a type and
  :>  left/right sub `++tree` of the same type. pretty-printed with `{}`.
  :>
  :>  a: type of tree node
  |*(a/mold $@($~ {n/a l/(tree a) r/(tree a)})) ::  binary tree
::
++  trel
  :>    triple tuple
  :>
  :>  mold generator: produces a tuple of the three types passed in.
  :>
  :>  a: first type, labeled {p}
  :>  b: second type, labeled {q}
  :>  c: third type, labeled {r}
  |*({a/mold b/mold c/mold} {p/a q/b r/c})
::
++  unit
  :>    maybe
  :>
  :>  mold generator: either `~` or `[~ u=a]` where `a` is the
  :>  type that was passed in.
  :>
  :>  a: type when non-null
  |*(a/mold $@($~ {$~ u/a}))
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
  ::    2q: molds and mold builders                     ::
  ::
~%  %two  +  ~
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
++  hunt                                                ::  first of units
  |*  {ord/$-({* *} ?) one/(unit) two/(unit)}
  ^-  (unit ?(_,.+.one _,.+.two))
  ?~  one  two
  ?~  two  one
  ?:((ord ,.+.one ,.+.two) one two)
::
++  lift                                                ::  lift mold (fmap)
  |*  a/mold                                            ::  flipped
  |*  b/(unit)                                          ::  curried
  (bind b a)                                            ::  bind
::
++  mate                                                ::  choose
  |*  {a/(unit) b/(unit)}
  ?~  b  a
  ?~  a  b
  ?.(=(u.a u.b) ~>(%mean.[%leaf "mate"] !!) a)
::
++  need                                                ::  demand
  |*  a/(unit)
  ?~  a  ~>(%mean.[%leaf "need"] !!)
  u.a
::
++  some                                                ::  lift (pure)
  |*  a/*
  [~ u=a]
::
::::  2b: list logic                                    ::
  ::                                                    ::
  ::                                                    ::
::
++  fand                                                ::  all indices
  ~/  %fand
  |=  {nedl/(list) hstk/(list)}
  =|  i/@ud
  =|  fnd/(list @ud)
  |-  ^+  fnd
  =+  [n=nedl h=hstk]
  |-
  ?:  |(?=($~ n) ?=($~ h))
    (flop fnd)
  ?:  =(i.n i.h)
    ?~  t.n
      ^$(i +(i), hstk +.hstk, fnd [i fnd])
    $(n t.n, h t.h)
  ^$(i +(i), hstk +.hstk)
::
++  find                                                ::  first index
  ~/  %find
  |=  {nedl/(list) hstk/(list)}
  =|  i/@ud
  |-   ^-  (unit @ud)
  =+  [n=nedl h=hstk]
  |-
  ?:  |(?=($~ n) ?=($~ h))
     ~
  ?:  =(i.n i.h)
    ?~  t.n
      `i
    $(n t.n, h t.h)
  ^$(i +(i), hstk +.hstk)
::
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
++  oust                                                ::  remove
  ~/  %oust
  |*  {{a/@ b/@} c/(list)}
  (weld (scag +<-< c) (slag (add +<-< +<->) c))
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
  |-  ^+  ,.+<+.b
  ?~  a
    +<+.b
  (b i.a $(a t.a))
::
++  roll                                                ::  left fold
  ~/  %roll
  |*  {a/(list) b/_|=({* *} +<+)}
  |-  ^+  ,.+<+.b
  ?~  a
    +<+.b
  $(a t.a, b b(+<+ (b i.a +<+.b)))
::
++  scag                                                ::  prefix
  ~/  %scag
  |*  {a/@ b/(list)}
  |-  ^+  b
  ?:  |(?=($~ b) =(0 a))  ~
  [i.b $(b t.b, a (dec a))]
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
  |-  ^+  ?>(?=(^ b) i.b)
  ?~  b
    ~_  leaf+"snag-fail"
    !!
  ?:  =(0 a)  i.b
  $(b t.b, a (dec a))
::
++  sort   !.                                           ::  quicksort
  ~/  %sort
  |*  {a/(list) b/$-({* *} ?)}
  =>  .(a ^.(homo a))
  |-  ^+  a
  ?~  a  ~
  %+  weld
    $(a (skim t.a |=(c/_i.a (b c i.a))))
  ^+  t.a
  [i.a $(a (skim t.a |=(c/_i.a !(b c i.a))))]
::
++  spin
  |*  {a/(list) b/_|=({* *} [** +<+]) c/*}
  ::  ?<  ?=($-([_?<(?=($~ a) i.a) _c] [* _c]) b)
  |-
  ?~  a
    ~
  =+  v=(b i.a c)
  [i=-.v t=$(a t.a, c +.v)]
::
++  spun
  |*  {a/(list) b/_|=({* *} [** +<+])}
  =|  c/_+<+.b
  |-
  ?~  a
    ~
  =+  v=(b i.a c)
  [i=-.v t=$(a t.a, c +.v)]
::
++  swag                                                ::  slice
  |*  {{a/@ b/@} c/(list)}
  (scag +<-> (slag +<-< c))
::
++  turn                                                ::  transform
  ~/  %turn
  |*  {a/(list) b/gate}
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
  =+  ~  ::REMOVEME jet dashboard bump
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
::
++  fe                                                  ::  modulo bloq
  |_  a/bloq
  ++  dif                                               ::  difference
    |=({b/@ c/@} (sit (sub (add out (sit b)) (sit c))))
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
::                                                      ::
::::  2d: bit logic                                     ::
  ::                                                    ::
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
  ::
++  fnv  |=(a/@ (end 5 1 (mul 16.777.619 a)))           ::  FNV scrambler
::
++  muk                                                 ::  standard murmur3
  ~%  %muk  ..muk  ~
  =+  ~(. fe 5)
  |=  {syd/@ len/@ key/@}
  ?>  &((lte (met 5 syd) 1) (lte (met 0 len) 31))
  =/  pad      (sub len (met 3 key))
  =/  data     (weld (rip 3 key) (reap pad 0))
  =/  nblocks  (div len 4)  ::  intentionally off-by-one
  =/  h1  syd
  =+  [c1=0xcc9e.2d51 c2=0x1b87.3593]
  =/  blocks  (rip 5 key)
  =/  i  nblocks
  =.  h1  =/  hi  h1  |-
    ?:  =(0 i)  hi
    =/  k1  (snag (sub nblocks i) blocks)  ::  negative array index
    =.  k1  (sit (mul k1 c1))
    =.  k1  (rol 0 15 k1)
    =.  k1  (sit (mul k1 c2))
    =.  hi  (mix hi k1)
    =.  hi  (rol 0 13 hi)
    =.  hi  (sum (sit (mul hi 5)) 0xe654.6b64)
    $(i (dec i))
  =/  tail  (slag (mul 4 nblocks) data)
  =/  k1    0
  =/  tlen  (dis len 3)
  =.  h1
    ?+  tlen  h1  ::  fallthrough switch
      $3  =.  k1  (mix k1 (lsh 0 16 (snag 2 tail)))
          =.  k1  (mix k1 (lsh 0 8 (snag 1 tail)))
          =.  k1  (mix k1 (snag 0 tail))
          =.  k1  (sit (mul k1 c1))
          =.  k1  (rol 0 15 k1)
          =.  k1  (sit (mul k1 c2))
          (mix h1 k1)
      $2  =.  k1  (mix k1 (lsh 0 8 (snag 1 tail)))
          =.  k1  (mix k1 (snag 0 tail))
          =.  k1  (sit (mul k1 c1))
          =.  k1  (rol 0 15 k1)
          =.  k1  (sit (mul k1 c2))
          (mix h1 k1)
      $1  =.  k1  (mix k1 (snag 0 tail))
          =.  k1  (sit (mul k1 c1))
          =.  k1  (rol 0 15 k1)
          =.  k1  (sit (mul k1 c2))
          (mix h1 k1)
    ==
  =.  h1  (mix h1 len)
  |^  (fmix32 h1)
  ++  fmix32
    |=  h/@
    =.  h  (mix h (rsh 0 16 h))
    =.  h  (sit (mul h 0x85eb.ca6b))
    =.  h  (mix h (rsh 0 13 h))
    =.  h  (sit (mul h 0xc2b2.ae35))
    =.  h  (mix h (rsh 0 16 h))
    h
  --
  ::
  ++  mum                                                 ::  mug with murmur3
  ~/  %mum
  |=  a/*
  |^  (trim ?@(a a (mix $(a -.a) (mix 0x7fff.ffff $(a +.a)))))
  ++  trim                                              ::  31-bit nonzero
    |=  key/@
    =+  syd=0xcafe.babe
    |-  ^-  @
    =+  haz=(muk syd (met 3 key) key)
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
    ?.  =(0 d)  d
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
  ::
++  in                                                  ::  set engine
  ~/  %in
  |_  a/(tree)  :: (set)
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
    =|  {l/(unit) r/(unit)}
    |-  ^-  ?
    ?~  a   &
    ?&  ?~(l & (hor n.a u.l))
        ?~(r & (hor u.r n.a))
        ?~(l.a & ?&((vor n.a n.l.a) $(a l.a, l `n.a)))
        ?~(r.a & ?&((vor n.a n.r.a) $(a r.a, r `n.a)))
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
    ~/  %run
    |*  b/gate
    =|  c/(set _?>(?=(^ a) (b n.a)))
    |-  ?~  a  c
    =.  c  (~(put in c) (b n.a))
    =.  c  $(a l.a, c c)
    $(a r.a, c c)
  ::
  +-  tap                                               ::  convert to list
    =<  $
    ~/  %tap
    =|  b/(list _?>(?=(^ a) n.a))
    |.  ^+  b
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
    =<  $
    ~%  %wyt  +  ~
    |.  ^-  @
    ?~(a 0 +((add $(a l.a) $(a r.a))))
  --
::                                                      ::
::::  2i: map logic                                     ::
  ::                                                    ::
  ::
++  by                                                  ::  map engine
  ~/  %by
  =|  a/(tree (pair))  ::  (map)
  =*  node  ?>(?=(^ a) n.a)
  |%
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
  +-  def                                               ::  difference
    |*  b/_a
    ^-  (map _p:node (pair (unit _q:node) (unit _q:node)))
    !!
  ::
  +-  dep                                               ::  difference as patch
    |*  b/_a
    ^+  [p=a q=a]
    =+  c=~(tap by (def b))
    =+  [d e]=[`_a`~ `_a`~]
    |-  ^+  [d e]
    ?~  c  [d e]
    %=  $
      c  t.c
      d  ?~(q.q.i.c d (~(put by d) p.i.c u.q.q.i.c))
      e  ?~(p.q.i.c e (~(put by e) p.i.c u.p.q.i.c))
    ==
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
    =+  c=(bif p.n.b q.n.b)
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
  +-  apt                                               ::  check correctness
    =|  {l/(unit) r/(unit)}
    |-  ^-  ?
    ?~  a   &
    ?&  ?~(l & (gor p.n.a u.l))
        ?~(r & (gor u.r p.n.a))
        ?~(l.a & ?&((vor p.n.a p.n.l.a) $(a l.a, l `p.n.a)))
        ?~(r.a & ?&((vor p.n.a p.n.r.a) $(a r.a, r `p.n.a)))
    ==
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
    =+  42
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
    |*  {b/* c/gate}
    |-  ^+  [b a]
    ?~  a  [b ~]
    =+  d=(c n.a b)
    =.  n.a  +.d
    =+  e=$(a l.a, b -.d)
    =+  f=$(a r.a, b -.e)
    [-.f [n.a +.e +.f]]
  ::
  +-  run                                               ::  apply gate to values
    |*  b/gate
    |-
    ?~  a  a
    [n=[p=p.n.a q=(b q.n.a)] l=$(a l.a) r=$(a r.a)]
  ::
  +-  rut                                               ::  apply gate to nodes
    |*  b/gate
    |-
    ?~  a  a
    [n=[p=p.n.a q=(b p.n.a q.n.a)] l=$(a l.a) r=$(a r.a)]
  ::
  +-  tap                                               ::  listify pairs
    =<  $
    ~/  %tap
    =|  b/(list _?>(?=(^ a) n.a))
    |.  ^+  b
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
  +-  uno                                               ::  general union
    |=  b/_a
    |=  meg/$-({_p:node _q:node _q:node} _q:node)
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
      :+  [p.n.a (meg p.n.a q.n.a q.n.b)]
        $(b l.b, a l.a)
      $(b r.b, a r.a)
    ?:  (gor p.n.a p.n.b)
      $(b [n.b $(b l.b, a [n.a l.a ~]) r.b], a r.a)
    $(b [n.b l.b $(b r.b, a [n.a ~ r.a])], a l.a)
  ::
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
  ::
  +-  key                                               ::  set of keys
    =|  b/(set _?>(?=(^ a) p.n.a))
    |-  ^+  b
    ?~  a   b
    $(a r.a, b $(a l.a, b (~(put in b) p.n.a)))
  ::
  +-  val                                               ::  list of vals
    =|  b/(list _?>(?=(^ a) q.n.a))
    |-  ^+  b
    ?~  a   b
    $(a r.a, b [q.n.a $(a l.a)])
  --
::                                                      ::
::::  2j: jar and jug logic                             ::
  ::                                                    ::
  ::
++  ja                                                  ::  jar engine
  |_  a/(tree (pair * (list)))  ::  (jar)
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
  |_  a/(tree (pair * (tree)))  ::  (jug)
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
  ::
++  to                                                  ::  queue engine
  |_  a/(tree)  ::  (qeu)
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
  +-  nip                                               ::  remove root
    |-  ^+  a
    ?~  a  ~
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (vor n.l.a n.r.a)
      [n.l.a l.l.a $(l.a r.l.a)]
    [n.r.a $(r.a l.r.a) r.r.a]
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
    =|  b/(list _?>(?=(^ a) n.a))
    |-  ^+  b
    =+  0                                               ::  hack for jet match
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
  ::
++  malt                                                ::  map from list
  |*  a/(list)
  (molt `(list {p/_-<.a q/_->.a})`a)
::
++  molt                                                ::  map from pair list
  |*  a/(list (pair))  ::  ^-  =,(i.-.a (map _p _q))
  (~(gas by `(tree {p/_p.i.-.a q/_q.i.-.a})`~) a)
::
++  silt                                                ::  set from list
  |*  a/(list)  ::  ^-  (set _i.-.a)
  =+  b=*(tree _?>(?=(^ a) i.a))
  (~(gas in b) a)
::                                                      ::
::::  2m: container from noun                           ::
  ::                                                    ::
  ::
++  ly                                                  ::  list from raw noun
  le:nl
::
++  my                                                  ::  map from raw noun
  my:nl
::
++  sy                                                  ::  set from raw noun
  si:nl
::
++  nl
  |%
  ::                                                      ::
  ++  le                                                  ::  construct list
    |*  a/(list)
    ^+  =<  $
      |%  +-  $  ?:(*? ~ [i=(snag 0 a) t=$])
      --
    a
  ::                                                      ::
  ++  my                                                  ::  construct map
    |*  a/(list (pair))
    =>  .(a ^+((le a) a))
    (~(gas by `(map _p.i.-.a _q.i.-.a)`~) a)
  ::                                                      ::
  ++  mz                                                  ::  construct map
    |*  a/(list (pair))
    =>  .(a ^+((le a) a))
    (~(gas by ~) a)
  ::                                                      ::
  ++  si                                                  ::  construct set
    |*  a/(list)
    =>  .(a ^+((le a) a))
    (~(gas in `(set _i.-.a)`~) a)
  ::                                                      ::
  ++  snag                                                ::  index
    |*  {a/@ b/(list)}
    ?~  b
      ~_  leaf+"snag-fail"
      !!
    ?:  =(0 a)  i.b
    $(b t.b, a (dec a))
  ::                                                      ::
  ++  weld                                                ::  concatenate
    |*  {a/(list) b/(list)}
    =>  .(a ^+((le a) a), b ^+((le b) b))
    =+  42
    |-
    ?~  a  b
    [i=i.a t=$(a t.a)]
  --
::                                                      ::
::::  2n: functional hacks                              ::
  ::                                                    ::
  ::
++  aftr  |*(a/gate |*(b/gate (pair b a)))              ::  pair after
++  cork  |*({a/_|=(* **) b/gate} (corl b a))           ::  compose forward
++  corl                                                ::  compose backwards
  |*  {a/gate b/_|=(* **)}
  =<  +:|.((a (b)))      ::  type check
  |*  c/_+<.b
  (a (b c))
::
++  cury                                                ::  curry left
  |*  {a/_|=(^ **) b/*}
  |*  c/_+<+.a
  (a b c)
::
++  curr                                                ::  curry right
  |*  {a/_|=(^ **) c/*}
  |*  b/_+<+.a
  (a b c)
::
++  fore  |*(a/gate |*(b/gate (pair a b)))              ::  pair before
++  hard                                                ::  force remold
  |*  han/gate
  |=  fud/*  ^-  han
  ~_  leaf+"hard"
  =+  gol=(han fud)
  ?>(=(gol fud) gol)
::
::
++  head  |*(^ ,:+<-)                                   ::  get head
++  same  |*(* +<)                                      ::  identity
++  soft                                                ::  maybe remold
  |*  han/gate
  |=  fud/*  ^-  (unit han)
  =+  gol=(han fud)
  ?.(=(gol fud) ~ [~ gol])
::
++  slog                                                ::  deify printf
  =|  pri/@                                             ::  priority level
  |=  a/tang  ^+  same                                  ::  .=  ~&(%a 1)
  ?~(a same ~>(%slog.[pri i.a] $(a t.a)))               ::  ((slog ~[>%a<]) 1)
::                                                      ::  
++  mean                                                ::  crash with trace
  |=  a/tang
  ^+  !!
  ?~  a  !!
  ~_(i.a $(a t.a))
::
++  tail  |*(^ ,:+<+)                                   ::  get tail
++  test  |=(^ =(+<- +<+))                              ::  equality
::
::                                                      ::
::::  2o: normalizing containers                        ::
  ::                                                    ::
  ::
++  jar  |*({a/mold b/mold} (map a (list b)))           ::  map of lists
++  jug  |*({a/mold b/mold} (map a (set b)))            ::  map of sets
++  map  |*  {a/mold b/mold}                            ::  table
         %+  cork  (tree (pair a b))                    ::
         |=  c/(tree (pair a b))  ^+  c                 ::
         ?.(~(apt by c) ~ c)                            ::
++  qeu  |*(a/mold (tree a))                            ::  queue
++  set  |*  a/mold                                     ::  set
         %+  cork  (tree a)                             ::
         |=  b/(tree a)  ^+  b                          ::
         ?.(~(apt in b) ~ b)                            ::
::
::::  2p: serialization                                 ::
  ::                                                    ::
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
++  tape  (list @tD)                                    ::  UTF8 string as list
++  tarp  {d/@ud h/@ud m/@ud s/@ud f/(list @ux)}        ::  parsed time
++  term  @tas                                          ::  ascii symbol
++  wain  (list cord)                                   ::  text lines
++  wall  (list tape)                                   ::  text lines
--  =>
::                                                      ::
::::  3: layer three                                    ::
  ::                                                    ::
  ::    3a: signed and modular ints                     ::
  ::    3b: floating point                              ::
  ::    3c: urbit time                                  ::
  ::    3d: SHA hash family                             ::
  ::    3e: (reserved)                                  ::
  ::    3f: scrambling                                  ::
  ::    3g: molds and mold builders                     ::
  ::                                                    ::
~%  %tri  +  ~
|%
::
::::  3a: signed and modular ints                       ::
  ::                                                    ::
  ::
++  egcd                                                ::  schneier's egcd
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
++  fo                                                  ::  modulo prime
  ^?
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
++  si                                                  ::  signed integer
  ^?
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
  ::
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
    ::  positive numbers. many of these error out if a=0.
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
      =+  ^=  x  %+  sum:si  e.b  (sun:si +(mb))
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
      =+  ^=  q  %+  max
          ?:  (gth m prc)  (^sub m prc)  0              ::  reduce precision
        %-  abs:si  ?:  =(den %i)  --0                  ::  enforce min. exp
        ?:  =((cmp:si e.a emn) -1)  (dif:si emn e.a)  --0
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
          $ne  ?:  s  [%f & ?:((lte b (bex (dec q))) zer spd)]
               [%f & ?:((^lth b (bex (dec q))) zer spd)]
          $nt  ?:  s  [%f & ?:((lte b (bex (dec q))) zer spd)]
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
               ?.  (lte (met 0 y) prc)  a(a (dec a.a))
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
    ++  drg                                             ::  dragon4; get
      ~/  %drg                                          ::  printable decimal;
      |=  {a/{e/@s a/@u}}  ^-  {@s @u}                  ::  guaranteed accurate
      ?<  =(a.a 0)                                      ::  for rounded floats
      =.  a  (xpd a)
      =+  r=(lsh 0 ?:((syn:si e.a) (abs:si e.a) 0) a.a)
      =+  s=(lsh 0 ?.((syn:si e.a) (abs:si e.a) 0) 1)
      =+  mn=(lsh 0 ?:((syn:si e.a) (abs:si e.a) 0) 1)
      =+  mp=mn
      =>  ?.
            ?&  =(a.a (bex (dec prc)))                  ::  if next smallest
                |(!=(e.a emn) =(den %i))                ::  float is half ULP,
            ==                                          ::  tighten lower bound
          .
        %=  .
          mp  (lsh 0 1 mp)
          r  (lsh 0 1 r)
          s  (lsh 0 1 s)
        ==
      =+  [k=--0 q=(^div (^add s 9) 10)]
      |-  ?:  (^lth r q)
        %=  $
          k  (dif:si k --1)
          r  (^mul r 10)
          mn  (^mul mn 10)
          mp  (^mul mp 10)
        ==
      |-  ?:  (gte (^add (^mul r 2) mp) (^mul s 2))
        $(s (^mul s 10), k (sum:si k --1))
      =+  [u=0 o=0]
      |-                                                ::  r/s+o = a*10^-k
      =+  v=(dvr (^mul r 10) s)
      =>  %=  .
          k  (dif:si k --1)
          u  p.v
          r  q.v
          mn  (^mul mn 10)
          mp  (^mul mp 10)
        ==
      =+  l=(^lth (^mul r 2) mn)                        ::  in lower bound
      =+  ^=  h                                         ::  in upper bound
        ?|  (^lth (^mul s 2) mp)
            (gth (^mul r 2) (^sub (^mul s 2) mp))
        ==
      ?:  &(!l !h)
        $(o (^add (^mul o 10) u))
      =+  q=&(h |(!l (gth (^mul r 2) s)))
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
      ~_  leaf+"need-float"
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
    %+  bind  (lth b a)  |=  a/?  !a
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
  ^?
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
    ~_  leaf+"rd-fail"
    (add:ma a b)
  ::
  ++  sub  ~/  %sub                                     ::  subtract
    |=  {a/@rd b/@rd}  ^-  @rd
    ~_  leaf+"rd-fail"
    (sub:ma a b)
  ::
  ++  mul  ~/  %mul                                     ::  multiply
    |=  {a/@rd b/@rd}  ^-  @rd
    ~_  leaf+"rd-fail"
    (mul:ma a b)
  ::
  ++  div  ~/  %div                                     ::  divide
    |=  {a/@rd b/@rd}  ^-  @rd
    ~_  leaf+"rd-fail"
    (div:ma a b)
  ::
  ++  fma  ~/  %fma                                     ::  fused multiply-add
    |=  {a/@rd b/@rd c/@rd}  ^-  @rd
    ~_  leaf+"rd-fail"
    (fma:ma a b c)
  ::
  ++  sqt  ~/  %sqt                                     ::  square root
    |=  {a/@rd}  ^-  @rd  ~_  leaf+"rd-fail"
    (sqt:ma a)
  ::
  ++  lth  ~/  %lth                                     ::  less-than
    |=  {a/@rd b/@rd}
    ~_  leaf+"rd-fail"
    (lth:ma a b)
  ::
  ++  lte  ~/  %lte                                     ::  less-equals
    |=  {a/@rd b/@rd}
    ~_  leaf+"rd-fail"
    (lte:ma a b)
  ::
  ++  equ  ~/  %equ                                     ::  equals
    |=  {a/@rd b/@rd}
    ~_  leaf+"rd-fail"
    (equ:ma a b)
  ::
  ++  gte  ~/  %gte                                     ::  greater-equals
    |=  {a/@rd b/@rd}
    ~_  leaf+"rd-fail"
    (gte:ma a b)
  ::
  ++  gth  ~/  %gth                                     ::  greater-than
    |=  {a/@rd b/@rd}
    ~_  leaf+"rd-fail"
    (gth:ma a b)
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
  ^?
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
    ~_  leaf+"rs-fail"
    (add:ma a b)
  ::
  ++  sub  ~/  %sub                                     ::  subtract
    |=  {a/@rs b/@rs}  ^-  @rs
    ~_  leaf+"rs-fail"
    (sub:ma a b)
  ::
  ++  mul  ~/  %mul                                     ::  multiply
    |=  {a/@rs b/@rs}  ^-  @rs
    ~_  leaf+"rs-fail"
    (mul:ma a b)
  ::
  ++  div  ~/  %div                                     ::  divide
    |=  {a/@rs b/@rs}  ^-  @rs
    ~_  leaf+"rs-fail"
    (div:ma a b)
  ::
  ++  fma  ~/  %fma                                     ::  fused multiply-add
    |=  {a/@rs b/@rs c/@rs}  ^-  @rs
    ~_  leaf+"rs-fail"
    (fma:ma a b c)
  ::
  ++  sqt  ~/  %sqt                                     ::  square root
    |=  {a/@rs}  ^-  @rs
    ~_  leaf+"rs-fail"
    (sqt:ma a)
  ::
  ++  lth  ~/  %lth                                     ::  less-than
    |=  {a/@rs b/@rs}
    ~_  leaf+"rs-fail"
    (lth:ma a b)
  ::
  ++  lte  ~/  %lte                                     ::  less-equals
    |=  {a/@rs b/@rs}
    ~_  leaf+"rs-fail"
    (lte:ma a b)
  ::
  ++  equ  ~/  %equ                                     ::  equals
    |=  {a/@rs b/@rs}
    ~_  leaf+"rs-fail"
    (equ:ma a b)
  ::
  ++  gte  ~/  %gte                                     ::  greater-equals
    |=  {a/@rs b/@rs}
    ~_  leaf+"rs-fail"
    (gte:ma a b)
  ::
  ++  gth  ~/  %gth                                     ::  greater-than
    |=  {a/@rs b/@rs}
    ~_  leaf+"rs-fail"
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
  ^?
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
    ~_  leaf+"rq-fail"
    (add:ma a b)
  ::
  ++  sub  ~/  %sub                                     ::  subtract
    |=  {a/@rq b/@rq}  ^-  @rq
    ~_  leaf+"rq-fail"
    (sub:ma a b)
  ::
  ++  mul  ~/  %mul                                     ::  multiply
    |=  {a/@rq b/@rq}  ^-  @rq
    ~_  leaf+"rq-fail"
    (mul:ma a b)
  ::
  ++  div  ~/  %div                                     ::  divide
    |=  {a/@rq b/@rq}  ^-  @rq
    ~_  leaf+"rq-fail"
    (div:ma a b)
  ::
  ++  fma  ~/  %fma                                     ::  fused multiply-add
    |=  {a/@rq b/@rq c/@rq}  ^-  @rq
    ~_  leaf+"rq-fail"
    (fma:ma a b c)
  ::
  ++  sqt  ~/  %sqt                                     ::  square root
    |=  {a/@rq}  ^-  @rq
    ~_  leaf+"rq-fail"
    (sqt:ma a)
  ::
  ++  lth  ~/  %lth                                     ::  less-than
    |=  {a/@rq b/@rq}
    ~_  leaf+"rq-fail"
    (lth:ma a b)
  ::
  ++  lte  ~/  %lte                                     ::  less-equals
    |=  {a/@rq b/@rq}
    ~_  leaf+"rq-fail"
    (lte:ma a b)
  ::
  ++  equ  ~/  %equ                                     ::  equals
    |=  {a/@rq b/@rq}
    ~_  leaf+"rq-fail"
    (equ:ma a b)
  ::
  ++  gte  ~/  %gte                                     ::  greater-equals
    |=  {a/@rq b/@rq}
    ~_  leaf+"rq-fail"
    (gte:ma a b)
  ::
  ++  gth  ~/  %gth                                     ::  greater-than
    |=  {a/@rq b/@rq}
    ~_  leaf+"rq-fail"
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
  ~%  %rh  +>  ~
  ^?
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
  ++  add  ~/  %add                                     ::  add
    |=  {a/@rh b/@rh}  ^-  @rh
    ~_  leaf+"rh-fail"
    (add:ma a b)
  ::
  ++  sub  ~/  %sub                                     ::  subtract
    |=  {a/@rh b/@rh}  ^-  @rh
    ~_  leaf+"rh-fail"
    (sub:ma a b)
  ::
  ++  mul  ~/  %mul                                     ::  multiply
    |=  {a/@rh b/@rh}  ^-  @rh
    ~_  leaf+"rh-fail"
    (mul:ma a b)
  ::
  ++  div  ~/  %div                                     ::  divide
    |=  {a/@rh b/@rh}  ^-  @rh
    ~_  leaf+"rh-fail"
    (div:ma a b)
  ::
  ++  fma  ~/  %fma                                     ::  fused multiply-add
    |=  {a/@rh b/@rh c/@rh}  ^-  @rh
    ~_  leaf+"rh-fail"
    (fma:ma a b c)
  ::
  ++  sqt  ~/  %sqt                                     ::  square root
    |=  {a/@rh}  ^-  @rh
    ~_  leaf+"rh-fail"
    (sqt:ma a)
  ::
  ++  lth  ~/  %lth                                     ::  less-than
    |=  {a/@rh b/@rh}
    ~_  leaf+"rh-fail"
    (lth:ma a b)
  ::
  ++  lte  ~/  %lte                                     ::  less-equals
    |=  {a/@rh b/@rh}
    ~_  leaf+"rh-fail"
    (lte:ma a b)
  ::
  ++  equ  ~/  %equ                                     ::  equals
    |=  {a/@rh b/@rh}
    ~_  leaf+"rh-fail"
    (equ:ma a b)
  ::
  ++  gte  ~/  %gte                                     ::  greater-equals
    |=  {a/@rh b/@rh}
    ~_  leaf+"rh-fail"
    (gte:ma a b)
  ::
  ++  gth  ~/  %gth                                     ::  greater-than
    |=  {a/@rh b/@rh}
    ~_  leaf+"rh-fail"
    (gth:ma a b)
  ::
  ++  tos                                               ::  @rh to @rs
    |=  {a/@rh}  (bit:rs (sea a))
  ::
  ++  fos                                               ::  @rs to @rh
    |=  {a/@rs}  (bit (sea:rs a))
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
::::  3d: SHA hash family                               ::
  ::                                                    ::
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
    [r +>.$(a (shas %og-s (mix a r)))]
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
    [r +>.$(a (shas %og-s (mix a r)))]
  --
::                                                      ::
::::  3e: AES encryption  (XX removed)                  ::
  ::                                                    ::
  ::
::                                                      ::
::::  3f: scrambling                                    ::
  ::                                                    ::
  ::    ob                                              ::
  ::
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
      $(pyn lo)
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
      $(cry lo)
    cry
  ::
  ++  fice                                              ::  adapted from
    |=  nor/@                                           ::  black and rogaway
    ^-  @                                               ::  "ciphers with
    =+  ^=  sel                                         ::   arbitrary finite
    %+  rynd  3                                         ::   domains", 2002
    %+  rynd  2
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
    %+  rund  3
    [(mod vip 65.535) (div vip 65.535)]
    (add (mul 65.535 -.sel) +.sel)
  ::
  ++  rynd                                              ::  feistel round
    |=  {n/@ l/@ r/@}
    ^-  {@ @}
    :-  r
    ?~  (mod n 2)
      (~(sum fo 65.535) l (muk (snag n raku) 2 r))
    (~(sum fo 65.536) l (muk (snag n raku) 2 r))
  ::
  ++  rund                                              ::  reverse round
    |=  {n/@ l/@ r/@}
    ^-  {@ @}
    :-  r
    ?~  (mod n 2)
      (~(dif fo 65.535) l (muk (snag n raku) 2 r))
    (~(dif fo 65.536) l (muk (snag n raku) 2 r))
  ::
  ++  raku
    ^-  (list @ux)
    :~  0xb76d.5eed
        0xee28.1300
        0x85bc.ae01
        0x4b38.7af7
    ==
  --
::
::::  3g: molds and mold builders
  ::
++  coin  $%  {$$ p/dime}                               ::  print format
              {$blob p/*}                               ::
              {$many p/(list coin)}                     ::
          ==                                            ::
++  dime  {p/@ta q/@}                                   ::
++  edge  {p/hair q/(unit {p/* q/nail})}                ::  parsing output
++  hair  {p/@ud q/@ud}                                 ::  parsing trace
++  like  |*  a/gate                                    ::  generic edge
          |=  b/_`*`[(hair) ~]                          ::
          :-  p=(hair -.b)                              ::
          ^=  q                                         ::
          ?@  +.b  ~                                    ::
          :-  ~                                         ::
          u=[p=(a +>-.b) q=[p=(hair -.b) q=(tape +.b)]] ::
++  nail  {p/hair q/tape}                               ::  parsing input
++  path  (list knot)                                   ::  like unix path
++  pint  {p/{p/@ q/@} q/{p/@ q/@}}                     ::  line+column range
++  rule  _|=(nail *edge)                               ::  parsing rule
++  spot  {p/path q/pint}                               ::  range in file
++  tone  $%  {$0 p/*}                                  ::  success
              {$1 p/(list)}                             ::  blocks
              {$2 p/(list {@ta *})}                     ::  error report
          ==                                            ::
++  toon  $%  {$0 p/*}                                  ::  success
              {$1 p/(list)}                             ::  blocks
              {$2 p/(list tank)}                        ::  stack trace
          ==                                            ::
++  wonk  |*(veq/edge ?~(q.veq !! p.u.q.veq))           ::  product from edge
--  =>
::                                                      ::
::::  4: layer four                                     ::
  ::                                                    ::
  ::    4a: exotic bases                                ::
  ::    4b: text processing                             ::
  ::    4c: tank printer                                ::
  ::    4d: parsing (tracing)                           ::
  ::    4e: parsing (combinators)                       ::
  ::    4f: parsing (rule builders)                     ::
  ::    4g: parsing (outside caller)                    ::
  ::    4h: parsing (ascii glyphs)                      ::
  ::    4i: parsing (useful idioms)                     ::
  ::    4j: parsing (bases and base digits)             ::
  ::    4k: atom printing                               ::
  ::    4l: atom parsing                                ::
  ::    4m: formatting functions                        ::
  ::    4n: virtualization                              ::
  ::    4o: molds and mold builders                     ::
  ::
~%    %qua
    +
  ==
    %mute  mute
    %show  show
  ==
|%
::
::::  4a: exotic bases
  ::
++  po                                                  ::  phonetic base
  ~/  %po
  =+  :-  ^=  sis                                       ::  prefix syllables
      'dozmarbinwansamlitsighidfidlissogdirwacsabwissib\
      /rigsoldopmodfoglidhopdardorlorhodfolrintogsilmir\
      /holpaslacrovlivdalsatlibtabhanticpidtorbolfosdot\
      /losdilforpilramtirwintadbicdifrocwidbisdasmidlop\
      /rilnardapmolsanlocnovsitnidtipsicropwitnatpanmin\
      /ritpodmottamtolsavposnapnopsomfinfonbanmorworsip\
      /ronnorbotwicsocwatdolmagpicdavbidbaltimtasmallig\
      /sivtagpadsaldivdactansidfabtarmonranniswolmispal\
      /lasdismaprabtobrollatlonnodnavfignomnibpagsopral\
      /bilhaddocridmocpacravripfaltodtiltinhapmicfanpat\
      /taclabmogsimsonpinlomrictapfirhasbosbatpochactid\
      /havsaplindibhosdabbitbarracparloddosbortochilmac\
      /tomdigfilfasmithobharmighinradmashalraglagfadtop\
      /mophabnilnosmilfopfamdatnoldinhatnacrisfotribhoc\
      /nimlarfitwalrapsarnalmoslandondanladdovrivbacpol\
      /laptalpitnambonrostonfodponsovnocsorlavmatmipfip'
      ^=  dex                                           ::  suffix syllables
      'zodnecbudwessevpersutletfulpensytdurwepserwylsun\
      /rypsyxdyrnuphebpeglupdepdysputlughecryttyvsydnex\
      /lunmeplutseppesdelsulpedtemledtulmetwenbynhexfeb\
      /pyldulhetmevruttylwydtepbesdexsefwycburderneppur\
      /rysrebdennutsubpetrulsynregtydsupsemwynrecmegnet\
      /secmulnymtevwebsummutnyxrextebfushepbenmuswyxsym\
      /selrucdecwexsyrwetdylmynmesdetbetbeltuxtugmyrpel\
      /syptermebsetdutdegtexsurfeltudnuxruxrenwytnubmed\
      /lytdusnebrumtynseglyxpunresredfunrevrefmectedrus\
      /bexlebduxrynnumpyxrygryxfeptyrtustyclegnemfermer\
      /tenlusnussyltecmexpubrymtucfyllepdebbermughuttun\
      /bylsudpemdevlurdefbusbeprunmelpexdytbyttyplevmyl\
      /wedducfurfexnulluclennerlexrupnedlecrydlydfenwel\
      /nydhusrelrudneshesfetdesretdunlernyrsebhulryllud\
      /remlysfynwerrycsugnysnyllyndyndemluxfedsedbecmun\
      /lyrtesmudnytbyrsenwegfyrmurtelreptegpecnelnevfes'
  |%
  ++  ins  ~/  %ins                                     ::  parse prefix
           |=  a/@tas
           =+  b=0
           |-  ^-  (unit @)
           ?:(=(256 b) ~ ?:(=(a (tos b)) [~ b] $(b +(b))))
  ++  ind  ~/  %ind                                     ::  parse suffix
           |=  a/@tas
           =+  b=0
           |-  ^-  (unit @)
           ?:(=(256 b) ~ ?:(=(a (tod b)) [~ b] $(b +(b))))
  ++  tos  ~/  %tos                                     ::  fetch prefix
           |=(a/@ ?>((lth a 256) (cut 3 [(mul 3 a) 3] sis)))
  ++  tod  ~/  %tod                                     ::  fetch suffix
           |=(a/@ ?>((lth a 256) (cut 3 [(mul 3 a) 3] dex)))
  --
::
++  fa                                                  ::  base58check
  =+  key='123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
  =+  ^-  yek/@ux  ~+
      =-  yek:(roll (rip 3 key) -)
      =+  [a=*char b=*@ yek=`@ux`(fil 3 256 0xff)]
      |.
      [+(b) (mix yek (lsh 3 `@u`a (~(inv fe 3) b)))]
  |%
  ++  cha  |=(a/char `(unit @uF)`=+(b=(cut 3 [`@`a 1] yek) ?:(=(b 0xff) ~ `b)))
  ++  tok
    |=  a/@ux  ^-  @ux
    =+  b=(pad a)
    =-  (~(net fe 5) (end 3 4 (shay 32 -)))
    (shay (add b (met 3 a)) (lsh 3 b (swp 3 a)))
  ::
  ++  pad  |=(a/@ =+(b=(met 3 a) ?:((gte b 21) 0 (sub 21 b))))
  ++  enc  |=(a/@ux `@ux`(mix (lsh 3 4 a) (tok a)))
  ++  den
    |=  a/@ux  ^-  (unit @ux)
    =+  b=(rsh 3 4 a)
    ?.  =((tok b) (end 3 4 a))
      ~
    `b
  --
::
::::  4b: text processing
  ::
++  at                                                  ::  basic printing
  |_  a/@
  ++  r
    ?:  ?&  (gte (met 3 a) 2)
            |-
            ?:  =(0 a)
              &
            =+  vis=(end 3 1 a)
            ?&  ?|(=('-' vis) ?&((gte vis 'a') (lte vis 'z')))
                $(a (rsh 3 1 a))
            ==
        ==
      rtam
    ?:  (lte (met 3 a) 2)
      rud
    rux
  ::
  ++  rf    `tape`[?-(a $& '&', $| '|', * !!) ~]
  ++  rn    `tape`[?>(=(0 a) '~') ~]
  ++  rt    `tape`['\'' (weld (mesc (trip a)) `tape`['\'' ~])]
  ++  rta   rt
  ++  rtam  `tape`['%' (trip a)]
  ++  rub   `tape`['0' 'b' (rum 2 ~ |=(b/@ (add '0' b)))]
  ++  rud   (rum 10 ~ |=(b/@ (add '0' b)))
  ++  rum
    |=  {b/@ c/tape d/$-(@ @)}
    ^-  tape
    ?:  =(0 a)
      [(d 0) c]
    =+  e=0
    |-  ^-  tape
    ?:  =(0 a)
      c
    =+  f=&(!=(0 e) =(0 (mod e ?:(=(10 b) 3 4))))
    %=  $
      a  (div a b)
      c  [(d (mod a b)) ?:(f [?:(=(10 b) ',' '-') c] c)]
      e  +(e)
    ==
  ::
  ++  rup
    =+  b=(met 3 a)
    ^-  tape
    :-  '-'
    |-  ^-  tape
    ?:  (gth (met 5 a) 1)
      %+  weld
        $(a (rsh 5 1 a), b (sub b 4))
      `tape`['-' '-' $(a (end 5 1 a), b 4)]
    ?:  =(0 b)
      ['~' ~]
    ?:  (lte b 1)
      (trip (tos:po a))
    |-  ^-  tape
    ?:  =(2 b)
      =+  c=(rsh 3 1 a)
      =+  d=(end 3 1 a)
      (weld (trip (tod:po c)) (trip (tos:po (mix c d))))
    =+  c=(rsh 3 2 a)
    =+  d=(end 3 2 a)
    (weld ^$(a c, b (met 3 c)) `tape`['-' $(a (mix c d), b 2)])
  ::
  ++  ruv
    ^-  tape
    :+  '0'
      'v'
    %^    rum
        64
      ~
    |=  b/@
    ?:  =(63 b)
      '+'
    ?:  =(62 b)
      '-'
    ?:((lth b 26) (add 65 b) ?:((lth b 52) (add 71 b) (sub b 4)))
  ::
  ++  rux  `tape`['0' 'x' (rum 16 ~ |=(b/@ (add b ?:((lth b 10) 48 87))))]
  --
++  cass                                                ::  lowercase
  |=  vib/tape
  ^-  tape
  (turn vib |=(a/@ ?.(&((gte a 'A') (lte a 'Z')) a (add 32 a))))
::
++  cuss                                                ::  uppercase
  |=  vib/tape
  ^-  tape
  (turn vib |=(a/@ ?.(&((gte a 'a') (lte a 'z')) a (sub a 32))))
::
++  crip  |=(a/tape `@t`(rap 3 a))                      ::  tape to cord
::
++  mesc                                                ::  ctrl code escape
  |=  vib/tape
  ^-  tape
  ?~  vib
    ~
  ?:  =('\\' i.vib)
    ['\\' '\\' $(vib t.vib)]
  ?:  ?|((gth i.vib 126) (lth i.vib 32) =(`@`39 i.vib))
    ['\\' (welp ~(rux at i.vib) '/' $(vib t.vib))]
  [i.vib $(vib t.vib)]
::
++  runt                                                ::  prepend repeatedly
  |=  {{a/@ b/@} c/tape}
  ^-  tape
  ?:  =(0 a)
    c
  [b $(a (dec a))]
::
++  sand                                                ::  atom sanity
  |=  a/@ta
  (flit (sane a))
::
++  sane                                                ::  atom sanity
  |=  a/@ta
  |=  b/@  ^-  ?
  ?>  =(%t (end 3 1 a))
  =+  [inx=0 len=(met 3 b)]
  ?:  =(%tas a)
    |-  ^-  ?
    ?:  =(inx len)  &
    =+  cur=(cut 3 [inx 1] b)
    ?&  ?|  &((gte cur 'a') (lte cur 'z'))
            &(=('-' cur) !=(0 inx) !=(len inx))
            &(&((gte cur '0') (lte cur '9')) !=(0 inx))
        ==
        $(inx +(inx))
    ==
  ?:  =(%ta a)
    |-  ^-  ?
    ?:  =(inx len)  &
    =+  cur=(cut 3 [inx 1] b)
    ?&  ?|  &((gte cur 'a') (lte cur 'z'))
            &((gte cur '0') (lte cur '9'))
            |(=('-' cur) =('~' cur) =('_' cur) =('.' cur))
        ==
        $(inx +(inx))
    ==
  |-  ^-  ?
  ?:  =(0 b)  &
  =+  cur=(end 3 1 b)
  ?:  &((lth cur 32) !=(10 cur))  |
  =+  len=(teff cur)
  ?&  |(=(1 len) =+(i=1 |-(|(=(i len) &((gte (cut 3 [i 1] b) 128) $(i +(i)))))))
      $(b (rsh 3 len b))
  ==
::
++  trim                                                ::  tape split
  |=  {a/@ b/tape}
  ^-  {p/tape q/tape}
  ?~  b
    [~ ~]
  ?:  =(0 a)
    [~ b]
  =+  c=$(a (dec a), b t.b)
  [[i.b p.c] q.c]
::
++  trip                                                ::  cord to tape
  ~/  %trip
  |=  a/@  ^-  tape
  ?:  =(0 (met 3 a))
    ~
  [^-(@ta (end 3 1 a)) $(a (rsh 3 1 a))]
::
++  teff                                                ::  length utf8
  |=  a/@t  ^-  @
  =+  b=(end 3 1 a)
  ?:  =(0 b)
    ?>(=(`@`0 a) 0)
  ?>  |((gte b 32) =(10 b))
  ?:((lte b 127) 1 ?:((lte b 223) 2 ?:((lte b 239) 3 4)))
::
++  turf                                                ::  utf8 to utf32
  |=  a/@t
  ^-  @c
  %+  rap  5
  |-  ^-  (list @c)
  =+  b=(teff a)
  ?:  =(0 b)  ~
  =+  ^=  c
      %+  can  0
      %+  turn
        ^-  (list {p/@ q/@})
        ?+  b  !!
          $1  [[0 7] ~]
          $2  [[8 6] [0 5] ~]
          $3  [[16 6] [8 6] [0 4] ~]
          $4  [[24 6] [16 6] [8 6] [0 3] ~]
        ==
      |=({p/@ q/@} [q (cut 0 [p q] a)])
  ?>  =((tuft c) (end 3 b a))
  [c $(a (rsh 3 b a))]
::
++  tuba                                                ::  utf8 to utf32 tape
  |=  a/tape
  ^-  (list @c)
  (rip 5 (turf (rap 3 a)))                              ::  XX horrible
::
++  tufa                                                ::  utf32 to utf8 tape
  |=  a/(list @c)
  ^-  tape
  ?~  a  ""
  (weld (rip 3 (tuft i.a)) $(a t.a))
::
++  tuft                                                ::  utf32 to utf8 text
  |=  a/@c
  ^-  @t
  %+  rap  3
  |-  ^-  (list @)
  ?:  =(`@`0 a)
    ~
  =+  b=(end 5 1 a)
  =+  c=$(a (rsh 5 1 a))
  ?:  (lte b 0x7f)
    [b c]
  ?:  (lte b 0x7ff)
    :*  (mix 0b1100.0000 (cut 0 [6 5] b))
        (mix 0b1000.0000 (end 0 6 b))
        c
    ==
  ?:  (lte b 0xffff)
    :*  (mix 0b1110.0000 (cut 0 [12 4] b))
        (mix 0b1000.0000 (cut 0 [6 6] b))
        (mix 0b1000.0000 (end 0 6 b))
        c
    ==
  :*  (mix 0b1111.0000 (cut 0 [18 3] b))
      (mix 0b1000.0000 (cut 0 [12 6] b))
      (mix 0b1000.0000 (cut 0 [6 6] b))
      (mix 0b1000.0000 (end 0 6 b))
      c
  ==
::
++  wack                                                ::  knot escape
  |=  a/@ta
  ^-  @ta
  =+  b=(rip 3 a)
  %+  rap  3
  |-  ^-  tape
  ?~  b
    ~
  ?:  =('~' i.b)  ['~' '~' $(b t.b)]
  ?:  =('_' i.b)  ['~' '-' $(b t.b)]
  [i.b $(b t.b)]
::
++  wick                                                ::  knot unescape
  |=  a/@
  ^-  (unit @ta)
  =+  b=(rip 3 a)
  =-  ?^(b ~ (some (rap 3 (flop c))))
  =|  c/tape
  |-  ^-  {b/tape c/tape}
  ?~  b  [~ c]
  ?.  =('~' i.b)
    $(b t.b, c [i.b c])
  ?~  t.b  [b ~]
  ?-  i.t.b
    $'~'  $(b t.t.b, c ['~' c])
    $'-'  $(b t.t.b, c ['_' c])
    @     [b ~]
  ==
::
++  woad                                                ::  cord unescape
  |=  a/@ta
  ^-  @t
  %+  rap  3
  |-  ^-  (list @)
  ?:  =(`@`0 a)
    ~
  =+  b=(end 3 1 a)
  =+  c=(rsh 3 1 a)
  ?:  =('.' b)
    [' ' $(a c)]
  ?.  =('~' b)
    [b $(a c)]
  =>  .(b (end 3 1 c), c (rsh 3 1 c))
  ?+  b  =-  (weld (rip 3 (tuft p.d)) $(a q.d))
         ^=  d
         =+  d=0
         |-  ^-  {p/@ q/@}
         ?:  =('.' b)
           [d c]
         ?<  =(0 c)
         %=    $
            b  (end 3 1 c)
            c  (rsh 3 1 c)
            d  %+  add  (mul 16 d)
               %+  sub  b
               ?:  &((gte b '0') (lte b '9'))  48
               ?>(&((gte b 'a') (lte b 'z')) 87)
         ==
    $'.'  ['.' $(a c)]
    $'~'  ['~' $(a c)]
  ==
::
++  wood                                                ::  cord escape
  |=  a/@t
  ^-  @ta
  %+  rap  3
  |-  ^-  (list @)
  ?:  =(`@`0 a)
    ~
  =+  b=(teff a)
  =+  c=(turf (end 3 b a))
  =+  d=$(a (rsh 3 b a))
  ?:  ?|  &((gte c 'a') (lte c 'z'))
          &((gte c '0') (lte c '9'))
          =(`@`'-' c)
      ==
    [c d]
  ?+  c
    :-  '~'
    =+  e=(met 2 c)
    |-  ^-  tape
    ?:  =(0 e)
      ['.' d]
    =.  e  (dec e)
    =+  f=(rsh 2 e c)
    [(add ?:((lte f 9) 48 87) f) $(c (end 2 e c))]
  ::
    $' '  ['.' d]
    $'.'  ['~' '.' d]
    $'~'  ['~' '~' d]
  ==
::
::::  4c: tank printer
  ::
++  wash                                                ::  render tank at width
  |=  {{tab/@ edg/@} tac/tank}  ^-  wall
  (~(win re tac) tab edg)
::
++  re
  |_  tac/tank
  ++  ram
    ^-  tape
    ?-    -.tac
        $leaf  p.tac
        $palm  ram(tac [%rose [p.p.tac (weld q.p.tac r.p.tac) s.p.tac] q.tac])
        $rose
      %+  weld
        q.p.tac
      |-  ^-  tape
      ?~  q.tac
        r.p.tac
      =+  voz=$(q.tac t.q.tac)
      (weld ram(tac i.q.tac) ?~(t.q.tac voz (weld p.p.tac voz)))
    ==
  ::
  ++  win
    |=  {tab/@ edg/@}
    =+  lug=`wall`~
    |^  |-  ^-  wall
        ?-    -.tac
            $leaf  (rig p.tac)
            $palm
          ?:  fit
            (rig ram)
          ?~  q.tac
            (rig q.p.tac)
          ?~  t.q.tac
            (rig(tab (add 2 tab), lug $(tac i.q.tac)) q.p.tac)
          =>  .(q.tac `(list tank)`q.tac)
          =+  lyn=(mul 2 (lent q.tac))
          =+  ^=  qyr
              |-  ^-  wall
              ?~  q.tac
                lug
              %=  ^$
                tac  i.q.tac
                tab  (add tab (sub lyn 2))
                lug  $(q.tac t.q.tac, lyn (sub lyn 2))
              ==
          (wig(lug qyr) q.p.tac)
        ::
            $rose
          ?:  fit
            (rig ram)
          =.  lug
            |-  ^-  wall
            ?~  q.tac
              ?:(=(~ r.p.tac) lug (rig r.p.tac))
            ^$(tac i.q.tac, lug $(q.tac t.q.tac), tab din)
          ?:  =(~ q.p.tac)
            lug
          (wig q.p.tac)
        ==
    ::
    ++  din  (mod (add 2 tab) (mul 2 (div edg 3)))
    ++  fit  (lte (lent ram) (sub edg tab))
    ++  rig
      |=  hom/tape
      ^-  wall
      ?:  (lte (lent hom) (sub edg tab))
        [(runt [tab ' '] hom) lug]
      =>  .(tab (add tab 2), edg (sub edg 2))
      =+  mut=(trim (sub edg tab) hom)
      :-  (runt [(sub tab 2) ' '] ['\\' '/' (weld p.mut `_hom`['\\' '/' ~])])
      =>  .(hom q.mut)
      |-
      ?~  hom
        :-  %+  runt
              [(sub tab 2) ' ']
            ['\\' '/' (runt [(sub edg tab) ' '] ['\\' '/' ~])]
        lug
      =>  .(mut (trim (sub edg tab) hom))
      [(runt [tab ' '] p.mut) $(hom q.mut)]
    ::
    ++  wig
      |=  hom/tape
      ^-  wall
      ?~  lug
        (rig hom)
      =+  lin=(lent hom)
      =+  wug=:(add 1 tab lin)
      ?.  =+  mir=i.lug
          |-  ?~  mir
                |
              ?|(=(0 wug) ?&(=(' ' i.mir) $(mir t.mir, wug (dec wug))))
        (rig hom)       :: ^ XX regular form?
      [(runt [tab ' '] (weld hom `tape`[' ' (slag wug i.lug)])) t.lug]
    --
  --
++  show                                                ::  XX deprecated!
  |=  vem/*
  |^  ^-  tank
      ?:  ?=(@ vem)
        [%leaf (mesc (trip vem))]
      ?-    vem
          {s/$~ c/*}
        [%leaf '\'' (weld (mesc (tape +.vem)) `tape`['\'' ~])]
      ::
          {s/$a c/@}        [%leaf (mesc (trip c.vem))]
          {s/$b c/*}        (shop c.vem |=(a/@ ~(rub at a)))
          {s/{$c p/@} c/*}
        :+  %palm
          [['.' ~] ['-' ~] ~ ~]
        [[%leaf (mesc (trip p.s.vem))] $(vem c.vem) ~]
      ::
          {s/$d c/*}        (shop c.vem |=(a/@ ~(rud at a)))
          {s/$k c/*}        (tank c.vem)
          {s/$h c/*}
        :+  %rose
          [['/' ~] ['/' ~] ~]
        =+  yol=((list @ta) c.vem)
        (turn yol |=(a/@ta [%leaf (trip a)]))
      ::
          {s/$l c/*}        (shol c.vem)
          {s/$o c/*}
        %=    $
            vem
          :-  [%m '%h:<[%d %d].[%d %d]>']
          [-.c.vem +<-.c.vem +<+.c.vem +>-.c.vem +>+.c.vem ~]
        ==
      ::
          {s/$p c/*}        (shop c.vem |=(a/@ ~(rup at a)))
          {s/$q c/*}        (shop c.vem |=(a/@ ~(r at a)))
          {s/$r c/*}        $(vem [[%r ' ' '{' '}'] c.vem])
          {s/$t c/*}        (shop c.vem |=(a/@ ~(rt at a)))
          {s/$v c/*}        (shop c.vem |=(a/@ ~(ruv at a)))
          {s/$x c/*}        (shop c.vem |=(a/@ ~(rux at a)))
          {s/{$m p/@} c/*}  (shep p.s.vem c.vem)
          {s/{$r p/@} c/*}
        $(vem [[%r ' ' (cut 3 [0 1] p.s.vem) (cut 3 [1 1] p.s.vem)] c.vem])
      ::
          {s/{$r p/@ q/@ r/@} c/*}
        :+  %rose
          :*  p=(mesc (trip p.s.vem))
              q=(mesc (trip q.s.vem))
              r=(mesc (trip r.s.vem))
          ==
        |-  ^-  (list tank)
        ?@  c.vem
          ~
        [^$(vem -.c.vem) $(c.vem +.c.vem)]
      ::
          {s/$z c/*}        $(vem [[%r %$ %$ %$] c.vem])
          *                 !!
      ==
  ++  shep
    |=  {fom/@ gar/*}
    ^-  tank
    =+  l=(met 3 fom)
    =+  i=0
    :-  %leaf
    |-  ^-  tape
    ?:  (gte i l)
      ~
    =+  c=(cut 3 [i 1] fom)
    ?.  =(37 c)
      (weld (mesc [c ~]) $(i +(i)))
    =+  d=(cut 3 [+(i) 1] fom)
    ?.  .?(gar)
      ['\\' '#' $(i (add 2 i))]
    (weld ~(ram re (show d -.gar)) $(i (add 2 i), gar +.gar))
  ::
  ++  shop
    |=  {aug/* vel/$-(a/@ tape)}
    ^-  tank
    ?:  ?=(@ aug)
      [%leaf (vel aug)]
    :+  %rose
      [[' ' ~] ['[' ~] [']' ~]]
    =>  .(aug `*`aug)
    |-  ^-  (list tank)
    ?:  ?=(@ aug)
      [^$ ~]
    [^$(aug -.aug) $(aug +.aug)]
  ::
  ++  shol
    |=  lim/*
    :+  %rose
      [['.' ~] ~ ~]
    |-    ^-  (list tank)
    ?:  ?=(@ lim)  ~
    :_  $(lim +.lim)
    ?+  -.lim  (show '#')
        $~   (show '$')
        c/@  (show c.lim)
        {$& $1}  (show '.')
        {$& c/@}
      [%leaf '+' ~(rud at c.lim)]
    ::
        {$| @ $~}  (show ',')
        {$| n/@ $~ c/@}
      [%leaf (weld (reap n.lim '^') ?~(c.lim "$" (trip c.lim)))]
    ==
  --
::
::::  4d: parsing (tracing)
  ::
++  last  |=  {zyc/hair naz/hair}                       ::  farther trace
          ^-  hair
          ?:  =(p.zyc p.naz)
            ?:((gth q.zyc q.naz) zyc naz)
          ?:((gth p.zyc p.naz) zyc naz)
::
++  lust  |=  {weq/char naz/hair}                       ::  detect newline
          ^-  hair
          ?:(=(`@`10 weq) [+(p.naz) 1] [p.naz +(q.naz)])
::
::::  4e: parsing (combinators)
  ::
++  bend                                                ::  conditional comp
  ~/  %bend
  |*  raq/_|*({a/* b/*} [~ u=[a b]])
  ~/  %fun
  |*  {vex/edge sab/rule}
  ?~  q.vex
    vex
  =+  yit=(sab q.u.q.vex)
  =+  yur=(last p.vex p.yit)
  ?~  q.yit
    [p=yur q=q.vex]
  =+  vux=(raq p.u.q.vex p.u.q.yit)
  ?~  vux
    [p=yur q=q.vex]
  [p=yur q=[~ u=[p=u.vux q=q.u.q.yit]]]
::
++  comp
  ~/  %comp
  |*  raq/_|*({a/* b/*} [a b])                          ::  arbitrary compose
  ~/  %fun
  |*  {vex/edge sab/rule}
  ~!  +<
  ?~  q.vex
    vex
  =+  yit=(sab q.u.q.vex)
  =+  yur=(last p.vex p.yit)
  ?~  q.yit
    [p=yur q=q.yit]
  [p=yur q=[~ u=[p=(raq p.u.q.vex p.u.q.yit) q=q.u.q.yit]]]
::
++  fail  |=(tub/nail [p=p.tub q=~])                    ::  never parse
++  glue                                                ::  add rule
  ~/  %glue
  |*  bus/rule
  ~/  %fun
  |*  {vex/edge sab/rule}
  (plug vex ;~(pfix bus sab))
::
++  less                                                ::  no first and second
  |*  {vex/edge sab/rule}
  ?~  q.vex
    =+  roq=(sab)
    [p=(last p.vex p.roq) q=q.roq]
  (fail +<.sab)
::
++  pfix                                                ::  discard first rule
  ~/  %pfix
  (comp |*({a/* b/*} b))
::
++  plug                                                ::  first then second
  ~/  %plug
  |*  {vex/edge sab/rule}
  ?~  q.vex
    vex
  =+  yit=(sab q.u.q.vex)
  =+  yur=(last p.vex p.yit)
  ?~  q.yit
    [p=yur q=q.yit]
  [p=yur q=[~ u=[p=[p.u.q.vex p.u.q.yit] q=q.u.q.yit]]]
::
++  pose                                                ::  first or second
  ~/  %pose
  |*  {vex/edge sab/rule}
  ?~  q.vex
    =+  roq=(sab)
    [p=(last p.vex p.roq) q=q.roq]
  vex
::
++  simu                                                ::  first and second
  |*  {vex/edge sab/rule}
  ?~  q.vex
    vex
  =+  roq=(sab)
  roq
::
++  sfix                                                ::  discard second rule
  ~/  %sfix
  (comp |*({a/* b/*} a))
::
::::  4f: parsing (rule builders)
  ::
++  bass                                                ::  leftmost base
  |*  {wuc/@ tyd/rule}
  %+  cook
    |=  waq/(list @)
    %+  roll
      waq
    =|({p/@ q/@} |.((add p (mul wuc q))))
  tyd
::
++  boss                                                ::  rightmost base
  |*  {wuc/@ tyd/rule}
  %+  cook
    |=  waq/(list @)
    %+  reel
      waq
    =|({p/@ q/@} |.((add p (mul wuc q))))
  tyd
::
++  cold                                                ::  replace w+ constant
  ~/  %cold
  |*  {cus/* sef/rule}
  ~/  %fun
  |=  tub/nail
  =+  vex=(sef tub)
  ?~  q.vex
    vex
  [p=p.vex q=[~ u=[p=cus q=q.u.q.vex]]]
::
++  cook                                                ::  apply gate
  ~/  %cook
  |*  {poq/gate sef/rule}
  ~/  %fun
  |=  tub/nail
  =+  vex=(sef tub)
  ?~  q.vex
    vex
  [p=p.vex q=[~ u=[p=(poq p.u.q.vex) q=q.u.q.vex]]]
::
++  easy                                                ::  always parse
  ~/  %easy
  |*  huf/*
  ~/  %fun
  |=  tub/nail
  ^-  (like _huf)
  [p=p.tub q=[~ u=[p=huf q=tub]]]
::
++  flag
  |=  {sic/@t non/@t}
  ;~(pose (cold %& (jest sic)) (cold %| (jest non)))
::
++  full                                                ::  has to fully parse
  |*  sef/rule
  |=  tub/nail
  =+  vex=(sef tub)
  ?~(q.vex vex ?:(=(~ q.q.u.q.vex) vex [p=p.vex q=~]))
::
++  funk                                                ::  add to tape first
  |*  {pre/tape sef/rule}
  |=  tub/nail
  (sef p.tub (weld pre q.tub))
::
++  here                                                ::  place-based apply
  ~/  %here
  |*  {hez/_|=({a/pint b/*} [a b]) sef/rule}
  ~/  %fun
  |=  tub/nail
  =+  vex=(sef tub)
  ?~  q.vex
    vex
  [p=p.vex q=[~ u=[p=(hez [p.tub p.q.u.q.vex] p.u.q.vex) q=q.u.q.vex]]]
::
++  inde  |*  sef/rule                                  :: indentation block
  |=  nail  ^+  (sef)
  =+  [har tap]=[p q]:+<
  =+  lev=(fil 3 (dec q.har) ' ')
  =+  eol=(just `@t`10)
  =+  =-  roq=((star ;~(pose prn ;~(sfix eol (jest lev)) -)) har tap)
      ;~(simu ;~(plug eol eol) eol)
  ?~  q.roq  roq
  =+  vex=(sef har(q 1) p.u.q.roq)
  =+  fur=p.vex(q (add (dec q.har) q.p.vex))
  ?~  q.vex  vex(p fur)
  =-  vex(p fur, u.q -)
  :+  &3.vex
    &4.vex(q.p (add (dec q.har) q.p.&4.vex))
  =+  res=|4.vex
  |-  ?~  res  |4.roq
  ?.  =(10 -.res)  [-.res $(res +.res)]
  (welp [`@t`10 (trip lev)] $(res +.res))
::
++  ifix
  |*  {fel/{rule rule} hof/rule}
  ~!  +<
  ~!  +<:-.fel
  ~!  +<:+.fel
  ;~(pfix -.fel ;~(sfix hof +.fel))
::
++  jest                                                ::  match a cord
  |=  daf/@t
  |=  tub/nail
  =+  fad=daf
  |-  ^-  (like @t)
  ?:  =(`@`0 daf)
    [p=p.tub q=[~ u=[p=fad q=tub]]]
  ?:  |(?=($~ q.tub) !=((end 3 1 daf) i.q.tub))
    (fail tub)
  $(p.tub (lust i.q.tub p.tub), q.tub t.q.tub, daf (rsh 3 1 daf))
::
++  just                                                ::  XX redundant, jest
  ~/  %just                                             ::  match a char
  |=  daf/char
  ~/  %fun
  |=  tub/nail
  ^-  (like char)
  ?~  q.tub
    (fail tub)
  ?.  =(daf i.q.tub)
    (fail tub)
  (next tub)
::
++  knee                                                ::  callbacks
  |*  {gar/* sef/_|.(*rule)}
  |=  tub/nail
  ^-  (like _gar)
  ((sef) tub)
::
++  mask                                                ::  match char in set
  ~/  %mask
  |=  bud/(list char)
  ~/  %fun
  |=  tub/nail
  ^-  (like char)
  ?~  q.tub
    (fail tub)
  ?.  (lien bud |=(a/char =(i.q.tub a)))
    (fail tub)
  (next tub)
::
++  more                                                ::  separated, *
  |*  {bus/rule fel/rule}
  ;~(pose (most bus fel) (easy ~))
::
++  most                                                ::  separated, +
  |*  {bus/rule fel/rule}
  ;~(plug fel (star ;~(pfix bus fel)))
::
++  next                                                ::  consume a char
  |=  tub/nail
  ^-  (like char)
  ?~  q.tub
    (fail tub)
  =+  zac=(lust i.q.tub p.tub)
  [zac [~ i.q.tub [zac t.q.tub]]]
::
++  perk                                                ::  parse cube fork
  |*  a/(pole @tas)
  ?~  a  fail
  ;~  pose
    (cold -.a (jest -.a))
    $(a +.a)
  ==
::
++  pick                                                ::  rule for ++each
  |*  {a/rule b/rule}
  ;~  pose
    (stag %& a)
    (stag %| b)
  ==
++  plus  |*(fel/rule ;~(plug fel (star fel)))          ::
++  punt  |*({a/rule} ;~(pose (stag ~ a) (easy ~)))     ::
++  sear                                                ::  conditional cook
  |*  {pyq/$-(* (unit)) sef/rule}
  |=  tub/nail
  =+  vex=(sef tub)
  ?~  q.vex
    vex
  =+  gey=(pyq p.u.q.vex)
  ?~  gey
    [p=p.vex q=~]
  [p=p.vex q=[~ u=[p=u.gey q=q.u.q.vex]]]
::
++  shim                                                ::  match char in range
  ~/  %shim
  |=  {les/@ mos/@}
  ~/  %fun
  |=  tub/nail
  ^-  (like char)
  ?~  q.tub
    (fail tub)
  ?.  ?&((gte i.q.tub les) (lte i.q.tub mos))
    (fail tub)
  (next tub)
::
++  stag                                                ::  add a label
  ~/  %stag
  |*  {gob/* sef/rule}
  ~/  %fun
  |=  tub/nail
  =+  vex=(sef tub)
  ?~  q.vex
    vex
  [p=p.vex q=[~ u=[p=[gob p.u.q.vex] q=q.u.q.vex]]]
::
++  stet                                                ::
  |*  leh/(list {?(@ {@ @}) rule})
  |-
  ?~  leh
    ~
  [i=[p=-.i.leh q=+.i.leh] t=$(leh t.leh)]
::
++  stew                                                ::  switch by first char
  ~/  %stew
  |*  leh/(list {p/?(@ {@ @}) q/rule})                  ::  char+range keys
  =+  ^=  wor                                           ::  range complete lth
      |=  {ort/?(@ {@ @}) wan/?(@ {@ @})}
      ?@  ort
        ?@(wan (lth ort wan) (lth ort -.wan))
      ?@(wan (lth +.ort wan) (lth +.ort -.wan))
  =+  ^=  hel                                           ::  build parser map
      =+  hel=`(tree _?>(?=(^ leh) i.leh))`~
      |-  ^+  hel
      ?~  leh
        ~
      =+  yal=$(leh t.leh)
      |-  ^+  hel
      ?~  yal
        [i.leh ~ ~]
      ?:  (wor p.i.leh p.n.yal)
        =+  nuc=$(yal l.yal)
        ?>  ?=(^ nuc)
        ?:  (vor p.n.yal p.n.nuc)
          [n.yal nuc r.yal]
        [n.nuc l.nuc [n.yal r.nuc r.yal]]
      =+  nuc=$(yal r.yal)
      ?>  ?=(^ nuc)
      ?:  (vor p.n.yal p.n.nuc)
        [n.yal l.yal nuc]
      [n.nuc [n.yal l.yal l.nuc] r.nuc]
  ~%  %fun  ..^$  ~
  |=  tub/nail
  ?~  q.tub
    (fail tub)
  |-
  ?~  hel
    (fail tub)
  ?:  ?@  p.n.hel
        =(p.n.hel i.q.tub)
      ?&((gte i.q.tub -.p.n.hel) (lte i.q.tub +.p.n.hel))
    ::  (q.n.hel [(lust i.q.tub p.tub) t.q.tub])
    (q.n.hel tub)
  ?:  (wor i.q.tub p.n.hel)
    $(hel l.hel)
  $(hel r.hel)
::
++  slug                                                ::
  |*  raq/_|*({a/* b/*} [a b])
  |*  {bus/rule fel/rule}
  ;~((comp raq) fel (stir +<+.raq raq ;~(pfix bus fel)))
::
++  star                                                ::  0 or more times
  |*  fel/rule
  (stir `(list _(wonk *fel))`~ |*({a/* b/*} [a b]) fel)
::
++  stir
  ~/  %stir
  |*  {rud/* raq/_|*({a/* b/*} [a b]) fel/rule}
  ~/  %fun
  |=  tub/nail
  ^-  (like _rud)
  =+  vex=(fel tub)
  ?~  q.vex
    [p.vex [~ rud tub]]
  =+  wag=$(tub q.u.q.vex)
  ?>  ?=(^ q.wag)
  [(last p.vex p.wag) [~ (raq p.u.q.vex p.u.q.wag) q.u.q.wag]]
::
++  stun                                                ::  parse several times
  |*  {lig/{@ @} fel/rule}
  |=  tub/nail
  ^-  (like (list _(wonk (fel))))
  ?:  =(0 +.lig)
    [p.tub [~ ~ tub]]
  =+  vex=(fel tub)
  ?~  q.vex
    ?:  =(0 -.lig)
      [p.vex [~ ~ tub]]
    vex
  =+  ^=  wag  %=  $
                 -.lig  ?:(=(0 -.lig) 0 (dec -.lig))
                 +.lig  ?:(=(0 +.lig) 0 (dec +.lig))
                 tub  q.u.q.vex
               ==
  ?~  q.wag
    wag
  [p.wag [~ [p.u.q.vex p.u.q.wag] q.u.q.wag]]
::
::::  4g: parsing (outside caller)
  ::
++  rash  |*({naf/@ sab/rule} (scan (trip naf) sab))   ::
++  rose  |*  {los/tape sab/rule}
          =+  vex=(sab [[1 1] los])
          =+  len=(lent los)
          ?.  =(+(len) q.p.vex)  [%| p=(dec q.p.vex)]
          ?~  q.vex
            [%& p=~]
          [%& p=[~ u=p.u.q.vex]]
++  rush  |*({naf/@ sab/rule} (rust (trip naf) sab))
++  rust  |*  {los/tape sab/rule}
          =+  vex=((full sab) [[1 1] los])
          ?~(q.vex ~ [~ u=p.u.q.vex])
++  scan  |*  {los/tape sab/rule}
          =+  vex=((full sab) [[1 1] los])
          ?~  q.vex
            ~_  (show [%m '{%d %d}'] p.p.vex q.p.vex ~)
            ~_(leaf+"syntax error" !!)
          p.u.q.vex
::
::::  4h: parsing (ascii glyphs)
  ::
++  ace  (just ' ')
++  bar  (just '|')
++  bas  (just '\\')
++  buc  (just '$')
++  cab  (just '_')
++  cen  (just '%')
++  col  (just ':')
++  com  (just ',')
++  doq  (just '"')
++  dot  (just '.')
++  fas  (just '/')
++  gal  (just '<')
++  gar  (just '>')
++  hax  (just '#')
++  kel  (just '{')
++  ker  (just '}')
++  ket  (just '^')
++  lus  (just '+')
++  hep  (just '-')
++  pel  (just '(')
++  pam  (just '&')
++  per  (just ')')
++  pat  (just '@')
++  sel  (just '[')
++  sem  (just ';')
++  ser  (just ']')
++  sig  (just '~')
++  soq  (just '\'')
++  tar  (just '*')
++  tec  (just '`')
++  tis  (just '=')
++  wut  (just '?')
++  zap  (just '!')
::
::::  4i: parsing (useful idioms)
  ::
++  alf  ;~(pose low hig)                               ::  alphabetic
++  aln  ;~(pose low hig nud)                           ::  alphanumeric
++  alp  ;~(pose low hig nud hep)                       ::  alphanumeric and -
++  bet  ;~(pose (cold 2 hep) (cold 3 lus))             ::  axis syntax - +
++  bin  (bass 2 (most gon but))                        ::  binary to atom
++  but  (cook |=(a/@ (sub a '0')) (shim '0' '1'))      ::  binary digit
++  cit  (cook |=(a/@ (sub a '0')) (shim '0' '7'))      ::  octal digit
++  dem  (bass 10 (most gon dit))                       ::  decimal to atom
++  dit  (cook |=(a/@ (sub a '0')) (shim '0' '9'))      ::  decimal digit
++  dog  ;~(plug dot gay)                               ::  .  number separator
++  doh  ;~(plug ;~(plug hep hep) gay)                  ::  --  phon separator
++  dun  (cold ~ ;~(plug hep hep))                      ::  -- (stop) to ~
++  duz  (cold ~ ;~(plug tis tis))                      ::  == (stet) to ~
++  gah  (mask [`@`10 ' ' ~])                           ::  newline or ace
++  gap  (cold ~ ;~(plug gaq (star ;~(pose vul gah))))  ::  plural space
++  gaq  ;~  pose                                       ::  end of line
             (just `@`10)
             ;~(plug gah ;~(pose gah vul))
             vul
         ==
++  gaw  (cold ~ (star ;~(pose vul gah)))               ::  classic white
++  gay  ;~(pose gap (easy ~))                          ::
++  gon  ;~(pose ;~(plug bas gay fas) (easy ~))         ::  long numbers \ /
++  gul  ;~(pose (cold 2 gal) (cold 3 gar))             ::  axis syntax < >
++  hex  (bass 16 (most gon hit))                       ::  hex to atom
++  hig  (shim 'A' 'Z')                                 ::  uppercase
++  hit  ;~  pose                                       ::  hex digits
           dit
           (cook |=(a/char (sub a 87)) (shim 'a' 'f'))
           (cook |=(a/char (sub a 55)) (shim 'A' 'F'))
         ==
++  iny                                                 :: indentation block
  |*  sef/rule
  |=  nail  ^+  (sef)
  =+  [har tap]=[p q]:+<
  =+  lev=(fil 3 (dec q.har) ' ')
  =+  eol=(just `@t`10)
  =+  =-  roq=((star ;~(pose prn ;~(sfix eol (jest lev)) -)) har tap)
      ;~(simu ;~(plug eol eol) eol)
  ?~  q.roq  roq
  =+  vex=(sef har(q 1) p.u.q.roq)
  =+  fur=p.vex(q (add (dec q.har) q.p.vex))
  ?~  q.vex  vex(p fur)
  =-  vex(p fur, u.q -)
  :+  &3.vex
    &4.vex(q.p (add (dec q.har) q.p.&4.vex))
  =+  res=|4.vex
  |-  ?~  res  |4.roq
  ?.  =(10 -.res)  [-.res $(res +.res)]
  (welp [`@t`10 (trip lev)] $(res +.res))
::
++  low  (shim 'a' 'z')                                 ::  lowercase
++  mes  %+  cook                                       ::  hexbyte
           |=({a/@ b/@} (add (mul 16 a) b))
         ;~(plug hit hit)
++  nix  (boss 256 (star ;~(pose aln cab)))             ::
++  nud  (shim '0' '9')                                 ::  numeric
++  prn  ;~(less (just `@`127) (shim 32 256))           ::  non-control
++  qat  ;~  pose                                       ::  chars in blockcord
             prn
             ;~(less ;~(plug (just `@`10) soz) (just `@`10))
         ==
++  qit  ;~  pose                                       ::  chars in a cord
             ;~(less bas soq prn)
             ;~(pfix bas ;~(pose bas soq mes))          ::  escape chars
         ==
++  qut  ;~  simu  soq                                  ::  cord
           ;~  pose
             ;~  less  soz
               (ifix [soq soq] (boss 256 (more gon qit)))
             ==
             =+  hed=;~(pose ;~(plug (plus ace) vul) (just '\0a'))
             %-  iny  %+  ifix
               :-  ;~(plug soz hed)
               ;~(plug (just '\0a') soz)
             (boss 256 (star qat))
           ==
         ==
++  soz  ;~(plug soq soq soq)                           ::  delimiting '''
++  sym                                                 ::  symbol
  %+  cook
    |=(a/tape (rap 3 ^-((list @) a)))
  ;~(plug low (star ;~(pose nud low hep)))
::
++  ven  ;~  (comp |=({a/@ b/@} (peg a b)))             ::  +>- axis syntax
           bet
           =+  hom=`?`|
           |=  tub/nail
           ^-  (like @)
           =+  vex=?:(hom (bet tub) (gul tub))
           ?~  q.vex
             [p.tub [~ 1 tub]]
           =+  wag=$(p.tub p.vex, hom !hom, tub q.u.q.vex)
           ?>  ?=(^ q.wag)
           [p.wag [~ (peg p.u.q.vex p.u.q.wag) q.u.q.wag]]
         ==
++  vit                                                 ::  base64 digit
  ;~  pose
    (cook |=(a/@ (sub a 65)) (shim 'A' 'Z'))
    (cook |=(a/@ (sub a 71)) (shim 'a' 'z'))
    (cook |=(a/@ (add a 4)) (shim '0' '9'))
    (cold 62 (just '-'))
    (cold 63 (just '+'))
  ==
++  vul  %+  cold   ~                                   ::  comments
         ;~  plug  col  col
           (star prn)
           (just `@`10)
         ==
::
::::  4j: parsing (bases and base digits)
  ::
++  ab
  |%
  ++  bix  (bass 16 (stun [2 2] six))
  ++  fem  (sear |=(a/@ (cha:fa a)) aln)
  ++  haf  (bass 256 ;~(plug tep tiq (easy ~)))
  ++  hef  %+  sear  |=(a/@ ?:(=(a 0) ~ (some a)))
           %+  bass  256
           ;~(plug tip tiq (easy ~))
  ++  hif  (bass 256 ;~(plug tip tiq (easy ~)))
  ++  hof  (bass 0x1.0000 ;~(plug hef (stun [1 3] ;~(pfix hep hif))))
  ++  huf  (bass 0x1.0000 ;~(plug hef (stun [0 3] ;~(pfix hep hif))))
  ++  hyf  (bass 0x1.0000 ;~(plug hif (stun [3 3] ;~(pfix hep hif))))
  ++  pev  (bass 32 ;~(plug sev (stun [0 4] siv)))
  ++  pew  (bass 64 ;~(plug sew (stun [0 4] siw)))
  ++  piv  (bass 32 (stun [5 5] siv))
  ++  piw  (bass 64 (stun [5 5] siw))
  ++  qeb  (bass 2 ;~(plug seb (stun [0 3] sib)))
  ++  qex  (bass 16 ;~(plug sex (stun [0 3] hit)))
  ++  qib  (bass 2 (stun [4 4] sib))
  ++  qix  (bass 16 (stun [4 4] six))
  ++  seb  (cold 1 (just '1'))
  ++  sed  (cook |=(a/@ (sub a '0')) (shim '1' '9'))
  ++  sev  ;~(pose sed sov)
  ++  sew  ;~(pose sed sow)
  ++  sex  ;~(pose sed sox)
  ++  sib  (cook |=(a/@ (sub a '0')) (shim '0' '1'))
  ++  sid  (cook |=(a/@ (sub a '0')) (shim '0' '9'))
  ++  siv  ;~(pose sid sov)
  ++  siw  ;~(pose sid sow)
  ++  six  ;~(pose sid sox)
  ++  sov  (cook |=(a/@ (sub a 87)) (shim 'a' 'v'))
  ++  sow  ;~  pose
             (cook |=(a/@ (sub a 87)) (shim 'a' 'z'))
             (cook |=(a/@ (sub a 29)) (shim 'A' 'Z'))
             (cold 62 (just '-'))
             (cold 63 (just '~'))
           ==
  ++  sox  (cook |=(a/@ (sub a 87)) (shim 'a' 'f'))
  ++  ted  (bass 10 ;~(plug sed (stun [0 2] sid)))
  ++  tep  (sear |=(a/@ ?:(=(a 'doz') ~ (ins:po a))) til)
  ++  tip  (sear |=(a/@ (ins:po a)) til)
  ++  tiq  (sear |=(a/@ (ind:po a)) til)
  ++  tid  (bass 10 (stun [3 3] sid))
  ++  til  (boss 256 (stun [3 3] low))
  ++  urs  %+  cook
             |=(a/tape (rap 3 ^-((list @) a)))
           (star ;~(pose nud low hep dot sig cab))
  ++  urt  %+  cook
             |=(a/tape (rap 3 ^-((list @) a)))
           (star ;~(pose nud low hep dot sig))
  ++  urx  %+  cook
             |=(a/tape (rap 3 ^-((list @) a)))
           %-  star
           ;~  pose
             nud
             low
             hep
             cab
             (cold ' ' dot)
             (cook tuft (ifix [sig dot] hex))
             ;~(pfix sig ;~(pose sig dot))
           ==
  ++  voy  ;~(pfix bas ;~(pose bas soq bix))
  --
++  ag
  |%
  ++  ape  |*(fel/rule ;~(pose (cold 0 (just '0')) fel))
  ++  bay  (ape (bass 16 ;~(plug qeb:ab (star ;~(pfix dog qib:ab)))))
  ++  bip  =+  tod=(ape qex:ab)
           (bass 0x1.0000 ;~(plug tod (stun [7 7] ;~(pfix dog tod))))
  ++  dem  (ape (bass 1.000 ;~(plug ted:ab (star ;~(pfix dog tid:ab)))))
  ++  dim  (ape dip)
  ++  dip  (bass 10 ;~(plug sed:ab (star sid:ab)))
  ++  dum  (bass 10 (plus sid:ab))
  ++  fed  %+  cook  fend:ob
           ;~  pose
             %+  bass  0x1.0000.0000.0000.0000          ::  oversized
               ;~  plug
                 huf:ab
                 (plus ;~(pfix doh hyf:ab))
               ==
             hof:ab                                     ::  planet or moon
             haf:ab                                     ::  star
             tiq:ab                                     ::  galaxy
           ==
  ++  fim  (sear den:fa (bass 58 (plus fem:ab)))
  ++  hex  (ape (bass 0x1.0000 ;~(plug qex:ab (star ;~(pfix dog qix:ab)))))
  ++  lip  =+  tod=(ape ted:ab)
           (bass 256 ;~(plug tod (stun [3 3] ;~(pfix dog tod))))
  ++  mot  ;~  pose
             ;~  pfix
               (just '1')
               (cook |=(a/@ (add 10 (sub a '0'))) (shim '0' '2'))
             ==
             sed:ab
           ==
  ++  viz  (ape (bass 0x200.0000 ;~(plug pev:ab (star ;~(pfix dog piv:ab)))))
  ++  vum  (bass 32 (plus siv:ab))
  ++  wiz  (ape (bass 0x4000.0000 ;~(plug pew:ab (star ;~(pfix dog piw:ab)))))
  --
++  mu
  |_  {top/@ bot/@}
  ++  zag  [p=(end 4 1 (add top bot)) q=bot]
  ++  zig  [p=(end 4 1 (add top (sub 0x1.0000 bot))) q=bot]
  ++  zug  (mix (lsh 4 1 top) bot)
  --
++  ne
  |_  tig/@
  ++  c  (cut 3 [tig 1] key:fa)
  ++  d  (add tig '0')
  ++  x  ?:((gte tig 10) (add tig 87) d)
  ++  v  ?:((gte tig 10) (add tig 87) d)
  ++  w  ?:(=(tig 63) '~' ?:(=(tig 62) '-' ?:((gte tig 36) (add tig 29) x)))
  --
::
::::  4k: atom printing
  ::
++  co  !.
  ~%  %co  ..co  ~
  =<  |_  lot/coin
      ++  rear  |=(rom/tape =>(.(rep rom) rend))
      ++  rent  `@ta`(rap 3 rend)
      ++  rend
        ^-  tape
        ?:  ?=($blob -.lot)
          ['~' '0' ((v-co 1) (jam p.lot))]
        ?:  ?=($many -.lot)
          :-  '.'
          |-  ^-  tape
          ?~   p.lot
            ['_' '_' rep]
          ['_' (weld (trip (wack rent(lot i.p.lot))) $(p.lot t.p.lot))]
        =+  [yed=(end 3 1 p.p.lot) hay=(cut 3 [1 1] p.p.lot)]
        |-  ^-  tape
        ?+    yed  (z-co q.p.lot)
            $c   ['~' '-' (weld (rip 3 (wood (tuft q.p.lot))) rep)]
            $d
          ?+    hay  (z-co q.p.lot)
              $a
            =+  yod=(yore q.p.lot)
            =>  ^+(. .(rep ?~(f.t.yod rep ['.' (s-co f.t.yod)])))
            =>  ^+  .
                %=    .
                    rep
                  ?:  &(=(~ f.t.yod) =(0 h.t.yod) =(0 m.t.yod) =(0 s.t.yod))
                    rep
                  =>  .(rep ['.' (y-co s.t.yod)])
                  =>  .(rep ['.' (y-co m.t.yod)])
                  ['.' '.' (y-co h.t.yod)]
                ==
            =>  .(rep ['.' (a-co d.t.yod)])
            =>  .(rep ['.' (a-co m.yod)])
            =>  .(rep ?:(a.yod rep ['-' rep]))
            ['~' (a-co y.yod)]
          ::
              $r
            =+  yug=(yell q.p.lot)
            =>  ^+(. .(rep ?~(f.yug rep ['.' (s-co f.yug)])))
            :-  '~'
            ?:  &(=(0 d.yug) =(0 m.yug) =(0 h.yug) =(0 s.yug))
              ['s' '0' rep]
            =>  ^+(. ?:(=(0 s.yug) . .(rep ['.' 's' (a-co s.yug)])))
            =>  ^+(. ?:(=(0 m.yug) . .(rep ['.' 'm' (a-co m.yug)])))
            =>  ^+(. ?:(=(0 h.yug) . .(rep ['.' 'h' (a-co h.yug)])))
            =>  ^+(. ?:(=(0 d.yug) . .(rep ['.' 'd' (a-co d.yug)])))
            +.rep
          ==
        ::
            $f
          ?:  =(& q.p.lot)
            ['.' 'y' rep]
          ?:(=(| q.p.lot) ['.' 'n' rep] (z-co q.p.lot))
        ::
            $n   ['~' rep]
            $i
          ?+  hay  (z-co q.p.lot)
            $f  ((ro-co [3 10 4] |=(a/@ ~(d ne a))) q.p.lot)
            $s  ((ro-co [4 16 8] |=(a/@ ~(x ne a))) q.p.lot)
          ==
        ::
            $p
          =+  sxz=(feen:ob q.p.lot)
          =+  dyx=(met 3 sxz)
          :-  '~'
          ?:  (lte dyx 1)
            (weld (trip (tod:po sxz)) rep)
          =+  dyy=(met 4 sxz)
          =+  imp=*@
          |-  ^-  tape
          ?:  =(imp dyy)
            rep
          %=  $
            sxz  (rsh 4 1 sxz)
            imp      +(imp)
            rep
              =+  log=(end 4 1 sxz)
              ;:  weld
                (trip (tos:po (rsh 3 1 log)))
                (trip (tod:po (end 3 1 log)))
                ?:(=((mod imp 4) 0) ?:(=(imp 0) "" "--") "-")
                rep
             ==
          ==
        ::
            $r
          ?+  hay  (z-co q.p.lot)
            $d  ['.' '~' (r-co (rlyd q.p.lot))]
            $h  ['.' '~' '~' (r-co (rlyh q.p.lot))]
            $q  ['.' '~' '~' '~' (r-co (rlyq q.p.lot))]
            $s  ['.' (r-co (rlys q.p.lot))]
          ==
        ::
            $u
          ?:  ?=($c hay)
            %+  welp  ['0' 'c' (reap (pad:fa q.p.lot) '1')]
            (c-co (enc:fa q.p.lot))
          =-  (weld p.gam ?:(=(0 q.p.lot) `tape`['0' ~] q.gam))
          ^=  gam  ^-  {p/tape q/tape}
          ?+  hay  [~ ((ox-co [10 3] |=(a/@ ~(d ne a))) q.p.lot)]
            $b  [['0' 'b' ~] ((ox-co [2 4] |=(a/@ ~(d ne a))) q.p.lot)]
            $i  [['0' 'i' ~] ((d-co 1) q.p.lot)]
            $x  [['0' 'x' ~] ((ox-co [16 4] |=(a/@ ~(x ne a))) q.p.lot)]
            $v  [['0' 'v' ~] ((ox-co [32 5] |=(a/@ ~(x ne a))) q.p.lot)]
            $w  [['0' 'w' ~] ((ox-co [64 5] |=(a/@ ~(w ne a))) q.p.lot)]
          ==
        ::
            $s
          %+  weld
            ?:((syn:si q.p.lot) "--" "-")
          $(yed 'u', q.p.lot (abs:si q.p.lot))
        ::
            $t
          ?:  =('a' hay)
            ?:  =('s' (cut 3 [2 1] p.p.lot))
              (weld (rip 3 q.p.lot) rep)
            ['~' '.' (weld (rip 3 q.p.lot) rep)]
          ['~' '~' (weld (rip 3 (wood q.p.lot)) rep)]
        ==
      --
  =+  rep=*tape
  =<  |%
      ++  a-co  |=(dat/@ ((d-co 1) dat))
      ++  c-co  (em-co [58 1] |=({? b/@ c/tape} [~(c ne b) c]))
      ++  d-co  |=(min/@ (em-co [10 min] |=({? b/@ c/tape} [~(d ne b) c])))
      ++  r-co
        |=  a/dn
        ?:  ?=({$i *} a)  (weld ?:(s.a "inf" "-inf") rep)
        ?:  ?=({$n *} a)  (weld "nan" rep)
        =+  ^=  e  %+  ed-co  [10 1]
          |=  {a/? b/@ c/tape}
          ?:  a  [~(d ne b) '.' c]
          [~(d ne b) c]
        =+  ^=  f
          =>(.(rep ~) (e a.a))
        =.  e.a  (sum:si e.a (sun:si (dec +.f)))
        =+  b=?:((syn:si e.a) "e" "e-")
        =>  .(rep ?~(e.a rep (weld b ((d-co 1) (abs:si e.a)))))
        =>  .(rep (weld -.f rep))
        ?:(s.a rep ['-' rep])
      ::
      ++  s-co
        |=  esc/(list @)  ^-  tape
        ?~  esc
          rep
        :-  '.'
        =>(.(rep $(esc t.esc)) ((x-co 4) i.esc))
      ::
      ++  v-co  |=(min/@ (em-co [32 min] |=({? b/@ c/tape} [~(v ne b) c])))
      ++  w-co  |=(min/@ (em-co [64 min] |=({? b/@ c/tape} [~(w ne b) c])))
      ++  x-co  |=(min/@ (em-co [16 min] |=({? b/@ c/tape} [~(x ne b) c])))
      ++  y-co  |=(dat/@ ((d-co 2) dat))
      ++  z-co  |=(dat/@ `tape`['0' 'x' ((x-co 1) dat)])
      --
  |%
  ++  em-co
    |=  {{bas/@ min/@} par/$-({? @ tape} tape)}
    |=  hol/@
    ^-  tape
    ?:  &(=(0 hol) =(0 min))
      rep
    =+  [rad=(mod hol bas) dar=(div hol bas)]
    %=  $
      min  ?:(=(0 min) 0 (dec min))
      hol  dar
      rep  (par =(0 dar) rad rep)
    ==
  ::
  ++  ed-co
    |=  {{bas/@ min/@} par/$-({? @ tape} tape)}
    =+  [fir=& cou=0]
    |=  hol/@
    ^-  {tape @}
    ?:  &(=(0 hol) =(0 min))
      [rep cou]
    =+  [rad=(mod hol bas) dar=(div hol bas)]
    %=  $
      min  ?:(=(0 min) 0 (dec min))
      hol  dar
      rep  (par &(=(0 dar) !fir) rad rep)
      fir  |
      cou  +(cou)
    ==
  ::
  ++  ox-co
    |=  {{bas/@ gop/@} dug/$-(@ @)}
    %+  em-co
      [|-(?:(=(0 gop) 1 (mul bas $(gop (dec gop))))) 0]
    |=  {top/? seg/@ res/tape}
    %+  weld
      ?:(top ~ `tape`['.' ~])
    %.  seg
    %+  em-co(rep res)
      [bas ?:(top 0 gop)]
    |=({? b/@ c/tape} [(dug b) c])
  ::
  ++  ro-co
    |=  {{buz/@ bas/@ dop/@} dug/$-(@ @)}
    |=  hol/@
    ^-  tape
    ?:  =(0 dop)
      rep
    =>  .(rep $(dop (dec dop)))
    :-  '.'
    %-  (em-co [bas 1] |=({? b/@ c/tape} [(dug b) c]))
    [(cut buz [(dec dop) 1] hol)]
  --
::
::::  4l: atom parsing
  ::
++  so
  ~%  %so  +  ~
  |%
  ++  bisk
    ~+
    ;~  pose
      ;~  pfix  (just '0')
        ;~  pose
          (stag %ub ;~(pfix (just 'b') bay:ag))
          (stag %uc ;~(pfix (just 'c') fim:ag))
          (stag %ui ;~(pfix (just 'i') dim:ag))
          (stag %ux ;~(pfix (just 'x') hex:ag))
          (stag %uv ;~(pfix (just 'v') viz:ag))
          (stag %uw ;~(pfix (just 'w') wiz:ag))
        ==
      ==
      (stag %ud dem:ag)
    ==
  ++  crub
    ~+
    ;~  pose
      %+  cook
        |=(det/date `dime`[%da (year det)])
      ;~  plug
        %+  cook
          |=({a/@ b/?} [b a])
        ;~(plug dim:ag ;~(pose (cold | hep) (easy &)))
        ;~(pfix dot mot:ag)   ::  month
        ;~(pfix dot dip:ag)   ::  day
        ;~  pose
          ;~  pfix
            ;~(plug dot dot)
            ;~  plug
              dum:ag
              ;~(pfix dot dum:ag)
              ;~(pfix dot dum:ag)
              ;~(pose ;~(pfix ;~(plug dot dot) (most dot qix:ab)) (easy ~))
            ==
          ==
          (easy [0 0 0 ~])
        ==
      ==
    ::
      %+  cook
        |=  {a/(list {p/?($d $h $m $s) q/@}) b/(list @)}
        =+  rop=`tarp`[0 0 0 0 b]
        |-  ^-  dime
        ?~  a
          [%dr (yule rop)]
        ?-  p.i.a
          $d  $(a t.a, d.rop (add q.i.a d.rop))
          $h  $(a t.a, h.rop (add q.i.a h.rop))
          $m  $(a t.a, m.rop (add q.i.a m.rop))
          $s  $(a t.a, s.rop (add q.i.a s.rop))
        ==
      ;~  plug
        %+  most
          dot
        ;~  pose
          ;~(pfix (just 'd') (stag %d dim:ag))
          ;~(pfix (just 'h') (stag %h dim:ag))
          ;~(pfix (just 'm') (stag %m dim:ag))
          ;~(pfix (just 's') (stag %s dim:ag))
        ==
        ;~(pose ;~(pfix ;~(plug dot dot) (most dot qix:ab)) (easy ~))
      ==
    ::
      (stag %p fed:ag)
      ;~(pfix dot (stag %ta urs:ab))
      ;~(pfix sig (stag %t urx:ab))
      ;~(pfix hep (stag %c (cook turf urx:ab)))
    ==
  ++  nuck
    ~/  %nuck  |=  a/nail  %.  a
    %+  knee  *coin  |.  ~+
    %-  stew
    ^.  stet  ^.  limo
    :~  :-  ['a' 'z']  (cook |=(a/@ta [%$ %tas a]) sym)
        :-  ['0' '9']  (stag %$ bisk)
        :-  '-'        (stag %$ tash)
        :-  '.'        ;~(pfix dot perd)
        :-  '~'        ;~(pfix sig ;~(pose twid (easy [%$ %n 0])))
    ==
  ++  nusk
    ~+
    :(sear |=(a/@ta (rush a nuck)) wick urt:ab)
  ++  perd
    ~+
    ;~  pose
      (stag %$ zust)
      (stag %many (ifix [cab ;~(plug cab cab)] (more cab nusk)))
    ==
  ++  royl
    ~+
    =+  ^=  moo
      |=  a/tape
      :-  (lent a)
      (scan a (bass 10 (plus sid:ab)))
    =+  ^=  voy
      %+  cook  royl-cell
      ;~  pose
        ;~  plug
          (easy %d)
          ;~  pose  (cold | hep)  (easy &)  ==
          ;~  plug  dim:ag
            ;~  pose
              ;~(pfix dot (cook moo (plus (shim '0' '9'))))
              (easy [0 0])
            ==
            ;~  pose
              ;~  pfix
                (just 'e')
                ;~(plug ;~(pose (cold | hep) (easy &)) dim:ag)
              ==
              (easy [& 0])
            ==
          ==
        ==
        ;~  plug
          (easy %i)
          ;~  sfix
            ;~  pose  (cold | hep)  (easy &)  ==
            (jest 'inf')
          ==
        ==
        ;~  plug
          (easy %n)
          (cold ~ (jest 'nan'))
        ==
      ==
    ;~  pose
      (stag %rh (cook rylh ;~(pfix ;~(plug sig sig) voy)))
      (stag %rq (cook rylq ;~(pfix ;~(plug sig sig sig) voy)))
      (stag %rd (cook ryld ;~(pfix sig voy)))
      (stag %rs (cook ryls voy))
    ==
  ::
  ++  royl-cell
    |=  rn
    ^-  dn
    ?.  ?=({$d *} +<)  +<
    =+  ^=  h
      (dif:si (new:si f.b i.b) (sun:si d.b))
    [%d a h (add (mul c.b (pow 10 d.b)) e.b)]
  ::
  ++  tash
    ~+
    =+  ^=  neg
        |=  {syn/? mol/dime}  ^-  dime
        ?>  =('u' (end 3 1 p.mol))
        [(cat 3 's' (rsh 3 1 p.mol)) (new:si syn q.mol)]
    ;~  pfix  hep
      ;~  pose
        (cook |=(a/dime (neg | a)) bisk)
        ;~(pfix hep (cook |=(a/dime (neg & a)) bisk))
      ==
    ==
  ::
  ++  twid
    ~+
    ;~  pose
      (cook |=(a/@ [%blob (cue a)]) ;~(pfix (just '0') vum:ag))
      (stag %$ crub)
    ==
  ::
  ++  zust
    ~+
    ;~  pose
      (stag %is bip:ag)
      (stag %if lip:ag)
      royl
      (stag %f ;~(pose (cold & (just 'y')) (cold | (just 'n'))))
    ==
  --
::
::::  4m: formatting functions
  ::
++  scot  |=(mol/dime ~(rent co %$ mol))
++  scow  |=(mol/dime ~(rend co %$ mol))
++  slat  |=(mod/@tas |=(txt/@ta (slaw mod txt)))
++  slav  |=({mod/@tas txt/@ta} (need (slaw mod txt)))
++  slaw
  ~/  %slaw
  |=  {mod/@tas txt/@ta}
  ^-  (unit @)
  =+  con=(slay txt)
  ?.(&(?=({$~ $$ @ @} con) =(p.p.u.con mod)) ~ [~ q.p.u.con])
::
++  slay
  |=  txt/@ta  ^-  (unit coin)
  =+  ^=  vex
      ?:  (gth 0x7fff.ffff txt)                         ::  XX  petty cache
        ~+  ((full nuck:so) [[1 1] (trip txt)])
      ((full nuck:so) [[1 1] (trip txt)])
  ?~  q.vex
    ~
  [~ p.u.q.vex]
::
++  smyt                                                ::  pretty print path
  |=  bon/path  ^-  tank
  :+  %rose  [['/' ~] ['/' ~] ~]
  (turn bon |=(a/@ [%leaf (trip a)]))
::
++  spat  |=(pax/path (crip (spud pax)))                ::  render path to cord
++  spud  |=(pax/path ~(ram re (smyt pax)))             ::  render path to tape
++  stab                                                ::  parse cord to path
  =+  fel=;~(pfix fas (more fas urs:ab))
  |=(zep/@t `path`(rash zep fel))
::
::::  4n: virtualization
  ::
++  mack
  |=  {sub/* fol/*}
  ^-  (unit)
  =+  ton=(mink [sub fol] |=({* *} ~))
  ?.(?=({$0 *} ton) ~ [~ p.ton])
::
++  mink
  ~/  %mink
  |=  {{sub/* fol/*} gul/$-({* *} (unit (unit)))}
  =+  tax=*(list {@ta *})
  |-  ^-  tone
  ?@  fol
    [%2 tax]
  ?:  ?=(^ -.fol)
    =+  hed=$(fol -.fol)
    ?:  ?=($2 -.hed)
      hed
    =+  tal=$(fol +.fol)
    ?-  -.tal
      $0  ?-(-.hed $0 [%0 p.hed p.tal], $1 hed)
      $1  ?-(-.hed $0 tal, $1 [%1 (weld p.hed p.tal)])
      $2  tal
    ==
  ?+    fol
    [%2 tax]
  ::
      {$0 b/@}
    ?:  =(0 b.fol)  [%2 tax]
    ?:  =(1 b.fol)  [%0 sub]
    ?:  ?=(@ sub)   [%2 tax]
    =+  [now=(cap b.fol) lat=(mas b.fol)]
    $(b.fol lat, sub ?:(=(2 now) -.sub +.sub))
  ::
      {$1 b/*}
    [%0 b.fol]
  ::
      {$2 b/{^ *}}
    =+  ben=$(fol b.fol)
    ?.  ?=($0 -.ben)  ben
    ?>(?=(^ p.ben) $(sub -.p.ben, fol +.p.ben))
    ::?>(?=(^ p.ben) $([sub fol] p.ben)
  ::
      {$3 b/*}
    =+  ben=$(fol b.fol)
    ?.  ?=($0 -.ben)  ben
    [%0 .?(p.ben)]
  ::
      {$4 b/*}
    =+  ben=$(fol b.fol)
    ?.  ?=($0 -.ben)  ben
    ?.  ?=(@ p.ben)  [%2 tax]
    [%0 .+(p.ben)]
  ::
      {$5 b/*}
    =+  ben=$(fol b.fol)
    ?.  ?=($0 -.ben)  ben
    ?.  ?=(^ p.ben)  [%2 tax]
    [%0 =(-.p.ben +.p.ben)]
  ::
      {$6 b/* c/* d/*}
    $(fol =>(fol [2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]))
  ::
      {$7 b/* c/*}       $(fol =>(fol [2 b 1 c]))
      {$8 b/* c/*}       $(fol =>(fol [7 [[7 [0 1] b] 0 1] c]))
      {$9 b/* c/*}       $(fol =>(fol [7 c 2 [0 1] 0 b]))
      {$10 @ c/*}        $(fol c.fol)
      {$10 {b/* c/*} d/*}
    =+  ben=$(fol c.fol)
    ?.  ?=($0 -.ben)  ben
    ?:  ?=(?($hunk $hand $lose $mean $spot) b.fol)
      $(fol d.fol, tax [[b.fol p.ben] tax])
    $(fol d.fol)
  ::
      {$11 b/* c/*}
    =+  ref=$(fol b.fol)
    =+  ben=$(fol c.fol)
    ?.  ?=($0 -.ref)  ref
    ?.  ?=($0 -.ben)  ben
    =+  val=(gul p.ref p.ben)
    ?~(val [%1 p.ben ~] ?~(u.val [%2 [[%hunk (mush p.ben)] tax]] [%0 u.u.val]))
  ==
::
++  mock
  |=  {{sub/* fol/*} gul/$-({* *} (unit (unit)))}
  (mook (mink [sub fol] gul))
::
++  musk                                                ::  nock with block set
  =>  |%
      ++  block  
        ::  identity of resource awaited
        ::  XX parameterize
        noun
      ::
      ++  result  
        ::  internal interpreter result
        ::
        $@(~ seminoun)
      ::
      ++  seminoun  
        ::  partial noun; blocked subtrees are ~ 
        ::
        {mask/stencil data/noun}
      ::
      ++  stencil  
        ::  noun knowledge map
        ::
        $%  ::  no; noun has partial block substructure
            ::
            {$| left/stencil rite/stencil}
            ::  yes; noun is either fully complete, or fully blocked
            ::
            {$& blocks/(set block)}
        == 
      ::
      ++  output
        ::  nil; interpreter stopped
        ::
        %-  unit
        ::  yes, complete noun; no, list of blocks
        ::
        (each noun (list block))
      -- 
  |%  
  ++  abet
    ::  simplify raw result
    ::
    |=  $:  ::  noy: raw result
            ::
            noy/result
        ==
    ^-  output
    ::  propagate stop
    ::
    ?~  noy  ~
    :-  ~
    ::  merge all blocking sets
    ::
    =/  blocks  (squash mask.noy) 
    ?:  =(~ blocks)
      ::  no blocks, data is complete
      ::
      &+data.noy
    ::  reduce block set to block list
    ::
    |+~(tap in blocks)
  ::
  ++  apex
    ::  execute nock on partial subject
    ::
    |=  $:  ::  bus: subject, a partial noun
            ::  fol: formula, a complete noun
            ::
            bus/seminoun
            fol/noun
        ==
    ^-  output
    ::  simplify result
    ::
    %-  abet
    ::  interpreter loop
    ::
    |-  ^-  result
    ?@  fol  
      ::  bad formula, stop
      ::
      ~
    ?:  ?=(^ -.fol)  
      ::  hed: interpret head
      ::
      =+  hed=$(fol -.fol)
      ::  propagate stop
      ::
      ?~  hed  ~
      ::  tal: interpret tail
      ::
      =+  tal=$(fol +.fol)
      ::  propagate stop
      ::
      ?~  tal  ~
      ::  combine 
      ::
      (combine hed tal)
    ?+    fol  
    ::  bad formula; stop
    ::
        ~
    ::  0; fragment
    ::
        {$0 b/@}
      ::  if bad axis, stop
      ::
      ?:  =(0 b.fol)  ~
      ::  reduce to fragment
      ::
      (fragment b.fol bus)
    ::
    ::  1; constant
    ::
        {$1 b/*}
      ::  constant is complete
      ::
      [&+~ b.fol]
    ::
    ::  2; recursion
    ::
        {$2 b/* c/*}
      ::  require complete formula
      ::
      %+  require
        ::  compute formula with current subject
        ::
        $(fol c.fol)
      |=  ::  ryf: next formula
          ::
          ryf/noun
      ::  lub: next subject
      ::
      =+  lub=^$(fol b.fol)
      ::  propagate stop
      ::
      ?~  lub  ~
      ::  recurse
      ::
      ^$(fol ryf, bus lub)
    ::
    ::  3; probe
    ::
        {$3 b/*}
      %+  require
        $(fol b.fol)
      |=  ::  fig: probe input
          ::
          fig/noun
      ::  yes if cell, no if atom
      ::
      [&+~ .?(fig)]
    ::
    ::  4; increment
    ::
        {$4 b/*}
      %+  require
        $(fol b.fol)
      |=  ::  fig: increment input
          ::
          fig/noun
      ::  stop for cells, increment for atoms
      ::
      ?^(fig ~ [&+~ +(fig)])
    ::
    ::  5; compare
    ::
        {$5 b/*}
      %+  require
        $(fol b.fol)
      |=  ::  fig: operator input
          ::
          fig/noun
      ::  stop for atoms, compare cells
      ::
      ?@(fig ~ [&+~ =(-.fig +.fig)])
    ::
    ::  6; if-then-else
    ::
        {$6 b/* c/* d/*}
      ::  use standard macro expansion (slow)
      ::
      $(fol =>(fol [2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]))
    ::
    ::  7; composition
    ::
        {$7 b/* c/*}       
      ::  use standard macro expansion (slow)
      ::
      $(fol =>(fol [2 b 1 c]))
    ::
    ::  8; declaration
    ::
        {$8 b/* c/*}       
      ::  use standard macro expansion (slow)
      ::
      $(fol =>(fol [7 [[7 [0 1] b] 0 1] c]))
    ::
    ::  9; invocation
    ::
        {$9 b/* c/*}       
      ::  use standard macro expansion (slow)
      ::
      $(fol =>(fol [7 c 2 [0 1] 0 b]))
    ::
    ::  10; static hint
    ::
        {$10 @ c/*}        
      ::  ignore hint
      ::
      $(fol c.fol)
    ::
    ::  10; dynamic hint
    ::
        {$10 {b/* c/*} d/*}
      ::  noy: dynamic hint
      ::
      =+  noy=$(fol c.fol)
      ::  propagate stop
      ::
      ?~  noy  ~
      ::  otherwise, ignore hint
      ::
      $(fol d.fol)
    ==
  ::
  ++  combine
    ::  combine a pair of seminouns
    ::
    |=  $:  ::  hed: head of pair
            ::  tal: tail of pair
            ::
            hed/seminoun 
            tal/seminoun
        ==
    ^-  seminoun
    ?.  ?&  &(?=($& -.mask.hed) ?=($& -.mask.tal))
            =(=(~ blocks.mask.hed) =(~ blocks.mask.tal))
        ==
      ::  default merge
      ::
      [|+[mask.hed mask.tal] [data.hed data.tal]]
    ::  both sides total
    ::
    ?:  =(~ blocks.mask.hed)
      ::  both sides are complete
      ::
      [&+~ data.hed data.tal]
    ::  both sides are blocked
    ::
    [&+(~(uni in blocks.mask.hed) blocks.mask.tal) ~]
  ::
  ++  fragment
    ::  seek to an axis in a seminoun
    ::
    |=  $:  ::  axe: tree address of subtree
            ::  bus: partial noun
            ::
            axe/axis
            bus/seminoun
        ==
    ^-  result
    ::  1 is the root
    ::
    ?:  =(1 axe)  bus
    ::  now: 2 or 3, top of axis
    ::  lat: rest of axis
    ::
    =+  [now=(cap axe) lat=(mas axe)]
    ?-  -.mask.bus
    ::  subject is fully blocked or complete
    ::
      $&  ::  if fully blocked, produce self
          ::
          ?^  blocks.mask.bus  bus
          ::  descending into atom, stop
          ::
          ?@  data.bus  ~
          ::  descend into complete cell
          ::
          $(axe lat, bus [&+~ ?:(=(2 now) -.data.bus +.data.bus)])
    ::  subject is partly blocked
    ::
      $|  ::  descend into partial cell
          ::
          %=  $
            axe  lat
            bus  ?:  =(2 now) 
                   [left.mask.bus -.data.bus] 
                 [rite.mask.bus +.data.bus]
    ==    ==
  ::  require complete intermediate step
  ::
  ++  require
    |=  $:  noy/result
            yen/$-(noun result)
        ==
    ^-  result
    ::  propagate stop
    ::
    ?~  noy  ~
    ::  if partial block, squash blocks and stop
    ::
    ?:  ?=($| -.mask.noy)  [&+(squash mask.noy) ~]
    ::  if full block, propagate block
    ::
    ?:  ?=(^ blocks.mask.noy)  [mask.noy ~]
    ::  otherwise use complete noun
    ::
    (yen data.noy)
  ::
  ++  squash
    ::  convert stencil to block set
    ::
    |=  tyn/stencil
    ^-  (set block)
    ?-  -.tyn
      $&  blocks.tyn
      $|  (~(uni in $(tyn left.tyn)) $(tyn rite.tyn))
    ==
  --
++  mook
  |=  ton/tone
  ^-  toon
  ?.  ?=({$2 *} ton)  ton
  :-  %2
  :: =.  p.ton  (moop p.ton)
  =+  yel=(lent p.ton)
  =.  p.ton
    ?.  (gth yel 256)  p.ton
    %+  weld
      (scag 128 p.ton)
    ^-  (list {@ta *})
    :_  (slag (sub yel 128) p.ton)
    :-  %lose
    %+  rap  3
    "[skipped {(scow %ud (sub yel 256))} frames]"
  |-  ^-  (list tank)
  ?~  p.ton  ~
  =+  rep=$(p.ton t.p.ton)
  ?+    -.i.p.ton  rep
      $hunk  [(tank +.i.p.ton) rep]
      $lose  [[%leaf (rip 3 (@ +.i.p.ton))] rep]
      $hand  [[%leaf (scow %p (mug +.i.p.ton))] rep]
      $mean  :_  rep
             ?@  +.i.p.ton  [%leaf (rip 3 (@ +.i.p.ton))]
             =+  mac=(mack +.i.p.ton +<.i.p.ton)
             ?~(mac [%leaf "####"] (tank u.mac))
      $spot  :_  rep
             =+  sot=(spot +.i.p.ton)
             :+  %rose  [":" ~ ~]
             :~  (smyt p.sot)
                 =>  [ud=|=(a/@u (scow %ud a)) q.sot]
                 leaf+"<[{(ud p.p)} {(ud q.p)}].[{(ud p.q)} {(ud q.q)}]>"
  ==         ==
::
++  mush                                                ::  sane name to leaf
  |=  val/*
  ^-  tank
  :+  %rose
    [['/' ~] ['/' ~] ~]
  (turn ((list @ta) val) |=(a/@ta [%leaf (trip a)]))
::
++  mong
  |=  {{gat/* sam/*} gul/$-({* *} (unit (unit)))}
  ^-  toon
  ?.  &(?=(^ gat) ?=(^ +.gat))
    [%2 ~]
  (mock [[-.gat [sam +>.gat]] -.gat] gul)
::
++  mule                                                ::  typed virtual
  ~/  %mule
  |*  taq/_|.(**)
  =+  mud=(mute taq)
  ?-  -.mud
    $&  [%& p=$:taq]                                    ::  XX transition
    $|  [%| p=p.mud]
  ==
::
++  mute                                                ::  untyped virtual
  |=  taq/_^?(|.(**))
  ^-  (each * (list tank))
  =+  ton=(mock [taq 9 2 0 1] |=({* *} ~))
  ?-  -.ton
    $0  [%& p.ton]
    $1  [%| (turn p.ton |=(a/* (smyt (path a))))]
    $2  [%| p.ton]
  ==
::
::::  4o: molds and mold builders
  ::
++  abel  typo                                          ::  original sin: type
++  alas  (list (pair term hoon))                       ::  alias list
++  atom  @                                             ::  just an atom
++  aura  @ta                                           ::  atom format
++  axis  @                                             ::  tree address
++  base                                                ::  base mold
  $@  $?  $noun                                         ::  any noun
          $cell                                         ::  any cell
          $bean                                         ::  loobean
          $void                                         ::  no nouns
          $null                                         ::  ~ == 0
      ==                                                ::
  {$atom p/aura}                                        ::  atom
::
++  bean  ?                                             ::  0=&=yes, 1=|=no
++  woof  $@(@ {$~ p/hoon})                             ::  simple embed
++  beet  $@  @                                         ::  advanced embed
          $%  {$a p/hoon}                               ::  take tape
              {$b p/hoon}                               ::  take manx
              {$c p/hoon}                               ::  take marl
              {$d p/hoon}                               ::  take $-(marl marl)
              {$e p/hoon q/(list tuna)}                 ::  element literal
          ==                                            ::
++  chap  (pair (unit term) what)                       ::  labeled help
++  chum  $?  lef/term                                  ::  jet name
              {std/term kel/@}                          ::  kelvin version
              {ven/term pro/term kel/@}                 ::  vendor and product
              {ven/term pro/term ver/@ kel/@}           ::  all of the above
          ==                                            ::
++  coil  $:  p/?($gold $iron $lead $zinc)              ::  core type
              q/type                                    ::  built with
              r/chap                                    ::  docs
              s/{p/?($~ ^) q/(map @ tomb)}              ::  arms
          ==                                            ::
++  foot  $%  {$ash p/hoon}                             ::  dry arm, geometric
              {$elm p/hoon}                             ::  wet arm, generic
          ==                                            ::
++  limb  $@  term                                      ::  wing element
          $%  {$& p/axis}                               ::  by geometry
              {$| p/@ud q/(unit term)}                  ::  by name
          ==                                            ::
++  line  {p/{$leaf p/aura q/@} q/tile}                 ::  %bccn case
++  metl  ?($gold $iron $zinc $lead)                    ::  core variance
++  noun  *                                             ::  any noun
++  null  $~                                            ::  null, nil, etc
++  onyx  (list (pair type foot))                       ::  arm activation
++  opal                                                ::  limb match
          $%  {$& p/type}                               ::  leg
              {$| p/axis q/(set {p/type q/foot})}       ::  arm
          ==                                            ::
++  pica  (pair ? cord)                                 ::  & prose, | code
++  palo  (pair vein opal)                              ::  wing trace, match
++  pock  (pair axis nock)                              ::  changes
++  port  (each palo (pair type nock))                  ::  successful match
++  root  hoon                                          ::  produce model
++  tent                                                ::  model builder
          $%  {$| p/wing q/tent r/(list tile)}          ::  ~(p q r...)
              {$& p/(list wing)}                        ::  a.b:c.d
          ==                                            ::
++  tiki                                                ::  test case
          $%  {$& p/(unit term) q/wing}                 ::  simple wing
              {$| p/(unit term) q/hoon}                 ::  named wing
          ==                                            ::
++  tile  $^  {p/tile q/tile}                           ::  ordered pair
          $%  {$axil p/base}                            ::  base type
              {$bark p/toga q/tile}                     ::  name
              {$deet p/spot q/tile}                     ::  set debug
              {$fern p/{i/tile t/(list tile)}}          ::  plain selection
              {$herb p/hoon}                            ::  assembly
              {$kelp p/{i/line t/(list line)}}          ::  tag selection
              {$leaf p/term q/@}                        ::  constant atom
              {$plow p/what q/tile}                     ::  apply help
              {$reed p/tile q/tile}                     ::  atom+cell
              {$vine p/tile q/tile}                     ::  pair+tag
              {$weed p/hoon}                            ::  example
          ==                                            ::
++  toga                                                ::  face control
          $@  p/term                                    ::  two togas
          $%  {$0 $~}                                   ::  no toga
              {$1 p/(pair what term) q/toga}            ::  deep toga
              {$2 p/toga q/toga}                        ::  cell toga
          ==                                            ::
++  tomb  (pair chap (map term (pair what foot)))       ::  core chapter
++  tope                                                ::  topographic type
  $@  $?  %&                                            ::  cell or atom
          %|                                            ::  atom
      ==                                                ::
  (pair tope tope)                                      ::  cell
++  tuna                                                ::  tagflow
          $%  {$a p/hoon}                               ::  plain text
              {$b p/hoon}                               ::  single tag
              {$c p/hoon}                               ::  simple list
              {$d p/hoon}                               ::  dynamic list
              {$e p/hoon q/(list tuna)}                 ::  element
              {$f p/(list tuna)}                        ::  subflow
          ==                                            ::
++  hoon                                                ::
  =>  |%                                                ::REVIEW
      ++  beer  $@(char {$~ p/hoon})                    ::  simple embed
      ++  mane  $@(@tas {@tas @tas})                    ::  XML name+space
      ++  manx  {g/marx c/marl}                         ::  dynamic XML node
      ++  marl  (list tuna)                             ::  dynamic XML nodes
      ++  mart  (list {n/mane v/(list beer)})           ::  dynamic XML attrs
      ++  marx  {n/mane a/mart}                         ::  dynamic XML tag
      ++  mare  (each manx marl)                        ::  node or nodes
      ++  maru  (each tuna marl)                        ::  interp or nodes
      ++  tuna                                          ::  maybe interpolation
          $^(manx {?($tape $manx $marl $call) p/hoon})  ::
      --                                                ::
  $^  {p/hoon q/hoon}                                   ::
  $%                                                    ::
    {$$ p/axis}                                         ::  simple leg
  ::                                                    ::
    {$base p/base}                                      ::  base
    {$bunt p/root}                                      ::  mold default value
    {$bust p/base}                                      ::  bunt base
    {$cold p/hoon}                                      ::  fold constant
    {$dbug p/spot q/hoon}                               ::  debug info in trace
    {$eror p/tape}                                      ::  assembly error
    {$hand p/type q/nock}                               ::  premade result
    {$docs p/(pair cord (list sect)) q/hoon}            ::  annotate image
    {$fits p/hoon q/wing}                               ::  underlying ?=
    {$halo p/what q/root}                               ::  annotate model
    {$knit p/(list woof)}                               ::  assemble string
    {$leaf p/(pair term @)}                             ::  symbol
    {$limb p/term}                                      ::  pulls limb p
    {$lost p/hoon}                                      ::  not to be taken
    {$rock p/term q/*}                                  ::  fixed constant
    {$sand p/term q/*}                                  ::  unfixed constant
    {$tell p/(list hoon)}                               ::  render as tape
    {$tune p/(pair what $@(term tune))}                 ::  minimal face
    {$wing p/wing}                                      ::  pulls p
    {$yell p/(list hoon)}                               ::  render as tank
    {$xray p/manx}                                       ::  ;foo; templating
  ::                                            ::::::  molds
    {$bcpt p/root q/root}                               ::  $@ depth fork
    {$bccb p/hoon}                                      ::  $_ example
    {$bccl p/(list root)}                               ::  $: tuple
    {$bccn p/(list root)}                               ::  $% tagged fork
    {$bchp p/root q/root}                               ::  $- function
    {$bckt p/root q/root}                               ::  $^ pairhead fork
    {$bcwt p/(list root)}                               ::  $? untagged fork
    {$bcts p/toga q/root}                               ::  $= name
    {$bcsm p/hoon}                                      ::  $; assembly
  ::                                            ::::::  cores
    {$brcb p/chap q/root r/alas s/(map @ tomb)}         ::  |_
    {$brcl p/chap q/hoon r/hoon}                        ::  |:
    {$brcn p/chap q/(map @ tomb)}                       ::  |%
    {$brdt p/chap q/hoon}                               ::  |.
    {$brkt p/chap q/hoon r/(map @ tomb)}                ::  |^
    {$brhp p/chap q/hoon}                               ::  |-
    {$brsg p/chap q/hoon r/hoon}                        ::  |~
    {$brtr p/chap q/root r/hoon}                        ::  |*
    {$brts p/chap q/root r/hoon}                        ::  |=
    {$brwt p/chap q/hoon}                               ::  |?
  ::                                            ::::::  tuples
    {$clcb p/hoon q/hoon}                                ::  :_ [q p]
    {$clkt p/hoon q/hoon r/hoon s/hoon}                  ::  :^ [p q r s]
    {$clhp p/hoon q/hoon}                                ::  :- [p q]
    {$clls p/hoon q/hoon r/hoon}                         ::  :+ [p q r]
    {$clsg p/(list hoon)}                                ::  :~ [p ~]
    {$cltr p/(list hoon)}                                ::  :* p as a tuple
  ::                                            ::::::  invocations
    {$cncb p/wing q/(list (pair wing hoon))}            ::  %_
    {$cndt p/hoon q/hoon}                               ::  %.
    {$cnhp p/hoon q/(list hoon)}                        ::  %-
    {$cntr p/wing q/hoon r/(list (pair wing hoon))}     ::  %*
    {$cnkt p/hoon q/hoon r/hoon s/hoon}                 ::  %^
    {$cnls p/hoon q/hoon r/hoon}                        ::  %+
    {$cnsg p/wing q/hoon r/(list hoon)}                 ::  %~
    {$cnts p/wing q/(list (pair wing hoon))}            ::  %=
  ::                                            ::::::  nock
    {$dtkt p/root q/hoon}                               ::  .^  nock 11
    {$dtls p/hoon}                                      ::  .+  nock 4
    {$dttr p/hoon q/hoon}                               ::  .*  nock 2
    {$dtts p/hoon q/hoon}                               ::  .=  nock 5
    {$dtwt p/hoon}                                      ::  .?  nock 3
  ::                                            ::::::  type conversion
    {$ktbr p/hoon}                                      ::  ^|
    {$ktcn p/hoon}                                      ::  ^%  enter test mode
    {$ktdt p/hoon q/hoon}                               ::  ^.
    {$ktls p/hoon q/hoon}                               ::  ^+
    {$kthp p/root q/hoon}                               ::  ^-
    {$ktpm p/hoon}                                      ::  ^&
    {$ktsg p/hoon}                                      ::  ^~
    {$ktts p/toga q/hoon}                               ::  ^=
    {$ktwt p/hoon}                                      ::  ^?
  ::                                            ::::::  hints
    {$sgbr p/hoon q/hoon}                               ::  ~|  sell on trace
    {$sgcb p/hoon q/hoon}                               ::  ~_  tank on trace
    {$sgcn p/chum q/hoon r/tyre s/hoon}                 ::  ~%  general jet hint
    {$sgfs p/chum q/hoon}                               ::  ~/  function j-hint
    {$sggl p/$@(term {p/term q/hoon}) q/hoon}           ::  ~<  backward hint
    {$sggr p/$@(term {p/term q/hoon}) q/hoon}           ::  ~>  forward hint
    {$sgbc p/term q/hoon}                               ::  ~$  profiler hit
    {$sgls p/@ q/hoon}                                  ::  ~+  cache/memoize
    {$sgpm p/@ud q/hoon r/hoon}                         ::  ~&  printf/priority
    {$sgts p/hoon q/hoon}                               ::  ~=  don't duplicate
    {$sgwt p/@ud q/hoon r/hoon s/hoon}                  ::  ~?  tested printf
    {$sgzp p/hoon q/hoon}                               ::  ~!  type on trace
  ::                                            ::::::  miscellaneous
    {$smts p/marl}                                       ::  ;=  list templating
    {$smcl p/hoon q/(list hoon)}                         ::  ;:  binary to nary
    {$smfs p/hoon}                                       ::  ;/  [%$ [%$ p ~] ~]
    {$smsg p/hoon q/(list hoon)}                         ::  ;~  kleisli arrow
    {$smsm p/hoon q/hoon}                                ::  ;;  normalize
  ::                                            ::::::  compositions
    {$tsbr p/root q/hoon}                                ::  =|  push bunt
    {$tscl p/(list (pair wing hoon)) q/hoon}             ::  =:  q w/ p changes
    {$tsfs p/toro q/hoon r/hoon}                         ::  =/  typed variable
    {$tssm p/toro q/hoon r/hoon}                         ::  =;  =/(q p r)
    {$tsdt p/wing q/hoon r/hoon}                         ::  =.  r with p as q
    {$tswt p/wing q/hoon r/hoon s/hoon}                  ::  =?  conditional =.
    {$tsgl p/hoon q/hoon}                                ::  =<  =>(q p)
    {$tshp p/hoon q/hoon}                                ::  =-  =+(q p)
    {$tsgr p/hoon q/hoon}                                ::  =>  q w/subject p
    {$tskt p/toro q/wing r/hoon s/hoon}                  ::  =^  state machine
    {$tsls p/hoon q/hoon}                                ::  =+  q w/[p subject]
    {$tssg p/(list hoon)}                                ::  =~  hoon stack
    {$tstr p/(pair what term) q/hoon r/hoon}             ::  =*  r w/alias p/q
    {$tscm p/hoon q/hoon}                                ::  =,  overload p in q
  ::                                            ::::::  conditionals
    {$wtbr p/(list hoon)}                                 ::  ?|  loobean or
    {$wthp p/wing q/(list (pair root hoon))}            ::  ?-  pick case in q
    {$wtcl p/hoon q/hoon r/hoon}                          ::  ?:  if/then/else
    {$wtdt p/hoon q/hoon r/hoon}                        ::  ?.  ?:(p r q)
    {$wtkt p/wing q/hoon r/hoon}                        ::  ?^  if p is a cell
    {$wtgl p/hoon q/hoon}                               ::  ?<  ?:(p !! q)
    {$wtgr p/hoon q/hoon}                               ::  ?>  ?:(p q !!)
    {$wtls p/wing q/hoon r/(list (pair root hoon))}     ::  ?+  ?-  w/default
    {$wtpm p/(list hoon)}                                ::  ?&  loobean and
    {$wtpt p/wing q/hoon r/hoon}                        ::  ?@  if p is atom
    {$wtsg p/wing q/hoon r/hoon}                        ::  ?~  if p is null
  ::
    {$wtts p/root q/wing}                               ::  ?=  if q matches p
    {$wtzp p/hoon}                                       ::  ?!  loobean not
  ::                                            ::::::  special
    {$zpcm p/hoon q/hoon}                               ::  !,
    {$zpgr p/hoon}                                      ::  !>
    {$zpsm p/hoon q/hoon}                               ::  !;
    {$zpts p/hoon}                                      ::  !=
    {$zpwt p/$@(p/@ {p/@ q/@}) q/hoon}                  ::  !?
    {$zpzp $~}                                          ::  !!
  ==                                                    ::
++  toro  (pair toga (unit root))                       ::
++  twit  hoon                                          ::  last-gen hoon
++  tyre  (list {p/term q/hoon})                        ::
++  tyke  (list (unit hoon))                            ::
::                                                      ::::::  virtual nock
++  nock  $^  {p/nock q/nock}                           ::  autocons
          $%  {$0 p/@}                                  ::  axis select
              {$1 p/*}                                  ::  constant
              {$2 p/nock q/nock}                        ::  compose
              {$3 p/nock}                               ::  cell test
              {$4 p/nock}                               ::  increment
              {$5 p/nock q/nock}                        ::  equality test
              {$6 p/nock q/nock r/nock}                 ::  if, then, else
              {$7 p/nock q/nock}                        ::  serial compose
              {$8 p/nock q/nock}                        ::  push onto subject
              {$9 p/@ q/nock}                           ::  select arm and fire
              {$10 p/$@(@ {p/@ q/nock}) q/nock}         ::  hint
              {$11 p/nock q/nock}                       ::  grab data from sky
          ==                                            ::
++  type  $@  $?  $noun                                 ::  any nouns
                  $void                                 ::  no noun
              ==                                        ::
          $%  {$atom p/term q/(unit @)}                 ::  atom / constant
              {$cell p/type q/type}                     ::  ordered pair
              {$core p/type q/coil}                     ::  object
              {$face p/{p/what q/$@(term tune)} q/type} ::  namespace (new)
              {$fork p/(set type)}                      ::  union
              {$help p/writ q/type}                     ::  description
              {$hold p/type q/hoon}                     ::  lazy evaluation
          ==                                            ::
++  tyro  $-(type type)                                 ::  type converter
++  tone  $%  {$0 p/*}                                  ::  success
              {$1 p/(list)}                             ::  blocks
              {$2 p/(list {@ta *})}                     ::  error ~_s
          ==                                            ::
++  tony                                                ::  ++tone done right
          $%  {$0 p/tine q/*}                           ::  success
              {$1 p/(set)}                              ::  blocks
              {$2 p/(list {@ta *})}                     ::  error ~_s
          ==                                            ::
++  tine                                                ::  partial noun
          $@  $~                                        ::  open
          $%  {$& p/tine q/tine}                        ::  half-blocked
              {$| p/(set)}                              ::  fully blocked
          ==                                            ::
++  tool  $@(term tune)                                 ::  type decoration
++  tune                                                ::  complex
          $:  p/(map term (pair what (unit hoon)))      ::  aliases
              q/(list hoon)                             ::  bridges
          ==                                            ::
++  typo  type                                          ::  old type
++  vase  {p/type q/*}                                  ::  type-value pair
++  vise  {p/typo q/*}                                  ::  old vase
++  vial  ?($read $rite $both $free)                    ::  co/contra/in/bi
++  vair  ?($gold $iron $lead $zinc)                    ::  in/contra/bi/co
++  vein  (list (unit axis))                            ::  search trace
++  sect  (list pica)                                   ::  paragraph
++  whit                                                ::  
          $:  lab/(unit term)                           ::  label
              boy/(unit (pair cord (list sect)))        ::  body
              def/(map term (pair cord (list sect)))    ::  definitions
              use/(set term)                            ::  defs used
          ==                                            ::
++  what  (unit (pair cord (list sect)))                ::  help slogan/section
++  wing  (list limb)                                   ::  search path
++  worm                                                ::  compiler cache
  $:  nes/(set ^)                                       ::  ++nest
      pay/(map (pair type hoon) type)                   ::  ++play
      mit/(map (pair type hoon) (pair type nock))       ::  ++mint
  ==                                                    ::
++  writ                                                ::  type annotation
          $%  {$docs p/cord q/(list sect)}              ::  description
              {$made p/type q/tile}                     ::  construction
          ::  $mark                                     ::  described as mark?
          ::  $mime                                     ::  described as mime:
          ::  $json                                     ::  json schema?
          ::  
          ==
--
::                                                      ::
::::  5: layer five                                     ::
  ::                                                    ::
  ::    5a: compiler utilities                          ::
  ::    5b: macro expansion                             ::
  ::    5c: compiler backend and prettyprinter          ::
  ::    5d: parser                                      ::
  ::    5e: caching compiler                            ::
  ::    5f: molds and mold builders                     ::
  ::    5g: profiling support (XX remove)               ::
  ::
~%    %pen
    +
  ==
    %al    al
    %ap    ap
    %ut    ut
  ==
|%
::
::::  5a: compiler utilities
  ::
++  bool  `type`(fork [%atom %f `0] [%atom %f `1] ~)    ::  make loobean
++  cell                                                ::  make %cell type
  ~/  %cell
  |=  {hed/type tal/type}
  ^-  type
  ?:(=(%void hed) %void ?:(=(%void tal) %void [%cell hed tal]))
::
++  core                                                ::  make %core type
  ~/  %core
  |=  {pac/type con/coil}
  ^-  type
  ?:(=(%void pac) %void [%core pac con])
::
++  help
  ~/  %help
  |=  {p/writ q/type}
  ^-  type
  ?:  =(%void q)
    %void
  [%help p q]
::
++  face                                                ::  make %face type
  ~/  %face
  |=  {giz/{what $@(term tune)} der/type}
  ^-  type
  ?:  =(%void der)
    %void
  [%face giz der]
::
++  fork                                                ::  make %fork type
  ~/  %fork
  |=  yed/(list type)
  =|  lez/(set type)
  |-  ^-  type
  ?~  yed
    ?~  lez  %void
    ?:  ?=({* $~ $~} lez)  n.lez
    [%fork lez]
  %=    $
      yed  t.yed
      lez
    ?:  =(%void i.yed)  lez
    ?:  ?=({$fork *} i.yed)  (~(uni in lez) p.i.yed)
    (~(put in lez) i.yed)
  ==
::
++  cove                                                ::  extract [0 *] axis
  |=  nug/nock
  ?-    nug
      {$0 *}   p.nug
      {$10 *}  $(nug q.nug)
      *        ~_(leaf+"cove" !!)
  ==
++  comb                                                ::  combine two formulas
  ~/  %comb
  |=  {mal/nock buz/nock}
  ^-  nock
  ?:  ?&(?=({$0 *} mal) !=(0 p.mal))
    ?:  ?&(?=({$0 *} buz) !=(0 p.buz))
      [%0 (peg p.mal p.buz)]
    ?:  ?=({$2 {$0 *} {$0 *}} buz)
      [%2 [%0 (peg p.mal p.p.buz)] [%0 (peg p.mal p.q.buz)]]
    [%7 mal buz]
  ?:  ?=({^ {$0 $1}} mal)
    [%8 p.mal buz]
  ?:  =([%0 %1] buz)
    mal
  [%7 mal buz]
::
++  cond                                                ::  ?:  compile
  ~/  %cond
  |=  {pex/nock yom/nock woq/nock}
  ^-  nock
  ?-  pex
    {$1 $0}  yom
    {$1 $1}  woq
    *        [%6 pex yom woq]
  ==
::
++  cons                                                ::  make formula cell
  ~/  %cons
  |=  {vur/nock sed/nock}
  ^-  nock
  ?:  ?=({{$0 *} {$0 *}} +<)
    ?:  ?&(=(+(p.vur) p.sed) =((div p.vur 2) (div p.sed 2)))
      [%0 (div p.vur 2)]
    [vur sed]
  ?:  ?=({{$1 *} {$1 *}} +<)
    [%1 p.vur p.sed]
  [vur sed]
::
++  fitz                                                ::  odor compatibility
  ~/  %fitz
  |=  {yaz/term wix/term}
  =+  ^=  fiz
      |=  mot/@ta  ^-  {p/@ q/@ta}
      =+  len=(met 3 mot)
      ?:  =(0 len)
        [0 %$]
      =+  tyl=(rsh 3 (dec len) mot)
      ?:  &((gte tyl 'A') (lte tyl 'Z'))
        [(sub tyl 64) (end 3 (dec len) mot)]
      [0 mot]
  =+  [yoz=(fiz yaz) wux=(fiz wix)]
  ?&  ?|  =(0 p.yoz)
          =(0 p.wux)
          &(!=(0 p.wux) (lte p.wux p.yoz))
      ==
      |-  ?|  =(%$ p.yoz)
              =(%$ p.wux)
              ?&  =((end 3 1 p.yoz) (end 3 1 p.wux))
                  $(p.yoz (rsh 3 1 p.yoz), p.wux (rsh 3 1 p.wux))
              ==
          ==
  ==
::
++  flan                                                ::  loobean  &
  ~/  %flan
  |=  {bos/nock nif/nock}
  ^-  nock
  ?-    bos
      {$1 $1}   bos
      {$1 $0}   nif
      *
    ?-    nif
        {$1 $1}   nif
        {$1 $0}   bos
        *       [%6 bos nif [%1 1]]
    ==
  ==
::
++  flip                                                ::  loobean negation
  ~/  %flip
  |=  dyr/nock
  [%6 dyr [%1 1] [%1 0]]
::
++  flor                                                ::  loobean  |
  ~/  %flor
  |=  {bos/nock nif/nock}
  ^-  nock
  ?-  bos
      {$1 $1}   nif
      {$1 $0}   bos
      *
    ?-  nif
        {$1 $1}   bos
        {$1 $0}   nif
        *         [%6 bos [%1 0] nif]
    ==
  ==
::
++  hike
  ~/  %hike
  |=  {axe/axis pac/(list {p/axis q/nock})}
  ^-  nock
  ?~  pac
    [%0 axe]
  =+  zet=(skim pac.$ |=({p/axis q/nock} [=(1 p)]))
  ?~  zet
    =+  tum=(skim pac.$ |=({p/axis q/nock} ?&(!=(1 p) =(2 (cap p)))))
    =+  gam=(skim pac.$ |=({p/axis q/nock} ?&(!=(1 p) =(3 (cap p)))))
    %+  cons
      %=  $
        axe  (peg axe 2)
        pac  (turn tum |=({p/axis q/nock} [(mas p) q]))
      ==
    %=  $
      axe  (peg axe 3)
      pac  (turn gam |=({p/axis q/nock} [(mas p) q]))
    ==
  ?>(?=({* $~} zet) q.i.zet)
::
++  jock
  |=  rad/?
  |=  lot/coin  ^-  hoon
  ?-    -.lot
      $~
    ?:(rad [%rock p.lot] [%sand p.lot])
  ::
      $blob
    ?:  rad
      [%rock %$ p.lot]
    ?@(p.lot [%sand %$ p.lot] [$(p.lot -.p.lot) $(p.lot +.p.lot)])
  ::
      $many
    [%cltr (turn p.lot |=(a/coin ^$(lot a)))]
  ==
::
++  look
  ~/  %look
  |=  {cog/term dab/(map term (pair what foot))}
  =+  axe=1
  |-  ^-  (unit {p/axis q/(pair what foot)})
  ?-  dab
      $~  ~
  ::
      {* $~ $~}
    ?:(=(cog p.n.dab) [~ axe q.n.dab] ~)
  ::
      {* $~ *}
    ?:  =(cog p.n.dab)
      [~ (peg axe 2) q.n.dab]
    ?:  (gor cog p.n.dab)
      ~
    $(axe (peg axe 3), dab r.dab)
  ::
      {* * $~}
    ?:  =(cog p.n.dab)
      [~ (peg axe 2) q.n.dab]
    ?:  (gor cog p.n.dab)
      $(axe (peg axe 3), dab l.dab)
    ~
  ::
      {* * *}
    ?:  =(cog p.n.dab)
      [~ (peg axe 2) q.n.dab]
    ?:  (gor cog p.n.dab)
      $(axe (peg axe 6), dab l.dab)
    $(axe (peg axe 7), dab r.dab)
  ==
::
++  loot
  ~/  %loot
  |=  {cog/term dom/(map @ tomb)}
  =+  axe=1
  |-  ^-  (unit {p/axis q/(pair what foot)})
  ?-  dom
      $~  ~
  ::
      {* $~ $~}
    %+  bind  (look cog q.q.n.dom) 
    |=((pair axis (pair what foot)) [(peg axe p) q])
  ::
      {* $~ *}
    =+  yep=(look cog q.q.n.dom)
    ?^  yep
      [~ (peg (peg axe 2) p.u.yep) q.u.yep]
    $(axe (peg axe 3), dom r.dom)
  ::
      {* * $~}
    =+  yep=(look cog q.q.n.dom)
    ?^  yep
      [~ (peg (peg axe 2) p.u.yep) q.u.yep]
    $(axe (peg axe 3), dom l.dom)
  ::
      {* * *}
    =+  yep=(look cog q.q.n.dom)
    ?^  yep
      [~ (peg (peg axe 2) p.u.yep) q.u.yep]
    =+  pey=$(axe (peg axe 6), dom l.dom)
    ?^  pey  pey
    $(axe (peg axe 7), dom r.dom)
  ==
::
::::  5b: macro expansion
  ::
++  ah                                                  ::  tiki engine
  |_  tik/tiki
  ++  blue
    |=  gen/hoon
    ^-  hoon
    ?.  &(?=($| -.tik) ?=($~ p.tik))  gen
    [%tsgr [%$ 3] gen]
  ::
  ++  gray
    |=  gen/hoon
    ^-  hoon
    ?-  -.tik
      $&  ?~(p.tik gen [%tstr [~ u.p.tik] [%wing q.tik] gen])
      $|  [%tsls ?~(p.tik q.tik [%ktts u.p.tik q.tik]) gen]
    ==
  ::
  ++  puce
    ^-  wing
    ?-  -.tik
      $&  ?~(p.tik q.tik [u.p.tik ~])
      $|  [[%& 2] ~]
    ==
  ::
  ++  wthp  |=  opt/(list (pair root hoon))
            %+  gray  %wthp
            [puce (turn opt |=({a/root b/hoon} [a (blue b)]))]
  ++  wtkt  |=({sic/hoon non/hoon} (gray [%wtkt puce (blue sic) (blue non)]))
  ++  wtls  |=  {gen/hoon opt/(list (pair root hoon))}
            %+  gray  %wtls
            [puce (blue gen) (turn opt |=({a/root b/hoon} [a (blue b)]))]
  ++  wtpt  |=({sic/hoon non/hoon} (gray [%wtpt puce (blue sic) (blue non)]))
  ++  wtsg  |=({sic/hoon non/hoon} (gray [%wtsg puce (blue sic) (blue non)]))
  ++  wtts  |=(gen/hoon (gray [%wtts (blue gen) puce]))
  --
::
++  ax
  =+  :*  dom=`axis`1
          doc=*(list what)
      ==
  |_  mod/tile
  ++  bunt  
    ::  ~&  [%bunt-model mod]
    ::  =-  ~&  [%bunt-product -]
    ::      -  
    ~$  %ut-ersatz-call
    ~+
    ~$  %ut-ersatz-make
    ersatz
  ++  clam  
    ::  ~&  [%clam-model mod]
    ::  =-  ~&  [%clam-product -]
    ::      -
    ~$  %ut-factory-call
    ~+
    ~$  %ut-factory-make
    factory
  ++  home  
    ::  express a hoon against the original subject
    ::
    |=(gen/hoon ^-(hoon ?:(=(1 dom) gen [%tsgr [%$ dom] gen])))
  ::
  ++  default
    ::  produce a hoon that makes the model's default value, untyped
    ::
    |-  ^-  hoon
    ?-    mod
        {^ *}
      [$(mod -.mod) $(mod +.mod)]
    ::
        {$axil *}
      ?+  p.mod  [%rock %$ 0]
        $cell  [[%rock %$ 0] [%rock %$ 0]]
        $void  [%zpzp ~]
      ==
    ::
        {$bark *}
      $(mod q.mod)
    ::
        {$herb *}
      =+  cys=~(boil ap p.mod)
      ?:  ?=($herb -.cys)
        (home [%tsgl [%limb %$] p.mod])
      $(mod cys)
    ::  
        {$deet *}
      $(mod q.mod)
    ::
        {$fern *}
      ::  last entry is the default value
      ::
      |-  ^-  hoon
      ?~(t.p.mod ^$(mod i.p.mod) $(i.p.mod i.t.p.mod, t.p.mod t.t.p.mod))
    ::
        {$kelp *}
      ::  last entry is the default value  
      ::
      |-  ^-  hoon
      ?~(t.p.mod ^$(mod i.p.mod) $(i.p.mod i.t.p.mod, t.p.mod t.t.p.mod))
    ::
        {$leaf *}  [%rock p.mod q.mod]
        {$plow *}  $(mod q.mod)
        {$reed *}  $(mod p.mod)
        {$vine *}  $(mod q.mod)
        {$weed *}  (home p.mod)
    ==
  ::
  ++  trivial
    ::  ersatz by trivial construction
    ::
    ^-  hoon
    :+  %tsls
      [%bust %noun]
    ~(construct sample(dom (peg 3 dom)) [2 %&])
  ::
  ++  basic
    |=  bas/base
    ?-    bas
    ::
        {$atom *}
      ::  trivial zero
      ::
      [%sand p.bas 0]
    ::
        $noun
      ::  raw nock produces noun type
      ::
      =+([%rock %$ 0] [%ktls [%dttr - - [%rock %$ 1]] -])
    ::
        $cell
      ::  reduce to pair of nouns
      ::
      =+($(bas %noun) [- -])
    ::
        $bean
      ::  comparison produces boolean type
      ::
      =+([%rock %$ 0] [%ktls [%dtts - -] -])
    ::
        $null
      [%rock %n 0]
    ::
        $void
      ::  should not actually be a thing
      ::
      [%zpzp ~] 
    ==
  ::
  ++  decorate
    ::  apply documentation to expression
    ::
    |=  gen/hoon
    ^-  hoon
    ?~  doc  gen
    =/  fin  $(doc t.doc)
    ?~(i.doc gen [%docs u.i.doc gen])
  ::
  ++  ersatz
    ::  produce a correctly typed instance without subject
    ::
    ~+
    ^-  hoon
    ?-    mod
        {^ *}
      %-  decorate
      =.  doc  ~
      [ersatz(mod -.mod) ersatz(mod +.mod)]
    ::
        {$axil *}  (decorate (basic p.mod))
        {$bark *}  [%ktts p.mod ersatz(mod q.mod)]
        {$herb *}
      %-  decorate
      =.  doc  ~
      =+  cys=~(boil ap p.mod)
      ?:  ?=($herb -.cys)
        (home [%tsgl [%limb %$] p.mod])
      ersatz(mod cys)
    ::  
        {$deet *}  [%dbug p.mod ersatz(mod q.mod)]
        {$fern *}  trivial
        {$kelp *}  trivial
        {$leaf *}  (decorate [%rock p.mod q.mod])
        {$plow *}  ersatz(mod q.mod, doc [p.mod doc])
        {$reed *}  trivial
        {$vine *}  trivial
        {$weed *}  (home p.mod)
    ==
  ::
  ++  factory
    ::  produce a normalizing gate (mold)
    ::
    ^-  hoon
    :+  %sgbc
      %run-mold
    :^  %brts  ~^~
      [%base %noun]
    :+  %sgbc
      %run-make
    ~(construct sample(dom (peg 7 dom)) [6 %&])
  ::
  ++  sample
    ::  normalize a sample of the subject
    ::
    |_  $:  ::  axe: axis to sample
            ::  top: topographic type of sample
            ::
            axe/axis
            top/tope 
        ==
    ++  basic
      |=  bas/base
      ::  apply documentation
      ::
      ?^  doc  document
      ?-    bas
          {%atom *}
        ::  rez: fake instance
        ::
        =/  rez  ersatz
        ?^  top  rez
        ?:  =(%| top)
          ::  xx sanitize
          ::
          fetch
        [%wtpt fetch-wing fetch rez]
      ::
          $noun
        fetch
      ::
          $cell
        ?^  top  fetch
        ::  rez: fake instance
        ::
        =/  rez  ersatz
        ?:  =(%| top)
          rez
        [%wtpt fetch-wing rez fetch]
      ::
          $bean
        ?^  top  ersatz
        :^    %wtcl
            [%dtts [%rock %$ |] [%$ axe]]
          [%rock %f |]
        [%rock %f &]
      ::
          $null
        ersatz
      ::
          $void
        ersatz
      ==
    ++  fetch
      ::  load the sample
      ::
      ^-  hoon
      [%$ axe]
    ::
    ++  fetch-wing
      ::  load, as a wing
      ::
      ^-  wing
      [[%& axe] ~]
    ::
    ++  choice
      ::  match full models, by trying them
      ::
      |=  $:  ::  one: first option
              ::  rep: other options
              ::
              one/tile
              rep/(list tile)
          ==
      ^-  hoon
      ::  if no other choices, construct head
      ::
      ?~  rep  construct(mod one)
      ::  fin: loop completion
      ::
      =/  fin/hoon  $(one i.rep, rep t.rep)
      ::  new: trial product
      ::  old: original subject
      ::
      =/  new  [%$ 2]
      =*  old  [%$ 3]
      ::  build trial noun
      ::
      :+  %tsls
        ::  build the sample with the first option
        ::
        construct(mod one)
      ::  build test
      ::
      :^    %wtcl
          ::  if the trial noun equals the sample
          ::
          [%dtts new fetch(axe (peg 3 axe))]
        ::  produce the trial noun
        ::
        new
      ::  continue with the original subject
      ::
      [%tsgr old fin]
    ::
    ++  switch
      |=  $:  ::  one: first format
              ::  two: more formats
              ::
              one/line
              rep/(list line)
          ==
      :+  %sgbc
        %run-switch
      |-  ^-  hoon
      ::  if no other choices, construct head
      ::
      ?~  rep  construct(mod `tile`one) 
      ::  fin: loop completion
      ::
      =/  fin/hoon  $(one i.rep, rep t.rep)
      ::  interrogate this instance
      ::
      :^    %wtcl
          ::  test if the head matches this wing
          ::
          [%wtts p.one fetch-wing(axe (peg axe 2))]
        ::  if so, use this form
        ::
        :-  [%rock p.p.one q.p.one]
        construct(mod q.one, top &, axe (peg axe 3))
      ::  continue in the loop
      ::
      fin
    ::
    ++  probe
      ::  probe for cell or default
      ::
      ^-  hoon
      ::  ~&  [%probe axe mod]
      ::
      ::  old subject is wrapped by trap
      ::
      =:  axe  (peg 3 axe)
          dom  (peg 3 dom)
        ==
      ::  guarded trap
      ::
      :+  %tsgr
        ::  construct within trap
        ::
        :+  %brdt  ~^~ 
        ::  trap is only kicked if sample is cell
        ::
        :+  %sgbc
          %run-deep
        construct(top [& &])
      ::  boc: call constructor
      ::  but: default
      ::
      =/  boc/hoon  [%limb %$]
      =/  but/hoon  default
      :+  %sgbc
        %run-probing
      :+  %ktls
        boc
      ?:  =(& top)
        ::  may be atom or cell; default or construct
        ::
        [%wtcl [%dtwt fetch] boc but]
      ::  must be atom; construct
      ::
      but
    ::
    ++  document
      ::  document and construct
      ::
      |-  ^-  hoon
      ?~  doc  construct
      =/  fin  $(doc t.doc)
      ?~(i.doc fin [%docs u.i.doc fin])
    ::
    ++  construct
      ::  constructor at arbitrary sample
      ::
      ::  ~&  [%construct axe mod]
      ~+
      ^-  hoon
      ?-    mod
      ::
      ::  cell
      ::
          {^ *}
        ::  apply help
        ::
        ?^  doc  document
        ::  probe unless we know the sample is a cell
        ::
        ?@  top  probe
        ::  if known cell, descend directly
        ::
        :-  construct(mod -.mod, top p.top, axe (peg axe 2))
        construct(mod +.mod, top q.top, axe (peg axe 3))
      ::
      ::  base
      ::
          {$axil *}
        (basic p.mod)
      ::
      ::  name, $=
      ::
          {$bark *}
        [%ktts p.mod construct(mod q.mod)]
      ::
      ::  debug
      ::
          {$deet *}
        [%dbug p.mod construct(mod q.mod)]
      ::
      ::  choice, $?
      ::
          {$fern *}
        (choice i.p.mod t.p.mod)
      ::
      ::  synthesis, $;
      ::
          {$herb *}
        ?^  doc  document
        =+  cys=~(boil ap p.mod)
        ?:  ?=($herb -.cys)
          [%cnhp (home p.mod) fetch ~]
        construct(mod cys)
      ::
      ::  switch, $%
      :: 
          {$kelp *}
        ::  if atom or unknown, probe
        ::
        ?@  top  probe
        ::  if cell, enter switch directly
        ::
        (switch i.p.mod t.p.mod)
      ::
      ::  constant
      ::
          {$leaf *}
        (decorate [%rock p.mod q.mod])
      ::
      ::  documentation
      ::
          {$plow *}
        construct(doc [p.mod doc], mod q.mod)
      ::
      ::  branch, $@
      ::
          {$reed *}
        ?^  doc  document
        ?@  top
          ?:  =(%| top)
            construct(mod p.mod)
          :^    %wtpt
              fetch-wing
            construct(top %|, mod p.mod)
          construct(top [%& %&], mod q.mod)
        construct(mod q.mod)
      ::
      ::  bridge, $^
      ::
          {$vine *}
        ?^  doc  document
        ?@  top  probe
        :^    %wtpt
            fetch-wing(axe (peg axe 2))
          construct(top [%| %&], mod q.mod) 
        construct(top [[%& %&] %&], mod p.mod)
      ::
      ::  weed, $_
      ::
          {$weed *}
        (decorate (home p.mod))
      ==
    --
  --
::
++  al                                                  ::  tile engine
  ~%    %al
      +>+
    ==
      %bunt  bunt
      %whip  whip
    ==
  =+  :*  nag=`*`& 
          gom=`axis`1
          wat=*what
      ==
  |_  sec/tile
  ::::
  ++  hail
    |=  gen/hoon
    ^-  hoon
    ?~(wat gen [%docs u.wat gen])
  ::
  ++  home  |=(gen/hoon ^-(hoon ?:(=(1 gom) gen [%tsgr [%$ gom] gen])))
  ::::
  ++  bunt
    |-  ^-  hoon
    ~+
    ?-    sec
        {^ *}
      %-  hail
      =.  wat  ~
      (hail [$(sec p.sec) $(sec q.sec)])
    ::
        {$axil *}
      %-  hail
      =.  wat  ~
      ?-  p.sec
        {$atom *}  [%sand p.p.sec 0]
        $noun      [%dttr [%rock %$ 0] [[%rock %$ 0] [%rock %$ 1]]]
        $cell      =+(nec=$(sec [%axil %noun]) [nec nec])
        $bean      [%dtts [%rock %$ 0] [%rock %$ 0]]
        $void      [%zpzp ~]
        $null      [%rock %n %$]
      ==
    ::
        {$bark *}
      [%ktts p.sec $(sec q.sec)]
    ::
        {$deet *}
      [%dbug p.sec $(sec q.sec)]
    ::
        {$fern *}
      %-  hail
      =.  wat  ~
      |-  ^-  hoon
      ?~  t.p.sec
        ^$(sec i.p.sec)
      [%wtcl [%bust %bean] $(p.sec t.p.sec) ^$(sec i.p.sec)]
    ::
        {$herb *}
      %-  hail
      =.  wat  ~
      ::  reduce excess model/value transitions
      ::
      =+  cys=~(boil ap p.sec)
      ?:  ?=($herb -.cys)
        (home [%tsgl [%limb %$] p.sec])
      $(sec cys)
    ::
        {$kelp *}
      %-  hail
      =.  wat  ~
      :+  %ktls
        |-  ^-  hoon
      ?~  t.p.sec
        ^$(sec i.p.sec)
        [%wtcl [%bust %bean] $(p.sec t.p.sec) ^$(sec i.p.sec)]
      %=    $
          sec
        |-  ^-  tile 
        ?~(t.p.sec i.p.sec $(i.p.sec i.t.p.sec, t.p.sec t.t.p.sec))
      ==
    ::
        {$leaf *}
      (hail [%rock p.sec q.sec])
    ::
        {$plow *}
      $(sec q.sec, wat p.sec)
    ::
        {$reed *}
      %-  hail
      =.  wat  ~
      [%wtcl [%bust %bean] $(sec p.sec) $(sec q.sec)]
    ::
        {$vine *}
      %-  hail
      =.  wat  ~
      [%wtcl [%bust %bean] $(sec p.sec) $(sec q.sec)]
    ::
        {$weed *}
      (hail (home p.sec))
    ==
  ++  clam  
    ^-  hoon
    =/  raw  [%brts [~ ~] [%base %noun] (whip(gom 7) 6)]
    ::
    ::  this performance fix should unify a bunch of trivial formulas,
    ::  but breaks on certain hacks in ++raid:zuse.
    ::
    ::  ?.  ?=(?($axil $leaf) -.sec)  raw
    ::  [%tsgr [%rock %n ~] raw]
    raw
  ::
  ++  whip
    |=  axe/axis
    =+  ^=  tun
        |=  $:  def/tile 
                noy/$-(* hoon)
            == 
        ^-  hoon
        ?@  nag
          =+  luz=[%cnts [[%& 1] ~] [[[%& axe] ~] bunt(sec def)] ~]
          ?:  =(& nag)
            [%tsgr [%wtpt [[%& axe] ~] luz [%$ 1]] (noy [& &])]
          [%tsgr luz (noy [& &])]
        (noy nag)
    ^-  hoon
    ~+
    ?-    sec
        {^ *}
      %-  hail
      =.  wat  ~
      %+  tun  [%axil %cell]  |=  gon/*  =>  .(nag gon)  ^-  hoon
      :-  ^$(sec -.sec, nag -.nag, axe (peg axe 2))
      ^$(sec +.sec, nag +.nag, axe (peg axe 3))
    ::
        {$axil *}
      %-  hail
      =.  wat  ~
      ?-    p.sec
          {$atom *}
        =+  buv=bunt
        |-  ^-  hoon
        ?@  nag
          ?:(=(& nag) [%wtpt [[%& axe] ~] $(nag |) buv] [%ktls buv [%$ axe]])
        buv
      ::
          $noun
        [%kthp [%base %noun] [%$ axe]]
      ::
          $cell
        =+  buv=bunt
        |-  ^-  hoon
        ?@  nag
          ?:(=(& nag) [%wtpt [[%& axe] ~] buv $(nag [& &])] buv)
        [%ktls buv [%$ axe]]
      ::
          $bean
        :^    %wtcl
            [%dtts [%rock %$ |] [%$ axe]]
          [%rock %f |]
        [%rock %f &]
      ::
          $void
        bunt
      ::
          $null
        bunt
      ==
    ::
        {$bark *}
      [%ktts p.sec $(sec q.sec)]
    ::
        {$vine *}
      %-  hail
      =.  wat  ~
      %+  tun  [%axil %cell]  |=  gon/*  =>  .(nag gon)  ^-  hoon
      ?@  -.nag
        ?:  =(& -.nag)
          [%wtpt [[%& (peg axe 2)] ~] ^$(sec q.sec) ^$(sec p.sec)]
        ^$(sec q.sec)
      ^$(sec p.sec)
    ::
        {$deet *}
      [%dbug p.sec $(sec q.sec)]
    ::
        {$fern *}
      %-  hail
      =.  wat  ~
      |-  ^-  hoon
      ?~  t.p.sec
        ^$(sec i.p.sec)
      :+  %tsls
        ^$(sec i.p.sec)
      =>  .(axe (peg 3 axe), gom (peg 3 gom))
      :^    %wtcl
          [%dtts [%$ axe] [%$ 2]]
        [%$ 2]
      $(i.p.sec i.t.p.sec, t.p.sec t.t.p.sec)
    ::
        {$herb *}
      %-  hail
      =.  wat  ~
      =+  cys=~(boil ap p.sec)
      ?:  ?=($herb -.cys)
        [%cnhp (home p.sec) [%$ axe] ~]
      $(sec cys)
    ::
        {$kelp *}
      %-  hail
      =.  wat  ~
      =/  def/tile
        :_  [%axil %noun]
        =<  p
        |-  ^-  line
        ?~(t.p.sec i.p.sec $(i.p.sec i.t.p.sec, t.p.sec t.t.p.sec))
      %+  tun  def  |=  gon/*  =>  .(nag gon)
      |-  ^-  hoon
      ?~  t.p.sec
        :-  [%rock +.p.i.p.sec]
        ^^$(axe (peg axe 3), sec q.i.p.sec, nag &)
      :^    %wtcl
          [%dtts [%$ (peg axe 2)] [%rock +.p.i.p.sec]]
        :-  [%rock +.p.i.p.sec]
        ^^$(axe (peg axe 3), sec q.i.p.sec, nag &)
      $(i.p.sec i.t.p.sec, t.p.sec t.t.p.sec)
    ::
        {$leaf *}
      (hail [%rock p.sec q.sec])
    ::
        {$plow *}
      $(sec q.sec, wat p.sec)
    ::
        {$reed *}
      %-  hail
      =.  wat  ~
      ?-  nag
        $&  [%wtpt [[%& axe] ~] $(sec p.sec, nag |) $(sec q.sec, nag [& &])]
        $|  $(sec p.sec)
        ^   $(sec q.sec)
        *   !!
      ==
    ::
        {$weed *}
      (hail (home p.sec))
    ==
  --
::
++  ap                                                  ::  hoon engine
  ~%    %ap
      +>
    ==
      %etch  etch
      %open  open
      %rake  rake
    ==
  |_  gen/hoon
  ++  etch
    ~_  leaf+"etch"
    |-  ^-  term
    ?:  ?=({$ktts *} gen)
      ?>(?=(@ p.gen) p.gen)
    =+  voq=~(open ap gen)
    ?<(=(gen voq) $(gen voq))
  ::
  ++  feck
    |-  ^-  (unit term)
    ?-  gen
      {$sand $tas @}  [~ q.gen]
      {$dbug *}       $(gen q.gen)
      *               ~
    ==
  ::
  ++  gi
    =|  whit
    =*  wit  -
    |%
    ++  gray
      ^-  ?
      |
      ::  on reflection, perhaps just obsessive linting
      ::
      ::  ?|  ?=(^ lab)
      ::      ?=(^ boy)
      ::      |-  ^-  ?
      ::      ?~  def  | 
      ::      |($(def l.def) $(def r.def) !(~(has in use) p.n.def))
      ::  ==
    ::
    ++  grad
      |=  $:  gen/hoon 
              wit/whit 
              aid/$-({? hoon whit} {hoon whit})
          ==
      ^-  (unit (pair hoon whit))
      =:  ^gen  gen
          ^wit  wit
        ==
      ?:  =([~ ~ ~ ~] wit)  `[gen wit]
      =<  apex
      |%
      ++  apex
        ^-  (unit (pair hoon whit))
        =^  one  wit  prim
        =^  two  wit  senc(gen one)
        ?:  =(gen two) 
          ~
        `(aid & two wit)
      ::
      ::  resolve body and label issues
      ::
      ++  prim
        ^-  (pair hoon whit)
        ?:  ?=(^ -.gen)  flam
        ?+  -.gen  flam
          $halo   flam
          $base   runk
          $leaf   runk
          $bcpt   runk
          $bccb   runk
          $bccl   runk
          $bccn   runk
          $bchp   runk
          $bckt   runk
          $bcwt   runk
          $bcts   flam 
          $bcsm   runk
          $brcb   ((doof -.gen +>.gen) p.gen)
          $brcl   ((doof -.gen +>.gen) p.gen)
          $brcn   ((doof -.gen +>.gen) p.gen)
          $brdt   ((doof -.gen +>.gen) p.gen)
          $brkt   ((doof -.gen +>.gen) p.gen)
          $brhp   ((doof -.gen +>.gen) p.gen)
          $brsg   ((doof -.gen +>.gen) p.gen)
          $brtr   ((doof -.gen +>.gen) p.gen)
          $brts   ((doof -.gen +>.gen) p.gen)
          $brwt   ((doof -.gen +>.gen) p.gen)
        ==
      ::
      ::  resolve variable issues
      ::
      ++  senc
        ^-  (pair hoon whit)
        ?:  ?=(^ -.gen)  flam
        ?+  -.gen  flam
          $ktts  ((helk -.gen +>.gen) p.gen)
          $bcts  ((helk -.gen +>.gen) p.gen)
          $tsfs   ((hulp -.gen +>.gen) p.gen)
          $tssm   ((hulp -.gen +>.gen) p.gen)
          $tskt   ((hulp -.gen +>.gen) p.gen)
          $tstr   ((humm -.gen +>.gen) p.gen)
        ==
      ::
      ++  flam  [gen wit]
      ++  grif
        |=  {cog/term wat/what}
        ^-  {what whit}
        ?:  =(~ def)
          ?~  boy  [wat wit]
          [boy wit(boy ~)]
        =+  yeb=(~(get by def) cog)
        ?~  yeb  [wat wit]
        [`u.yeb wit(use (~(put in use) cog))]
      ::
      ++  doof
        |*  {pif/@tas suf/*}
        |=  pac/chap
        ^-  (pair hoon whit)
        :_  wit(lab ~, boy ~)
        =-  [pif - suf]
        ^-  chap
        :-  ?~(lab p.pac [~ u.lab])
            ?~(boy q.pac boy)
      ::
      ++  helk
        |*  {pif/@tas suf/*}
        |=  got/toga
        ^-  (pair hoon whit)
        =^  gef  wit  (tong got)
        [[pif gef suf] wit]
      ::
      ++  hulp
        |*  {pif/@tas suf/*}
        |=  hot/toro
        ^-  (pair hoon whit)
        =^  tog  wit  (tong p.hot)
        [[pif [tog q.hot] suf] wit] 
      ::
      ++  humm
        |*  {pif/@tas suf/*}
        |=  {wat/what cog/term}
        ^-  (pair hoon whit)
        =^  taw  wit  (grif cog wat)
        [[pif [taw cog] suf] wit] 
      ::
      ++  runk
        ^-  (pair hoon whit)
        ?~  boy  flam
        [[%halo boy gen] wit(boy ~)]
      ::
      ++  tong
        |=  got/toga
        ^-  {toga whit}
        ?@  got
          =^  wat  wit  (grif got ~)
          ?~  wat  [got wit]
          [[%1 [wat got] [%0 ~]] wit]
        ?-  -.got
          $0  [got wit]
          $1  =^  wat  wit  (grif q.p.got p.p.got)
              =^  sub  wit  $(got q.got)
              [[%1 [wat q.p.got] sub] wit]
          $2  =^  one  wit  $(got p.got)
              =^  two  wit  $(got q.got)
              [[%2 one two] wit]
        ==
      --
    ::
    ++  graf
      ::  ^-  (unit hoon)
      ::  =^  nex  wit  ((walk whit) wit grad)
      ::  ?:(gray ~ `nex)
      =^  nex  wit  ((walk whit) wit grad)
      nex
    --
  ::
  ::  not used at present; see comment at $csng in ++open
::::
::++  hail
::  |=  axe/axis
::  =|  air/(list (pair wing hoon))
::  |-  ^+  air
::  =+  hav=half
::  ?~  hav  [[[[%| 0 ~] [%& axe] ~] gen] air]
::  $(gen p.u.hav, axe (peg axe 2), air $(gen q.u.hav, axe (peg axe 3)))
::::
::++  half
::  |-  ^-  (unit (pair hoon hoon))
::  ?+  gen  ~
::    {^ *}       `[p.gen q.gen]
::    {$dbug *}   $(gen q.gen)
::    {$clcb *}   `[q.gen p.gen]
::    {$clhp *}   `[p.gen q.gen]
::    {$clkt *}   `[p.gen %clls q.gen r.gen s.gen]
::    {$clsg *}   ?~(p.gen ~ `[i.p.gen %clsg t.p.gen])
::    {$cltr *}   ?~  p.gen  ~
::                ?~(t.p.gen $(gen i.p.gen) `[i.p.gen %cltr t.p.gen])
::  ==
::::
  ++  hock
    |-  ^-  toga
    ?-  gen
      {$cnts {@ $~} $~}  i.p.gen
      {$limb @}          p.gen
      {$wing {@ $~}}     i.p.gen
      {$dbug *}          $(gen q.gen)
      {@ *}              =+(neg=open ?:(=(gen neg) [%0 ~] $(gen neg)))
      {^ *}              =+  toe=[$(gen p.gen) $(gen q.gen)]
                         ?:(=(toe [[%0 ~] [%0 ~]]) [%0 ~] [%2 toe])
    ==
  ++  bent
    |-  ^-  (list wing)
    ?+  gen  !!
      {$$ *}     [[[%& p.gen] ~] ~]
      {$dbug *}  ~_((show %o p.gen) $(gen q.gen))
      {$tsgl *}  $(gen open)
      {$tsgr *}  (weld $(gen p.gen) $(gen q.gen))
      {$wing *}  [p.gen ~]
      {$limb *}  [[p.gen ~] ~]
    ==                 
  ::
  ++  bawl
    ~|  %bawl-failure
    ~|  [%bawl gen]
    |-  ^-  tent
    ?+  gen   [%& bent]
      {$cnsg *}  [%| p.gen $(gen q.gen) (turn r.gen |=(hoon boil(gen +<)))]
      {$cnhp *}  $(gen open)
      {$cnkt *}  $(gen open)
      {$cnls *}  $(gen open)
      {$cndt *}  $(gen open)
      {$dbug *}  ~_((show %o p.gen) $(gen q.gen))
    ==
  ::
  ++  bile
    =+  sec=boil
    |-  ^-  (each line tile)
    ?:  ?=({$plow *} sec)
      $(sec q.sec)
    ?:  ?=({$deet *} sec)
      $(sec q.sec)
    ?:  ?=({{$deet *} *} sec)
      $(p.sec q.p.sec)
    ?:  ?=({{$leaf *} *} sec)
      [%& [%leaf p.p.sec q.p.sec] q.sec]
    [%| sec]
  ::
  ++  boil
    ^-  tile
    ?+  gen        [%herb gen]
        {$base *}  [%axil p.gen]
        {$dbug *}  [%deet p.gen boil(gen q.gen)]
        {$leaf *}  [%leaf p.gen]
    ::
        {$bcpt *}  [%reed boil(gen p.gen) boil(gen q.gen)]
        {$bccb *}  [%weed p.gen]
        {$bccl *}
      |-  ^-  tile
      ?~  p.gen  [%axil %null]
      ?~  t.p.gen  boil(gen i.p.gen)
      [boil(gen i.p.gen) $(p.gen t.p.gen)]
    ::
        {$bccn *}
      ?~  p.gen
        [%axil %void]
      ?~  t.p.gen
        boil(gen i.p.gen)
      =+  :*  def=bile(gen i.p.gen)
              ^=  end  ^-  (list line)
              ~_  leaf+"book-foul"
              %+  turn  `(list hoon)`t.p.gen
              |=(a/hoon =+(bile(gen a) ?>(?=($& -<) ->)))
          ==
      ?-  -.def
        $&  [%kelp p.def end]
        $|  ?~(end p.def [%fern p.def [%kelp end] ~])
      ==
    ::
        {$bckt *}  [%vine boil(gen p.gen) boil(gen q.gen)]
        {$bchp *}  [%weed [%brsg [~ ~] p.gen [%bunt [%tsgr [%$ 7] q.gen]]]]
        {$halo *}  [%plow p.gen boil(gen q.gen)]
        {$bcts *}  [%bark p.gen boil(gen q.gen)]
        {$bcwt *}  =+  (turn p.gen |=(a/hoon boil(gen a)))
                   ?~(- [%axil %void] [%fern -])
        {$bcsm *}  [%herb p.gen]
    ==
  ::
  ++  open
    ^-  hoon
    ?-    gen
        {$~ *}     [%cnts [[%& p.gen] ~] ~]
    ::
        {$base *}  ~(clam al boil)
        {$bust *}  ~(bunt al %axil p.gen)
        {$cold *}  p.gen
        {$dbug *}   q.gen
        {$eror *}  ~|(p.gen !!)
    ::
        {$knit *}                                       ::
      :+  %tsgr  [%ktts %v %$ 1]                         ::  =>  v=.
      :+  %brhp  [~ ~]                                  ::  |-
      :+  %ktls                                         ::  ^+
        :+  %brhp  [~ ~]                                ::  |-
        :^    %wtcl                                       ::  ?:
            [%bust %bean]                               ::  ?
          [%bust %null]                                 ::  ~
        :-  [%ktts %i [%sand 'tD' *@]]                  ::  :-  i=~~
        [%ktts %t [%limb %$]]                           ::  t=$
      |-  ^-  hoon                                      ::
      ?~  p.gen                                         ::
        [%bust %null]                                   ::  ~
      =+  res=$(p.gen t.p.gen)                          ::
      ^-  hoon                                          ::
      ?@  i.p.gen                                       ::
        [[%sand 'tD' i.p.gen] res]                      ::  [~~{i.p.gen} {res}]
      :+  %tsls                                          ::
        :-  :+  %ktts                                   ::  ^=
              %a                                        ::  a
            :+  %ktls                                   ::  ^+
              [%limb %$]                                ::  $
            [%tsgr [%limb %v] p.i.p.gen]                 ::  =>(v {p.i.p.gen})
        [%ktts %b res]                                  ::  b={res}
      ^-  hoon                                          ::
      :+  %brhp  [~ ~]                                  ::  |-
      :^    %wtpt                                       ::  ?@
          [%a ~]                                        ::  a
        [%limb %b]                                      ::  b
      :-  [%tsgl [%$ 2] [%limb %a]]                      ::  :-  -.a
      :+  %cnts                                         ::  %=
        [%$ ~]                                          ::  $
      [[[%a ~] [%tsgl [%$ 3] [%limb %a]]] ~]             ::  a  +.a
    ::
        {$leaf *}  ~(clam al boil)
        {$limb *}  [%cnts [p.gen ~] ~]
        {$tell *}  [%cnhp [%limb %noah] [%zpgr [%cltr p.gen]] ~]
        {$wing *}  [%cnts p.gen ~]
        {$yell *}  [%cnhp [%limb %cain] [%zpgr [%cltr p.gen]] ~]
    ::
        {$bcpt *}  ~(clam al boil)
        {$bccb *}  ~(clam al boil)
        {$bccl *}  ~(clam al boil)
        {$bccn *}  ~(clam al boil)
        {$bchp *}  ~(clam al boil)
        {$bckt *}  ~(clam al boil)
        {$bcwt *}  ~(clam al boil)
        {$bcts *}  ~(clam al boil)
        {$halo *}  ~(clam al boil)
        {$bcsm *}  p.gen
    ::
        {$brcb *}  :+  %tsls  [%bunt q.gen]
                   :+  %brcn  p.gen
                   %-  ~(run by s.gen)
                   |=  tom/tomb
                   ^+  tom
                   :-  p.tom
                   %-  ~(run by q.tom)
                   |=  a/(pair what foot)
                   ^+  a
                   :-  p.a
                   =-  ?:(?=({$ash *} q.a) [-.q.a -] [-.q.a -])
                   |-  ^-  hoon
                   ?~  r.gen  p.q.a
                   [%tstr [~ p.i.r.gen] q.i.r.gen $(r.gen t.r.gen)]
        {$brcl *}  [%tsls [%cold q.gen] [%brdt p.gen r.gen]]
        {$brdt *}  :+  %brcn  p.gen
                   =-  [[0 [~ ~] -] ~ ~]
                   (~(put by *(map term (pair what foot))) %$ ~ [%ash q.gen])
        {$brkt *}  :+  %tsgr 
                      :+  %brcn  p.gen
                      =+  one=(~(got by r.gen) 0)
                      %+  ~(put by r.gen)  0
                      one(q (~(put by q.one) %$ [~ [%ash q.gen]]))
                   [%limb %$]
        {$brhp *}  [%tsgl [%limb %$] [%brdt p.gen q.gen]]
        {$brsg *}  [%ktbr [%brts p.gen q.gen r.gen]]
        {$brtr *}  :+  %tsls  [%bunt q.gen]
                   :+  %brcn  p.gen
                   =-  [[0 [~ ~] -] ~ ~]
                   (~(put by *(map term (pair what foot))) %$ ~ [%elm r.gen])
        {$brts *}  :^  %brcb  p.gen  q.gen 
                   =-  [~ [[0 [~ ~] -] ~ ~]]
                   (~(put by *(map term (pair what foot))) %$ ~ [%ash r.gen])
        {$brwt *}  [%ktwt %brdt p.gen q.gen]
    ::
        {$clkt *}  [p.gen q.gen r.gen s.gen]
        {$clls *}  [p.gen q.gen r.gen]
        {$clcb *}  [q.gen p.gen]
        {$clhp *}  [p.gen q.gen]
        {$clsg *}
      |-  ^-  hoon
      ?~  p.gen
        [%rock %n ~]
      [i.p.gen $(p.gen t.p.gen)]
    ::
        {$cltr *}
      |-  ^-  hoon
      ?~  p.gen
        [%zpzp ~]
      ?~  t.p.gen
        i.p.gen
      [i.p.gen $(p.gen t.p.gen)]
    ::
        {$bunt *}  [%cold ~(bunt al %herb p.gen)]
        {$cncb *}  [%ktls [%wing p.gen] %cnts p.gen q.gen]
        {$cndt *}  [%cnhp q.gen [p.gen ~]]
        {$cnkt *}  [%cnhp p.gen q.gen r.gen s.gen ~]
        {$cnls *}  [%cnhp p.gen q.gen r.gen ~]
        {$cnhp *}  [%cnsg [%$ ~] p.gen q.gen]
        {$cnsg *}  :: [%cntr p.gen q.gen (hail(gen [%cltr r.gen]) 6)]
      :^  %cntr  p.gen  q.gen
      ::
      ::  the use of ++hail is probably the right language design, but
      ::  it's impractically slow without validating %=.
      ::
::    ?:(=(~ r.gen) ~ (hail(gen [%cltr r.gen]) 6))
      =+  axe=6
      |-  ^-  (list {wing hoon})
      ?~  r.gen  ~
      ?~  t.r.gen  [[[[%| 0 ~] [%& axe] ~] i.r.gen] ~]
      :-  [[[%| 0 ~] [%& (peg axe 2)] ~] i.r.gen]
      $(axe (peg axe 3), r.gen t.r.gen)
    ::
        {$cntr *}
      ?:  =(~ r.gen)
        [%tsgr q.gen [%wing p.gen]]
      :+  %tsls
        q.gen
      :+  %cnts
        (weld p.gen `wing`[[%& 2] ~])
      (turn r.gen |=({p/wing q/hoon} [p [%tsgr [%$ 3] q]]))
    ::
        {$ktdt *}  [%ktls [%cnhp p.gen q.gen ~] q.gen]
        {$kthp *}  [%ktls ~(bunt al [%herb p.gen]) q.gen]
        {$sgbr *}
      :+  %sggr
        :-  %mean
        =+  fek=~(feck ap p.gen)
        ?^  fek  [%rock %tas u.fek]
        [%brdt [~ ~] [%cnhp [%limb %cain] [%zpgr [%tsgr [%$ 3] p.gen]] ~]]
      q.gen
    ::
        {$sgcb *}  [%sggr [%mean [%brdt [~ ~] p.gen]] q.gen]
        {$sgcn *}
      :+  %sggl
        :-  %fast
        :-  %clls
        :+  [%rock %$ p.gen]
          [%zpts q.gen]
        :-  %clsg
        =+  nob=`(list hoon)`~
        |-  ^-  (list hoon)
        ?~  r.gen
          nob
        [[[%rock %$ p.i.r.gen] [%zpts q.i.r.gen]] $(r.gen t.r.gen)]
      s.gen
    ::
        {$sgfs *}  [%sgcn p.gen [%$ 7] ~ q.gen]
        {$sggl *}  [%tsgl [%sggr p.gen [%$ 1]] q.gen]
        {$sgbc *}  [%sggr [%live [%rock %$ p.gen]] q.gen]
        {$sgls *}  [%sggr [%memo %rock %$ p.gen] q.gen]
        {$sgpm *}
      :+  %sggr
        [%slog [%sand %$ p.gen] [%cnhp [%limb %cain] [%zpgr q.gen] ~]]
      r.gen
    ::
        {$sgts *}  [%sggr [%germ p.gen] q.gen]
        {$sgwt *}
      :+  %tsls  [%wtdt q.gen [%bust %null] [[%bust %null] r.gen]]
      :^  %wtsg  [%& 2]~
        [%tsgr [%$ 3] s.gen]
      [%sgpm p.gen [%$ 5] [%tsgr [%$ 3] s.gen]]
    ::
        {$smts *}
      |-
      ?~  p.gen  [%bust %null]
      ?-  -.i.p.gen
        ^      [[%xray i.p.gen] $(p.gen t.p.gen)]
        $manx  [p.i.p.gen $(p.gen t.p.gen)]
        $tape  [[%smfs p.i.p.gen] $(p.gen t.p.gen)]
        $call  [%cnhp p.i.p.gen [$(p.gen t.p.gen)]~]
        $marl  =-  [%cndt [p.i.p.gen $(p.gen t.p.gen)] -]
               ^-  hoon
               :+  %tsbr  [%base %cell]
               :+  %brcn  *chap
               ^-  (map @ tomb)
               =-  [[0 [~ ~] -] ~ ~]
               ^-  (map term (pair what foot))
               :_  [~ ~]
               =+  sug=[[%& 12] ~]
               :^  %$  ~  %elm
               :^  %wtsg  sug
                 [%cnts sug [[[[%& 1] ~] [%$ 13]] ~]]
               [%cnts sug [[[[%& 3] ~] [%cnts [%$ ~] [[sug [%$ 25]] ~]]] ~]]
      ==
    ::
        {$smcl *}
      ?-    q.gen
          $~      [%zpzp ~]
          {* $~}  i.q.gen
          ^
        :+  %tsls
          p.gen
        =+  yex=`(list hoon)`q.gen
        |-  ^-  hoon
        ?-  yex
          {* $~}  [%tsgr [%$ 3] i.yex]
          {* ^}   [%cnhp [%$ 2] [%tsgr [%$ 3] i.yex] $(yex t.yex) ~]
          $~      !!
        ==
      ==
    ::
        {$smfs *}  =+(zoy=[%rock %ta %$] [%clsg [zoy [%clsg [zoy p.gen] ~]] ~])
        {$smsg *}                                       ::                  ;~
      |-  ^-  hoon
      ?-  q.gen
          $~      ~_(leaf+"open-smsg" !!)
          ^
        :+  %tsgr  [%ktts %v %$ 1]                      ::  =>  v=.
        |-  ^-  hoon                                    ::
        ?:  ?=($~ t.q.gen)                              ::
          [%tsgr [%limb %v] i.q.gen]                    ::  =>(v {i.q.gen})
        :+  %tsls  [%ktts %a $(q.gen t.q.gen)]          ::  =+  ^=  a
        :+  %tsls                                       ::    {$(q.gen t.q.gen)}
          [%ktts %b [%tsgr [%limb %v] i.q.gen]]         ::  =+  ^=  b
        :+  %tsls                                       ::    =>(v {i.q.gen})
          :+  %ktts  %c                                 ::  =+  c=,.+6.b
          :+  %tsgl                                     ::
            [%wing [%| 0 ~] [%& 6] ~]                   ::
          [%limb %b]                                    ::
        :+  %brdt  [~ ~]                                ::  |.
        :^    %cnls                                     ::  %+
            [%tsgr [%limb %v] p.gen]                    ::      =>(v {p.gen})
          [%cnhp [%limb %b] [%limb %c] ~]               ::    (b c)
        :+  %cnts  [%a ~]                               ::  a(,.+6 c)
        [[[[%| 0 ~] [%& 6] ~] [%limb %c]] ~]            ::
      ==                                                ::
    ::
        {$smsm *}                                       ::                  ;;
      :+  %tsgr  [%ktts %v %$ 1]                        ::  =>  v=.
      :+  %tsls  :+  %ktts  %a                          ::  =+  ^=  a
                 [%tsgr [%limb %v] p.gen]               ::  =>(v {p.gen})
      :+  %tsls  [%ktts %b [%tsgr [%limb %v] q.gen]]    ::  =+  b==>(v {q.gen})
      :+  %tsls                                         ::  =+  c=(a b)
        [%ktts %c [%cnhp [%limb %a] [%limb %b] ~]]      ::
      :+  %wtgr                                         ::  ?>(=(`*`c `*`b) c)
        :+  %dtts                                       ::
        [%kthp [%base %noun] [%limb %c]]                ::
        [%kthp [%base %noun] [%limb %b]]                ::
      [%limb %c]                                        ::
    ::
        {$tsbr *}
      [%tsls ~(bunt al %herb p.gen) q.gen]
    ::
        {$tscl *}
      [%tsgr [%cncb [[%& 1] ~] p.gen] q.gen]
    ::
        {$tsfs *}
      ?~  q.p.gen
        [%tsls [%ktts p.p.gen q.gen] r.gen]
      [%tsls [%kthp [%bcts p.p.gen u.q.p.gen] q.gen] r.gen]
    ::
        {$tssm *}  [%tsfs p.gen r.gen q.gen]
        {$tsdt *}
      [%tsgr [%cncb [[%& 1] ~] [[p.gen q.gen] ~]] r.gen]
        {$tswt *}                                        ::                  =?
      [%tsdt p.gen [%wtcl q.gen r.gen [%wing p.gen]] s.gen]
    ::
        {$tskt *}                                        ::                  =^
      =+  wuy=(weld q.gen `wing`[%v ~])                 ::
      :+  %tsgr  [%ktts %v %$ 1]                         ::  =>  v=.
      :+  %tsls  [%ktts %a %tsgr [%limb %v] r.gen]        ::  =+  a==>(v \r.gen)
      :^  %tsdt  wuy  [%tsgl [%$ 3] [%limb %a]]
      :+  %tsgr  :-  ?~  q.p.gen
                       :+  %ktts  p.p.gen
                       [%tsgl [%$ 2] [%limb %a]]
                     :+  %kthp
                        :+  %bcts  p.p.gen
                        [%tsgr [%limb %v] u.q.p.gen]
                     [%tsgl [%$ 2] [%limb %a]] 
                [%limb %v]
      s.gen
    ::
        {$tsgl *}  [%tsgr q.gen p.gen]
        {$tsls *}  [%tsgr [p.gen [%$ 1]] q.gen]
        {$tshp *}  [%tsls q.gen p.gen]
        {$tssg *}
      |-  ^-  hoon
      ?~  p.gen    [%$ 1]
      ?~  t.p.gen  i.p.gen
      [%tsgr i.p.gen $(p.gen t.p.gen)]
    ::
        {$wtbr *}
      |-
      ?~(p.gen [%rock %f 1] [%wtcl i.p.gen [%rock %f 0] $(p.gen t.p.gen)])
    ::
        {$wtdt *}   [%wtcl p.gen r.gen q.gen]
        {$wtgl *}   [%wtcl p.gen [%zpzp ~] q.gen]
        {$wtgr *}   [%wtcl p.gen q.gen [%zpzp ~]]
        {$wtkt *}   [%wtcl [%wtts [%base %atom %$] p.gen] r.gen q.gen]
    ::
        {$wthp *}
      |-
      ?~  q.gen
        [%lost [%wing p.gen]]
      :^    %wtcl
          [%wtts p.i.q.gen p.gen]
        q.i.q.gen
      $(q.gen t.q.gen)
    ::
        {$wtls *}
      [%wthp p.gen (weld r.gen `_r.gen`[[[%base %noun] q.gen] ~])]
    ::
        {$wtpm *}
      |-
      ?~(p.gen [%rock %f 0] [%wtcl i.p.gen $(p.gen t.p.gen) [%rock %f 1]])
    ::
        {$xray *}
      |^  :-  [(open-mane n.g.p.gen) %clsg (turn a.g.p.gen open-mart)] 
          [%smts c.p.gen]
      ::
      ++  open-mane
        |=  a/mane:hoon
        ?@(a [%rock %tas a] [[%rock %tas -.a] [%rock %tas +.a]])
      ::
      ++  open-mart
        |=  {n/mane:hoon v/(list beer:hoon)} 
        [(open-mane n) %knit v]
      --
    ::
        {$wtpt *}   [%wtcl [%wtts [%base %atom %$] p.gen] q.gen r.gen]
        {$wtsg *}   [%wtcl [%wtts [%base %null] p.gen] q.gen r.gen]
        {$wtts *}   [%fits ~(bunt al %herb p.gen) q.gen]
        {$wtzp *}   [%wtcl p.gen [%rock %f 1] [%rock %f 0]]
        {$zpgr *}
      [%cnhp [%limb %onan] [%zpsm [%bunt [%limb %abel]] p.gen] ~]
    ::
        {$zpwt *}
      ?:  ?:  ?=(@ p.gen)
            (lte hoon-version p.gen)
          &((lte hoon-version p.p.gen) (gte hoon-version q.p.gen))
        q.gen
      ~_(leaf+"hoon-version" !!)
    ::
        *           gen
    ==
  ::
  ++  rake  ~>(%mean.[%leaf "rake-hoon"] (need reek))
  ++  reek
    ^-  (unit wing)
    ?+  gen  ~
      {$~ *}        `[[%& p.gen] ~]
      {$limb *}     `[p.gen ~]
      {$wing *}     `p.gen
      {$cnts * $~}  `p.gen
      {$dbug *}     reek(gen q.gen)
    ==
  ++  rusk
    ^-  term
    =+  wig=rake
    ?.  ?=({@ $~} wig)
      ~>(%mean.[%leaf "rusk-hoon"] !!)
    i.wig
  ::
  ++  walk                                              ::  forward traverse
    |*  life/mold
    |=  $:  vit/life
            $=  mac
            $-  $:  hoon
                    life
                    $-({? hoon life} {hoon life})
                ==
            (unit (pair hoon life))
        ==
    ^-  {hoon life}
    =/  use  &
    =<  apex 
    |%  
    ++  apex
      |-  ^-  {hoon life}
      =*  aid  |=  {use/? gen/hoon vit/life} 
               ^$(use use, gen gen, vit vit)
      =/  gun  ?:(use (mac gen vit aid) ~)
      ?^  gun  u.gun
      ?:  ?=(^ -.gen)
        %.(gen dubs)
      ?-  -.gen
        $$     (lead -.gen %.(+.gen noop))
        $base  (lead -.gen %.(+.gen noop))
        $bunt  (lead -.gen %.(+.gen expr))
        $bust  (lead -.gen %.(+.gen noop))
        $docs  (lead -.gen %.(+.gen nexp))
        $dbug  (lead -.gen %.(+.gen nexp))
        $hand  (lead -.gen %.(+.gen noop))
        $knit  (lead -.gen %.(+.gen (moto bark)))
        $leaf  (lead -.gen %.(+.gen noop))
        $limb  (lead -.gen %.(+.gen noop))
        $lost  (lead -.gen %.(+.gen expr))
        $rock  (lead -.gen %.(+.gen noop))
        $sand  (lead -.gen %.(+.gen noop))
        $tell  (lead -.gen %.(+.gen moar))
        $tune  (lead -.gen %.(+.gen tung))
        $wing  (lead -.gen %.(+.gen noop))
        $yell  (lead -.gen %.(+.gen moar))
        $bcpt  (lead -.gen %.(+.gen dubs))
        $bccb  (lead -.gen %.(+.gen expr))
        $bccl  (lead -.gen %.(+.gen moar))
        $bccn  (lead -.gen %.(+.gen moar))
        $bchp  (lead -.gen %.(+.gen dubs))
        $bckt  (lead -.gen %.(+.gen dubs))
        $bcwt  (lead -.gen %.(+.gen moar))
        $bcts  (lead -.gen %.(+.gen nexp))
        $bcsm  (lead -.gen %.(+.gen expr))
        $brcb  (lead -.gen %.(+.gen (quad noop expr exps arms)))
        $brcl  (lead -.gen %.(+.gen (twin noop dubs)))
        $brcn  (lead -.gen %.(+.gen (twin noop arms)))
        $brdt  (lead -.gen %.(+.gen (twin noop expr)))
        $brkt  (lead -.gen %.(+.gen (trio noop expr arms)))
        $brhp  (lead -.gen %.(+.gen (twin noop expr)))
        $brsg  (lead -.gen %.(+.gen (twin noop dubs)))
        $brtr  (lead -.gen %.(+.gen (twin noop dubs)))
        $brts  (lead -.gen %.(+.gen (twin noop dubs)))
        $brwt  (lead -.gen %.(+.gen (twin noop expr)))
        $clcb  (lead -.gen %.(+.gen dubs))
        $clkt  (lead -.gen %.(+.gen (quad expr expr expr expr)))
        $clhp  (lead -.gen %.(+.gen dubs))
        $clls  (lead -.gen %.(+.gen trey))
        $clsg  (lead -.gen %.(+.gen moar))
        $cltr  (lead -.gen %.(+.gen moar))
        $cncb  (lead -.gen %.(+.gen (twin noop moan)))
        $cndt  (lead -.gen %.(+.gen dubs))
        $cnhp  (lead -.gen %.(+.gen (twin expr moar)))
        $cntr  (lead -.gen %.(+.gen (trio noop expr moan)))
        $cnkt  (lead -.gen %.(+.gen (quad expr expr expr expr)))
        $cnls  (lead -.gen %.(+.gen trey))
        $cnsg  (lead -.gen %.(+.gen (trio noop expr moar)))
        $cnts  (lead -.gen %.(+.gen (twin noop moan)))
        $dtkt  (lead -.gen %.(+.gen dubs))
        $dtls  (lead -.gen %.(+.gen expr))
        $dttr  (lead -.gen %.(+.gen dubs))
        $dtts  (lead -.gen %.(+.gen dubs))
        $dtwt  (lead -.gen %.(+.gen expr))
        $ktbr  (lead -.gen %.(+.gen expr))
        $ktdt  (lead -.gen %.(+.gen dubs))
        $ktls  (lead -.gen %.(+.gen dubs))
        $kthp  (lead -.gen %.(+.gen dubs))
        $ktpm  (lead -.gen %.(+.gen expr))
        $ktsg  (lead -.gen %.(+.gen expr))
        $ktts  (lead -.gen %.(+.gen nexp))
        $ktwt  (lead -.gen %.(+.gen expr))
        $halo  (lead -.gen %.(+.gen nexp))
        $sgbr  (lead -.gen %.(+.gen dubs))
        $sgcb  (lead -.gen %.(+.gen dubs))
        $crap  (lead -.gen %.(+.gen (raid expr)))
        $sgcn  (lead -.gen %.(+.gen (quad noop expr moan expr)))
        $sgfs  (lead -.gen %.(+.gen nexp))
        $sggl  (lead -.gen %.(+.gen (twin toad expr)))
        $sggr  (lead -.gen %.(+.gen (twin toad expr)))
        $sgbc  (lead -.gen %.(+.gen nexp))
        $sgls  (lead -.gen %.(+.gen nexp))
        $sgpm  (lead -.gen %.(+.gen trip))
        $sgts  (lead -.gen %.(+.gen dubs))
        $sgwt  (lead -.gen %.(+.gen (quad noop expr expr expr)))
        $sgzp  (lead -.gen %.(+.gen dubs))
        $smcl   (lead -.gen %.(+.gen (twin expr moar)))
        $smfs   (lead -.gen %.(+.gen expr))
        $smsg   (lead -.gen %.(+.gen (twin expr moar)))
        $smsm   (lead -.gen %.(+.gen dubs))
        $tsbr   (lead -.gen %.(+.gen dubs))
        $tscl   (lead -.gen %.(+.gen (twin moan expr)))
        $tsfs   (lead -.gen %.(+.gen (trio tora expr expr)))
        $tssm   (lead -.gen %.(+.gen (trio tora expr expr)))
        $tsdt   (lead -.gen %.(+.gen trip))
        $tswt   (lead -.gen %.(+.gen (quad noop expr expr expr)))
        $tsgl   (lead -.gen %.(+.gen dubs))
        $tshp   (lead -.gen %.(+.gen dubs))
        $tsgr   (lead -.gen %.(+.gen dubs))
        $tskt   (lead -.gen %.(+.gen (quad tora noop expr expr)))
        $tsls   (lead -.gen %.(+.gen dubs))
        $tssg   (lead -.gen %.(+.gen moar))
        $tstr   (lead -.gen %.(+.gen trip))
        $tscm   (lead -.gen %.(+.gen dubs))
        $wtbr    (lead -.gen %.(+.gen moar))
        $wthp  (lead -.gen %.(+.gen (twin noop (moto dubs))))
        $wtcl    (lead -.gen %.(+.gen trey))
        $wtdt  (lead -.gen %.(+.gen trey))
        $wtkt  (lead -.gen %.(+.gen trip))
        $wtgl  (lead -.gen %.(+.gen dubs))
        $wtgr  (lead -.gen %.(+.gen dubs))
        $wtls  (lead -.gen %.(+.gen (trio noop expr (moto dubs))))
        $wtpm   (lead -.gen %.(+.gen moar))
        $wtpt  (lead -.gen %.(+.gen trip))
        $wtsg  (lead -.gen %.(+.gen trip))
        $wtts  (lead -.gen %.(+.gen (twin expr noop)))
        $wtzp   (lead -.gen %.(+.gen expr))
        $zpcm  (lead -.gen %.(+.gen dubs))
        $zpgr  (lead -.gen %.(+.gen expr))
        $zpsm  (lead -.gen %.(+.gen dubs))
        $zpts  (lead -.gen %.(+.gen expr))
        $zpwt  (lead -.gen %.(+.gen nexp))
        $zpzp  (lead -.gen %.(+.gen noop))
      ==
    ++  arms
      (raid (twin noop (raid (twin noop heel))))
    ::
    ++  bark 
      |=  wof/woof
      ?-(wof @ [wof vit], ^ (lead ~ (expr p.wof)))
    ::
    ++  dubs
      (twin expr expr)
    ::
    ++  expr
      |=  p/hoon
      ^$(gen p)
    ::
    ++  exps
      |=  p/(list (pair term hoon))
      =|  out/(list (pair term hoon))
      |-  ^+  [out vit]
      ?~  p
        [out vit]
      =^  nex  vit  ^^$(gen q.i.p)
      $(p t.p, out [[p.i.p nex] out])
    ::
    ++  heel
      |=  bud/foot
      ?-  -.bud
        $ash  =^(nex vit ^$(gen p.bud) [[%ash nex] vit])
        $elm  =^(nex vit ^$(gen p.bud) [[%elm nex] vit])
      ==
    ::
    ++  lead
      |*  {sem/@tas out/{* life}}
      [[sem -.out] +.out]
    ::
    ++  moan
      (moto nexp)
    ::
    ++  moar
      (moto expr)
    ::
    ++  moto
      |*  etc/$-(* {* life})
      |*  bud/*
      ^+  [bud vit]
      ?:  =(~ bud)  [bud vit]
      =^  heb  vit  (etc -.bud)
      =^  mor  vit  $(bud +.bud)
      [[heb mor] vit]
    ::
    ++  nexp
      (twin noop expr)
    ::
    ++  noop
      |*  bud/*
      [bud vit]
    ::
    ++  quad
      |*  $:  one/$-(* {* life})
              two/$-(* {* life})
              tri/$-(* {* life})
              qua/$-(* {* life})
          ==
      |*  bud/*
      =^  yal  vit  (one -.bud)
      =^  ves  vit  (two +<.bud)
      =^  jot  vit  (tri +>-.bud)
      =^  wip  vit  (qua +>+.bud)
      [[yal ves jot wip] vit]
    ::
    ++  raid
      |*  etc/$-(* {* life})
      |*  bud/*
      ^+  [bud vit]
      ?:  =(~ bud)  [bud vit]
      =^  lef  vit  $(bud +<.bud)
      =^  ryt  vit  $(bud +>.bud)
      =^  top  vit  (etc ->.bud)
      [[[-<.bud top] lef ryt] vit]
    ::
    ++  trey
      (trio expr expr expr)
    ::
    ++  trio
      |*  $:  one/$-(* {* life})
              two/$-(* {* life})
              tri/$-(* {* life})
          ==
      |*  bud/*
      =^  yal  vit  (one -.bud)
      =^  ves  vit  (two +<.bud)
      =^  jot  vit  (tri +>.bud)
      [[yal ves jot] vit]
    ::
    ++  trip
      (trio noop expr expr)
    ::
    ++  toad
      |=  bud/$@(term {p/term q/hoon})
      ?@  bud  [bud vit]
      (lead p.bud (expr q.bud))
    ::
    ++  tora
      (twin noop twee)
    ::
    ++  tung
      |=  bud/{p/what q/$@(term tune)}
      ?@  q.bud  [bud vit]
      (lead p.bud %.(q.bud (twin (raid (twin noop twee)) (moto expr))))
    ::
    ++  twee
      |=  bud/(unit hoon)
      ?~  bud  [~ vit]
      (lead ~ (expr u.bud))
    ::
    ++  twin
      |*  {one/$-(* {* life}) two/$-(* {* life})}
      |*  bud/*
      =^  yal  vit  (one -.bud)
      =^  ves  vit  (two +.bud)
      [[yal ves] vit]
    --
  --
::
::::  5c: compiler backend and prettyprinter
  ::
++  ut
  ~%    %ut
      +>+
    ==
      %fan    fan
      %rib    rib
      %vet    vet
      %fab    fab
      %blow   blow
      %burn   burn
      %busk   busk
      %buss   buss
      %crop   crop
      %duck   duck
      %dune   dune
      %dunk   dunk
      %epla   epla
      %emin   emin
      %emul   emul
      %felt   felt
      %fine   fine
      %fire   fire
      %fish   fish
      %fond   fond
      %fund   fund
      %funk   funk
      %fuse   fuse
      %gain   gain
      %lose   lose
      %mint   mint
      %moot   moot
      %mull   mull
      %nest   nest
      %peel   peel
      %play   play
      %peek   peek
      %repo   repo
      %rest   rest
      %tack   tack
      %toss   toss
      %wrap   wrap
    ==
  =+  :*  fan=*(set {type hoon})
          rib=*(set {type type hoon})
          vet=`?`&
          fab=`?`&
      ==
  =+  sut=`type`%noun
  |%
  ++  blow
    |=  {gol/type gen/hoon}
    ^-  {type nock}
    =+  pro=(mint gol gen)
    ::  ~>  %slog.[%0 (dunk 'blow-subject')]
    =+  bus=bran
    ::  ~|  mask.bus
    =+  jon=(apex:musk bus q.pro)
    ?~  jon
      ?:  fab
        pro
      ~|  %musk-stopped
      !!
    ?:  ?=($| -.u.jon)
      ?:  fab
        pro
      ~|  %musk-blocked
      !!
    [p.pro [%1 p.u.jon]]
  ::
  ++  burn
    =+  gil=*(set type)
    |-  ^-  (unit)
    ?-    sut
        {$atom *}   q.sut
        {$cell *}   %+  biff  $(sut p.sut)
                  |=(a/* (biff ^$(sut q.sut) |=(b/* `[a b])))
        {$core *}   (biff $(sut p.sut) |=(* `[p.s.q.sut +<]))
        {$face *}   $(sut repo)
        {$fork *}   ~
        {$help *}   $(sut repo)
        {$hold *}   ?:  (~(has in gil) sut)
                      ~
                    $(sut repo, gil (~(put in gil) sut))
        $noun       ~
        $void       ~
    ==
  ++  bran
    =+  gil=*(set type)
    |-  ^-  seminoun:musk
    ?-    sut
      $noun      [&+[~ ~ ~] ~]
      $void      [&+[~ ~ ~] ~]
      {$atom *}  ?~(q.sut [&+[~ ~ ~] ~] [&+~ u.q.sut])
      {$cell *}  (combine:musk $(sut p.sut) $(sut q.sut))
      {$core *}  %+  combine:musk 
                   ?~  p.s.q.sut  [&+[~ ~ ~] ~]
                   [&+~ p.s.q.sut]
                 $(sut p.sut) 
      {$face *}  $(sut repo)
      {$fork *}  [&+[~ ~ ~] ~]
      {$help *}  $(sut repo)
      {$hold *}  ?:  (~(has in gil) sut)
                   [&+[~ ~ ~] ~]
                 $(sut repo, gil (~(put in gil) sut))
    ==
  ::
  ++  busk
    ~/  %busk
    |=  gen/hoon
    ^-  type
    [%face [~ ~ [gen ~]] sut]
  ::
  ++  buss
    ~/  %buss
    |=  {{wat/what cog/term} gen/hoon}
    ^-  type
    [%face [wat [[cog ~ ~ gen] ~ ~] ~] sut]
  ::
  ++  conk
    |=  got/toga
    ^-  type
    ?@  got  [%face [~ got] sut]
    ?-  -.got
      $0  sut
      $1  [%face p.got $(got q.got)]
      $2  ?>  |(!vet (nest(sut [%cell %noun %noun]) & sut))
          :+  %cell
            $(got p.got, sut (peek %both 2))
          $(got q.got, sut (peek %both 3))
    ==
  ::
  ++  crop
    ~/  %crop
    |=  ref/type
    =+  bix=*(set {type type})
    =<  dext
    |%
    ++  dext
      ^-  type
      ~_  leaf+"crop"
      ::  ~_  (dunk 'dext: sut')
      ::  ~_  (dunk(sut ref) 'dext: ref')
      ?:  |(=(sut ref) =(%noun ref))
        %void
      ?:  =(%void ref)
        sut
      ?-    sut
          {$atom *}
        ?+  ref      sint
          {$atom *}  ?^  q.sut
                       ?^(q.ref ?:(=(q.ref q.sut) %void sut) %void)
                     ?^(q.ref sut %void)
          {$cell *}  sut
        ==
      ::
          {$cell *}
        ?+  ref      sint
          {$atom *}  sut
          {$cell *}  ?.  (nest(sut p.ref) | p.sut)  sut
                     (cell p.sut dext(sut q.sut, ref q.ref))
        ==
      ::
          {$core *}  ?:(?=(?({$atom *} {$cell *}) ref) sut sint)
          {$face *}  (face p.sut dext(sut q.sut))
          {$fork *}  (fork (turn ~(tap in p.sut) |=(type dext(sut +<))))
          {$help *}  (help p.sut dext(sut q.sut))
          {$hold *}  ?<  (~(has in bix) [sut ref])
                     dext(sut repo, bix (~(put in bix) [sut ref]))
          $noun      dext(sut repo)
          $void      %void
      ==
    ::
    ++  sint
      ^-  type
      ?+    ref    !!
        {$core *}  sut
        {$face *}  dext(ref repo(sut ref))
        {$fork *}  =+  yed=~(tap in p.ref)
                   |-  ^-  type
                   ?~  yed  sut
                   $(yed t.yed, sut dext(ref i.yed))
        {$hold *}  dext(ref repo(sut ref))
      ==
    --
  ::
  ++  cool
    |=  {pol/? hyp/wing ref/type}
    ^-  type
    =+  fid=(find %both hyp)
    ?-  -.fid
      $|  sut
      $&  =<  q
          %+  take  p.p.fid
          |=(a/type ?:(pol (fuse(sut a) ref) (crop(sut a) ref)))
    ==
  ::
  ++  duck  ^-(tank ~(duck us sut))
  ++  dune  |.(duck)
  ++  dunk
    |=  paz/term  ^-  tank
    :+  %palm
      [['.' ~] ['-' ~] ~ ~]
    [[%leaf (mesc (trip paz))] duck ~]
  ::
  ++  elbo
    |=  {lop/palo rig/(list (pair wing hoon))}
    ^-  type
    ?:  ?=($& -.q.lop)
      |-  ^-  type
      ?~  rig
        p.q.lop
      =+  zil=(play q.i.rig)
      =+  dar=(tack(sut p.q.lop) p.i.rig zil)
      %=  $
        rig      t.rig
        p.q.lop  q.dar
      ==
    =+  hag=~(tap in q.q.lop)
    %-  fire
    |-  ^+  hag
    ?~  rig
      hag
    =+  zil=(play q.i.rig)
    =+  dix=(toss p.i.rig zil hag)
    %=  $
      rig  t.rig
      hag  q.dix
    ==
  ::
  ++  ergo
    |=  {lop/palo rig/(list (pair wing hoon))}
    ^-  (pair type nock)
    =+  axe=(tend p.lop)
    =|  hej/(list (pair axis nock))
    ?:  ?=($& -.q.lop)
      =-  [p.- (hike axe q.-)]
      |-  ^-  (pair type (list (pair axis nock)))
      ?~  rig
        [p.q.lop hej]
      =+  zil=(mint %noun q.i.rig)
      =+  dar=(tack(sut p.q.lop) p.i.rig p.zil)
      %=  $
        rig      t.rig
        p.q.lop  q.dar
        hej      [[p.dar q.zil] hej]
      ==
    =+  hag=~(tap in q.q.lop)
    =-  [(fire p.-) [%9 p.q.lop (hike axe q.-)]]
    |-  ^-  (pair (list (pair type foot)) (list (pair axis nock)))
    ?~  rig
      [hag hej]
    =+  zil=(mint %noun q.i.rig)
    =+  dix=(toss p.i.rig p.zil hag)
    %=  $
      rig  t.rig
      hag  q.dix
      hej  [[p.dix q.zil] hej]
    ==
  ::
  ++  endo
    |=  {lop/(pair palo palo) dox/type rig/(list (pair wing hoon))}
    ^-  (pair type type)
    ?:  ?=($& -.q.p.lop)
      ?>  ?=($& -.q.q.lop)
      |-  ^-  (pair type type)
      ?~  rig
        [p.q.p.lop p.q.q.lop]
      =+  zil=(mull %noun dox q.i.rig)
      =+  ^=  dar
          :-  p=(tack(sut p.q.p.lop) p.i.rig p.zil)
              q=(tack(sut p.q.q.lop) p.i.rig q.zil)
      ?>  =(p.p.dar p.q.dar)
      %=  $
        rig        t.rig
        p.q.p.lop  q.p.dar
        p.q.q.lop  q.q.dar
      ==
    ?>  ?=($| -.q.q.lop)
    ?>  =(p.q.p.lop p.q.q.lop)
    =+  hag=[p=~(tap in q.q.p.lop) q=~(tap in q.q.q.lop)]
    =-  [(fire p.-) (fire(vet |) q.-)]
    |-  ^-  (pair (list (pair type foot)) (list (pair type foot)))
    ?~  rig
      hag
    =+  zil=(mull %noun dox q.i.rig)
    =+  ^=  dix
        :-  p=(toss p.i.rig p.zil p.hag)
            q=(toss p.i.rig q.zil q.hag)
    ?>  =(p.p.dix p.q.dix)
    %=  $
      rig  t.rig
      hag  [q.p.dix q.q.dix]
    ==
  ::
  ++  ad
    |%
    ++  arc
      |%
      ++  deft                                          ::  generic
        |%
        ++  bath  *                                     ::  leg match type
        ++  claw  *                                     ::  arm match type
        ++  form  |*({* *} p=+<-)                       ::  attach build state
        ++  skin  |*(p/* p)                             ::  reveal build state
        ++  meat  |*(p/* p)                             ::  remove build state
        --
      ++  make                                          ::  for mint
        |%
        ++  bath  type                                  ::  leg match type
        ++  claw  onyx                                  ::  arm
        ++  form  |*({* *} [p=+<- q=+<+])               ::
        ++  skin  |*({p/* q/*} q)                       ::  unwrap baggage
        ++  meat  |*({p/* q/*} p)                       ::  unwrap filling
        --
      --
    ++  def
      =+  deft:arc
      |%  +-  $
      =>  +<
      |%
      ++  pord  |*(* (form +< *nock))                   ::  wrap mint formula
      ++  rosh  |*(* (form +< *(list pock)))            ::  wrap mint changes
      ++  fleg  _(pord *bath)                           ::  legmatch + code
      ++  fram  _(pord *claw)                           ::  armmatch +
      ++  foat  _(rosh *bath)                           ::  leg with changes
      ++  fult  _(rosh *claw)                           ::  arm with changes
      --  --
    ::
    ++  lib
      |%
      ++  deft
        =>  (def deft:arc)
        |%
        ++  halp  $-(hoon fleg)
        ++  vant
          |%  ++  trep  $-({bath wing bath} {axis bath})
              ++  tasp  $-({{axis bath} fleg foat} foat)
              ++  tyle  $-(foat foat)
          --
        ++  vunt
          |%  ++  trep  $-({claw wing bath} {axis claw})
              ++  tasp  $-({{axis claw} fleg fult} fult)
              ++  tyle  $-(fult foat)
        --  --
      ::
      ++  make
        =>  (def make:arc)
        |%
        ++  halp  |~  a/hoon
                  ^-  fleg
                  (mint %noun a)
        ++  vant
          |%  ++  trep  |=  {a/type b/wing c/type}
                        ^-  {axis type}
                        (tack(sut a) b c)
              ++  tasp  |=  {a/(pair axis type) b/fleg c/foat}
                        ^-  foat
                        [q.a [[p.a (skin b)] (skin c)]]
              ++  tyle  |=(foat +<)
          --
        ++  vunt
          |%  ++  trep  |=  {a/claw b/wing c/bath}
                        ^-  (pair axis claw)
                        (toss b c a)
              ++  tasp  |~  {a/(pair axis claw) b/fleg c/fult}
                        ^-  fult
                        [q.a [[p.a (skin b)] (skin c)]]
              ++  tyle  |~  fult
                        ^-  foat
                        [(fire +<-) +<+]
      --  --  --
    ::
    ++  bin
      =+  deft:lib
      |%  +-  $
      =>  +<
      |%
      ++  rame
        =>  vant  |%
            ++  clom  bath
            ++  chog  fleg
            ++  ceut  foat
        --
      ++  gelp
        =>  vunt  |%
            ++  clom  claw
            ++  chog  fram
            ++  ceut  fult
        --
      ++  ecbo  (ecco rame)
      ++  eclo  (ecco gelp)
      ++  ecco
        =+  rame
        |%  +-  $
        =>  +<
        |=  {rum/clom rig/(list (pair wing hoon))}
        ^-  foat
        %-  tyle
        |-  ^-  ceut
        ?~  rig  (rosh rum)
        =+  mor=$(rig t.rig)
        =+  zil=(halp q.i.rig)
        =+  dar=(trep (meat mor) p.i.rig (meat zil))
        (tasp dar zil mor)
      --  --  --  --
  ::
  ++  oc
    =+  inc=(bin:ad)
    |%  +-  $
    =>  inc
    |%
    ++  echo
      |=  {rum/bath rig/(list (pair wing hoon))}
      (ecbo rum rig)
    ::
    ++  ecmo
      |=  {hag/claw rig/(list (pair wing hoon))}
      (eclo hag rig)
    --  --
  ::
  ++  etco
    |=  {lop/palo rig/(list (pair wing hoon))}
    ^-  (pair type nock)
    =+  cin=(oc (bin:ad make:lib:ad))
    =.  rig  (flop rig)         ::  XX this unbreaks, void order in devulc
    =+  axe=(tend p.lop)
    ?:  ?=($& -.q.lop)
      =-  [p.- (hike axe q.-)]
      (echo:cin p.q.lop rig)
    =-  [p.- [%9 p.q.lop (hike axe q.-)]]
    (ecmo:cin ~(tap in q.q.lop) rig)
  ::
  ++  et
    |_  {hyp/wing rig/(list (pair wing hoon))}
    ::
    ++  play
      ^-  type
      =+  lug=(find %read hyp)
      ?:  ?=($| -.lug)  ~>(%mean.[%leaf "hoon"] ?>(?=($~ rig) p.p.lug))
      (elbo p.lug rig)
    ::
    ++  mint
      |=  gol/type
      ^-  (pair type nock)
      =+  lug=(find %read hyp)
      ?:  ?=($| -.lug)  ~>(%mean.[%leaf "hoon"] ?>(?=($~ rig) p.lug))
      =-  ?>(?|(!vet (nest(sut gol) & p.-)) -)
      (etco p.lug rig)
    ::
    ++  mull
      |=  {gol/type dox/type}
      ^-  {type type}
      =+  lug=[p=(find %read hyp) q=(find(sut dox) %read hyp)]
      ?:  ?=($| -.p.lug)
        ?>   &(?=($| -.q.lug) ?=($~ rig))
        [p.p.p.lug p.p.q.lug]
      ?>  ?=($& -.q.lug)
      =-  ?>(?|(!vet (nest(sut gol) & p.-)) -)
      (endo [p.p.lug p.q.lug] dox rig)
    --
  ::
  ++  epla
    ~/  %epla
    |=  {hyp/wing rig/(list (pair wing hoon))}
    ^-  type
    ~(play et hyp rig)
  ::
  ++  emin
    ~/  %emin
    |=  {gol/type hyp/wing rig/(list (pair wing hoon))}
    ^-  (pair type nock)
    (~(mint et hyp rig) gol)
  ::
  ++  emul
    ~/  %emul
    |=  {gol/type dox/type hyp/wing rig/(list (pair wing hoon))}
    ^-  (pair type type)
    (~(mull et hyp rig) gol dox)
  ::
  ++  felt
    ~/  %felt
    |=  lap/opal
    ^-  type
    ?-  -.lap
      $&  p.lap
      $|  %-  fire
          %+  turn  ~(tap in q.lap)
          |=  {a/type b/foot}
          [a [%ash %$ 1]]
    ==
  ::
  ++  fond
    ~/  %fond
    |=  {way/vial hyp/wing}
    =>  |%
        ++  pony                                        ::  raw match
                  $@  $~                                ::  void
                  %+  each                              ::  natural/abnormal
                    (pair what palo)                    ::  arm or leg
                  %+  each                              ::  abnormal
                    @ud                                 ::  unmatched
                  (pair what (pair type nock))          ::  synthetic
        --
    ^-  pony
    ?~  hyp
      [%& ~ ~ %& sut]
    =+  mor=$(hyp t.hyp)
    ?-    -.mor
        $|
      ?-    -.p.mor
          $&  mor
          $|
        =+  fex=(mint(sut p.q.p.p.mor) %noun [%wing i.hyp ~])
        [%| %| p.p.p.mor p.fex (comb q.q.p.p.mor q.fex)]
      ==
    ::
        $&
      =.  sut  (felt q.q.p.mor)
      =>  :_  +
          :*  axe=`axis`1
              lon=p.q.p.mor
              heg=?^(i.hyp i.hyp [%| p=0 q=(some i.hyp)])
          ==
      ?:  ?=($& -.heg)
        [%& ~ [`p.heg lon] %& (peek way p.heg)]
      =|  gil/(set type)
      =<  $
      |%  ++  here  ?:  =(0 p.heg)
                      [%& ~ [~ `axe lon] %& sut]
                    [%| %& (dec p.heg)]
          ++  join  |=({a/what b/what} ?~(a b a))
          ++  lose  [%| %& p.heg]
          ++  stop  ?~(q.heg here lose)
          ++  twin  |=  {hax/pony yor/pony}
                    ^-  pony
                    ~_  leaf+"find-fork"
                    ?:  =(hax yor)  hax
                    ?~  hax  yor
                    ?~  yor  hax
                    ?:  ?=($| -.hax)
                      ?>  ?&  ?=($| -.yor)
                              ?=($| -.p.hax)
                              ?=($| -.p.yor)
                              =(q.q.p.p.hax q.q.p.p.yor)
                          ==
                      :^    %|
                          %|
                        (join p.p.p.hax p.p.p.yor)
                      [(fork p.q.p.p.hax p.q.p.p.yor ~) q.q.p.p.hax]
                    ?>  ?=($& -.yor)
                    ?>  =(p.q.p.hax p.q.p.yor)
                    :^    %&  
                        (join p.p.hax p.p.yor)
                      p.q.p.hax
                    ?:  &(?=($& -.q.q.p.hax) ?=($& -.q.q.p.yor))
                      [%& (fork p.q.q.p.hax p.q.q.p.yor ~)]
                    ?>  &(?=($| -.q.q.p.hax) ?=($| -.q.q.p.yor))
                    ?>  =(p.q.q.p.hax p.q.q.p.yor)
                    =+  wal=(~(uni in q.q.q.p.hax) q.q.q.p.yor)
                    [%| p.q.q.p.hax wal]
          ++  $
            ^-  pony
            ?-    sut
                $void       stop
                $noun       stop
                {$atom *}   stop
                {$cell *}
              ?~  q.heg  here
              =+  taf=$(axe (peg axe 2), sut p.sut)
              ?~  taf  ~
              ?:  |(?=($& -.taf) ?=($| -.p.taf))
                taf
              $(axe (peg axe 3), p.heg p.p.taf, sut q.sut)
            ::
                {$core *}
              ?~  q.heg  here
              =^  zem  p.heg
                  =+  zem=(loot u.q.heg q.s.q.sut)
                  ?~  zem  [~ p.heg]
                  ?:(=(0 p.heg) [zem 0] [~ (dec p.heg)])
              ?^  zem
                :^    %&  
                    p.q.u.zem
                  [`axe lon]
                [%| (peg 2 p.u.zem) [[sut(p.q %gold) q.q.u.zem] ~ ~]]
              =+  pec=(peel way p.q.sut)
              ?.  sam.pec  lose
              ?:  con.pec  $(sut p.sut, axe (peg axe 3))
              $(sut (peek(sut p.sut) way 2), axe (peg axe 6))
            ::
                {$help *}
              $(sut repo)
            ::
                {$face *}
              ?:  ?=($~ q.heg)  here(sut q.sut)
              =*  zot  q.p.sut
              ?@  zot
                ?:(=(u.q.heg zot) here(sut q.sut) lose)
              =<  main
              |%
              ++  main
                ^-  pony
                =+  tyr=(~(get by p.zot) u.q.heg)
                ?~  tyr
                  next
                ?~  q.u.tyr
                  $(sut q.sut, lon [~ lon], p.heg +(p.heg))
                ?.  =(0 p.heg)
                  next(p.heg (dec p.heg))
                =+  tor=(fund way u.q.u.tyr)
                ?-  -.tor
                  $&  [%& ~ (weld p.p.tor `vein`[~ `axe lon]) q.p.tor]
                  $|  [%| %| p.u.tyr p.p.tor (comb [%0 axe] q.p.tor)]
                ==
              ++  next
                |-  ^-  pony
                ?~  q.zot
                  ^$(sut q.sut, lon [~ lon])
                =+  tiv=(mint(sut q.sut) %noun i.q.zot)
                =+  fid=^$(sut p.tiv, lon ~, axe 1, gil ~)
                ?~  fid  ~
                ?:  ?=({$| $& *} fid)
                  $(q.zot t.q.zot, p.heg p.p.fid)
                =+  ^-  {wat/what vat/(pair type nock)}
                    ?-    -.fid
                      $&  [p.p.fid (fine %& q.p.fid)]
                      $|  [p.p.p.fid (fine %| q.p.p.fid)]
                    ==
                [%| %| wat p.vat (comb (comb [%0 axe] q.tiv) q.vat)]
              --
            ::
                {$fork *}
              =+  wiz=(turn ~(tap in p.sut) |=(a/type ^$(sut a)))
              ?~  wiz  ~
              |-  ^-  pony
              ?~  t.wiz  i.wiz
              (twin i.wiz $(wiz t.wiz))
            ::
                {$hold *}
              ?:  (~(has in gil) sut)
                ~
              $(gil (~(put in gil) sut), sut repo)
            ==
      --
    ==
  ::
  ++  find
    ~/  %find
    |=  {way/vial hyp/wing}
    ^-  port
    ~_  (show [%c %find] %l hyp)
    =-  ?@  -  !!
        ?-    -<
          $&  [%& q.p.-]
          $|  ?-  -.p.-
                $|  [%| q.p.p.-]
                $&  !!
        ==    ==
    (fond way hyp)
  ::
  ++  fund
    ~/  %fund
    |=  {way/vial gen/hoon}
    ^-  port
    =+  hup=~(reek ap gen)
    ?~  hup
      [%| (mint %noun gen)]
    (find way u.hup)
  ::
  ++  fine
    ~/  %fine
    |=  tor/port
    ^-  (pair type nock)
    ?-  -.tor
      $|  p.tor
      $&  =+  axe=(tend p.p.tor)
          ?-  -.q.p.tor
            $&  [`type`p.q.p.tor %0 axe]
            $|  [(fire ~(tap in q.q.p.tor)) [%9 p.q.p.tor %0 axe]]
    ==    ==
  ::
  ++  fire
    |=  hag/(list {p/type q/foot})
    ^-  type
    ?:  ?=({{* {$elm $~ $1}} $~} hag)
      p.i.hag
    %-  fork
    %+  turn
      hag.$
    |=  {p/type q/foot}
    :-  %hold
    ?.  ?=({$core *} p)
      ~_  (dunk %fire-type)
      ~>(%mean.[%leaf "fire-core"] !!)
    =+  dox=[%core q.q.p q.p]
    ?:  ?=($ash -.q)
      ::  ~_  (dunk(sut [%cell q.q.p p.p]) %fire-dry)
      ?>  ?|(!vet (nest(sut q.q.p) & p.p))
      [dox p.q]
    ?>  ?=($elm -.q)
    ::  ~_  (dunk(sut [%cell q.q.p p.p]) %fire-wet)
    ::  =.  p.p  ?:(fab p.p (redo(sut p.p) q.q.p))
    =.  p.p  (redo(sut p.p) q.q.p)
    ?>  ?|  !vet
            (~(has in rib) [sut dox p.q])
            !=(** (mull(sut p, rib (~(put in rib) sut dox p.q)) %noun dox p.q))
        ==
    [p p.q]
  ::
  ++  fish
    ~/  %fish
    |=  axe/axis
    =+  vot=*(set type)
    |-  ^-  nock
    ?-  sut
        $void       [%1 1]
        $noun       [%1 0]
        {$atom *}   ?~  q.sut
                      (flip [%3 %0 axe])
                    [%5 [%1 u.q.sut] [%0 axe]]
        {$cell *}
      %+  flan
        [%3 %0 axe]
      (flan $(sut p.sut, axe (peg axe 2)) $(sut q.sut, axe (peg axe 3)))
    ::
        {$core *}   [%0 0]
        {$face *}   $(sut q.sut)
        {$fork *}   =+  yed=~(tap in p.sut)
                    |-  ^-  nock
                    ?~(yed [%1 1] (flor ^$(sut i.yed) $(yed t.yed)))
        {$help *}   $(sut q.sut)
        {$hold *}
      ?:  (~(has in vot) sut)
        [%0 0]
      =>  %=(. vot (~(put in vot) sut))
      $(sut repo)
    ==
  ::
  ++  fuse
    ~/  %fuse
    |=  ref/type
    =+  bix=*(set {type type})
    |-  ^-  type
    ?:  ?|(=(sut ref) =(%noun ref))
      sut
    ?-    sut
        {$atom *}
      ?-    ref
          {$atom *}   =+  foc=?:((fitz p.ref p.sut) p.sut p.ref)
                      ?^  q.sut
                        ?^  q.ref
                          ?:  =(q.sut q.ref)
                            [%atom foc q.sut]
                          %void
                        [%atom foc q.sut]
                      [%atom foc q.ref]
          {$cell *}   %void
          *           $(sut ref, ref sut)
      ==
        {$cell *}
      ?-  ref
        {$cell *}   (cell $(sut p.sut, ref p.ref) $(sut q.sut, ref q.ref))
        *           $(sut ref, ref sut)
      ==
    ::
        {$core *}  $(sut repo)
        {$face *}  (face p.sut $(sut q.sut))
        {$fork *}  (fork (turn ~(tap in p.sut) |=(type ^$(sut +<))))
        {$help *}  (help p.sut $(sut q.sut))
        {$hold *}
      ?:  (~(has in bix) [sut ref])
        ~>(%mean.[%leaf "fuse-loop"] !!)
      $(sut repo, bix (~(put in bix) [sut ref]))
    ::
        $noun       ref
        $void       %void
    ==
  ::
  ++  gain
    ~/  %gain
    |=  gen/hoon  ^-  type
    (chip & gen)
  ::
  ++  harp
    |=  dab/(map term (pair what foot))
    ^-  ?($~ ^)
    ?:  ?=($~ dab)
      ~
    =+  ^=  vad
        ?-  -.q.q.n.dab
          $ash  q:(mint %noun p.q.q.n.dab)
          $elm  q:(mint(vet |) %noun p.q.q.n.dab)
        ==
    ?-    dab
      {* $~ $~}   vad
      {* $~ *}    [vad $(dab r.dab)]
      {* * $~}    [vad $(dab l.dab)]
      {* * *}     [vad $(dab l.dab) $(dab r.dab)]
    ==
  ::
  ++  hope
    |=  dom/(map @ tomb)
    ^-  ?($~ ^)
    ?:  ?=($~ dom)
      ~
    =+  dov=(harp q.q.n.dom)
    ?-    dom
      {* $~ $~}   dov
      {* $~ *}    [dov $(dom r.dom)]
      {* * $~}    [dov $(dom l.dom)]
      {* * *}     [dov $(dom l.dom) $(dom r.dom)]
    ==
  ::
  ++  lose
    ~/  %lose
    |=  gen/hoon  ^-  type
    (chip | gen)
  ::
  ++  chip
    ~/  %chip
    |=  {how/? gen/hoon}  ^-  type
    ?:  ?=({$wtts *} gen)
      (cool how q.gen (play ~(bunt al [%herb p.gen])))
    ?:  ?&(how ?=({$wtpm *} gen))
      |-(?~(p.gen sut $(p.gen t.p.gen, sut ^$(gen i.p.gen))))
    ?:  ?&(!how ?=({$wtbr *} gen))
      |-(?~(p.gen sut $(p.gen t.p.gen, sut ^$(gen i.p.gen))))
    =+  neg=~(open ap gen)
    ?:(=(neg gen) sut $(gen neg))
  ::
  ++  mint
    ~/  %mint
    |=  {gol/type gen/hoon}
    ^-  {p/type q/nock}
    ~&  %pure-mint
    |^  ^-  {p/type q/nock}
    ?:  ?&(=(%void sut) !?=({$dbug *} gen))
      ?.  |(!vet ?=({$lost *} gen) ?=({$zpzp *} gen))
        ~>(%mean.[%leaf "mint-vain"] !!)
      [%void %0 0]
    ?-    gen
    ::
        {^ *}
      =+  hed=$(gen p.gen, gol %noun)
      =+  tal=$(gen q.gen, gol %noun)
      [(nice (cell p.hed p.tal)) (cons q.hed q.tal)]
    ::
        {$cold *}  (blow gol p.gen)
        {$ktcn *}  $(fab |, gen p.gen)
        {$brcn *}  (grow %gold [%$ 1] p.gen q.gen)
    ::
        {$cnts *}  (~(mint et p.gen q.gen) gol)
        {$dtkt *}
      =+  nef=$(gen [%bunt p.gen])
      [p.nef [%11 [%1 %151 p.nef] q:$(gen q.gen, gol %noun)]]
    ::
        {$dtls *}  [(nice [%atom %$ ~]) [%4 q:$(gen p.gen, gol [%atom %$ ~])]]
        {$sand *}  [(nice (play gen)) [%1 q.gen]]
        {$rock *}  [(nice (play gen)) [%1 q.gen]]
    ::
        {$dttr *}
      [(nice %noun) [%2 q:$(gen p.gen, gol %noun) q:$(gen q.gen, gol %noun)]]
    ::
        {$dtts *}
      =+  [one two]=[$(gen p.gen, gol %noun) $(gen q.gen, gol %noun)]
      [(nice bool) [%5 q:$(gen p.gen, gol %noun) q:$(gen q.gen, gol %noun)]]
    ::
        {$dtwt *}  [(nice bool) [%3 q:$(gen p.gen, gol %noun)]]
        {$hand *}  [p.gen q.gen]
        {$ktbr *}  =+(vat=$(gen p.gen) [(wrap(sut p.vat) %iron) q.vat])
    ::
        {$ktls *}
      =+(hif=(nice (play p.gen)) [hif q:$(gen q.gen, gol hif)])
    ::
        {$ktpm *}  =+(vat=$(gen p.gen) [(wrap(sut p.vat) %zinc) q.vat])
        {$ktsg *}
      =+  nef=$(gen p.gen)
      :-  p.nef
      =+  cag=burn
      ?~  cag  q.nef
      =+  moc=(mink [u.cag q.nef] |=({* *} ~))
      ?:(?=($0 -.moc) [%1 p.moc] q.nef)
    ::
        {$ktts *}  =+(vat=$(gen q.gen) [(conk(sut p.vat) p.gen) q.vat])
        {$tune *}  [(face p.gen sut) [%0 %1]]
        {$ktwt *}  =+(vat=$(gen p.gen) [(wrap(sut p.vat) %lead) q.vat])
    ::
        {$docs *}
      =+  hum=$(gen q.gen)
      [(help [%docs p.gen] p.hum) q.hum]
    ::
        {$sgzp *}  ~_(duck(sut (play p.gen)) $(gen q.gen))
        {$sggr *}
      =+  hum=$(gen q.gen)
      :: ?:  &(huz !?=($|(@ [?(%sgcn %sgls) ^]) p.gen))
      ::  hum
      :-  p.hum
      :+  %10
        ?-    p.gen
            @   p.gen
            ^   [p.p.gen q:$(gen q.p.gen, gol %noun)]
        ==
      q.hum
    ::
        {$tsgr *}
      =+  fid=$(gen p.gen, gol %noun)
      =+  dov=$(sut p.fid, gen q.gen)
      [p.dov (comb q.fid q.dov)]
    ::
        {$tstr *}
      $(gen r.gen, sut (buss p.gen q.gen))
    ::
        {$tscm *}
      $(gen q.gen, sut (busk p.gen))
    ::
        {$wtcl *}
      =+  nor=$(gen p.gen, gol bool)
      =+  fex=(gain p.gen)
      =+  wux=(lose p.gen)
      =+  ^=  duy
          ?:  =(%void fex)
            ?:(=(%void wux) [%0 0] [%1 1])
          ?:(=(%void wux) [%1 0] q.nor)
      =+  hiq=$(sut fex, gen q.gen)
      =+  ran=$(sut wux, gen r.gen)
      [(fork p.hiq p.ran ~) (cond duy q.hiq q.ran)]
    ::
        {$fits *}
      :-  (nice bool)
      =+  ref=(play p.gen)
      =+  fid=(find %read q.gen)
      ~|  [%test q.gen]
      |-  ^-  nock
      ?-  -.fid
        $&  ?-  -.q.p.fid
              $&  (fish(sut ref) (tend p.p.fid))
              $|  $(fid [%| (fine fid)])
            ==
        $|  [%7 q.p.fid (fish(sut ref) 1)]
      ==
    ::
        {$dbug *}
      ~_  (show %o p.gen)
      =+  hum=$(gen q.gen)
      [p.hum [%10 [%spot %1 p.gen] q.hum]]
    ::
        {$zpcm *}   [(nice (play p.gen)) [%1 q.gen]]   ::  XX validate!
        {$lost *}
      ?:  vet
        ~_  (dunk(sut (play p.gen)) 'lost')
        ~>(%mean.[%leaf "mint-lost"] !!)
      [%void [%0 0]]
    ::
        {$zpsm *}
      =+  vos=$(gol %noun, gen q.gen)
      =+  ref=p:$(gol %noun, gen p.gen)
      ?>  (~(nest ut p:!>(*type)) & ref)
      [(nice (cell ref p.vos)) (cons [%1 p.vos] q.vos)]
    ::
        {$zpgr *}
      =+  vat=$(gen p.gen)
      %=    $
          gen
        :-  [%cnhp [%limb %onan] [%hand p:!>(*type) [%1 p.vat]] ~]
        [%hand p.vat q.vat]
      ==
    ::
        {$zpts *}   [(nice %noun) [%1 q:$(vet |, gen p.gen)]]
        {$zpzp $~}  [%void [%0 0]]
        *
      =+  doz=~(open ap gen)
      ?:  =(doz gen)
        ~_  (show [%c 'hoon'] [%q gen])
        ~>(%mean.[%leaf "mint-open"] !!)
      $(gen doz)
    ==
    ::
    ++  nice
      |=  typ/type
      ~_  leaf+"mint-nice"
      ?>  ?|(!vet (nest(sut gol) & typ))
      typ
    ::
    ++  grow
      |=  {mel/vair ruf/hoon wad/chap dom/(map @ tomb)}
      ^-  {p/type q/nock}
      =+  dan=^$(gen ruf, gol %noun)
      =+  toc=(core p.dan [%gold p.dan wad [~ dom]])
      =+  dez=(hope(sut toc) dom)
      :-  (nice (core p.dan mel p.dan wad [dez dom]))
      (cons [%1 dez] q.dan)
    --
  ::
  ++  moot
    =+  gil=*(set type)
    |-  ^-  ?
    ?-  sut
      {$atom *}  |
      {$cell *}  |($(sut p.sut) $(sut q.sut))
      {$core *}  $(sut p.sut)
      {$face *}  $(sut q.sut)
      {$fork *}  (lien ~(tap in p.sut) |=(type ^$(sut +<)))
      {$help *}  $(sut q.sut)
      {$hold *}  |((~(has in gil) sut) $(gil (~(put in gil) sut), sut repo))
      $noun      |
      $void      &
    ==
  ::
  ++  mull
    ~/  %mull
    |=  {gol/type dox/type gen/hoon}
    |^  ^-  {p/type q/type}
    ?:  =(%void sut)
      ~>(%mean.[%leaf "mull-none"] !!)
    ?-    gen
    ::
        {^ *}
      =+  hed=$(gen p.gen, gol %noun)
      =+  tal=$(gen q.gen, gol %noun)
      [(nice (cell p.hed p.tal)) (cell q.hed q.tal)]
    ::
        {$ktcn *}  $(fab |, gen p.gen)
        {$brcn *}  (grow %gold [%$ 1] p.gen q.gen)
        {$cnts *}  (~(mull et p.gen q.gen) gol dox)
        {$dtkt *}  =+($(gen q.gen, gol %noun) $(gen [%bunt p.gen]))
        {$dtls *}  =+($(gen p.gen, gol [%atom %$ ~]) (beth [%atom %$ ~]))
        {$sand *}  (beth (play gen))
        {$rock *}  (beth (play gen))
    ::
        {$dttr *}
      =+([$(gen p.gen, gol %noun) $(gen q.gen, gol %noun)] (beth %noun))
    ::
        {$dtts *}
      =+([$(gen p.gen, gol %noun) $(gen q.gen, gol %noun)] (beth bool))
    ::
        {$dtwt *}  =+($(gen p.gen, gol %noun) (beth bool))    ::  XX  =|
        {$hand *}  [p.gen p.gen]
        {$ktbr *}
      =+(vat=$(gen p.gen) [(wrap(sut p.vat) %iron) (wrap(sut q.vat) %iron)])
    ::
        {$ktls *}
      =+  hif=[p=(nice (play p.gen)) q=(play(sut dox) p.gen)]
      =+($(gen q.gen, gol p.hif) hif)
    ::
        {$ktpm *}
      =+(vat=$(gen p.gen) [(wrap(sut p.vat) %zinc) (wrap(sut q.vat) %zinc)])
    ::
        {$ktts *}
      =+(vat=$(gen q.gen) [(conk(sut p.vat) p.gen) (conk(sut q.vat) p.gen)])
    ::
        {$tune *}
      [(face p.gen sut) (face p.gen dox)]
    ::
        {$ktwt *}
      =+(vat=$(gen p.gen) [(wrap(sut p.vat) %lead) (wrap(sut q.vat) %lead)])
    ::
        {$docs *}
      =+  vat=$(gen q.gen) 
      [(help [%docs p.gen] p.vat) (help [%docs p.gen] q.vat)]
    ::
        {$ktsg *}  $(gen p.gen)
        {$sgzp *}  ~_(duck(sut (play p.gen)) $(gen q.gen))
        {$sggr *}  $(gen q.gen)
        {$tsgr *}
      =+  lem=$(gen p.gen, gol %noun)
      $(gen q.gen, sut p.lem, dox q.lem)
    ::
        {$tstr *}
      %=  $
        gen  r.gen
        sut  (buss p.gen q.gen)
        dox  (buss(sut dox) p.gen q.gen)
      ==
    ::
        {$wtcl *}
      =+  nor=$(gen p.gen, gol bool)
      =+  ^=  hiq  ^-  {p/type q/type}
          =+  fex=[p=(gain p.gen) q=(gain(sut dox) p.gen)]
          ?:  =(%void p.fex)
            :-  %void
            ?:  =(%void q.fex)
              %void
            ~>(%mean.[%leaf "if-z"] (play(sut q.fex) q.gen))
          ?:  =(%void q.fex)
            ~>(%mean.[%leaf "mull-bonk-b"] !!)
          $(sut p.fex, dox q.fex, gen q.gen)
      =+  ^=  ran  ^-  {p/type q/type}
          =+  wux=[p=(lose p.gen) q=(lose(sut dox) p.gen)]
          ?:  =(%void p.wux)
            :-  %void
            ?:  =(%void q.wux)
              %void
            ~>(%mean.[%leaf "if-a"] (play(sut q.wux) r.gen))
          ?:  =(%void q.wux)
            ~>(%mean.[%leaf "mull-bonk-c"] !!)
          $(sut p.wux, dox q.wux, gen r.gen)
      [(nice (fork p.hiq p.ran ~)) (fork q.hiq q.ran ~)]
    ::
        {$wtts *}
      =+  nob=~(bunt al %herb p.gen)
      =+  waz=[p=(play nob) q=(play(sut dox) nob)]
      =+  ^=  syx  :-  p=(cove q:(mint %noun [%wing q.gen]))
                   q=(cove q:(mint(sut dox) %noun [%wing q.gen]))
      =+  pov=[p=(fish(sut p.waz) p.syx) q=(fish(sut q.waz) q.syx)]
      ?.  &(=(p.syx q.syx) =(p.pov q.pov))
        ~>(%mean.[%leaf "mull-bonk-a"] !!)
      (beth bool)
    ::
        {$dbug *}  ~_((show %o p.gen) $(gen q.gen))
        {$zpcm *}  [(nice (play p.gen)) (play(sut dox) p.gen)]
        {$lost *}
      ?:  vet
        ::  ~_  (dunk(sut (play p.gen)) 'also')
        ~>(%mean.[%leaf "mull-skip"] !!)
      (beth %void)
    ::
        {$zpts *}  (beth %noun)
        {$zpsm *}
      =+  vos=$(gol %noun, gen q.gen)       ::  XX validate!
      [(nice (cell (play p.gen) p.vos)) (cell (play(sut dox) p.gen) q.vos)]
    ::
        {$zpgr *}
      ?>  =(sut dox)
      =+(typ=(play gen) [typ typ])
    ::
        {$zpzp *}  (beth %void)
        *
      =+  doz=~(open ap gen)
      ?:  =(doz gen)
        ~_  (show [%c 'hoon'] [%q gen])
        ~>(%mean.[%leaf "mull-open"] !!)
      $(gen doz)
    ==
    ::
    ++  beth
      |=  typ/type
      [(nice typ) typ]
    ::
    ++  nice
      |=  typ/type
      ::  ~_  (dunk(sut gol) 'need')
      ::  ~_  (dunk(sut typ) 'have')
      ~_  leaf+"mull-nice"
      ?>  ?|(!vet (nest(sut gol) & typ))
      typ
    ::
    ++  grow
      |=  {mel/vair ruf/hoon wad/chap dom/(map @ tomb)}
      ~_  leaf+"mull-grow"
      ^-  {p/type q/type}
      =+  dan=^$(gen ruf, gol %noun)
      =+  ^=  toc  :-  p=(core p.dan [%gold p.dan wad [~ dom]])
                   q=(core q.dan [%gold q.dan wad [~ dom]])
      =+  (balk(sut p.toc, dox q.toc) dom)
      :-  (nice (core p.dan mel p.dan wad [[%0 0] dom]))
      (core q.dan [mel q.dan wad [[%0 0] dom]])
    ::
    ++  bake
      |=  dab/(map term (pair what foot))
      ^-  *
      ?:  ?=($~ dab)
        ~
      =+  ^=  dov
          ?-  -.q.q.n.dab
            $ash  ^$(gol %noun, gen p.q.q.n.dab)
            $elm  ~
          ==
      ?-  dab
        {* $~ $~}  dov
        {* $~ *}   [dov $(dab r.dab)]
        {* * $~}   [dov $(dab l.dab)]
        {* * *}    [dov $(dab l.dab) $(dab r.dab)]
      ==
    ::
    ++  balk
      |=  dom/(map @ tomb)
      ^-  *
      ?:  ?=($~ dom)
        ~
      =+  dov=(bake q.q.n.dom)
      ?-    dom
        {* $~ $~}   dov
        {* $~ *}    [dov $(dom r.dom)]
        {* * $~}    [dov $(dom l.dom)]
        {* * *}     [dov $(dom l.dom) $(dom r.dom)]
      ==
    --
  ++  meet  |=(ref/type &((nest | ref) (nest(sut ref) | sut)))
  ::                                                    ::
  ++  miss                                              ::  nonintersection
    |=  $:  ::  ref: symmetric type
            ::
            ref/type
        ==
    ::  intersection of sut and ref is empty
    ::
    ^-  ?
    =|  gil/(set (set type))
    =<  dext
    |%
    ++  dext
      ^-  ?
      ::  
      ?:  =(ref sut)
        (nest(sut %void) | sut)
      ?-  sut
        $void      &
        $noun      (nest(sut %void) | ref)
        {$atom *}  sint
        {$cell *}  sint
        {$core *}  sint(sut [%cell %noun %noun])
        {$fork *}  %+  levy  ~(tap in p.sut)
                   |=(type dext(sut +<))
        {$face *}  dext(sut q.sut) 
        {$help *}  dext(sut q.sut)
        {$hold *}  =+  (~(gas in *(set type)) `(list type)`[sut ref ~])
                   ?:  (~(has in gil) -)
                      &
                   %=  dext
                     sut  repo
                     gil  (~(put in gil) -)
      ==           ==
    ++  sint
      ?+  ref      dext(sut ref, ref sut)
        {$atom *}  ?.  ?=({$atom *} sut)  &
                   ?&  ?=(^ q.ref)
                       ?=(^ q.sut)
                       !=(q.ref q.sut)
                   ==
        {$cell *}  ?.  ?=({$cell *} sut)  &
                   ?|  dext(sut p.sut, ref p.ref)
                       dext(sut q.sut, ref q.ref)
      ==           ==
    --
  ++  mite  |=(ref/type |((nest | ref) (nest(sut ref) & sut)))
  ++  nest
    ~/  %nest
    |=  {tel/? ref/type}
    =|  $:  seg/(set type)                              ::  degenerate sut
            reg/(set type)                              ::  degenerate ref
            gil/(set {p/type q/type})                   ::  assume nest
        ==
    =<  dext
    |%
    ++  deem
      |=  {mel/vair ram/vair}
      ^-  ?
      ?.  |(=(mel ram) =(%lead mel) =(%gold ram))  |
      ?:  ?=($lead mel)  &
      ?:  ?=($gold mel)  meet
      =+  vay=?-(mel $iron %rite, $zinc %read)
      dext(sut (peek vay 2), ref (peek(sut ref) vay 2))
    ::
    ++  deep
      |=  $:  dom/(map @ tomb)
              vim/(map @ tomb)
          ==
      ^-  ?
      ?:  ?=($~ dom)  =(vim ~)
      ?:  ?=($~ vim)  |
      ?&  =(p.n.dom p.n.vim)
          $(dom l.dom, vim l.vim)
          $(dom r.dom, vim r.vim)
      ::
          =+  [dab hem]=[q.q.n.dom q.q.n.vim]
          |-  ^-  ?
          ?:  ?=($~ dab)  =(hem ~)
          ?:  ?=($~ hem)  |
          ?&  =(p.n.dab p.n.hem)
              $(dab l.dab, hem l.hem)
              $(dab r.dab, hem r.hem)
              ?-  -.q.q.n.dab
                $elm  =(q.q.n.dab q.q.n.hem)
                $ash  ?&  ?=($ash -.q.q.n.hem)
                          %=  dext
                            sut  (play p.q.q.n.dab)
                            ref  (play(sut ref) p.q.q.n.hem)
      ==  ==  ==      ==  ==
    ::
    ++  dext
      ^-  ?
      =-  ?:  -  &
          ?.  tel  |
          ::  ~_  (dunk %zpwt)
          ::  ~_  (dunk(sut ref) %have)
          ~>(%mean.[%leaf "nest-fail"] !!)
      ?:  =(sut ref)  &
      ?-  sut
        $void      sint
        $noun      &
        {$atom *}  ?.  ?=({$atom *} ref)  sint
                   ?&  (fitz p.sut p.ref)
                       |(?=($~ q.sut) =(q.sut q.ref))
                   ==
        {$cell *}  ?.  ?=({$cell *} ref)  sint
                   ?&  dext(sut p.sut, ref p.ref, seg ~, reg ~)
                       dext(sut q.sut, ref q.ref, seg ~, reg ~)
                   ==
        {$core *}  ?.  ?=({$core *} ref)  sint
                   ?:  =(q.sut q.ref)  dext(sut p.sut, ref p.ref)
                   ?&  meet(sut q.q.sut, ref p.sut)
                       dext(sut q.q.ref, ref p.ref)
                       (deem(sut q.q.sut, ref q.q.ref) p.q.sut p.q.ref)
                       ?|  (~(has in gil) [sut ref])
                           %.  [q.s.q.sut q.s.q.ref]
                           %=  deep
                             gil  (~(put in gil) [sut ref])
                             sut  sut(p q.q.sut, p.q %gold)
                             ref  ref(p q.q.ref, p.q %gold)
                       ==  ==
                   ==
        {$face *}  dext(sut q.sut)
        {$fork *}  ?.  ?=(?({$atom *} $noun {$cell *} {$core *}) ref)  sint
                   (lien ~(tap in p.sut) |=(type dext(tel |, sut +<)))
        {$help *}  dext(sut q.sut)
        {$hold *}  ?:  (~(has in seg) sut)  |
                   ?:  (~(has in gil) [sut ref])  &
                   %=  dext
                     sut  repo
                     seg  (~(put in seg) sut)
                     gil  (~(put in gil) [sut ref])
      ==           ==
    ::
    ++  meet  &(dext dext(sut ref, ref sut))
    ++  sint
      ^-  ?
      ?-  ref
        $noun       |
        $void       &
        {$atom *}   |
        {$cell *}   |
        {$core *}   dext(ref repo(sut ref))
        {$face *}   dext(ref q.ref)
        {$fork *}   (levy ~(tap in p.ref) |=(type dext(ref +<)))
        {$help *}   dext(ref q.ref)
        {$hold *}   ?:  (~(has in reg) ref)  &
                    ?:  (~(has in gil) [sut ref])  &
                    %=  dext
                      ref  repo(sut ref)
                      reg  (~(put in reg) ref)
                      gil  (~(put in gil) [sut ref])
      ==            ==
    --
  ::
  ++  peek
    ~/  %peek
    |=  {way/?($read $rite $both $free) axe/axis}
    ^-  type
    ?:  =(1 axe)
      sut
    =+  [now=(cap axe) lat=(mas axe)]
    =+  gil=*(set type)
    |-  ^-  type
    ?-    sut
        {$atom *}   %void
        {$cell *}   ?:(=(2 now) ^$(sut p.sut, axe lat) ^$(sut q.sut, axe lat))
        {$core *}
      ?.  =(3 now)  %noun
      =+  pec=(peel way p.q.sut)
      %=    ^$
          axe  lat
          sut
        ?:  =([& &] pec)  p.sut
        %+  cell
          ?.(sam.pec %noun ^$(sut p.sut, axe 2))
        ?.(con.pec %noun ^$(sut p.sut, axe 3))
      ==
    ::
        {$fork *}   (fork (turn ~(tap in p.sut) |=(type ^$(sut +<))))
        {$hold *}
      ?:  (~(has in gil) sut)
        %void
      $(gil (~(put in gil) sut), sut repo)
    ::
        $void       %void
        $noun       %noun
        *           $(sut repo)
    ==
  ::
  ++  peel
    |=  {way/vial met/?($gold $iron $lead $zinc)}
    ^-  {sam/? con/?}
    ?:  ?=($gold met)  [& &]
    ?-  way
      $both  [| |]
      $free  [& &]
      $read  [?=($zinc met) |]
      $rite  [?=($iron met) |]
    ==
  ::
  ++  play
    ~/  %play
    =>  .(vet |)
    |=  gen/hoon
    ^-  type
    ?-  gen
      {^ *}      (cell $(gen p.gen) $(gen q.gen))
      {$ktcn *}  $(fab |, gen p.gen)
      {$brcn *}  (core sut %gold sut p.gen [[%0 0] q.gen])
      {$cnts *}  ~(play et p.gen q.gen)
      {$dtkt *}  $(gen [%bunt p.gen])
      {$dtls *}  [%atom %$ ~]
      {$rock *}  |-  ^-  type
                 ?@  q.gen  [%atom p.gen `q.gen]
                 [%cell $(q.gen -.q.gen) $(q.gen +.q.gen)]
      {$sand *}  |-  ^-  type
                 ?@  q.gen
                   ?:  =(%n p.gen)  ?>(=(0 q.gen) [%atom p.gen ~ q.gen])
                   ?:(=(%f p.gen) ?>((lte q.gen 1) bool) [%atom p.gen ~])
                 [%cell $(q.gen -.q.gen) $(q.gen +.q.gen)]
      {$tune *}  (face p.gen sut)
      {$dttr *}  %noun
      {$dtts *}  bool
      {$dtwt *}  bool
      {$hand *}  p.gen
      {$ktbr *}  (wrap(sut $(gen p.gen)) %iron)
      {$ktls *}  $(gen p.gen)
      {$ktpm *}  (wrap(sut $(gen p.gen)) %zinc)
      {$ktsg *}  $(gen p.gen)
      {$ktts *}  (conk(sut $(gen q.gen)) p.gen)
      {$ktwt *}  (wrap(sut $(gen p.gen)) %lead)
      {$docs *}  (help [%docs p.gen] $(gen q.gen))
      {$sgzp *}  ~_(duck(sut ^$(gen p.gen)) $(gen q.gen))
      {$sggr *}  $(gen q.gen)
      {$tsgr *}   $(gen q.gen, sut $(gen p.gen))
      {$tstr *}   $(gen r.gen, sut (buss p.gen q.gen))
      {$wtcl *}    =+  [fex=(gain p.gen) wux=(lose p.gen)]
                 %-  fork  :~
                   ?:(=(%void fex) %void $(sut fex, gen q.gen))
                   ?:(=(%void wux) %void $(sut wux, gen r.gen))
                 ==
      {$wtts *}  bool
      {$dbug *}  ~_((show %o p.gen) $(gen q.gen))
      {$zpcm *}  (play p.gen)
      {$zpgr *}  %=    $
                     gen
                   [%cnhp [%limb %onan] [%hand p:!>(*type) [%1 $(gen p.gen)]] ~]
                 ==
      {$lost *}  %void
      {$zpsm *}  (cell $(gen p.gen) $(gen q.gen))
      {$zpts *}  %noun
      {$zpzp *}  %void
      *          =+  doz=~(open ap gen)
                 ?:  =(doz gen)
                   ~_  (show [%c 'hoon'] [%q gen])
                   ~>(%mean.[%leaf "play-open"] !!)
                 $(gen doz)
    ==
  ::                                                    ::
  ++  redo                                              ::  refurbish faces
    |=  $:  ::  ref: raw payload
            ::
            ref/type
        ==
    ::  :type: subject refurbished to reference namespace
    ::
    ^-  type
    ::  hos: subject tool stack
    ::  wec: reference tool stack set
    ::  gil: repetition set
    ::
    =|  hos/(list tool)
    =/  wec/(set (list tool))  [~ ~ ~]
    =|  gil/(set (pair type type))
    =<  ::  errors imply subject/reference mismatch
        ::
        ~|  %redo-match
        ::  reduce by subject
        ::
        dext
    |%
    ::                                                  ::
    ++  dear                                            ::  resolve tool stack
      ::  :(unit (list tool)): unified tool stack
      ::
      ^-  (unit (list tool))
      ::  empty implies void
      ::
      ?~  wec  `~
      ::  any reference faces must be clear
      ::
      ?.  ?=({* $~ $~} wec)  
        ~&  [%dear-many wec]
        ~
      :-  ~
      ::  har: single reference tool stack
      ::
      =/  har  n.wec
      ::  len: lengths of [sut ref] face stacks
      ::
      =/  len  [p q]=[(lent hos) (lent har)]
      ::  lip: length of sut-ref face stack overlap
      ::
      ::      AB
      ::       BC
      ::
      ::    +lip is (lent B), where +hay is forward AB
      ::    and +liv is forward BC (stack BA and CB).
      ::
      ::    overlap is a weird corner case.  +lip is
      ::    almost always 0.  brute force is fine.
      :: 
      =/  lip
        =|  lup/(unit @ud)
        =|  lip/@ud
        |-  ^-  @ud
        ?:  |((gth lip p.len) (gth lip q.len))
          (fall lup 0)
        ::  lep: overlap candidate: suffix of subject face stack
        ::
        =/  lep  (slag (sub p.len lip) hos) 
        ::  lap: overlap candidate: prefix of reference face stack
        ::
        =/  lap  (scag lip har)
        ::  save any match and continue
        ::
        $(lip +(lip), lup ?.(=(lep lap) lup `lip))
      ::  ~&  [har+har hos+hos len+len lip+lip]
      ::  produce combined face stack (forward ABC, stack CBA)
      ::
      (weld hos (slag lip har))
    ::                                                  ::
    ++  dext                                            ::  subject traverse
      ::  :type: refurbished subject
      ::
      ^-  type
      ::  check for trivial cases
      ::
      ?:  ?|  =(sut ref) 
              ?=(?($noun $void {?($atom $core) *}) ref)
          ==
        done
      ::  ~_  (dunk 'redo: dext: sut')
      ::  ~_  (dunk(sut ref) 'redo: dext: ref')  
      ?-    sut
          ?($noun $void {?($atom $core) *})
        ::  reduce reference and reassemble leaf
        ::
        done:(sint &)
      ::
          {$cell *}
        ::  reduce reference to match subject
        ::
        =>  (sint &)
        ?>  ?=({$cell *} sut)
        ::  leaf with possible recursive descent
        ::
        %=    done
            sut
          ::  clear face stacks for descent
          ::
          =:  hos  ~
              wec  [~ ~ ~]
            ==
          ::  descend into cell
          ::
          :+  %cell
            dext(sut p.sut, ref (peek(sut ref) %free 2))
          dext(sut q.sut, ref (peek(sut ref) %free 3))
        ==
      ::
          {$face *}
        ::  push face on subject stack, and descend
        ::
        dext(hos [q.p.sut hos], sut q.sut)
      ::
          {$help *}
        ::  work through help
        ::
        [%help p.sut dext(sut q.sut)]
      ::
          {$fork *}
        ::  reconstruct each case in fork 
        ::
        (fork (turn ~(tap in p.sut) |=(type dext(sut +<))))
      ::
          {$hold *}
        ::  reduce to hard 
        ::
        =>  (sint |)
        ?>  ?=({$hold *} sut)
        ?:  (~(has in fan) [p.sut q.sut])
          ::  repo loop; redo depends on its own product
          ::
          done:(sint &)
        ?:  (~(has in gil) [sut ref])
          ::  type recursion, stop renaming
          ::
          done:(sint |)
        ::  restore unchanged holds
        ::
        =+  repo
        =-  ?:(=(- +<) sut -)
        dext(sut -, gil (~(put in gil) sut ref))
      ==
    ::                                                  ::
    ++  done                                            ::  complete assembly
      ^-  type
      ::  :type: subject refurbished 
      ::
      ::  lov: combined face stack
      ::
      =/  lov  
          =/  lov  dear
          ?~  lov
            ::  ~_  (dunk 'redo: dear: sut')
            ::  ~_  (dunk(sut ref) 'redo: dear: ref')
            ~&  [%wec wec]
            !!
          (need lov)
      ::  recompose faces
      ::
      |-  ^-  type
      ?~  lov  sut
      $(lov t.lov, sut (face [~ i.lov] sut))
    ::                                                  ::
    ++  sint                                            ::  reduce by reference
      |=  $:  ::  hod: expand holds
              ::
              hod/?
          ==
      ::  ::.: reference with face/fork/hold reduced
      ::
      ^+  .
      ::  =-  ~>  %slog.[0 (dunk 'sint: sut')]
      ::      ~>  %slog.[0 (dunk(sut ref) 'sint: ref')]
      ::      ~>  %slog.[0 (dunk(sut =>(- ref)) 'sint: pro')]
      ::      -
      ?+    ref  .
          {$help *}  $(ref q.ref)
          {$face *}
        ::  extend all stacks in set
        ::
        %=  $
          ref  q.ref
          wec  (~(run in wec) |=((list tool) [q.p.ref +<]))
        ==
      ::
          {$fork *}  
        ::  reconstruct all relevant cases
        ::
        =-  ::  ~>  %slog.[0 (dunk 'fork: sut')]
            ::  ~>  %slog.[0 (dunk(sut ref) 'fork: ref')]
            ::  ~>  %slog.[0 (dunk(sut (fork ->)) 'fork: pro')]
            +(wec -<, ref (fork ->))
        =/  moy  ~(tap in p.ref)
        |-  ^-  (pair (set (list tool)) (list type))
        ?~  moy  [~ ~]
        ::  head recurse
        ::
        =/  mor  $(moy t.moy)
        ::  prune reference cases outside subject
        ::
        ?:  (miss i.moy)  mor
        ::  unify all cases
        ::
        =/  dis  ^$(ref i.moy)
        [(~(uni in p.mor) wec.dis) [ref.dis q.mor]]
      ::
          {$hold *}
        ?.  hod  .
        $(ref repo(sut ref))
      ==
    --
  ::
  ++  repo
    ^-  type
    ?-  sut
      {$core *}   [%cell %noun p.sut]
      {$face *}   q.sut
      {$help *}   q.sut
      {$hold *}   (rest [[p.sut q.sut] ~])
      $noun       (fork [%atom %$ ~] [%cell %noun %noun] ~)
      *           ~>(%mean.[%leaf "repo-fltt"] !!)
    ==
  ::
  ++  rest
    ~/  %rest
    |=  leg/(list {p/type q/hoon})
    ^-  type
    ?:  (lien leg |=({p/type q/hoon} (~(has in fan) [p q])))
      ~>(%mean.[%leaf "rest-loop"] !!)
    =>  .(fan (~(gas in fan) leg))
    %-  fork
    %~  tap  in
      %-  ~(gas in *(set type))
      (turn leg |=({p/type q/hoon} (play(sut p) q)))
    ==
  ::
  ++  take
    |=  {vit/vein duz/tyro}
    ^-  (pair axis type)
    :-  (tend vit)
    =.  vit  (flop vit)
    |-  ^-  type
    ?~  vit  (duz sut)
    ?~  i.vit
      |-  ^-  type
      ?+  sut      ^$(vit t.vit)
        {$face *}  (face p.sut ^$(vit t.vit, sut q.sut))
        {$fork *}  (fork (turn ~(tap in p.sut) |=(type ^$(sut +<))))
        {$hold *}  $(sut repo)
      ==
    =+  vil=*(set type)
    |-  ^-  type
    ?:  =(1 u.i.vit)
      ^$(vit t.vit)
    =+  [now lat]=(cap u.i.vit)^(mas u.i.vit)
    ?-  sut
      $noun      $(sut [%cell %noun %noun])
      $void      %void
      {$atom *}  %void
      {$cell *}  ?:  =(2 now)
                   (cell $(sut p.sut, u.i.vit lat) q.sut)
                  (cell p.sut $(sut q.sut, u.i.vit lat))
      {$core *}  ?:  =(2 now)
                   $(sut repo)
                 (core $(sut p.sut, u.i.vit lat) q.sut)
      {$face *}  (face p.sut $(sut q.sut))
      {$fork *}  (fork (turn ~(tap in p.sut) |=(type ^$(sut +<))))
      {$help *}  (help p.sut $(sut q.sut))
      {$hold *}  ?:  (~(has in vil) sut)
                   %void
                 $(sut repo, vil (~(put in vil) sut))
    ==
  ::
  ++  tack
    |=  {hyp/wing mur/type}
    ~_  (show [%c %tack] %l hyp)
    =+  fid=(find %rite hyp)
    ?>  ?=($& -.fid)
    (take p.p.fid |=(type mur))
  ::
  ++  tend
    |=  vit/vein
    ^-  axis
    ?~(vit 1 (peg $(vit t.vit) ?~(i.vit 1 u.i.vit)))
  ::
  ++  toss
    ~/  %toss
    |=  {hyp/wing mur/type men/(list {p/type q/foot})}
    ^-  {p/axis q/(list {p/type q/foot})}
    =-  [(need p.wib) q.wib]
    ^=  wib
    |-  ^-  {p/(unit axis) q/(list {p/type q/foot})}
    ?~  men
      [*(unit axis) ~]
    =+  geq=(tack(sut p.i.men) hyp mur)
    =+  mox=$(men t.men)
    [(mate p.mox `_p.mox`[~ p.geq]) [[q.geq q.i.men] q.mox]]
  ::
  ++  wrap
    ~/  %wrap
    |=  yoz/?($lead $iron $zinc)
    ~_  leaf+"wrap"
    ^-  type
    ?+  sut  sut
      {$cell *}  (cell $(sut p.sut) $(sut q.sut))
      {$core *}  ?>(|(=(%gold p.q.sut) =(%lead yoz)) sut(p.q yoz))
      {$face *}  (face p.sut $(sut q.sut))
      {$fork *}  (fork (turn ~(tap in p.sut) |=(type ^$(sut +<))))
      {$help *}  (help p.sut $(sut q.sut))
      {$hold *}  $(sut repo)
    ==
  --
++  us                                                  ::  prettyprinter
  =>  |%
      ++  cape  {p/(map @ud wine) q/wine}               ::
      ++  wine                                          ::
                $@  $?  $noun                           ::
                        $path                           ::
                        $type                           ::
                        $void                           ::
                        $wall                           ::
                        $wool                           ::
                        $yarn                           ::
                    ==                                  ::
                $%  {$mato p/term}                      ::
                    {$core p/(list @ta) q/wine}         ::
                    {$face p/term q/wine}               ::
                    {$list p/term q/wine}               ::
                    {$pear p/term q/@}                  ::
                    {$bcwt p/(list wine)}               ::
                    {$plot p/(list wine)}               ::
                    {$stop p/@ud}                       ::
                    {$tree p/term q/wine}               ::
                    {$unit p/term q/wine}               ::
                ==                                      ::
      --
  |_  sut/type
  ++  dash
    |=  {mil/tape lim/char lam/tape}
    ^-  tape
    =/  esc  (~(gas in *(set @tD)) lam)
    :-  lim
    |-  ^-  tape
    ?~  mil  [lim ~]
    ?:  ?|  =(lim i.mil)
            =('\\' i.mil)
            (~(has in esc) i.mil)
        ==
      ['\\' i.mil $(mil t.mil)]
    ?:  (lte ' ' i.mil)
      [i.mil $(mil t.mil)]
    ['\\' ~(x ne (rsh 2 1 i.mil)) ~(x ne (end 2 1 i.mil)) $(mil t.mil)]
  ::
  ++  deal  |=(lum/* (dish dole lum))
  ++  dial
    |=  ham/cape
    =+  gid=*(set @ud)
    =<  `tank`-:$
    |%
    ++  many
      |=  haz/(list wine)
      ^-  {(list tank) (set @ud)}
      ?~  haz  [~ gid]
      =^  mor  gid  $(haz t.haz)
      =^  dis  gid  ^$(q.ham i.haz)
      [[dis mor] gid]
    ::
    ++  $
      ^-  {tank (set @ud)}
      ?-    q.ham
          $noun      :_(gid [%leaf '*' ~])
          $path      :_(gid [%leaf '/' ~])
          $type      :_(gid [%leaf '#' 't' ~])
          $void      :_(gid [%leaf '#' '!' ~])
          $wool      :_(gid [%leaf '*' '"' '"' ~])
          $wall      :_(gid [%leaf '*' '\'' '\'' ~])
          $yarn      :_(gid [%leaf '"' '"' ~])
          {$mato *}  :_(gid [%leaf '@' (trip p.q.ham)])
          {$core *}
        =^  cox  gid  $(q.ham q.q.ham)
        :_  gid
        :+  %rose
          [[' ' ~] ['<' ~] ['>' ~]]
        |-  ^-  (list tank)
        ?~  p.q.ham  [cox ~]
        [[%leaf (rip 3 i.p.q.ham)] $(p.q.ham t.p.q.ham)]
      ::
          {$face *}
        =^  cox  gid  $(q.ham q.q.ham)
        :_(gid [%palm [['/' ~] ~ ~ ~] [%leaf (trip p.q.ham)] cox ~])
      ::
          {$list *}
        =^  cox  gid  $(q.ham q.q.ham)
        :_(gid [%rose [" " (weld (trip p.q.ham) "(") ")"] cox ~])
      ::
          {$bcwt *}
        =^  coz  gid  (many p.q.ham)
        :_(gid [%rose [[' ' ~] ['?' '(' ~] [')' ~]] coz])
      ::
          {$plot *}
        =^  coz  gid  (many p.q.ham)
        :_(gid [%rose [[' ' ~] ['{' ~] ['}' ~]] coz])
      ::
          {$pear *}
        :_(gid [%leaf '$' ~(rend co [%$ p.q.ham q.q.ham])])
      ::
          {$stop *}
        =+  num=~(rend co [%$ %ud p.q.ham])
        ?:  (~(has in gid) p.q.ham)
          :_(gid [%leaf '#' num])
        =^  cox  gid
            %=  $
              gid    (~(put in gid) p.q.ham)
              q.ham  (~(got by p.ham) p.q.ham)
            ==
        :_(gid [%palm [['.' ~] ~ ~ ~] [%leaf ['^' '#' num]] cox ~])
      ::
          {$tree *}
        =^  cox  gid  $(q.ham q.q.ham)
        :_(gid [%rose [" " (weld (trip p.q.ham) "(") ")"] cox ~])
      ::
          {$unit *}
        =^  cox  gid  $(q.ham q.q.ham)
        :_(gid [%rose [" " (weld (trip p.q.ham) "(") ")"] cox ~])
      ==
    --
  ::
  ++  dish
    |=  {ham/cape lum/*}  ^-  tank
    ~|  [%dish-h ?@(q.ham q.ham -.q.ham)]
    ~|  [%lump lum]
    ~|  [%ham ham]
    %-  need
    =|  gil/(set {@ud *})
    |-  ^-  (unit tank)
    ?-    q.ham
        $noun
      %=    $
          q.ham
        ?:  ?=(@ lum)
          [%mato %$]
        :-  %plot
        |-  ^-  (list wine)
        [%noun ?:(?=(@ +.lum) [[%mato %$] ~] $(lum +.lum))]
      ==
    ::
        $path
      :-  ~
      :+  %rose
        [['/' ~] ['/' ~] ~]
      |-  ^-  (list tank)
      ?~  lum  ~
      ?@  lum  !!
      ?>  ?=(@ -.lum)
      [[%leaf (rip 3 -.lum)] $(lum +.lum)]
    ::
        $type
      =+  tyr=|.((dial dole))
      =+  vol=tyr(sut lum)
      =+  cis=((hard tank) .*(vol -:vol))
      :^  ~   %palm
        [~ ~ ~ ~]
      [[%leaf '#' 't' '/' ~] cis ~]
    ::
        $wall
      :-  ~
      :+  %rose
        [[' ' ~] ['<' '|' ~] ['|' '>' ~]]
      |-  ^-  (list tank)
      ?~  lum  ~
      ?@  lum  !!
      [[%leaf (trip ((hard @) -.lum))] $(lum +.lum)]
    ::
        $wool
      :-  ~
      :+  %rose
        [[' ' ~] ['<' '<' ~] ['>' '>' ~]]
      |-  ^-  (list tank)
      ?~  lum  ~
      ?@  lum  !!
      [(need ^$(q.ham %yarn, lum -.lum)) $(lum +.lum)]
    ::
        $yarn
      [~ %leaf (dash (tape lum) '"' "\{")]
    ::
        $void
      ~
    ::
        {$mato *}
      ?.  ?=(@ lum)
        ~
      :+  ~
        %leaf
      ?+    (rash p.q.ham ;~(sfix (cook crip (star low)) (star hig)))
          ~(rend co [%$ p.q.ham lum])
        $$    ~(rend co [%$ %ud lum])
        $t    (dash (rip 3 lum) '\'' ~)
        $tas  ['%' ?.(=(0 lum) (rip 3 lum) ['$' ~])]
      ==
    ::
        {$core *}
      ::  XX  needs rethinking for core metal
      ::  ?.  ?=(^ lum)  ~
      ::  =>  .(lum `*`lum)
      ::  =-  ?~(tok ~ [~ %rose [[' ' ~] ['<' ~] ['>' ~]] u.tok])
      ::  ^=  tok
      ::  |-  ^-  (unit (list tank))
      ::  ?~  p.q.ham
      ::    =+  den=^$(q.ham q.q.ham)
      ::    ?~(den ~ [~ u.den ~])
      ::  =+  mur=$(p.q.ham t.p.q.ham, lum +.lum)
      ::  ?~(mur ~ [~ [[%leaf (rip 3 i.p.q.ham)] u.mur]])
      [~ (dial ham)]
    ::
        {$face *}
      =+  wal=$(q.ham q.q.ham)
      ?~  wal
        ~
      [~ %palm [['=' ~] ~ ~ ~] [%leaf (trip p.q.ham)] u.wal ~]
    ::
        {$list *}
      ?:  =(~ lum)
        [~ %leaf '~' ~]
      =-  ?~  tok
            ~
          [~ %rose [[' ' ~] ['~' '[' ~] [']' ~]] u.tok]
      ^=  tok
      |-  ^-  (unit (list tank))
      ?:  ?=(@ lum)
        ?.(=(~ lum) ~ [~ ~])
      =+  [for=^$(q.ham q.q.ham, lum -.lum) aft=$(lum +.lum)]
      ?.  &(?=(^ for) ?=(^ aft))
        ~
      [~ u.for u.aft]
    ::
        {$bcwt *}
      |-  ^-  (unit tank)
      ?~  p.q.ham
        ~
      =+  wal=^$(q.ham i.p.q.ham)
      ?~  wal
        $(p.q.ham t.p.q.ham)
      wal
    ::
        {$plot *}
      =-  ?~  tok
            ~
          [~ %rose [[' ' ~] ['[' ~] [']' ~]] u.tok]
      ^=  tok
      |-  ^-  (unit (list tank))
      ?~  p.q.ham
        ~
      ?:  ?=({* $~} p.q.ham)
        =+  wal=^$(q.ham i.p.q.ham)
        ?~(wal ~ [~ [u.wal ~]])
      ?@  lum
        ~
      =+  gim=^$(q.ham i.p.q.ham, lum -.lum)
      ?~  gim
        ~
      =+  myd=$(p.q.ham t.p.q.ham, lum +.lum)
      ?~  myd
        ~
      [~ u.gim u.myd]
    ::
        {$pear *}
      ?.  =(lum q.q.ham)
        ~
      =.  p.q.ham
        (rash p.q.ham ;~(sfix (cook crip (star low)) (star hig)))
      =+  fox=$(q.ham [%mato p.q.ham])
      ?>  ?=({$~ $leaf ^} fox)
      ?:  ?=(?($n $tas) p.q.ham)
        fox
      [~ %leaf '%' p.u.fox]
    ::
        {$stop *}
      ?:  (~(has in gil) [p.q.ham lum])  ~
      =+  kep=(~(get by p.ham) p.q.ham)
      ?~  kep
        ~|([%stop-loss p.q.ham] !!)
      $(gil (~(put in gil) [p.q.ham lum]), q.ham u.kep)
    ::
        {$tree *}
      =-  ?~  tok
            ~
          [~ %rose [[' ' ~] ['{' ~] ['}' ~]] u.tok]
      ^=  tok
      =+  tuk=*(list tank)
      |-  ^-  (unit (list tank))
      ?:  =(~ lum)
        [~ tuk]
      ?.  ?=({n/* l/* r/*} lum)
        ~
      =+  rol=$(lum r.lum)
      ?~  rol
        ~
      =+  tim=^$(q.ham q.q.ham, lum n.lum)
      ?~  tim
        ~
      $(lum l.lum, tuk [u.tim u.rol])
    ::
        {$unit *}
      ?@  lum
        ?.(=(~ lum) ~ [~ %leaf '~' ~])
      ?.  =(~ -.lum)
        ~
      =+  wal=$(q.ham q.q.ham, lum +.lum)
      ?~  wal
        ~
      [~ %rose [[' ' ~] ['[' ~] [']' ~]] [%leaf '~' ~] u.wal ~]
    ==
  ::
  ++  doge
    |=  ham/cape
    =-  ?+  woz  woz
          {$list * {$mato $'ta'}}  %path
          {$list * {$mato $'t'}}   %wall
          {$list * {$mato $'tD'}}  %yarn
          {$list * $yarn}          %wool
        ==
    ^=  woz
    ^-  wine
    ?.  ?=({$stop *} q.ham)
      ?:  ?&  ?=  {$bcwt {$pear $n $0} {$plot {$pear $n $0} {$face *} $~} $~}
                q.ham
              =(1 (met 3 p.i.t.p.i.t.p.q.ham))
          ==
        [%unit =<([p q] i.t.p.i.t.p.q.ham)]
      q.ham
    =+  may=(~(get by p.ham) p.q.ham)
    ?~  may
      q.ham
    =+  nul=[%pear %n 0]
    ?.  ?&  ?=({$bcwt *} u.may)
            ?=({* * $~} p.u.may)
            |(=(nul i.p.u.may) =(nul i.t.p.u.may))
        ==
      q.ham
    =+  din=?:(=(nul i.p.u.may) i.t.p.u.may i.p.u.may)
    ?:  ?&  ?=({$plot {$face *} {$face * $stop *} $~} din)
            =(p.q.ham p.q.i.t.p.din)
            =(1 (met 3 p.i.p.din))
            =(1 (met 3 p.i.t.p.din))
        ==
      :+  %list
        (cat 3 p.i.p.din p.i.t.p.din)
      q.i.p.din
    ?:  ?&  ?=  $:  $plot
                    {$face *}
                    {$face * $stop *}
                    {{$face * $stop *} $~}
                ==
                din
            =(p.q.ham p.q.i.t.p.din)
            =(p.q.ham p.q.i.t.t.p.din)
            =(1 (met 3 p.i.p.din))
            =(1 (met 3 p.i.t.p.din))
            =(1 (met 3 p.i.t.t.p.din))
        ==
      :+  %tree
        %^    cat
            3
          p.i.p.din
        (cat 3 p.i.t.p.din p.i.t.t.p.din)
      q.i.p.din
    q.ham
  ::
  ++  dole
    ^-  cape
    =+  gil=*(set type)
    =+  dex=[p=*(map type @) q=*(map @ wine)]
    =<  [q.p q]
    |-  ^-  {p/{p/(map type @) q/(map @ wine)} q/wine}
    =-  [p.tez (doge q.p.tez q.tez)]
    ^=  tez
    ^-  {p/{p/(map type @) q/(map @ wine)} q/wine}
    ?:  (~(meet ut sut) -:!>(*type))
      [dex %type]
    ?-    sut
        $noun      [dex sut]
        $void      [dex sut]
        {$atom *}  [dex ?~(q.sut [%mato p.sut] [%pear p.sut u.q.sut])]
        {$cell *}
      =+  hin=$(sut p.sut)
      =+  yon=$(dex p.hin, sut q.sut)
      :-  p.yon
      :-  %plot
      ?:(?=({$plot *} q.yon) [q.hin p.q.yon] [q.hin q.yon ~])
    ::
        {$core *}
      =+  yad=$(sut p.sut)
      :-  p.yad
      =+  ^=  doy  ^-  {p/(list @ta) q/wine}
          ?:  ?=({$core *} q.yad)
            [p.q.yad q.q.yad]
          [~ q.yad]
      :-  %core
      :_  q.doy
      :_  p.doy
      %^  cat  3
        %~  rent  co
            :+  %$  %ud
            %-  ~(rep by (~(run by q.s.q.sut) |=(tomb ~(wyt by q))))
            |=([[@ a=@u] b=@u] (add a b))
        ==
      %^  cat  3
        ?-(p.q.sut $gold '.', $iron '|', $lead '?', $zinc '&')
      =+  gum=(mug q.s.q.sut)
      %+  can  3
      :~  [1 (add 'a' (mod gum 26))]
          [1 (add 'a' (mod (div gum 26) 26))]
          [1 (add 'a' (mod (div gum 676) 26))]
      ==
    ::
        {$help *}
      $(sut q.sut)
    ::
        {$face *}
      =+  yad=$(sut q.sut)
      ?^(q.p.sut yad [p.yad [%face q.p.sut q.yad]])
    ::
        {$fork *}
      =+  yed=~(tap in p.sut)
      =-  [p [%bcwt q]]
      |-  ^-  {p/{p/(map type @) q/(map @ wine)} q/(list wine)}
      ?~  yed
        [dex ~]
      =+  mor=$(yed t.yed)
      =+  dis=^$(dex p.mor, sut i.yed)
      [p.dis q.dis q.mor]
    ::
        {$hold *}
      =+  hey=(~(get by p.dex) sut)
      ?^  hey
        [dex [%stop u.hey]]
      ?:  (~(has in gil) sut)
        =+  dyr=+(~(wyt by p.dex))
        [[(~(put by p.dex) sut dyr) q.dex] [%stop dyr]]
      =+  rom=$(gil (~(put in gil) sut), sut ~(repo ut sut))
      =+  rey=(~(get by p.p.rom) sut)
      ?~  rey
        rom
      [[p.p.rom (~(put by q.p.rom) u.rey q.rom)] [%stop u.rey]]
    ==
  ::
  ++  duck  (dial dole)
  --
++  cain  sell                                          ::  $-(vase tank)
++  noah  text                                          ::  $-(vase tape)
++  onan  seer                                          ::  $-(vise vase)
++  text                                                ::  tape pretty-print
  |=  vax/vase  ^-  tape
  ~(ram re (sell vax))
::
++  seem  |=(toy/typo `type`toy)                        ::  promote typo
++  seer  |=(vix/vise `vase`vix)                        ::  promote vise
++  sell                                                ::  tank pretty-print
  |=  vax/vase  ^-  tank
  ~|  %sell
  (~(deal us p.vax) q.vax)
::
++  skol                                                ::  $-(type tank) for ~!
  |=  typ/type  ^-  tank
  ~(duck ut typ)
::
++  slam                                                ::  slam a gate
  |=  {gat/vase sam/vase}  ^-  vase
  =+  :-  ^=  typ  ^-  type
          [%cell p.gat p.sam]
      ^=  gen  ^-  hoon
      [%cnsg [%$ ~] [%$ 2] [%$ 3] ~]
  =+  gun=(~(mint ut typ) %noun gen)
  [p.gun .*([q.gat q.sam] q.gun)]
::
++  slab                                                ::  test if contains
  |=  {cog/@tas typ/type}
  =(& -:(~(find ut typ) %free [cog ~]))
::
++  slap
  |=  {vax/vase gen/hoon}  ^-  vase                     ::  untyped vase .*
  =+  gun=(~(mint ut p.vax) %noun gen)
  [p.gun .*(q.vax q.gun)]
::
++  slew                                                ::  get axis in vase
  |=  {axe/@ vax/vase}  ^-  (unit vase)
  ?.  |-  ^-  ?
      ?:  =(1 axe)  &
      ?.  ?=(^ q.vax)  |
      $(axe (mas axe), q.vax .*(q.vax [0 (cap axe)]))
    ~
  `[(~(peek ut p.vax) %free axe) .*(q.vax [0 axe])]
::
++  slim                                                ::  identical to seer?
  |=  old/vise  ^-  vase
  old
::
++  slit                                                ::  type of slam
  |=  {gat/type sam/type}
  ?>  (~(nest ut (~(peek ut gat) %free 6)) & sam)
  (~(play ut [%cell gat sam]) [%cnsg [%$ ~] [%$ 2] [%$ 3] ~])
::
++  slob                                                ::  superficial arm
  |=  {cog/@tas typ/type}
  ^-  ?
  ?+  typ  |
      {$hold *}  $(typ ~(repo ut typ))
      {$core *}
    |-  ^-  ?
    ?~  q.s.q.typ  |
    ?|  (~(has by q.q.n.q.s.q.typ) cog)
        $(q.s.q.typ l.q.s.q.typ)
        $(q.s.q.typ r.q.s.q.typ)
    ==
  ==
::
++  sloe                                                ::  get arms in core
  |=  typ/type
  ^-  (list term)
  ?+    typ  ~
      {$hold *}  $(typ ~(repo ut typ))
      {$core *}
    %-  zing
    %+  turn  ~(tap by q.s.q.typ)
      |=  {* b/tomb}
    %+  turn  ~(tap by q.b)
      |=  {a/term *}
    a
  ==
::
++  slop                                                ::  cons two vases
  |=  {hed/vase tal/vase}
  ^-  vase
  [[%cell p.hed p.tal] [q.hed q.tal]]
::
++  slot                                                ::  got axis in vase
  |=  {axe/@ vax/vase}  ^-  vase
  [(~(peek ut p.vax) %free axe) .*(q.vax [0 axe])]
::
++  slym                                                ::  slam w+o sample-type
  |=  {gat/vase sam/*}  ^-  vase
  (slap gat(+<.q sam) [%limb %$])
::
++  spec                                                ::  reconstruct type
  |=  vax/vase
  ^-  vase
  :_  q.vax
  ?@  q.vax  (~(fuse ut p.vax) [%atom %$ ~])
  ?@  -.q.vax
    ^=  typ
    %-  ~(play ut p.vax)
    [%wtgr [%wtts [%leaf %tas -.q.vax] [%& 2]~] [%$ 1]]
  (~(fuse ut p.vax) [%cell %noun %noun])
::
::::  5d: parser
  ::
++  vang                                                ::  set ++vast params
  |=  {bug/? wer/path}                                  ::  bug: debug mode
  %*(. vast bug bug, wer wer)                           ::  wer: where we are
::
++  vast                                                ::  main parsing core
  =+  [bug=`?`| wer=*path]
  |%
  ++  gash  %+  cook                                    ::  parse path
              |=  a/(list tyke)  ^-  tyke
              ?~(a ~ (weld i.a $(a t.a)))
            (more fas limp)
  ++  gasp  ;~  pose                                    ::  parse =path= etc.
              %+  cook
                |=({a/tyke b/tyke c/tyke} :(weld a b c))
              ;~  plug
                (cook |=(a/(list) (turn a |=(b/* ~))) (star tis))
                (cook |=(a/hoon [[~ a] ~]) hasp)
                (cook |=(a/(list) (turn a |=(b/* ~))) (star tis))
              ==
              (cook |=(a/(list) (turn a |=(b/* ~))) (plus tis))
            ==
  ++  glam  ~+((glue ace))
  ++  hasp  ;~  pose                                    ::  path element
              (ifix [sel ser] wide)
              (stag %cnhp (ifix [pel per] (most ace wide)))
              (stag %sand (stag %tas (cold %$ buc)))
              (stag %sand (stag %t qut))
              %+  cook
                |=(a/coin [%sand ?:(?=({$~ $tas *} a) %tas %ta) ~(rent co a)])
              nuck:so
            ==
  ++  limp  %+  cook
              |=  {a/(list) b/tyke}
              ?~  a  b
              $(a t.a, b [`[%sand %tas %$] b])
            ;~(plug (star fas) gasp)
  ++  mota  %+  cook
              |=({a/tape b/tape} (rap 3 (weld a b)))
            ;~(plug (star low) (star hig))
  ++  glom
    |=  {wit/whit taw/whit}
    ^-  whit
    :*  ?~(lab.wit lab.taw lab.wit)
        ?~(boy.wit boy.taw boy.wit)
        (~(uni by def.wit) def.taw)
        (~(uni in use.wit) use.taw)
    ==
  ++  docs
    |%
    ::
    ::  above core
    ::
    ++  apex
      ;~  plug
        =/  ron  (punt (indo noel))
        (punt (ifix [ron ron] (into head)))             ::  label
      ::
        =/  ron  (punt (indo null))
        (ifix [ron ron] (punt body))                    ::  body
      ::
        (cook malt (star fill))                         ::  definitions
        (easy ~)                                        ::  defs used (none)
      ==
    ::
    ::  backward line
    ::
    ++  apse
      ;~  pose
        %+  cook  |=({a/term b/cord} %*(. *whit def (my [a b ~] ~)))
        (exit fine)
      ::
        %+  cook  |=(a/cord %*(. *whit boy `[a ~]))
        (exit line)
      ::
        (easy *whit)
      ==
    ::
    ::
    ++  beer
      |=  $:  lab/(unit term)
              boy/(unit (pair cord (list sect)))
              def/(list (pair (pair term cord) (list sect)))
          ==
      ^-  whit
      =;  def  [lab boy (malt def) ~]
      (turn def |=({{a/term b/cord} c/(list sect)} [a [b c]]))
    ::
    ::
    ++  body
      ;~  pose
        ;~  plug                                        :: can duplicate :>
          (into ;~(pfix (punt ;~(plug null col gar step)) line))
          (easy ~)
        ==
        ;~  plug
          (into ;~(pfix step line))
          (rant text)
        ==
      ==
    ::
    ++  text  (pick line code)                          ::  text line
    ++  line  ;~(less ace (cook crip (star prn)))       ::  prose line
    ++  code  ;~(pfix step step (cook crip (star prn))) ::  code line
    ++  noel  ;~(plug (punt ;~(pfix step hax)) null)    ::  header padding
    ++  head  ;~(pfix hax step cen sym)                 ::  header line
    ++  null  (cold ~ (star ace))                       ::  blank line
    ++  fine                                            ::  definition line
      ;~  (glue ;~(plug col ace))
        sym
        (cook crip (star prn))
      ==
    ::
    ::
    ::  step: indent
    ::  into: :> and indent to end of line, consuming following space.
    ::  indo: :> to end of line, consuming following space.
    ::  exit: :< to end of line, not consuming following space.
    ::
    ++  step  ;~(plug ace ace)
    ++  into  |*(bod/rule (indo ;~(pfix step bod)))
    ::
    ++  indo
      |*  bod/rule
      ;~(pfix col gar ;~(sfix bod (just `@`10) (punt gap)))
    ::
    ++  exit
      |*  bod/rule
      ;~(pfix (star ace) col gal step bod)
    ::
    ::  fill: full definition
    ::
    ++  fill
      %+  cook  |=({{a/term b/cord} c/(list sect) (unit $~)} [a b c])
      ;~  plug
        (into fine)
        (rant ;~(pfix step text))
        (punt (indo null))
      ==
    ::
    ::  rant: series of sections.
    ::
    ++  rant
      |*  sec/rule
      %-  star
      ;~  pfix  
        (indo null)
        (plus (into sec))
      ==
    --
  ::
  ++  plex                                              ::  reparse static path
    |=  gen/hoon  ^-  (unit path)
    ?:  ?=({$dbug *} gen)                               ::  unwrap $dbug
      $(gen q.gen)
    ?.  ?=({$clsg *} gen)  ~                            ::  require :~ hoon
    %+  reel  p.gen                                     ::  build using elements
    |=  {a/hoon b/_`(unit path)`[~ u=/]}                ::  starting from just /
    ?~  b  ~
    ?.  ?=({$sand ?($ta $tas) @} a)  ~                  ::  /foo constants
    `[q.a u.b]
  ::
  ++  phax
    |=  ruw/(list (list woof))
    =+  [yun=*(list hoon) cah=*(list @)]
    =+  wod=|=({a/tape b/(list hoon)} ^+(b ?~(a b [[%smfs %knit (flop a)] b])))
    |-  ^+  yun
    ?~  ruw
      (flop (wod cah yun))
    ?~  i.ruw  $(ruw t.ruw)
    ?@  i.i.ruw
      $(i.ruw t.i.ruw, cah [i.i.ruw cah])
    $(i.ruw t.i.ruw, cah ~, yun [p.i.i.ruw (wod cah yun)])
  ::
  ++  posh
    |=  {pre/(unit tyke) pof/(unit {p/@ud q/tyke})}
    ^-  (unit (list hoon))
    =-  ?^(- - ~&(%posh-fail -))
    =+  wom=(poof wer)
    %+  biff
      ?~  pre  `u=wom
      %+  bind  (poon wom u.pre)
      |=  moz/(list hoon)
      ?~(pof moz (weld moz (slag (lent u.pre) wom)))
    |=  yez/(list hoon)
    ?~  pof  `yez
    =+  zey=(flop yez)
    =+  [moz=(scag p.u.pof zey) gul=(slag p.u.pof zey)]
    =+  zom=(poon (flop moz) q.u.pof)
    ?~(zom ~ `(weld (flop gul) u.zom))
  ::
  ++  poof                                              ::  path -> (list hoon)
    |=(pax/path ^-((list hoon) (turn pax |=(a/@ta [%sand %ta a]))))
  ::
  ::  tyke is =foo== as ~[~ `foo ~ ~]
  ::  interpolate '=' path components
  ++  poon                                              ::  try to replace '='s
    |=  {pag/(list hoon) goo/tyke}                      ::    default to pag
    ^-  (unit (list hoon))                              ::    for null goo's
    ?~  goo  `~                                         ::  keep empty goo
    %+  both                                            ::  otherwise head comes
      ?^(i.goo i.goo ?~(pag ~ `u=i.pag))                ::    from goo or pag
    $(goo t.goo, pag ?~(pag ~ t.pag))                   ::  recurse on tails
  ::
  ++  poor
    %+  sear  posh
    ;~  plug
      (stag ~ gash)
      ;~(pose (stag ~ ;~(pfix cen porc)) (easy ~))
    ==
  ::
  ++  porc
    ;~  plug
      (cook |=(a/(list) (lent a)) (star cen))
      ;~(pfix fas gash)
    ==
  ::
  ++  rump
    %+  sear
      |=  {a/wing b/(unit hoon)}  ^-  (unit hoon)
      ?~(b [~ %wing a] ?.(?=({@ $~} a) ~ [~ [%rock %tas i.a] u.b]))
    ;~(plug rope ;~(pose (stag ~ wede) (easy ~)))
  ::
  ++  rood
    ;~  pfix  fas
      (stag %clsg poor)
    ==
  ::
  ++  rupl
    %+  cook
      |=  {a/? b/(list hoon) c/?}
      ?:  a
        ?:  c
          [%clsg [%clsg b] ~]
        [%clsg b]
      ?:  c
        [%clsg [%cltr b] ~]
      [%cltr b]
    ;~  plug
      ;~  pose
        (cold | (just '['))
        (cold & (jest '~['))
      ==
    ::
      ;~  pose
        (ifix [ace gap] (most gap tall))
        (most ace wide)
      ==
    ::
      ;~  pose
        (cold & (jest ']~'))
        (cold | (just ']'))
      ==
    ==
  ::
  ::
  ++  sail                                              ::  xml template
    |=  in-tall-form/?  =|  lin/?
    |%
    ::
    ++  apex                                            ::  product hoon
      %+  cook
        |=  tum/(each manx:hoon marl:hoon)  ^-  hoon
        ?-  -.tum
          $&  [%xray p.tum]
          $|  [%smts p.tum]
        ==
      top-level
    ::
    ++  top-level                                        ::  entry-point
      ;~(pfix sem ?:(in-tall-form tall-top wide-top))
    ::
    ++  inline-embed                                    ::  brace interpolation
      %+  cook  |=(a/tuna:hoon a)
      ;~  pose
        ;~(pfix sem bracketed-elem(in-tall-form |))
        ;~(plug tuna-mode sump)
        (stag %tape sump)
      ==
    ::
    ++  script-or-style                                 ::  script or style
      %+  cook  |=(a/marx:hoon a)
      ;~  plug
        ;~(pose (jest %script) (jest %style))
        wide-attrs
      ==
    ::
    ++  tuna-mode                                       ::  xml node(s) kind
      ;~  pose
        (cold %tape hep)
        (cold %manx lus)
        (cold %marl tar)
        (cold %call cen)
      ==
    ::
    ++  wide-top                                        ::  wide outer top
      %+  knee  *(each manx:hoon marl:hoon)  |.  ~+
      ;~  pose
        (stag %| wide-quote)
        (stag %| wide-paren-elems)
        (stag %& ;~(plug tag-head wide-tail))
      ==
    ::
    ++  wide-inner-top                                  ::  wide inner top
      %+  knee  *(each tuna:hoon marl:hoon)  |.  ~+
      ;~  pose
        wide-top
        (stag %& ;~(plug tuna-mode wide))
      ==
    ::
    ++  wide-attrs                                      ::  wide attributes
      %+  cook  |=(a/(unit mart:hoon) (fall a ~))
      %-  punt
      %+  ifix  [pel per]
      %+  more  (jest ', ')
      ;~((glue ace) a-mane hopefully-quote)
    ::
    ++  wide-tail                                       ::  wide elements
      %+  cook  |=(a/marl:hoon a)
      ;~(pose ;~(pfix col wrapped-elems) (cold ~ sem) (easy ~))
    ::
    ++  wide-elems                                      ::  wide elements
      %+  cook  |=(a/marl:hoon a)
      %+  cook  join-tops
      (star ;~(pfix ace wide-inner-top))
    ::
    ++  wide-paren-elems                                ::  wide flow
      %+  cook  |=(a/marl:hoon a)
      %+  cook  join-tops
      (ifix [pel per] (more ace wide-inner-top))
    ::
    ::+|
    ::
    ++  drop-top
      |=  a/(each tuna:hoon marl:hoon)  ^-  marl:hoon
      ?-  -.a
        $&  [p.a]~
        $|  p.a
      ==
    ::
    ++  join-tops
      |=  a/(list (each tuna:hoon marl:hoon))  ^-  marl:hoon
      (zing (turn a drop-top))
    ::
    ::+|
    ::
    ++  wide-quote                                      ::  wide quote
      %+  cook  |=(a/marl:hoon a)
      ;~  pose
        ;~  less  (jest '"""')
          (ifix [doq doq] (cook collapse-chars quote-innards))
        ==
      ::
        %-  inde
        %+  ifix  [(jest '"""\0a') (jest '\0a"""')]
        (cook collapse-chars quote-innards(lin |))
      ==
    ::
    ++  quote-innards                                   ::  wide+tall flow
      %+  cook  |=(a/(list $@(@ tuna:hoon)) a)
      %-  star
      ;~  pose
        ;~(pfix bas ;~(pose (mask "-+*%;\{") bas doq bix:ab))
        inline-embed
        ;~(less bas kel ?:(in-tall-form fail doq) prn)
        ?:(lin fail ;~(less (jest '\0a"""') (just '\0a')))
      ==
    ::
    ++  bracketed-elem                                  ::  bracketed element
      %+  ifix  [kel ker]
      ;~(plug tag-head wide-elems)
    ::
    ++  wrapped-elems                                   ::  wrapped tuna
      %+  cook  |=(a/marl:hoon a)
      ;~  pose
        wide-paren-elems
        (cook |=(@t `marl`[;/((trip +<))]~) qut)
        (cook drop-top wide-top)
      ==
    ::
    ::+|
    ::
    ++  a-mane                                          ::  mane as hoon
      %+  cook
        |=  {a/@tas b/(unit @tas)}
        ?~(b a [a u.b])
      ;~(plug sym ;~(pose (stag ~ ;~(pfix cab sym)) (easy ~)))
    ::
    ++  en-class
      |=  a/(list {$class p/term})
      ^-  (unit {$class tape})
      ?~  a  ~
      %-  some
      :-  %class
      |-
      %+  welp  (trip p.i.a)
      ?~  t.a  ~
      [' ' $(a t.a)]
    ::
    ++  tag-head                                        ::  tag head
      %+  cook
        =+  hoon  ::REVIEW rename dynamic xml types
        |=  {a/mane b/mart c/mart}
        ^-  marx
        [a (weld b c)]
      ;~  plug
        a-mane
      ::
        %+  cook
          |=  a/(list (unit {term (list beer:hoon)}))
          ^-  (list {term (list beer:hoon)})
          :: discard nulls
          (murn a same)
        ;~  plug
          (punt ;~(plug (cold %id hax) (cook trip sym)))
          (cook en-class (star ;~(plug (cold %class dot) sym)))
          (punt ;~(plug ;~(pose (cold %href fas) (cold %src pat)) soil))
          (easy ~)
        ==
      ::
        wide-attrs
      ==
    ::
    ::+|
    ::
    ++  tall-top                                        ::  tall top
      %+  knee  *(each manx:hoon marl:hoon)  |.  ~+
      ;~  pose
        (stag %| ;~(pfix (plus ace) (cook collapse-chars quote-innards)))
        (stag %& ;~(plug script-or-style script-style-tail))
        (stag %& tall-elem)
        (stag %| wide-quote)
        (stag %| ;~(pfix tis tall-tail))
        (stag %& ;~(pfix gar gap (stag [%div ~] cram)))
        (stag %| ;~(plug ;~((glue gap) tuna-mode tall) (easy ~)))
        (easy %| [;/("\0a")]~)
      ==
    ::
    ++  tall-attrs                                      ::  tall attributes
      %-  star
      ;~  pfix  ;~(plug gap tis)
        ;~((glue gap) a-mane hopefully-quote)
      ==
    ::
    ++  tall-elem                                       ::  tall preface
      %+  cook
        =+  hoon  ::REVIEW rename dynamic xml types
        |=  {a/{p/mane q/mart} b/mart c/marl}
        ^-  manx
        [[p.a (weld q.a b)] c]
      ;~(plug tag-head tall-attrs tall-tail)
    ::
    ::+|
    ::
    ::REVIEW is there a better way to do this?
    ++  hopefully-quote                                 :: prefer "quote" form
      %+  cook  |=(a/(list beer:hoon) a)
      %+  cook  |=(a/hoon ?:(?=($knit -.a) p.a [~ a]~))
      wide
    ::
    ++  script-style-tail                               ::  unescaped tall tail
      %+  cook  |=(a/marl:hoon a)
      %+  ifix  [gap ;~(plug gap duz)]
      %+  most  gap
      ;~  pfix  sem
        %+  cook  |=(a/tape ;/(a))
        ;~  pose
          ;~(pfix ace (star prn))
          (easy "\0a")
        ==
      ==
    ::
    ++  tall-tail                                       ::  tall tail
      ?>  in-tall-form
      %+  cook  |=(a/marl:hoon a)
      ;~  pose
        (cold ~ sem)
        ;~(pfix col wrapped-elems(in-tall-form |))
        ;~(pfix col ace (cook collapse-chars(in-tall-form |) quote-innards))
        (ifix [gap ;~(plug gap duz)] tall-kids)
      ==
    ::
    ++  tall-kids                                       ::  child elements
      %+  cook  join-tops
      ::  look for sail first, or markdown if not
      (most gap ;~(pose top-level (stag %| cram)))
    ::
    ++  collapse-chars                                  ::  group consec chars
      |=  reb/(list $@(@ tuna:hoon))
      ^-  marl:hoon
      =|  {sim/(list @) tuz/marl:hoon}
      |-  ^-  marl:hoon
      ?~  reb
        =.  sim
          ?.  in-tall-form   sim
          [10 |-(?~(sim sim ?:(=(32 i.sim) $(sim t.sim) sim)))]
        ?~(sim tuz [;/((flop sim)) tuz])
      ?@  i.reb
        $(reb t.reb, sim [i.reb sim])
      ?~  sim  [i.reb $(reb t.reb, sim ~)]
      [;/((flop sim)) i.reb $(reb t.reb, sim ~)]
    --
  ++  cram                                              ::  parse unmark
    =>  |%
        ++  item  (pair mite marl:hoon)                 ::  xml node generator
        ++  colm  @ud                                   ::  column
        ++  tarp  marl:hoon                             ::  node or generator
        ++  mite                                        ::  context
          $?  $down                                     ::  outer embed
              $lunt                                     ::  unordered list
              $lime                                     ::  list item
              $lord                                     ::  ordered list
              $poem                                     ::  verse
              $bloc                                     ::  blockquote
              $head                                     ::  heading
          ==                                            ::
        ++  trig                                        ::  line style
          $:  col/@ud                                   ::  start column
              sty/trig-style                            ::  style
          ==                                            ::
        ++  trig-style                                  ::  type of parsed line
          $%  $:  $end                                  ::  terminator
          $?  $done                                     ::  end of input
                  $stet                                 ::    == end of markdown
                  $dent                                 ::    outdent
              ==  ==                                    ::
              $:  $one                                  ::  leaf node
              $?  $rule                                 ::    --- horz rule
                  $fens                                 ::    ``` code fence
                  $expr                                 ::    ;sail expression
              ==  ==                                    ::
              {$new p/trig-new}                         ::  open container 
              {$old $text}                              ::  anything else
          ==                                            ::
        ++  trig-new                                    ::  start a
          $?  $lite                                     ::    + line item
              $lint                                     ::    - line item
              $head                                     ::  # heading
              $bloc                                     ::  > block-quote
              $poem                                     ::    [ ]{8} poem
          ==                                            ::
        ++  graf                                        ::  paragraph element
          $%  {$bold p/(list graf)}                     ::  *bold*
              {$talc p/(list graf)}                     ::  _italics_
              {$quod p/(list graf)}                     ::  "double quote"
              {$code p/tape}                            ::  code literal
              {$text p/tape}                            ::  text symbol
              {$link p/(list graf) q/tape}              ::  URL
              {$expr p/tuna:hoon}                       ::  interpolated hoon
          ==
        --
    =<  (non-empty:parse |=(nail `(like tarp)`~($ main +<)))
    |%
    ++  main
      ::
      ::  state of the parsing loop.  we maintain a construction
      ::  stack for elements and a line stack for lines in the
      ::  current block.  a blank line causes the current block
      ::  to be parsed and thrown in the current element.  when
      ::  the indent column retreats, the element stack rolls up.
      ::
      ::  verbose: debug printing enabled
      ::  err: error position
      ::  ind: outer and inner indent level
      ::  hac: stack of items under construction
      ::  cur: current item under construction
      ::  par: current "paragraph" being read in
      ::  [loc txt]: parsing state
      ::
      =/  verbose  &
      =|  err/(unit hair)
      =|  ind/{out/@ud inr/@ud}
      =|  hac/(list item)
      =/  cur/item  [%down ~]
      =|  par/(unit (pair hair wall))
      |_  {loc/hair txt/tape}
      ::
      ++  $                                           ::  resolve
        ^-  (like tarp)
        =>  line
        ::
        ::  if error position is set, produce error
        ?.  =(~ err)
          ~&  err+err
          [+.err ~]
        ::
        ::  all data was consumed
        =-  [loc `[- [loc txt]]]
        =>  close-par
        |-  ^-  tarp
        ::
        ::  fold all the way to top
        ?~  hac  cur-to-tarp
        $(..^$ close-item)
      ::
      ::+|
      ::
      ++  cur-indent
        ?-  p.cur
          $down  2
          $head  0
          $lunt  0
          $lime  2
          $lord  0
          $poem  8
          $bloc  2
        ==
      ::
      ++  back                                          ::  column retreat
        |=  luc/@ud
        ^+  +>
        ?:  (gte luc inr.ind)  +>
        ::
        ::  nex: next backward step that terminates this context
        =/  nex/@ud  cur-indent  ::REVIEW code and poem blocks are handled elsewhere
        ?:  (gth nex (sub inr.ind luc))
          ::
          ::  indenting pattern violation
          ~?  verbose  indent-pattern-violation+[p.cur nex inr.ind luc]
          ..^$(inr.ind luc, err `[p.loc luc])
        =.  ..^$  close-item
        $(inr.ind (sub inr.ind nex))
      ::
      ++  cur-to-tarp                                   ::  item to tarp
        ^-  tarp
        ?:  ?=(?($down $head $expr) p.cur)
          (flop q.cur)
        =-  [[- ~] (flop q.cur)]~
        ?-  p.cur
          $lunt  %ul
          $lord  %ol
          $lime  %li
          $poem  %div ::REVIEW actual container element?
          $bloc  %blockquote
        ==
      ::
      ++  close-item  ^+  .                             ::  complete and pop
        ?~  hac  .
        %=  .
          hac  t.hac
          cur  [p.i.hac (weld cur-to-tarp q.i.hac)]
        ==
      ::
      ++  read-line                                     ::  capture raw line
        =|  lin/tape
        |-  ^+  [[lin *(unit _err)] +<.^$]  :: parsed tape and halt/error
        ::
        ::  no unterminated lines
        ?~  txt
          ~?  verbose  %unterminated-line
          [[~ ``loc] +<.^$]
        ?.  =(`@`10 i.txt)
          ?:  (gth inr.ind q.loc)
            ?.  =(' ' i.txt)
              ~?  verbose  expected-indent+[inr.ind loc txt]
              [[~ ``loc] +<.^$]
            $(txt t.txt, q.loc +(q.loc))
          ::
          ::  save byte and repeat
          $(txt t.txt, q.loc +(q.loc), lin [i.txt lin])
        =.  lin
        ::
        ::  trim trailing spaces
        |-  ^-  tape
          ?:  ?=({$' ' *} lin)
            $(lin t.lin)
          (flop lin)
          ::
        =/  eat-newline/nail  [[+(p.loc) 1] t.txt]
        =/  saw  look(+<.$ eat-newline)
        ::
        ?:  ?=({$~ @ $end ?($stet $dent)} saw)          ::  stop on == or dedent
          [[lin `~] +<.^$]
        [[lin ~] eat-newline]
      ::
      ++  look                                          ::  inspect line
        ^-  (unit trig)
        %+  bind  (wonk (look:parse loc txt))
        |=  a/trig  ^+  a
        ::
        ::  treat a non-terminator as a terminator
        ::  if it's outdented
        ?:  =(%end -.sty.a)  a
        ?:  (lth col.a out.ind)
          a(sty [%end %dent])
        a
      ::
      ++  close-par                                     ::  make block
        ^+  .
        ::
        ::  empty block, no action
        ?~  par  .
        ::
        ::  if block is verse
        ?:  ?=($poem p.cur)
          ::
          ::  add break between stanzas
          =.  q.cur  ?~(q.cur q.cur [[[%br ~] ~] q.cur])
          =-  close-item(par ~, q.cur (weld - q.cur), inr.ind (sub inr.ind 8))
          %+  turn  q.u.par
          |=  tape  ^-  manx
          ::
          ::  each line is a paragraph
          :-  [%p ~]
          :_  ~
          ;/("{+<}\0a")
        ::
        ::  yex: block recomposed, with newlines
        =/  yex/tape
          (zing (turn (flop q.u.par) |=(a/tape (runt [(dec inr.ind) ' '] "{a}\0a"))))
        ::
        ::  vex: parse of paragraph
        =/  vex/(like tarp)
          ::
          ::  either a one-line header or a paragraph
          %.  [p.u.par yex]
          ?:  ?=($head p.cur)
            (full head:parse)
          (full para:parse)
        ::
        ::  if error, propagate correctly
        ?~  q.vex
          ~?  verbose  [%close-par p.cur yex]
          ..$(err `p.vex)
        ::
        ::  finish tag if it's a header
        =<  ?:(?=($head p.cur) close-item ..$)
        ::
        ::  save good result, clear buffer
        ..$(par ~, q.cur (weld p.u.q.vex q.cur))
      ::
      ++  line  ^+  .                                   ::  body line loop
        ::
        ::  abort after first error
        ?:  !=(~ err)  .
        ::
        ::  saw: profile of this line
        =/  saw  look
        ~?  [debug=|]  [%look ind=ind saw=saw txt=txt]
        ::
        ::  if line is blank
        ?~  saw
          ::
          ::  break section
          =^  a/{tape fin/(unit _err)}  +<.$  read-line
          ?^  fin.a
            ..$(err u.fin.a)
          =>(close-par line)
        ::
        ::  line is not blank
        =>  .(saw u.saw)
        ::
        ::  if end of input, complete
        ?:  ?=($end -.sty.saw)
          ..$(q.loc col.saw)
        ::
        =.  ind  ?~(out.ind [col.saw col.saw] ind)      ::  init indents
        ::
        ?:  ?|  ?=($~ par)                          :: if after a paragraph or
                ?&  ?=(?($down $lime $bloc) p.cur)  :: unspaced new container
                    |(!=(%old -.sty.saw) (gth col.saw inr.ind))
            ==  ==
          =>  .(..$ close-par)
            ::
          ::  if column has retreated, adjust stack
          =.  ..$  (back col.saw)
            ::
          =^  col-ok  sty.saw
            ?+  (sub col.saw inr.ind)  [| sty.saw]        :: columns advanced
              $0  [& sty.saw]
              $8  [& %new %poem]
            ==
          ?.  col-ok
            ~?  verbose  [%columns-advanced col.saw inr.ind]
            ..$(err `[p.loc col.saw])
        ::
          =.  inr.ind  col.saw
      ::
          ::  unless adding a matching item, close lists
          =.  ..$
            ?:  ?|  &(?=($lunt p.cur) !?=($lint +.sty.saw))
                    &(?=($lord p.cur) !?=($lite +.sty.saw))
                ==
              close-item
            ..$
        ::
          =<  line(par `[loc ~])  ^+  ..$               ::  continue with para
          ?-    -.sty.saw
              $one  (read-one +.sty.saw)                ::  parse leaves
              $new  (open-item p.sty.saw)               ::  open containers
              $old  ..$                                 ::  just text
          ==
        ::
        ::
        ::- - - foo
        ::  detect bad block structure
        ?.  ::  first line of container is legal
            ?~  q.u.par  &
            ?-  p.cur
        ::
            ::  can't(/directly) contain text
              ?($lord $lunt)  ~|(bad-leaf-container+p.cur !!)
        ::
            ::  only one line in a header
              $head  |
          ::
            ::  indented literals need to end with a blank line
              $poem  (gte col.saw inr.ind)
        ::
            ::  text tarps must continue aligned
              ?($down $lunt $lime $lord $bloc)  =(col.saw inr.ind)
          ==
          ~?  verbose  bad-block-structure+[p.cur inr.ind col.saw]
          ..$(err `[p.loc col.saw])
        ::
        ::  accept line and maybe continue
        =^  a/{lin/tape fin/(unit _err)}  +<.$  read-line
        =.  par  par(q.u [lin.a q.u.par])
        ?^  fin.a  ..$(err u.fin.a)
        line
        ::
      ++  parse-block                                   ::  execute parser
        |=  fel/$-(nail (like tarp))  ^+  +>
        =/  vex/(like tarp)  (fel loc txt)
        ?~  q.vex
          ~?  verbose  [%parse-block txt]
          +>.$(err `p.vex)
        =+  [res loc txt]=u.q.vex
        %_  +>.$
          loc  loc
          txt  txt
          q.cur  (weld (flop `tarp`res) q.cur)          ::  prepend to the stack
        ==
      ::
      ++  read-one                                      ::  read %one item
        |=  sty/?($expr $rule $fens)  ^+  +>
        ?-  sty
          $expr  (parse-block expr:parse)
          $rule  (parse-block hrul:parse)
          $fens  (parse-block (fens:parse inr.ind))
        ==
      ::
      ++  open-item                                     ::  enter list/quote
        |=  saw/trig-new
        =<  +>.$:apex
        |%
        ++  apex  ^+  .                                 ::  open container
          ?-  saw
            $poem  (push %poem)                         ::  verse literal
            $head  (push %head)                         ::  heading
            $bloc  (entr %bloc)                         ::  blockquote line
            $lint  (lent %lunt)                         ::  unordered list
            $lite  (lent %lord)                         ::  ordered list
          ==
        ::
        ++  push                                        ::  push context
          |=(mite +>(hac [cur hac], cur [+< ~]))
        ::
        ++  entr                                        ::  enter container
          |=  typ/mite
          ^+  +>
          ::
          ::  indent by 2
          =.  inr.ind  (add 2 inr.ind)
          ::
          ::  "parse" marker
          =.  txt  (slag (sub inr.ind q.loc) txt)
          =.  q.loc  inr.ind
          ::
          (push typ)
        ::
        ++  lent                                        ::  list entry
          |=  ord/?($lord $lunt)
          ^+  +>
          =>  ?:(=(ord p.cur) +>.$ (push ord))          ::  push list if new 
          (entr %lime)
        --
      --
    ::
    ++  parse                                           ::  individual parsers
      |%
      ++  look                                          ::  classify line
        %+  cook  |=(a/(unit trig) a)
        ;~  pfix  (star ace)
          %+  here                                      ::  report indent
            |=({a/pint b/?($~ trig-style)} ?~(b ~ `[q.p.a b]))
          ;~  pose
            (cold ~ (just `@`10))                       ::  blank line
          ::
            (full (easy [%end %done]))                  ::  end of input
            (cold [%end %stet] duz)                     ::  == end of markdown
          ::
            (cold [%one %rule] ;~(plug hep hep hep))    ::  --- horizontal ruler
            (cold [%one %fens] ;~(plug tec tec tec))    ::  ``` code fence
            (cold [%one %expr] sem)                     ::  ;sail expression
          ::
            (cold [%new %head] ;~(plug (star hax) ace)) ::  # heading
            (cold [%new %lint] ;~(plug hep ace))        ::  - line item
            (cold [%new %lite] ;~(plug lus ace))        ::  + line item
            (cold [%new %bloc] ;~(plug gar ace))        ::  > block-quote
          ::
            (easy [%old %text])                         ::  anything else
          ==
        ==
      ::
      ::
      ++  cash                                          ::  escaped fence
        |*  tem/rule
        %-  echo
        %-  star
        ;~  pose
          whit
          ;~(plug bas tem)
          ;~(less tem prn)
        ==
      ::
      ++  cool                                          ::  reparse
        |*  $:  ::  fex: primary parser
                ::  sab: secondary parser
                ::
                fex/rule
                sab/rule
            ==
        |=  {loc/hair txt/tape}
        ^+  *sab
        ::
        ::  vex: fenced span
        =/  vex/(like tape)  (fex loc txt)
        ?~  q.vex  vex
        ::
        ::  hav: reparse full fenced text
        =/  hav  ((full sab) [loc p.u.q.vex])
        ::
        ::  reparsed error position is always at start
        ?~  q.hav  [loc ~]
        ::
        ::  the complete type with the main product
        :-  p.vex
        `[p.u.q.hav q.u.q.vex]
      ::
      ::REVIEW surely there is a less hacky "first or after space" solution
      ++  easy-sol                                      ::  parse start of line
        |*  a/*
        |=  b/nail
        ?:  =(1 q.p.b)  ((easy a) b)
        (fail b)
      ::
      ++  echo                                          ::  hoon literal
        |*  sab/rule
        |=  {loc/hair txt/tape}
        ^-  (like tape)
        ::
        ::  vex: result of parsing wide hoon
        =/  vex  (sab loc txt)
        ::
        ::  use result of expression parser
        ?~  q.vex  vex
        =-  [p.vex `[- q.u.q.vex]]
        ::
        ::  but replace payload with bytes consumed
        |-  ^-  tape
        ?:  =(q.q.u.q.vex txt)  ~
        ?~  txt  ~
        [i.txt $(txt +.txt)]
      ::
      ++  non-empty
        |*  a/rule
        |=  tub/nail  ^+  (a)
        =/  vex  (a tub)
        ~!  vex
        ?~  q.vex  vex
        ?.  =(tub q.u.q.vex)  vex
        (fail tub)
      ::
      ::
      ++  word                                          ::  tarp parser
        %+  knee  *(list graf)  |.  ~+
        %+  cook  |=(a/?(graf (list graf)) ?+(a a {@ *} [a]~))
        ;~  pose
        ::
        ::  ordinary word
        ::
          %+  stag  %text
          ;~(plug ;~(pose low hig) (star ;~(pose nud low hig hep)))
        ::
        ::  naked \escape
        ::
          (stag %text ;~(pfix bas (cook trip ;~(less ace prn))))
        ::
        ::  trailing \ to add <br>
        ::
          (stag %expr (cold [[%br ~] ~] ;~(plug bas (just '\0a'))))
        ::
        ::  *bold literal*
        ::
          (stag %bold (ifix [tar tar] (cool (cash tar) work)))
        ::
        ::  _italic literal_
        ::
          (stag %talc (ifix [cab cab] (cool (cash cab) work)))
        ::
        ::  "quoted text"
        ::
          (stag %quod (ifix [doq doq] (cool (cash doq) work)))
        ::
        ::  `classic markdown quote`
        ::
          (stag %code (ifix [tec tec] (cash tec)))
        ::
        ::  ++arm
        ::
          (stag %code ;~(plug lus lus low (star ;~(pose nud low hep))))
        ::
        ::  [arbitrary *content*](url)
        ::
          %+  stag  %link
          ;~  (glue (punt whit))
            (ifix [sel ser] (cool (cash ser) work))
            (ifix [pel per] (cash per))
          ==
        ::
        ::  #hoon
        ::
          ;~  plug
            (stag %text ;~(pose (cold " " whit) (easy-sol ~)))
            (stag %code ;~(pfix hax (echo wide)))
            ;~(simu whit (easy ~))
          ==
        ::
        ::  direct hoon constant
        ::
          ;~  plug
            (stag %text ;~(pose (cold " " whit) (easy-sol ~)))
          ::
            %+  stag  %code
            %-  echo
            ;~  pose
              ::REVIEW just copy in 0x... parsers directly?
              ;~(simu ;~(plug (just '0') alp) bisk:so)
            ::
              tash:so
              ;~(pfix dot perd:so)
              ;~(pfix sig ;~(pose twid:so (easy [%$ %n 0])))
              ;~(pfix cen ;~(pose sym buc pam bar qut nuck:so))
            ==
          ::
            ;~(simu whit (easy ~))
          ==
        ::
        ::  whitespace
        ::
          (stag %text (cold " " whit))
        ::
        ::  {interpolated} sail
        ::
          (stag %expr inline-embed:(sail |))
        ::
        ::  just a byte
        ::
          (stag %text (cook trip ;~(less ace prn)))
        ==
      ::
      ++  work  (cook zing (star word))                 ::  indefinite tarp
      ::
      ++  down                                          ::  parse inline tarp
        %+  knee  *tarp  |.  ~+
        =-  (cook - work)
        ::
        ::  collect raw tarp into xml tags
        |=  gaf/(list graf)
        ^-  tarp
        =<  main
        |%
        ++  main
          ^-  tarp
          ?~  gaf  ~
          ?.  ?=($text -.i.gaf)
            (weld (item i.gaf) $(gaf t.gaf))
          ::
          ::  fip: accumulate text blocks
          =/  fip/(list tape)  [p.i.gaf]~
          |-  ^-  tarp
          ?~  t.gaf  [;/((zing (flop fip))) ~]
          ?.  ?=($text -.i.t.gaf)
            [;/((zing (flop fip))) ^$(gaf t.gaf)]
          $(gaf t.gaf, fip :_(fip p.i.t.gaf))
        ::
        ++  item
          |=  nex/graf
          ^-  tarp  ::CHECK can be tuna:hoon?
          ?-  -.nex
            $text  !!  :: handled separately
            $expr  [p.nex]~
            $bold  [[%b ~] ^$(gaf p.nex)]~
            $talc  [[%i ~] ^$(gaf p.nex)]~
            $code  [[%code ~] ;/(p.nex) ~]~
            $quod  ::
                   ::  smart quotes
                   %=    ^$
                       gaf
                     :-  [%text (tufa ~-~201c. ~)]
                     %+  weld  p.nex
                     `(list graf)`[%text (tufa ~-~201d. ~)]~
                   ==
            $link  [[%a [%href q.nex] ~] ^$(gaf p.nex)]~
          ==
        --
      ::
      ++  hrul                                          ::  empty besides fence
        %+  cold  [[%hr ~] ~]~
        ;~(plug (star ace) hep hep hep (star hep) (just '\0a'))
      ::
      ++  tecs
        ;~(plug tec tec tec (just '\0a'))
      ::
      ++  fens
        |=  col/@u  ~+
        =/  ind  (stun [(dec col) (dec col)] ace)
        =/  ind-tecs  ;~(plug ind tecs)
        %+  cook  |=(txt/tape `tarp`[[%pre ~] ;/(txt) ~]~)
        ::
        ::  leading outdent is ok since container may
        ::  have already been parsed and consumed
        %+  ifix  [;~(plug (star ace) tecs) ind-tecs]
        %^  stir  ""  |=({a/tape b/tape} "{a}\0a{b}")
        ;~  pose
          %+  ifix  [ind (just '\0a')]
          ;~(less tecs (star prn))
        ::
          (cold "" ;~(plug (star ace) (just '\0a')))
        ==
      ::
      ++  para                                          ::  paragraph
        %+  cook
          |=(a/tarp ?~(a ~ [[%p ~] a]~))
        ;~(pfix (punt whit) down)
      ::
      ++  expr                                          ::  expression
        =>  (sail &)                                    ::  tall-form
        %+  ifix  [(star ace) ;~(simu gap (easy))]      ::  look-ahead for gap
        (cook drop-top top-level)                        ::  list of tags
        ::  
      ::
      ++  whit                                          ::  whitespace
        (cold ' ' (plus ;~(pose (just ' ') (just '\0a'))))
      ::
      ++  head                                          ::  parse heading
        %+  cook
          |=  {haxes/tape kids/tarp}  ^-  tarp
          =/  tag  (crip 'h' <(lent haxes)>)            ::  e.g. ### -> %h3
          =/  id  (contents-to-id kids)
          [[tag [%id id]~] kids]~
        ::
        ;~(pfix (star ace) ;~((glue whit) (stun [1 6] hax) down))
      ::
      ++  contents-to-id                                ::  # text into elem id
        |=  a/(list tuna:hoon)  ^-  tape
        =;  raw/tape
          %+  turn  raw
          |=  @tD
          ^-  @tD
          ?:  ?|  &((gte +< 'a') (lte +< 'z'))
                  &((gte +< '0') (lte +< '9'))
              ==
            +<
          ?:  &((gte +< 'A') (lte +< 'Z'))
            (add 32 +<)
          '-'
        ::
        ::  collect all text in header tarp
        |-  ^-  tape
        ?~  a  ~
        %+  weld
          ^-  tape
          ?-    i.a
              {{$$ {$$ *} $~} $~}                       ::  text node contents
            (murn v.i.a.g.i.a |=(a/beer:hoon ?^(a ~ (some a))))
              {^ *}  $(a c.i.a)                         ::  concatenate children
              {@ *}  ~                                  ::  ignore interpolation
          ==
        $(a t.a)
      --
    --
  ::
  ++  scab
    %+  cook
      |=  a/(list wing)  ^-  hoon
      :-  %bcsm
      |-  ^-  hoon
      ?~(a !! ?~(t.a [%wing i.a] [%tsgl [%wing i.a] $(a t.a)]))
    (most col rope)
  ::
  ++  scad
    %+  knee  *root  |.  ~+
    %-  stew
    ^.  stet  ^.  limo
    :~
      :-  '_'
        ;~(pfix cab (stag %bccb wide))
      :-  ','
        ;~(pfix com (stag %bcsm wide))
      :-  '$'
        ;~  pose
          ;~  pfix  buc
            ;~  pose
              (stag %leaf (stag %tas (cold %$ buc)))
              (stag %leaf (stag %f (cold & pam)))
              (stag %leaf (stag %f (cold | bar)))
              (stag %leaf (stag %t qut))
              (stag %leaf (sear |=(a/coin ?:(?=($$ -.a) (some +.a) ~)) nuck:so))
            ==
          ==
          (stag %bcsm rump)
        ==
      :-  '%'
        ;~  pose
          ;~  pfix  cen
            ;~  pose
              (stag %leaf (stag %tas (cold %$ buc)))
              (stag %leaf (stag %f (cold & pam)))
              (stag %leaf (stag %f (cold | bar)))
              (stag %leaf (stag %t qut))
              (stag %leaf (sear |=(a/coin ?:(?=($$ -.a) (some +.a) ~)) nuck:so))
            ==
          ==
        ==
      :-  '('
        %+  stag  %bcsm
        %+  stag  %cnhp
        %+  ifix  [pel per]
        ;~(plug wide ;~(pose ;~(pfix ace (most ace wyde)) (easy ~)))
      :-  '{'
        (stag %bccl (ifix [kel ker] (most ace wyde)))
      :-  '['
        (stag %bccl (ifix [sel ser] (most ace wyde)))
      :-  '*'
        (cold [%base %noun] tar)
      :-  '@'
        ;~(pfix pat (stag %base (stag %atom mota)))
      :-  '?'
        ;~  pose
          (stag %bcwt ;~(pfix wut (ifix [pel per] (most ace wyde))))
          (cold [%base %bean] wut)
        ==
      :-  '~'
        (cold [%base %null] sig)  
      :-  '^'
        ;~  pose
          scab
          (cold [%base %cell] ket)
        ==
      :-  '='
        ;~  pfix  tis
          %+  sear
            |=  hon/hoon
            ^-  (unit hoon)      
            %+  bind
              |-  ^-  (unit term)
              ?+  hon  ~
                {$bcsm *}  $(hon p.hon)
                {$wing *}  ?~(p.hon ~ ?^(i.p.hon ~ `i.p.hon))
                {$limb *}  `p.hon
                {$dbug *}  $(hon ~(open ap hon))
                {$tsgl *}  $(hon ~(open ap hon))
                {$tsgr *}  $(hon q.hon)
              ==
            |=(term [%bcts +< hon])
          wyde
        ==
      :-  ['a' 'z']
        ;~  pose
          (stag %bcts ;~(plug sym ;~(pfix ;~(pose fas tis) wyde)))
          scab
        ==
    ==
  ::
  ++  scat  !:
    %+  knee  *hoon  |.  ~+
    %-  stew
    ^.  stet  ^.  limo
    :~
      :-  ','
        ;~  pose
          ;~(pfix com wyde)
          (stag %wing rope)
        ==
      :-  '!'
        ;~  pose
          (stag %wtzp ;~(pfix zap wide))
          (stag %zpzp (cold ~ ;~(plug zap zap)))
        ==
      :-  '_'
        ;~(pfix cab (stag %bccb wide))
      :-  '$'
        ;~  pose
          ;~  pfix  buc
            ;~  pose
              (stag %leaf (stag %tas (cold %$ buc)))
              (stag %leaf (stag %f (cold & pam)))
              (stag %leaf (stag %f (cold | bar)))
              (stag %leaf (stag %t qut))
              (stag %leaf (sear |=(a/coin ?:(?=($$ -.a) (some +.a) ~)) nuck:so))
            ==
          ==
          rump
        ==
      :-  '%'
        ;~  pfix  cen
          ;~  pose
            (stag %clsg (sear |~({a/@ud b/tyke} (posh ~ ~ a b)) porc))
            (stag %rock (stag %tas (cold %$ buc)))
            (stag %rock (stag %f (cold & pam)))
            (stag %rock (stag %f (cold | bar)))
            (stag %rock (stag %t qut))
            (cook (jock &) nuck:so)
            (stag %clsg (sear |=(a/(list) (posh ~ ~ (lent a) ~)) (star cen)))
          ==
        ==
      :-  '&'
        ;~  pose
          (cook |=(a/wing [%cnts a ~]) rope)
          (stag %wtpm ;~(pfix pam (ifix [pel per] (most ace wide))))
          ;~(plug (stag %rock (stag %f (cold & pam))) wede)
          (stag %sand (stag %f (cold & pam)))
        ==
      :-  '\''
        (stag %sand (stag %t qut))
      :-  '('
        (stag %cnhp (ifix [pel per] (most ace wide)))
      :-  '{'
        (stag %bccl (ifix [kel ker] (most ace wyde)))
      :-  '*'
        ;~  pose
          (stag %bunt ;~(pfix tar wyde))
          (cold [%base %noun] tar)
        ==
      :-  '@'
        ;~(pfix pat (stag %base (stag %atom mota)))
      :-  '+'
        ;~  pose
          (stag %dtls ;~(pfix lus (ifix [pel per] wide)))
        ::
          %+  cook
            |=  a/(list (list woof))
            :-  %smfs
            [%knit |-(^-((list woof) ?~(a ~ (weld i.a $(a t.a)))))]
          (most dog ;~(pfix lus soil))
        ::
          (cook |=(a/wing [%cnts a ~]) rope)
        ==
      :-  '-'
        ;~  pose
          (stag %sand tash:so)
        ::
          %+  cook
            |=  a/(list (list woof))
            [%clsg (phax a)]
          (most dog ;~(pfix hep soil))
        ::
          (cook |=(a/wing [%cnts a ~]) rope)
        ==
      :-  '.'
        ;~  pose
          (cook (jock |) ;~(pfix dot perd:so))
          (cook |=(a/wing [%cnts a ~]) rope)
        ==
      :-  ['0' '9']
        %+  cook
          |=  {a/dime b/(unit hoon)}
          ?~(b [%sand a] [[%rock a] u.b])
        ;~(plug bisk:so (punt wede))
      :-  ':'
        ;~  pfix  col
          ;~  pose
            (stag %smcl (ifix [pel per] (most ace wide)))
            ;~(pfix fas (stag %smfs wide))
          ==
        ==
      :-  '='
        (stag %dtts ;~(pfix tis (ifix [pel per] ;~(glam wide wide))))
      :-  '?'
        ;~  pose
          (stag %bcwt ;~(pfix wut (ifix [pel per] (most ace wyde))))
          (cold [%base %bean] wut)
        ==
      :-  '['
        rupl
      :-  '^'
        ;~  pose
          (stag %wing rope)
          (cold [%base %cell] ket)
        ==
      :-  '`'
        ;~  pfix  tec
          ;~  pose
            %+  cook
              |=({a/@ta b/hoon} [%ktls [%sand a 0] [%ktls [%sand %$ 0] b]])
            ;~(pfix pat ;~(plug mota ;~(pfix tec wide)))
            ;~  pfix  tar
              (stag %kthp (stag [%base %noun] ;~(pfix tec wide)))
            ==
            (stag %kthp ;~(plug wyde ;~(pfix tec wide)))
            (stag %ktls ;~(pfix lus ;~(plug wide ;~(pfix tec wide))))
            (cook |=(a/hoon [[%rock %n ~] a]) wide)
          ==
        ==
      :-  '"'
        %+  cook
          |=  a/(list (list woof))
          [%knit |-(^-((list woof) ?~(a ~ (weld i.a $(a t.a)))))]
        (most dog soil)
      :-  ['a' 'z']
        rump
      :-  '|'
        ;~  pose
          (cook |=(a/wing [%cnts a ~]) rope)
          (stag %wtbr ;~(pfix bar (ifix [pel per] (most ace wide))))
          ;~(plug (stag %rock (stag %f (cold | bar))) wede)
          (stag %sand (stag %f (cold | bar)))
        ==
      :-  '~'
        ;~  pose
          rupl
        ::
          ;~  pfix  sig
            ;~  pose
              (stag %clsg (ifix [sel ser] (most ace wide)))
            ::
              %+  stag  %cnsg
              %+  ifix
                [pel per]
              ;~(glam rope wide (most ace wide))
            ::
              (cook (jock |) twid:so)
              (stag [%bust %null] wede)
              (easy [%bust %null])
            ==
          ==
        ==
      :-  '/'
        rood
      :-  '<'
        (ifix [gal gar] (stag %tell (most ace wide)))
      :-  '>'
        (ifix [gar gal] (stag %yell (most ace wide)))
    ==
  ++  soil
    ;~  pose
      ;~  less  (jest '"""')
        %+  ifix  [doq doq]
        %-  star
        ;~  pose
          ;~(pfix bas ;~(pose bas doq kel bix:ab))
          ;~(less doq bas kel prn)
          (stag ~ sump)
        ==
      ==
    ::
      %-  iny  %+  ifix
        [(jest '"""\0a') (jest '\0a"""')]
      %-  star
      ;~  pose
        ;~(pfix bas ;~(pose bas kel bix:ab))
        ;~(less bas kel prn)
        ;~(less (jest '\0a"""') (just `@`10))
        (stag ~ sump)
      ==
    ==
  ++  sump  (ifix [kel ker] (stag %cltr (most ace wide)))
  ++  norm                                              ::  rune regular form
    |=  {rut/? tol/?}
    =<  ?:  rut
          %-  stew
          ^.  stet  ^.  limo
          :~  :-  '$'
                ;~  pfix  buc
                  %-  stew
                  ^.  stet  ^.  limo
                  :~  ['@' (rune pat %bcpt exqb)]
                      ['_' (rune cab %bccb expa)]
                      [':' (rune col %bccl exqs)]
                      ['%' (rune cen %bccn exqs)]
                      ['^' (rune ket %bckt exqb)]
                      ['-' (rune hep %bchp exqb)]
                      ['=' (rune tis %bcts exqg)]
                      ['?' (rune wut %bcwt exqs)]
                      [';' (rune sem %bcsm expa)]
                  ==
                ==
            :-  '%'
              ;~  pfix  cen
                %-  stew
                ^.  stet  ^.  limo
                :~  ['^' (rune ket %cnkt exqy)]
                    ['+' (rune lus %cnls exqx)]
                    ['-' (rune hep %cnhp exqk)]
                    [':' (rune col %cnhp exqz)]
                ==
              ==
          ==
        %-  stew
        ^.  stet  ^.  limo
        :~  :-  '|'
              ;~  pfix  bar
                %-  stew
                ^.  stet  ^.  limo
                :~  ['_' (runo cab %brcb [~ ~] exqr)]
                    ['%' (runo cen %brcn [~ ~] expe)]
                    [':' (runo col %brcl [~ ~] expb)]
                    ['.' (runo dot %brdt [~ ~] expa)]
                    ['-' (runo hep %brhp [~ ~] expa)]
                    ['^' (runo ket %brkt [~ ~] expx)]
                    ['~' (runo sig %brsg [~ ~] exqc)]
                    ['*' (runo tar %brtr [~ ~] exqc)]
                    ['=' (runo tis %brts [~ ~] exqc)]
                    ['?' (runo wut %brwt [~ ~] expa)]
                ==
              ==
            :-  '$'
              ;~  pfix  buc
                %-  stew
                ^.  stet  ^.  limo
                :~  ['@' (rune pat %bcpt exqb)]
                    ['_' (rune cab %bccb expa)]
                    [':' (rune col %bccl exqs)]
                    ['%' (rune cen %bccn exqs)]
                    ['^' (rune ket %bckt exqb)]
                    ['-' (rune hep %bchp exqb)]
                    ['=' (rune tis %bcts exqg)]
                    ['?' (rune wut %bcwt exqs)]
                    [';' (rune sem %bcsm exqa)]
                ==
              ==
            :-  '%'
              ;~  pfix  cen
                %-  stew
                ^.  stet  ^.  limo
                :~  ['_' (rune cab %cncb exph)]
                    ['.' (rune dot %cndt expb)]
                    ['^' (rune ket %cnkt expd)]
                    ['+' (rune lus %cnls expc)]
                    ['-' (rune hep %cnhp expk)]
                    [':' (rune col %cnhp expi)]
                    ['~' (rune sig %cnsg expu)]
                    ['*' (rune tar %cntr expm)]
                    ['=' (rune tis %cnts exph)]
                ==
              ==
            :-  ':'
              ;~  pfix  col
                %-  stew
                ^.  stet  ^.  limo
                :~  ['_' (rune cab %clcb expb)]
                    ['^' (rune ket %clkt expd)]
                    ['+' (rune lus %clls expc)]
                    ['-' (rune hep %clhp expb)]
                    ['~' (rune sig %clsg exps)]
                    ['*' (rune tar %cltr exps)]
                ==
              ==
            :-  '.'
              ;~  pfix  dot
                %-  stew
                ^.  stet  ^.  limo
                :~  ['+' (rune lus %dtls expa)]
                    ['*' (rune tar %dttr expb)]
                    ['=' (rune tis %dtts expb)]
                    ['?' (rune wut %dtwt expa)]
                    ['^' (rune ket %dtkt exqn)]
                ==
              ==
            :-  '^'
              ;~  pfix  ket
                %-  stew
                ^.  stet  ^.  limo
                :~  ['|' (rune bar %ktbr expa)]
                    ['.' (rune dot %ktdt expb)]
                    ['-' (rune hep %kthp exqc)]
                    ['+' (rune lus %ktls expb)]
                    ['&' (rune pam %ktpm expa)]
                    ['~' (rune sig %ktsg expa)]
                    ['=' (rune tis %ktts expg)]
                    ['?' (rune wut %ktwt expa)]
                    ['%' (rune cen %ktcn expa)]
                ==
              ==
            :-  '~'
              ;~  pfix  sig
                %-  stew
                ^.  stet  ^.  limo
                :~  ['|' (rune bar %sgbr expb)]
                    ['$' (rune buc %sgbc expf)]
                    ['_' (rune cab %sgcb expb)]
                    ['%' (rune cen %sgcn hind)]
                    ['/' (rune fas %sgfs hine)]
                    ['<' (rune gal %sggl hinb)]
                    ['>' (rune gar %sggr hinb)]
                    ['+' (rune lus %sgls hinc)]
                    ['&' (rune pam %sgpm hinf)]
                    ['?' (rune wut %sgwt hing)]
                    ['=' (rune tis %sgts expb)]
                    ['!' (rune zap %sgzp expb)]
                ==
              ==
            :-  ';'
              ;~  pfix  sem
                %-  stew
                ^.  stet  ^.  limo
                :~  [':' (rune col %smcl expi)]
                    ['/' (rune fas %smfs expa)]
                    ['~' (rune sig %smsg expi)]
                    [';' (rune sem %smsm expb)]
                ==
              ==
            :-  '='
              ;~  pfix  tis
                %-  stew
                ^.  stet  ^.  limo
                :~  ['|' (rune bar %tsbr exqc)]
                    ['.' (rune dot %tsdt expq)]
                    ['?' (rune wut %tswt expw)]
                    ['^' (rune ket %tskt expt)]
                    [':' (rune col %tscl expp)]
                    ['/' (rune fas %tsfs expo)]
                    [';' (rune sem %tssm expo)]
                    ['<' (rune gal %tsgl expb)]
                    ['>' (rune gar %tsgr expb)]
                    ['-' (rune hep %tshp expb)]
                    ['*' (rune tar %tstr expl)]
                    [',' (rune com %tscm expb)]
                    ['+' (rune lus %tsls expb)]
                    ['~' (rune sig %tssg expi)]
                ==
              ==
            :-  '?'
              ;~  pfix  wut
                %-  stew
                ^.  stet  ^.  limo
                :~  ['|' (rune bar %wtbr exps)]
                    [':' (rune col %wtcl expc)]
                    ['.' (rune dot %wtdt expc)]
                    ['<' (rune gal %wtgl expb)]
                    ['>' (rune gar %wtgr expb)]
                    ['-' ;~(pfix hep (toad tkhp))]
                    ['^' ;~(pfix ket (toad tkkt))]
                    ['=' ;~(pfix tis (toad tkts))]
                    ['+' ;~(pfix lus (toad tkls))]
                    ['&' (rune pam %wtpm exps)]
                    ['@' ;~(pfix pat (toad tkpt))]
                    ['~' ;~(pfix sig (toad tksg))]
                    ['!' (rune zap %wtzp expa)]
                ==
              ==
            :-  '!'
              ;~  pfix  zap
                %-  stew
                ^.  stet  ^.  limo
                :~  [':' ;~(pfix col (toad expz))]
                    ['.' ;~(pfix dot (toad |.(loaf(bug |))))]
                    [',' (rune com %zpcm expb)]
                    [';' (rune sem %zpsm expb)]
                    ['>' (rune gar %zpgr expa)]
                    ['=' (rune tis %zpts expa)]
                    ['?' (rune wut %zpwt hinh)]
                ==
              ==
        ==
    |%
    ++  boog                                            ::  core arms
      %+  knee  [p=*term q=*(pair what foot)]  |.  ~+
      %+  cook
        |=  {a/whit b/term c/whit d/foot}
        =+  e=(glom a c)
        =?  boy.e  =(~ boy.e)
          |-(?+(-.p.d ~ $dbug $(p.d q.p.d), %halo p.p.d, %docs `p.p.d))
        =.  p.d
          |-
          ?+  p.d  ?~(boy.e p.d [%docs u.boy.e p.d])
            [%dbug ^]  p.d(q $(p.d q.p.d))
            [%docs ^]  p.d
            [%halo p=~ *]  p.d(p boy.e)
          ==
        [b boy.e d]
      ::
      ;~  plug
        apex:docs
        ;~  pose
          %+  cook
              |=({a/$ash b/term c/whit d/hoon} [b c a d])
          ;~  plug
            (cold %ash (jest '++'))
            ;~(pfix gap ;~(pose (cold %$ buc) sym))
            apse:docs
            ;~(pfix gap loaf)
          ==
        ::
          %+  cook
              |=({a/$elm b/term c/whit d/hoon} [b c a d])
          ;~  plug
            (cold %elm (jest '+-'))
            ;~(pfix gap ;~(pose (cold %$ buc) sym))
            apse:docs
            ;~(pfix gap loaf)
          ==
        ::
          %+  cook
            |=({a/$ash b/term c/whit d/root} [b c a d])
          ;~  plug
            (cold %ash (jest '+='))
            ;~(pfix gap sym)
            apse:docs
            ;~(pfix gap loan)
          ==
        ==
      ==
    ::
    ++  whap                                            ::  chapter
      %+  cook
        |=  a=(list (trel term what foot))
        %+  roll  a
        |=  [b=(trel term what foot) c=(map term [what foot])]
        ^+  c
        =?  p.r.b  (~(has by c) p.b)  [%eror "duplicate arm {<p.b>}"]
        (~(put by c) b)
      (most muck boog)
    ::
    ++  wasp                                            ::  $brcb aliases
      ;~  pose  
        %+  ifix
          [;~(plug lus tar muck) muck]
        (most muck ;~(gunk sym loaf))
      ::
        (easy ~)
      ==
    ::
    ++  wisp                                            ::  core tail
      ?.  tol  fail
      %+  cook
        |=  a=(list (pair whit (map term [what foot])))  ^-  (map @ tomb)
        =<  tos
        %+  roll  a
        |=  $:  [wit=whit wap=(map term (pair what foot))]
                [all=(map term *) num=@ tos=(map @ tomb)]
            ==
        =.  wap
          %-  ~(urn by wap)
          |=  b=(trel term what foot)  ^+  +.b
          ?.  (~(has by all) p.b)  +.b
          +.b(p.r [%eror "duplicate arm {<p.b>}"])
        ::
        =.  all  (~(uni by all) `(map term *)`wap)
        [all +(num) (~(put by tos) num [[lab boy]:wit wap])]
      ::
      ;~  pose
        dun
        ;~  sfix
          ;~  pose
            (most muck ;~(plug apex:docs ;~(pfix (jest '+|') gap whap)))
            ;~(plug (stag *whit whap) (easy ~))
          ==
          gap
          dun
        ==
      ==
    ::
    ++  toad                                            ::  untrap parser exp
      |*  har/_expa
      =+  dur=(ifix [pel per] $:har(tol |))
      ?:(tol ;~(pose ;~(pfix gap $:har(tol &)) dur) dur)
    ::
    ++  rune                                            ::  build rune
      |*  {dif/rule tuq/* har/_expa}
      ;~(pfix dif (stag tuq (toad har)))
    ::
    ++  runo                                            ::  rune plus
      |*  {dif/rule hil/* tuq/* har/_expa}
      ;~(pfix dif (stag hil (stag tuq (toad har))))
    ::
    ++  glop  ~+((glue mash))                           ::  separated by space
    ++  gunk  ~+((glue muck))                           ::  separated list
    ++  butt  |*  zor/rule                              ::  closing == if tall
              ?:(tol ;~(sfix zor ;~(plug gap duz)) zor)
    ++  ulva  |*  zor/rule                              ::  closing -- and tall
              ?.(tol fail ;~(sfix zor ;~(plug gap dun)))
    ++  hank  (most muck loaf)                          ::  gapped hoons
    ++  hunk  (most muck loan)                          ::  gapped roots
    ++  loaf  ?:(tol tall wide)                         ::  tall/wide hoon
    ++  loan  ?:(tol till wyde)                         ::  tall/wide root
    ++  mash  ?:(tol gap ;~(plug com ace))              ::  list separator
    ++  muck  ?:(tol gap ace)                           ::  general separator
    ++  teak  %+  knee  *tiki  |.  ~+                   ::  wing or hoon
              =+  ^=  gub
                  |=  {a/term b/$%({$& p/wing} {$| p/hoon})}
                  ^-  tiki
                  ?-(-.b $& [%& [~ a] p.b], $| [%| [~ a] p.b])
              =+  ^=  wyp
                  ;~  pose
                     %+  cook  gub
                     ;~  plug
                       sym
                       ;~(pfix tis ;~(pose (stag %& rope) (stag %| wide)))
                     ==
                  ::
                     (stag %& (stag ~ rope))
                     (stag %| (stag ~ wide))
                  ==
              ?.  tol  wyp
              ;~  pose
                wyp
              ::
                ;~  pfix
                  ;~(plug ket tis gap)
                  %+  cook  gub
                  ;~  plug
                    sym
                    ;~(pfix gap ;~(pose (stag %& rope) (stag %| tall)))
                  ==
                ==
              ::
                (stag %| (stag ~ tall))
              ==
    ++  rack  (most mash ;~(gunk loaf loaf))            ::  list [hoon hoon]
    ++  ruck  (most mash ;~(gunk loan loaf))            ::  list [root hoon]
    ++  rick  (most mash ;~(gunk rope loaf))            ::  list [wing hoon]
    ::
    ::    hoon contents
    ::
    ++  expa  |.(loaf)                                  ::  one hoon
    ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
    ++  expc  |.(;~(gunk loaf loaf loaf))               ::  three hoons
    ++  expd  |.(;~(gunk loaf loaf loaf loaf))          ::  four hoons
    ++  expe  |.(wisp)                                  ::  core tail
    ++  expf  |.(;~(gunk ;~(pfix cen sym) loaf))        ::  %term and hoon
    ++  expg  |.(;~(gunk sym loaf))                     ::  term and hoon
    ++  exph  |.((butt ;~(gunk rope rick)))             ::  wing, [tile hoon]s
    ++  expi  |.((butt ;~(gunk loaf hank)))             ::  one or more hoons
    ++  expk  |.(;~(gunk loaf ;~(plug loaf (easy ~))))  ::  list of two hoons
    ++  expl  |.(;~(gunk (stag ~ sym) loaf loaf))       ::  term, two hoons
    ++  expm  |.((butt ;~(gunk rope loaf rick)))        ::  several [tile hoon]s
    ++  expo  |.(;~(gunk wise loaf loaf))               ::  =;
    ++  expp  |.(;~(gunk (butt rick) loaf))             ::  [wing hoon]s, hoon
    ++  expq  |.(;~(gunk rope loaf loaf))               ::  wing and two hoons
    ++  expr  |.(;~(gunk loaf wisp))                    ::  hoon and core tail
    ++  exps  |.((butt hank))                           ::  closed gapped hoons
    ++  expt  |.(;~(gunk wise rope loaf loaf))          ::  =^
    ++  expu  |.(;~(gunk rope loaf (butt hank)))        ::  wing, hoon, hoons
    ++  expv  |.((butt rick))                           ::  just changes
    ++  expw  |.(;~(gunk rope loaf loaf loaf))          ::  wing and three hoons
    ++  expx  |.  ;~  gunk  loaf                        ::  hoon and core tail
                    %+  sear                            ::
                      |=  a/(map @ tomb)                ::
                      ^-  (unit (map @ tomb))           ::
                      =+  fir=(~(got by a) 0)           ::
                      ?:  (~(has by q.fir) %$)          ::  %$ in first chapter
                         ~                              ::
                      [~ u=a]                           ::
                    wisp                                ::
                  ==                                    ::
    ++  expz  |.(loaf(bug &))                           ::  hoon with tracing
    ::    root contents
    ::
    ++  exqa  |.(loan)                                  ::  one hoon
    ++  exqb  |.(;~(gunk loan loan))                    ::  two roots
    ++  exqc  |.(;~(gunk loan loaf))                    ::  root then hoon
    ++  exqs  |.((butt hunk))                           ::  closed gapped roots
    ++  exqg  |.(;~(gunk sym loan))                     ::  term and root
    ++  exqk  |.(;~(gunk loaf ;~(plug loan (easy ~))))  ::  hoon with one root
    ++  exqr  |.(;~(gunk loan ;~(plug wasp wisp)))      ::  root/aliases?/tail
    ++  exqn  |.(;~(gunk loan (stag %cltr (butt hank))))::  autoconsed hoons
    ++  exqw  |.(;~(gunk loaf loan))                    ::  hoon and root
    ++  exqx  |.(;~(gunk loaf loan loan))               ::  hoon, two roots
    ++  exqy  |.(;~(gunk loaf loan loan loan))          ::  hoon, three roots
    ++  exqz  |.(;~(gunk loaf (butt hunk)))             ::  hoon, n roots
    ::
    ::    tiki expansion for %wt runes
    ::
    ++  tkhp  |.  %+  cook  |=  {a/tiki b/(list (pair root hoon))}
                            (~(wthp ah a) b)
                  (butt ;~(gunk teak ruck))
    ++  tkkt  |.  %+  cook  |=  {a/tiki b/hoon c/hoon}
                            (~(wtkt ah a) b c)
                  ;~(gunk teak loaf loaf)
    ++  tkls  |.  %+  cook  |=  {a/tiki b/hoon c/(list (pair root hoon))}
                            (~(wtls ah a) b c)
                  (butt ;~(gunk teak loaf ruck))
    ++  tkpt  |.  %+  cook  |=  {a/tiki b/hoon c/hoon}
                            (~(wtpt ah a) b c)
                  ;~(gunk teak loaf loaf)
    ++  tksg  |.  %+  cook  |=  {a/tiki b/hoon c/hoon}
                            (~(wtsg ah a) b c)
                  ;~(gunk teak loaf loaf)
    ++  tkts  |.  %+  cook  |=  {a/root b/tiki}
                            (~(wtts ah b) a)
                  ;~(gunk loan teak)
    ::
    ::    hint syntax
    ::
    ++  hinb  |.(;~(gunk bont loaf))                    ::  hint and hoon
    ++  hinc  |.                                        ::  optional =en, hoon
              ;~(pose ;~(gunk bony loaf) (stag ~ loaf)) ::
    ++  hind  |.(;~(gunk bonk loaf bonz loaf))          ::  jet hoon "bon"s hoon
    ++  hine  |.(;~(gunk bonk loaf))                    ::  jet-hint and hoon
    ++  hinf  |.                                        ::  0-3 >s, two hoons
      ;~  pose
        ;~(gunk (cook lent (stun [1 3] gar)) loaf loaf)
        (stag 0 ;~(gunk loaf loaf))
      ==
    ++  hing  |.                                        ::  0-3 >s, three hoons
      ;~  pose
        ;~(gunk (cook lent (stun [1 3] gar)) loaf loaf loaf)
        (stag 0 ;~(gunk loaf loaf loaf))
      ==
    ++  bonk                                            ::  jet signature
      ;~  pfix  cen
        ;~  pose
          ;~(plug sym ;~(pfix col ;~(plug sym ;~(pfix dot ;~(pfix dot dem)))))
          ;~(plug sym ;~(pfix col ;~(plug sym ;~(pfix dot dem))))
          ;~(plug sym ;~(pfix dot dem))
          sym
        ==
      ==
    ++  hinh  |.                                        ::  1/2 numbers, hoon
        ;~  gunk
          ;~  pose
            dem
            (ifix [sel ser] ;~(plug dem ;~(pfix ace dem)))
          ==
          loaf
        ==
    ++  bont  ;~  (bend)                                ::  term, optional hoon
                ;~(pfix cen sym)
                ;~(pfix dot ;~(pose wide ;~(pfix muck loaf)))
              ==
    ++  bony  (cook |=(a/(list) (lent a)) (plus tis))   ::  base 1 =en count
    ++  bonz                                            ::  term-labelled hoons
      ;~  pose
        (cold ~ sig)
        %+  ifix
          ?:(tol [;~(plug duz gap) ;~(plug gap duz)] [pel per])
        (more mash ;~(gunk ;~(pfix cen sym) loaf))
      ==
    --
  ::
  ++  lang                                              ::  lung sample
    $:  ros/hoon
        $=  vil
        $%  {$tis p/hoon}
            {$col p/hoon}
            {$ket p/hoon}
            {$fas p/hoon}
            {$pel p/(list (pair wing hoon))}
        ==
    ==
  ::
  ++  lung
    ~+
    %-  bend
    |=  lang
    ^-  (unit hoon)
    ?-    -.vil
      $col  ?:(=([%base %bean] ros) ~ [~ %tsgl ros p.vil])
      $pel  (bind ~(reek ap ros) |=(hyp/wing [%cnts hyp p.vil]))
      $ket  [~ ros p.vil]
      $fas  =+  tog=~(hock ap ros)
            ?.(?=(@ tog) ~ [~ %bcts tog p.vil])
      $tis  =+  tog=~(hock ap ros)
            ?:(=([%0 ~] tog) ~ [~ %ktts tog p.vil])
    ==
  ::
  ++  long
    %+  knee  *hoon  |.  ~+
    ;~  lung
      scat
      ;~  pose
        ;~(plug (cold %tis tis) wide)
        ;~(plug (cold %col col) wide)
        ;~(plug (cold %ket ket) wide)
        ;~(plug (cold %fas fas) wide)
        ;~  plug
          (easy %pel)
          (ifix [pel per] lobo)
        ==
      ==
    ==
  ::
  ++  lobo  (most ;~(plug com ace) ;~(glam rope wide))
  ++  loon  (most ;~(plug com ace) ;~(glam wide wide))
  ++  lute                                              ::  tall [] noun
    ~+
    %+  stag  %cltr
    %+  ifix
      [;~(plug sel gap) ;~(plug gap ser)]
    (most gap tall)
  ::
  ++  rope                                              ::  wing form
    %+  knee  *wing
    |.  ~+
    %+  (slug |=({a/limb b/wing} [a b]))
      dot
    ;~  pose
      (cold [%| 0 ~] com)
      %+  cook
        |=({a/(list) b/term} ?~(a b [%| (lent a) `b]))
      ;~(plug (star ket) ;~(pose sym (cold %$ buc)))
    ::
      %+  cook
        |=(a/axis [%& a])
      ;~  pose
        ;~(pfix lus dim:ag)
        ;~(pfix pam (cook |=(a/@ ?:(=(0 a) 0 (mul 2 +($(a (dec a)))))) dim:ag))
        ;~(pfix bar (cook |=(a/@ ?:(=(0 a) 1 +((mul 2 $(a (dec a)))))) dim:ag))
        ven
        (cold 1 dot)
      ==
    ==
  ::
  ++  wise  ;~(plug sym (punt ;~(pfix ;~(pose fas tis) wyde)))
  ++  wrap
    |*  fel/rule
    %+  cook
      |=  {a/whit b/hoon c/whit}
      ^-  hoon
      ~(graf ~(gi ap b) (glom a c))
    ::
    ::  XX performance: this makes the parser about 50% slower.
    ::  because we double-parse most of the spaces in the file.
    ::  just so we can do a postfix doc-comment.
    ::  
    ::  the correct solution to this problem is to unify the
    ::  parsing of docs with the parsing of comments/spaces.
    ::  but at this point we're pretty much in parser rewrite.
    ::
    ::  it should go without saying that ++vast needs a rewrite.
    ::  it dates to 2011.
    ::
    ;~  plug
      apex:docs
      fel
      apse:docs
    ==  
  ++  tall                                              ::  full tall form
    %+  knee  *hoon
    |.(~+((wart (wrap ;~(pose (norm | &) long lute apex:(sail &))))))
  ++  till                                              ::  mold tall form
    %+  knee  *root
    |.(~+((wart (wrap ;~(pose (norm & &) scad)))))
  ++  wede                                              ::  wide bulb
    ;~(pfix ;~(pose lus fas) wide)
  ++  wide                                              ::  full wide form
    %+  knee  *hoon
    |.(~+((wart ;~(pose (norm | |) long apex:(sail |)))))
  ++  wyde                                              ::  mold wide form
    %+  knee  *root
    |.(~+((wart ;~(pose (norm & |) scad))))
  ++  wart
    |*  zor/rule
    %+  here
      |=  {a/pint b/hoon}
      ?:(bug [%dbug [wer a] b] b)
    zor
  --
::
++  vest
  ~/  %vest
  |=  tub/nail
  ^-  (like hoon)
  %.  tub
  %-  full
  (ifix [gay gay] tall:vast)
::
++  vice
  |=  txt/@ta
  ^-  hoon
  (rash txt wide:vast)
::
++  make                                                ::  compile cord to nock
  |=  txt/@
  q:(~(mint ut %noun) %noun (ream txt))
::
++  rain                                                ::  parse with % path
  |=  {bon/path txt/@}
  ^-  hoon
  =+  vaz=vast
  ~|  bon
  (scan (trip txt) (full (ifix [gay gay] tall:vaz(wer bon))))
::
++  ream                                                ::  parse cord to hoon
  |=  txt/@
  ^-  hoon
  (rash txt vest)
::
++  reck                                                ::  parse hoon file
  |=  bon/path
  (rain bon .^(@t %cx (weld bon `path`[%hoon ~])))
::
++  ride                                                ::  end-to-end compiler
  |=  {typ/type txt/@}
  ^-  (pair type nock)
  (~(mint ut typ) %noun (ream txt))
::
::::  5e: caching compiler
  ::
++  wa  !:                                              ::  cached compile
  |_  worm
  ++  nell  |=(ref/type (nest [%cell %noun %noun] ref)) ::  nest in cell
  ++  nest                                              ::  nest:ut
    |=  {sut/type ref/type}
    ^-  {? worm}
    ?:  (~(has in nes) [sut ref])  [& +>+<]
    ?.  (~(nest ut sut) | ref)
      ~&  %nest-failed
      =+  foo=(skol ref)
      =+  bar=(skol sut)
      ~&  %nets-need
      ~>  %slog.[0 bar]
      ~&  %nest-have
      ~>  %slog.[0 foo]
      [| +>+<.$]
    [& +>+<(nes (~(put in nes) [sut ref]))]
  ::
  ++  call                                              ::  call gate
    |=  {vax/vase nam/term som/(each vase ^)}
    ^-  {vase worm}
    =^  duf  +>+<.$  (open vax nam som)
    (slap duf [%limb %$])
  ::
  ++  open                                              ::  assemble door
    |=  {vax/vase nam/term som/(each vase ^)}
    ^-  {vase worm}
    =*  key  [%cncb [[%& 2] ~] [[[%& 6] ~] [%$ 3]] ~]
    =^  dor  +>+<.$  (slap vax [%limb nam])
    =^  mes  +>+<.$  (slot 6 dor)
    =^  hip  +>+<.$
      ?-  -.som
         $&  (nest p.mes p.p.som)
         $|  (nets p.mes -.p.som)
      ==
    ?>  hip
    [[p.dor q.dor(+6 +7.som)] +>+<.$] 
  ::
  ++  neat                                              ::  type compliance
    |=  {typ/type som/(each vase ^)}
    ^-  worm
    =^  hip  +>+<.$
      ?-  -.som
        $&  (nest typ p.p.som)
        $|  (nets typ -.p.som)
      ==
    ?>  hip
    +>+<.$
  ::
  ++  nets                                              ::  typeless nest
    |=  {sut/* ref/*}
    ^-  {? worm}
    ?:  (~(has in nes) [sut ref])  [& +>+<]
    =+  gat=|=({a/type b/type} (~(nest ut a) | b))
    ?.  (? .*(gat(+< [sut ref]) -.gat))
      ~&  %nets-failed
      =+  tag=`*`skol
      =+  foo=(tank .*(tag(+< ref) -.tag))
      =+  bar=(tank .*(tag(+< sut) -.tag))
      ~&  %nets-need
      ~>  %slog.[0 bar]
      ~&  %nets-have
      ~>  %slog.[0 foo]
      [| +>+<.$]
    [& +>+<.$(nes (~(put in nes) [sut ref]))]
  ::
  ++  play                                              ::  play:ut
    |=  {sut/type gen/hoon}
    ^-  {type worm}
    =+  old=(~(get by pay) [sut gen])
    ?^  old  [u.old +>+<.$]
    =+  new=(~(play ut sut) gen)
    [new +>+<.$(pay (~(put by pay) [sut gen] new))]
  ::
  ++  mint                                              ::  mint:ut to noun
    |=  {sut/type gen/hoon}
    ^-  {(pair type nock) worm}
    =+  old=(~(get by mit) [sut gen])
    ?^  old  [u.old +>+<.$]
    =+  new=(~(mint ut sut) %noun gen)
    [new +>+<.$(mit (~(put by mit) [sut gen] new))]
  ::
  ++  slap                                              ::  ++slap, cached
    |=  {vax/vase gen/hoon}
    ^-  {vase worm}
    =^  gun  +>+<  (mint p.vax gen)
    [[p.gun .*(q.vax q.gun)] +>+<.$]
  ::
  ++  slot                                              ::  ++slot, cached
    |=  {axe/@ vax/vase}
    ^-  {vase worm}
    =^  gun  +>+<  (mint p.vax [%$ axe])
    [[p.gun .*(q.vax [0 axe])] +>+<.$]
  ::
  ++  spec                                              ::  specialize vase
    |=  vax/vase
    ^-  {vase worm}
    =+  ^=  gen  ^-  hoon
      ?@  q.vax    [%wtts [%base [%atom %$]] [%& 1]~]
      ?@  -.q.vax  [%wtts [%leaf %tas -.q.vax] [%& 2]~]
      [%wtts [%base %cell] [%& 1]~]
    =^  typ  +>+<.$  (play p.vax [%wtgr gen [%$ 1]])
    [[typ q.vax] +>+<.$]
  ::
  ++  spot                                              ::  slot then spec
    |=  {axe/@ vax/vase}
    ^-  {vase worm}
    =^  xav  +>+<  (slot axe vax)
    (spec xav)
  ::
  ++  stop                                              ::  spec then slot
    |=  {axe/@ vax/vase}
    ^-  {vase worm}
    =^  xav  +>+<  (spec vax)
    (slot axe xav)
  --
::
::::  5f: molds and mold builders
  ::
++  arch  {fil/(unit @uvI) dir/(map @ta $~)}            ::  fundamental node
++  arvo  (wind {p/term q/mill} mill)                   ::  arvo card
++  beak  {p/ship q/desk r/case}                        ::  path prefix
++  beam  {{p/ship q/desk r/case} s/spur}               ::  global name
++  bone  @ud                                           ::  opaque duct
++  case                                                ::  version
         $%  {$da p/@da}                               ::  date
             {$tas p/@tas}                             ::  label
             {$ud p/@ud}                               ::  sequence
         ==                                            ::
++  desk  @tas                                          ::  ship desk case spur
++  cage  (cask vase)                                   ::  global metadata
++  cask  |*(a/mold (pair mark a))                      ::  global data
++  cuff                                                ::  permissions
          $:  p/(unit (set monk))                       ::  can be read by
              q/(set monk)                              ::  caused or created by
          ==                                            ::
++  curd  {p/@tas q/*}                                  ::  typeless card
++  dock  (pair @p term)                                ::  message target
++  duct  (list wire)                                   ::  causal history
++  hypo  |*(a/mold (pair type a))                      ::  type associated
++  hobo  |*  a/gate                                    ::  task wrapper
          $?  $%  {$soft p/*}                           ::
              ==                                        ::
              a                                         ::
          ==                                            ::
++  kirk  (unit (set monk))                             ::  audience
++  lens                                                ::  observation core
  $_  ^?                                                ::
  |%  ++  u  *(unit (unit $~))                          ::  existence
      ++  v  *(unit (unit cage))                        ::  full history
      ++  w  *(unit (unit (unit cage)))                 ::  latest diff
      ++  x  *(unit (unit cage))                        ::  data at path
      ++  y  *(unit (unit arch))                        ::  directory
      ++  z  *(unit (unit cage))                        ::  current subtree
  --                                                    ::
++  mane  $@(@tas {@tas @tas})                          ::  XML name+space
++  manx  {g/marx c/marl}                               ::  XML node
++  marc                                                ::  structured mark
  $@  mark                                              ::  plain mark
  $%  {$tabl p/(list (pair marc marc))}                 ::  map
  ==                                                    ::
++  mark  @tas                                          ::  content type
++  marl  (list manx)                                   ::  XML node list
++  mars  {t/{n/$$ a/{i/{n/$$ v/tape} t/$~}} c/$~}      ::  XML cdata
++  mart  (list {n/mane v/tape})                        ::  XML attributes
++  marx  {n/mane a/mart}                               ::  XML tag
++  mash  |=(* (mass +<))                               ::  producing mass
++  mass  (pair cord (each noun (list mash)))           ::  memory usage
++  mill  (each vase milt)                              ::  vase+metavase
++  milt  {p/* q/*}                                     ::  metavase
++  mite  (list @ta)                                    ::  mime type
++  monk  (each ship {p/@tas q/@ta})                    ::  general identity
++  muse  {p/@tas q/duct r/arvo}                        ::  sourced move
++  move  {p/duct q/arvo}                               ::  arvo move
++  ovum  {p/wire q/curd}                               ::  typeless ovum
++  pane  (list {p/@tas q/vase})                        ::  kernel modules
++  pass  @                                             ::  public key
++  pone  (list {p/@tas q/vise})                        ::  kernel modules old
++  ring  @                                             ::  private key
++  ship  @p                                            ::  network identity
++  shop  (each ship (list @ta))                        ::  urbit/dns identity
++  sink  (trel bone ship path)                         ::  subscription
++  sley  $-  {* (unit (set monk)) term beam}           ::  namespace function
          (unit (unit cage))                            ::
++  slyd  $-  {* (unit (set monk)) term beam}           ::  super advanced
          (unit (unit (cask)))                          ::
++  slyt  $-({* *} (unit (unit)))                       ::  old namespace
++  spur  path                                          ::  ship desk case spur
++  time  @da                                           ::  galactic time
++  vile                                                ::  reflexive constants
          $:  typ/type                                  ::  -:!>(*type)
              duc/type                                  ::  -:!>(*duct)
              pah/type                                  ::  -:!>(*path)
              mev/type                                  ::  -:!>([%meta *vase])
          ==                                            ::
++  wind                                                ::  new kernel action
          |*  {a/gate b/gate}                           ::  forward+reverse
          $%  {$pass p/path q/a}                        ::  advance
              {$slip p/a}                               ::  lateral
              {$give p/b}                               ::  retreat
          ==                                            ::
++  wire  path                                          ::  event pretext
::
::::  5g: profiling support (XX move)
  ::
++  doss
  $:  mon/moan                                          ::  sample count
      hit/(map term @ud)                                ::  hit points
      cut/(map path hump)                               ::  cut points
  ==
++  moan                                                ::  sample metric
  $:  fun/@ud                                           ::  samples in C
      noc/@ud                                           ::  samples in nock
      glu/@ud                                           ::  samples in glue
      mal/@ud                                           ::  samples in alloc
      far/@ud                                           ::  samples in frag
      coy/@ud                                           ::  samples in copy
      euq/@ud                                           ::  samples in equal
  ==                                                    ::
::
++  hump
  $:  mon/moan                                          ::  sample count
      out/(map path @ud)                                ::  calls out of
      inn/(map path @ud)                                ::  calls into
  ==
::
++  pi-heck
    |=  {nam/@tas day/doss}
    ^-  doss
    =+  lam=(~(get by hit.day) nam)
    day(hit (~(put by hit.day) nam ?~(lam 1 +(u.lam))))
::
++  pi-noon                                             ::  sample trace
  |=  {mot/term paz/(list path) day/doss}
  =|  lax/(unit path)
  |-  ^-  doss
  ?~  paz  day(mon (pi-mope mot mon.day))
  %=    $
      paz  t.paz
      lax  `i.paz
      cut.day
    %+  ~(put by cut.day)  i.paz
    ^-  hump
    =+  nax=`(unit path)`?~(t.paz ~ `i.t.paz)
    =+  hup=`hump`=+(hup=(~(get by cut.day) i.paz) ?^(hup u.hup [*moan ~ ~]))
    :+  (pi-mope mot mon.hup)
      ?~  lax  out.hup
      =+  hag=(~(get by out.hup) u.lax)
      (~(put by out.hup) u.lax ?~(hag 1 +(u.hag)))
    ?~  nax  inn.hup
    =+  hag=(~(get by inn.hup) u.nax)
    (~(put by inn.hup) u.nax ?~(hag 1 +(u.hag)))
  ==
++  pi-mope                                             ::  add sample
  |=  {mot/term mon/moan}
  ?+  mot  mon
    $fun  mon(fun +(fun.mon))
    $noc  mon(noc +(noc.mon))
    $glu  mon(glu +(glu.mon))
    $mal  mon(mal +(mal.mon))
    $far  mon(far +(far.mon))
    $coy  mon(coy +(coy.mon))
    $euq  mon(euq +(euq.mon))
  ==
++  pi-moth                                             ::  count sample
  |=  mon/moan  ^-  @ud
  :(add fun.mon noc.mon glu.mon mal.mon far.mon coy.mon euq.mon)
::
++  pi-mumm                                             ::  print sample
  |=  mon/moan  ^-  tape
  =+  tot=(pi-moth mon)
  ;:  welp
    ^-  tape
    ?:  =(0 noc.mon)  ~
    (welp (scow %ud (div (mul 100 noc.mon) tot)) "n ")
  ::
    ^-  tape
    ?:  =(0 fun.mon)  ~
    (welp (scow %ud (div (mul 100 fun.mon) tot)) "c ")
  ::
    ^-  tape
    ?:  =(0 glu.mon)  ~
    (welp (scow %ud (div (mul 100 glu.mon) tot)) "g ")
  ::
    ^-  tape
    ?:  =(0 mal.mon)  ~
    (welp (scow %ud (div (mul 100 mal.mon) tot)) "m ")
  ::
    ^-  tape
    ?:  =(0 far.mon)  ~
    (welp (scow %ud (div (mul 100 far.mon) tot)) "f ")
  ::
    ^-  tape
    ?:  =(0 coy.mon)  ~
    (welp (scow %ud (div (mul 100 coy.mon) tot)) "y ")
  ::
    ^-  tape
    ?:  =(0 euq.mon)  ~
    (welp (scow %ud (div (mul 100 euq.mon) tot)) "e ")
  ==
::
++  pi-tell                                             ::  produce dump
  |=  day/doss
  ^-  (list tape)
  ?:  =(day *doss)  ~
  =+  tot=(pi-moth mon.day)
  ;:  welp
    [(welp "events: " (pi-mumm mon.day)) ~]
  ::
    %+  turn
      %+  sort  ~(tap by hit.day)
      |=  {a/{* @} b/{* @}}
      (lth +.a +.b)
    |=  {nam/term num/@ud}
    :(welp (trip nam) ": " (scow %ud num))
    ["" ~]
  ::
    %-  zing
    ^-  (list (list tape))
    %+  turn
      %+  sort  ~(tap by cut.day)
      |=  {one/(pair path hump) two/(pair path hump)}
      (gth (pi-moth mon.q.one) (pi-moth mon.q.two))
    |=  {pax/path hup/hump}
    =+  ott=(pi-moth mon.hup)
    ;:  welp
      [(welp "label: " (spud pax)) ~]
      [(welp "price: " (scow %ud (div (mul 100 ott) tot))) ~]
      [(welp "shape: " (pi-mumm mon.hup)) ~]
    ::
      ?:  =(~ out.hup)  ~
      :-  "into:"
      %+  turn
        %+  sort  ~(tap by out.hup)
        |=({{* a/@ud} {* b/@ud}} (gth a b))
      |=  {pax/path num/@ud}
      ^-  tape
      :(welp "  " (spud pax) ": " (scow %ud num))
    ::
      ?:  =(~ inn.hup)  ~
      :-  "from:"
      %+  turn
        %+  sort  ~(tap by inn.hup)
        |=({{* a/@ud} {* b/@ud}} (gth a b))
      |=  {pax/path num/@ud}
      ^-  tape
      :(welp "  " (spud pax) ": " (scow %ud num))
    ::
      ["" ~]
      ~
    ==
  ==
--
