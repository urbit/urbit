::
::::    /sys/hoon                                       ::
  ::                                                    ::
=<  ride
=>  %141  =>
::                                                      ::
::::    0: version stub                                 ::
  ::                                                    ::
~%  %k.141  ~  ~                                        ::
|%
++  hoon-version  141
--  =>
~%  %one  +  ~
::  #  %base
::
::    basic mathematical operations
|%
::  #  %math
::    unsigned arithmetic
+|  %math
++  add
  ~/  %add
  ::  unsigned addition
  ::
  ::  a: augend
  ::  b: addend
  |=  [a=@ b=@]
  ::  sum
  ^-  @
  ?:  =(0 a)  b
  $(a (dec a), b +(b))
::
++  dec
  ~/  %dec
  ::  unsigned decrement by one.
  |=  a=@
  ~_  leaf+"decrement-underflow"
  ?<  =(0 a)
  =+  b=0
  ::  decremented integer
  |-  ^-  @
  ?:  =(a +(b))  b
  $(b +(b))
::
++  div
  ~/  %div
  ::  unsigned divide
  ::
  ::  a: dividend
  ::  b: divisor
  |:  [a=`@`1 b=`@`1]
  ::  quotient
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
  ::  unsigned divide with remainder
  ::
  ::  a: dividend
  ::  b: divisor
  |=  [a=@ b=@]
  ::  p: quotient
  ::  q: remainder
  ^-  [p=@ q=@]
  [(div a b) (mod a b)]
::
++  gte
  ~/  %gte
  ::    unsigned greater than or equals
  ::
  ::  returns whether {a >= b}.
  ::
  ::  a: left hand operand (todo: name)
  ::  b: right hand operand
  |=  [a=@ b=@]
  ::  greater than or equal to?
  ^-  ?
  !(lth a b)
::
++  gth
  ~/  %gth
  ::    unsigned greater than
  ::
  ::  returns whether {a > b}
  ::
  ::  a: left hand operand (todo: name)
  ::  b: right hand operand
  |=  [a=@ b=@]
  ::  greater than?
  ^-  ?
  !(lte a b)
::
++  lte
  ~/  %lte
  ::    unsigned less than or equals
  ::
  ::  returns whether {a >= b}.
  ::
  ::  a: left hand operand (todo: name)
  ::  b: right hand operand
  |=  [a=@ b=@]
  ::  less than or equal to?
  |(=(a b) (lth a b))
::
++  lth
  ~/  %lth
  ::    unsigned less than
  ::
  ::  a: left hand operand (todo: name)
  ::  b: right hand operand
  |=  [a=@ b=@]
  ::  less than?
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
  ::  unsigned maximum
  |=  [a=@ b=@]
  ::  the maximum
  ^-  @
  ?:  (gth a b)  a
  b
::
++  min
  ~/  %min
  ::  unsigned minimum
  |=  [a=@ b=@]
  ::  the minimum
  ^-  @
  ?:  (lth a b)  a
  b
::
++  mod
  ~/  %mod
  ::  unsigned modulus
  ::
  ::  a: dividend
  ::  b: divisor
  |:  [a=`@`1 b=`@`1]
  ::  the remainder
  ^-  @
  ?<  =(0 b)
  (sub a (mul b (div a b)))
::
++  mul
  ~/  %mul
  ::  unsigned multiplication
  ::
  ::  a: multiplicand
  ::  b: multiplier
  |:  [a=`@`1 b=`@`1]
  ::  product
  ^-  @
  =+  c=0
  |-
  ?:  =(0 a)  c
  $(a (dec a), c (add b c))
::
++  sub
  ~/  %sub
  ::  unsigned subtraction
  ::
  ::  a: minuend
  ::  b: subtrahend
  |=  [a=@ b=@]
  ~_  leaf+"subtract-underflow"
  ::  difference
  ^-  @
  ?:  =(0 b)  a
  $(a (dec a), b (dec b))
::
::  #  %tree
::
::    tree addressing
+|  %tree
++  cap
  ~/  %cap
  ::    tree head
  ::
  ::  tests whether an `a` is in the head or tail of a noun. produces %2 if it
  ::  is within the head, or %3 if it is within the tail.
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
  ::    axis within head/tail
  ::
  ::  computes the axis of `a` within either the head or tail of a noun
  ::  (depends whether `a` lies within the the head or tail).
  |=  a=@
  ^-  @
  ?-  a
    ?(%2 %3)  1
    ?(%0 %1)  !!
    *         (add (mod a 2) (mul $(a (div a 2)) 2))
  ==
::
++  peg
  ~/  %peg
  ::    axis within axis
  ::
  ::  computes the axis of {b} within axis {a}.
  |=  [a=@ b=@]
  ?<  =(0 a)
  ::  a composed axis
  ^-  @
  ?-  b
    $1  a
    $2  (mul a 2)
    $3  +((mul a 2))
    *   (add (mod b 2) (mul $(b (div b 2)) 2))
  ==
::                                                      ::
::::  2n: functional hacks                              ::
  ::                                                    ::
  ::
++  aftr  |*(a/$-(* *) |*(b/$-(* *) (pair b a)))        ::  pair after
++  cork  |*({a/$-(* *) b/$-(* *)} (corl b a))          ::  compose forward
++  corl                                                ::  compose backwards
  |*  {a/$-(* *) b/$-(* *)}
  =<  +:|.((a (b)))      ::  type check
  =+  c=+<.b
  |@  ++  $  (a (b c))
  --
::
++  cury                                                ::  curry left
  |*  {a/$-(^ *) b/*}
  =+  c=+<+.a
  |@  ++  $  (a b c)
  --
::
++  curr                                                ::  curry right
  |*  {a/$-(^ *) c/*}
  =+  b=+<+.a
  |@  ++  $  (a b c)
  --
::
++  fore  |*(a/$-(* *) |*(b/$-(* *) (pair a b)))        ::  pair before
::
++  head  |*(^ ,:+<-)                                   ::  get head
++  same  |*(* +<)                                      ::  identity
::
++  tail  |*(^ ,:+<+)                                   ::  get tail
++  test  |=(^ =(+<- +<+))                              ::  equality
::
::  #  %containers
::
::    the most basic of data types
+|  %containers
++  bloq
  ::    blocksize
  ::
  ::  a blocksize is the power of 2 size of an atom. ie, 3 is a byte as 2^3 is
  ::  8 bits.
  @
::
++  each
  |$  [this that]
  ::    either {a} or {b}, defaulting to {a}.
  ::
  ::  mold generator: produces a discriminated fork between two types,
  ::  defaulting to {a}.
  ::
  $%  [%| p=that]
      [%& p=this]
  ==
::
++  gate
  ::    function
  ::
  ::  a core with one arm, `$`--the empty name--which transforms a sample noun
  ::  into a product noun. If used dryly as a type, the subject must have a
  ::  sample type of `*`.
  $-(* *)
::
++  list
  |$  [item]
  ::    null-terminated list
  ::
  ::  mold generator: produces a mold of a null-terminated list of the
  ::  homogeneous type {a}.
  ::
  $@(~ [i=item t=(list item)])
::
++  lone
  |$  [item]
  ::    single item tuple
  ::
  ::  mold generator: puts the face of `p` on the passed in mold.
  ::
  p=item
::
++  lest
  |$  [item]
  ::    null-terminated non-empty list
  ::
  ::  mold generator: produces a mold of a null-terminated list of the
  ::  homogeneous type {a} with at least one element.
  [i/item t/(list item)]
::
++  mold
  ::    normalizing gate
  ::
  ::  a gate that accepts any noun, and validates its shape, producing the
  ::  input if it fits or a default value if it doesn't.
  ::
  ::  examples: * @ud ,[p=time q=?(%a %b)]
  $~(* $-(* *))
::
++  pair
  |$  [head tail]
  ::    dual tuple
  ::
  ::  mold generator: produces a tuple of the two types passed in.
  ::
  ::  a: first type, labeled {p}
  ::  b: second type, labeled {q}
  ::
  [p=head q=tail]
::
++  pole
  |$  [item]
  ::    faceless list
  ::
  ::  like ++list, but without the faces {i} and {t}.
  ::
  $@(~ [item (pole item)])
::
++  qual
  |$  [first second third fourth]
  ::    quadruple tuple
  ::
  ::  mold generator: produces a tuple of the four types passed in.
  ::
  [p=first q=second r=third s=fourth]
::
++  quip
  |$  [item state]
  ::    pair of list of first and second
  ::
  ::  a common pattern in hoon code is to return a ++list of changes, along with
  ::  a new state.
  ::
  ::  a: type of list item
  ::  b: type of returned state
  ::
  [(list item) state]
::
++  trap
  |$  [product]
  ::    a core with one arm `$`
  ::
  _|?($:product)
::
++  tree
  |$  [node]
  ::    tree mold generator
  ::
  ::  a `++tree` can be empty, or contain a node of a type and
  ::  left/right sub `++tree` of the same type. pretty-printed with `{}`.
  ::
  $@(~ [n=node l=(tree node) r=(tree node)])
::
++  trel
  |$  [first second third]
  ::    triple tuple
  ::
  ::  mold generator: produces a tuple of the three types passed in.
  ::
  [p=first q=second r=third]
::
++  unit
  |$  [item]
  ::    maybe
  ::
  ::  mold generator: either `~` or `[~ u=a]` where `a` is the
  ::  type that was passed in.
  ::
  $@(~ [~ u=item])
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
  |*  {a/(unit) b/(unit) c/_=>(~ |=(^ +<-))}
  ?~  a  b
  ?~  b  a
  [~ u=(c u.a u.b)]
::
++  clef                                                ::  compose
  |*  {a/(unit) b/(unit) c/_=>(~ |=(^ `+<-))}
  ?~  a  ~
  ?~  b  ~
  (c u.a u.b)
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
  |*  [ord=$-(^ ?) a=(unit) b=(unit)]
  ^-  %-  unit
      $?  _?>(?=(^ a) u.a)
          _?>(?=(^ b) u.b)
      ==
  ?~  a  b
  ?~  b  a
  ?:((ord u.a u.b) a b)
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
  ?.(=(u.a u.b) ~>(%mean.'mate' !!) a)
::
++  need                                                ::  demand
  ~/  %need
  |*  a/(unit)
  ?~  a  ~>(%mean.'need' !!)
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
::  +snoc: append an element to the end of a list
::
++  snoc
  |*  [a/(list) b/*]
  (weld a ^+(a [b]~))
::
++  fand                                                ::  all indices
  ~/  %fand
  |=  {nedl/(list) hstk/(list)}
  =|  i/@ud
  =|  fnd/(list @ud)
  |-  ^+  fnd
  =+  [n=nedl h=hstk]
  |-
  ?:  |(?=(~ n) ?=(~ h))
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
  ?:  |(?=(~ n) ?=(~ h))
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
  ?>  (lte a b)
  |-  ^-  (list @)
  ?:(=(a +(b)) ~ [a $(a +(a))])
::
++  homo                                                ::  homogenize
  |*  a/(list)
  ^+  =<  $
    |@  ++  $  ?:(*? ~ [i=(snag 0 a) t=$])
    --
  a
::  +join: construct a new list, placing .sep between every pair in .lit
::
++  join
  |*  [sep=* lit=(list)]
  =.  sep  `_?>(?=(^ lit) i.lit)`sep
  ?~  lit  ~
  =|  out=(list _?>(?=(^ lit) i.lit))
  |-  ^+  out
  ?~  t.lit
    (flop [i.lit out])
  $(out [sep i.lit out], lit t.lit)
::
::  +bake: convert wet gate to dry gate by specifying argument mold
::
++  bake
  |*  [f=gate a=mold]
  |=  arg=a
  (f arg)
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
    |@  ++  $  ?~(a ~ ?:(*? [i=-.a t=$] $(a +.a)))
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
  |*  {a/(list) b/_=>(~ |=({* *} +<+))}
  |-  ^+  ,.+<+.b
  ?~  a
    +<+.b
  (b i.a $(a t.a))
::
++  roll                                                ::  left fold
  ~/  %roll
  |*  {a/(list) b/_=>(~ |=({* *} +<+))}
  |-  ^+  ,.+<+.b
  ?~  a
    +<+.b
  $(a t.a, b b(+<+ (b i.a +<+.b)))
::
++  scag                                                ::  prefix
  ~/  %scag
  |*  {a/@ b/(list)}
  |-  ^+  b
  ?:  |(?=(~ b) =(0 a))  ~
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
++  sort  !.                                            ::  quicksort
  ~/  %sort
  |*  {a/(list) b/$-({* *} ?)}
  =>  .(a ^.(homo a))
  |-  ^+  a
  ?~  a  ~
  =+  s=(skid t.a |:(c=i.a (b c i.a)))
  %+  weld
    $(a p.s)
  ^+  t.a
  [i.a $(a q.s)]
::
++  spin                                                ::  stateful turn
  ::
  ::  a: list
  ::  b: state
  ::  c: gate from list-item and state to product and new state
  ~/  %spin
  |*  [a=(list) b=* c=_|=(^ [** +<+])]
  =>  .(c `$-([_?>(?=(^ a) i.a) _b] [_-:(c) _b])`c)
  =/  acc=(list _-:(c))  ~
  ::  transformed list and updated state
  |-  ^-  (pair _acc _b)
  ?~  a
    [(flop acc) b]
  =^  res  b  (c i.a b)
  $(acc [res acc], a t.a)
::
++  spun                                                ::  internal spin
  ::
  ::  a: list
  ::  b: gate from list-item and state to product and new state
  ~/  %spun
  |*  [a=(list) b=_|=(^ [** +<+])]
  ::  transformed list
  p:(spin a +<+.b b)
::
++  swag                                                ::  slice
  |*  {{a/@ b/@} c/(list)}
  (scag +<-> (slag +<-< c))
::  +turn: transform each value of list :a using the function :b
::
++  turn
  ~/  %turn
  |*  [a=(list) b=gate]
  =>  .(a (homo a))
  ^-  (list _?>(?=(^ a) (b i.a)))
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
++  snap                                               ::  replace item
  ~/  %snap
  |*  [a=(list) b=@ c=*]
  ^+  a
  (weld (scag b a) [c (slag +(b) a)])
::
++  into                                               ::  insert item
  ~/  %into
  |*  [a=(list) b=@ c=*]
  ^+  a
  (weld (scag b a) [c (slag b a)])
::
++  welp                                                ::  faceless weld
  =|  {* *}
  |@
  ++  $
    ?~  +<-
      +<-(. +<+)
    +<-(+ $(+<- +<->))
  --
::
++  zing                                                ::  promote
  =|  *
  |@
  ++  $
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
++  repn
  ~/  %repn
  |=  [bits=@ud x=(list @)]
  =|  c=@ud
  |-  ^-  @
  ?~  x  0
  (add (lsh 0 (mul bits c) (end 0 bits i.x)) $(c +(c), x t.x))
::
++  rev
  ::  reverses block order, accounting for leading zeroes
  ::
  ::  boz: block size
  ::  len: size of dat, in boz
  ::  dat: data to flip
  ~/  %rev
  |=  [boz=bloq len=@ud dat=@]
  ^-  @
  =.  dat  (end boz len dat)
  %^  lsh  boz
    (sub len (met boz dat))
  (swp boz dat)
::
::  Like `rip` but produces n-bit blocks instead of 2^n bit blocks.
::
++  ripn
  ~/  %ripn
  |=  {bits=@ud x=@}
  ^-  (list @)
  ?:  =(0 x)  ~
  [(end 0 bits x) $(x (rsh 0 bits x))]
::
++  rip                                                 ::  disassemble
  ~/  %rip
  |=  {=bloq x=@}
  ^-  (list @)
  ?:  =(0 x)  ~
  [(end bloq 1 x) $(x (rsh bloq 1 x))]
::
++  rsh                                                 ::  right-shift
  ~/  %rsh
  |=  {a/bloq b/@u c/@}
  (div c (bex (mul (bex a) b)))
::
++  swp                                                 ::  naive rev bloq order
  ~/  %swp
  |=  {a/bloq b/@}
  (rep a (flop (rip a b)))
::
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
++  mug                                                 ::  mug with murmur3
  ~/  %mug
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
::                                                      ::
::::  2f: noun ordering                                 ::
  ::                                                    ::
  ::    aor, dor, gor, mor                              ::
  ::
::  +aor: alphabetical order
::
::    Orders atoms before cells, and atoms in ascending LSB order.
::
++  aor
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
::  +dor: depth order
::
::    Orders in ascending tree depth.
::
++  dor
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
::  +gor: mug order
::
::    Orders in ascending +mug hash order, collisions fall back to +dor.
::
++  gor
  ~/  %gor
  |=  {a/* b/*}
  ^-  ?
  =+  [c=(mug a) d=(mug b)]
  ?:  =(c d)
    (dor a b)
  (lth c d)
::  +mor: (more) mug order
::
::    Orders in ascending double +mug hash order, collisions fall back to +dor.
::
++  mor
  ~/  %mor
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
  =|  a/(tree)  :: (set)
  |@
  ++  all                                               ::  logical AND
    ~/  %all
    |*  b/$-(* ?)
    |-  ^-  ?
    ?~  a
      &
    ?&((b n.a) $(a l.a) $(a r.a))
  ::
  ++  any                                               ::  logical OR
    ~/  %any
    |*  b/$-(* ?)
    |-  ^-  ?
    ?~  a
      |
    ?|((b n.a) $(a l.a) $(a r.a))
  ::
  ++  apt                                               ::  check correctness
    =|  {l/(unit) r/(unit)}
    |-  ^-  ?
    ?~  a   &
    ?&  ?~(l & (gor n.a u.l))
        ?~(r & (gor u.r n.a))
        ?~(l.a & ?&((mor n.a n.l.a) $(a l.a, l `n.a)))
        ?~(r.a & ?&((mor n.a n.r.a) $(a r.a, r `n.a)))
    ==
  ::
  ++  bif                                               ::  splits a by b
    ~/  %bif
    |*  b/*
    ^+  [l=a r=a]
    =<  +
    |-  ^+  a
    ?~  a
      [b ~ ~]
    ?:  =(b n.a)
      a
    ?:  (gor b n.a)
      =+  c=$(a l.a)
      ?>  ?=(^ c)
      c(r a(l r.c))
    =+  c=$(a r.a)
    ?>  ?=(^ c)
    c(l a(r l.c))
  ::
  ++  del                                               ::  b without any a
    ~/  %del
    |*  b/*
    |-  ^+  a
    ?~  a
      ~
    ?.  =(b n.a)
      ?:  (gor b n.a)
        a(l $(a l.a))
      a(r $(a r.a))
    |-  ^-  {$?(~ _a)}
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (mor n.l.a n.r.a)
      l.a(r $(l.a r.l.a))
    r.a(l $(r.a l.r.a))
  ::
  ++  dif                                               ::  difference
    ~/  %dif
    =+  b=a
    |@
    ++  $
      |-  ^+  a
      ?~  b
        a
      =+  c=(bif n.b)
      ?>  ?=(^ c)
      =+  d=$(a l.c, b l.b)
      =+  e=$(a r.c, b r.b)
      |-  ^-  {$?(~ _a)}
      ?~  d  e
      ?~  e  d
      ?:  (mor n.d n.e)
        d(r $(d r.d))
      e(l $(e l.e))
    --
  ::
  ++  dig                                               ::  axis of a in b
    |=  b/*
    =+  c=1
    |-  ^-  (unit @)
    ?~  a  ~
    ?:  =(b n.a)  [~ u=(peg c 2)]
    ?:  (gor b n.a)
      $(a l.a, c (peg c 6))
    $(a r.a, c (peg c 7))
  ::
  ++  gas                                               ::  concatenate
    ~/  %gas
    |=  b/(list _?>(?=(^ a) n.a))
    |-  ^+  a
    ?~  b
      a
    $(b t.b, a (put i.b))
  ::  +has: does :b exist in :a?
  ::
  ++  has
    ~/  %has
    |*  b=*
    ^-  ?
    ::  wrap extracted item type in a unit because bunting fails
    ::
    ::    If we used the real item type of _?^(a n.a !!) as the sample type,
    ::    then hoon would bunt it to create the default sample for the gate.
    ::
    ::    However, bunting that expression fails if :a is ~. If we wrap it
    ::    in a unit, the bunted unit doesn't include the bunted item type.
    ::
    ::    This way we can ensure type safety of :b without needing to perform
    ::    this failing bunt. It's a hack.
    ::
    %.  [~ b]
    |=  b=(unit _?>(?=(^ a) n.a))
    =>  .(b ?>(?=(^ b) u.b))
    |-  ^-  ?
    ?~  a
      |
    ?:  =(b n.a)
      &
    ?:  (gor b n.a)
      $(a l.a)
    $(a r.a)
  ::
  ++  int                                               ::  intersection
    ~/  %int
    =+  b=a
    |@
    ++  $
      |-  ^+  a
      ?~  b
        ~
      ?~  a
        ~
      ?.  (mor n.a n.b)
        $(a b, b a)
      ?:  =(n.b n.a)
        a(l $(a l.a, b l.b), r $(a r.a, b r.b))
      ?:  (gor n.b n.a)
        %-  uni(a $(a l.a, r.b ~))  $(b r.b)
      %-  uni(a $(a r.a, l.b ~))  $(b l.b)
    --
  ::
  ++  put                                               ::  puts b in a, sorted
    ~/  %put
    |*  b/*
    |-  ^+  a
    ?~  a
      [b ~ ~]
    ?:  =(b n.a)
      a
    ?:  (gor b n.a)
      =+  c=$(a l.a)
      ?>  ?=(^ c)
      ?:  (mor n.a n.c)
        a(l c)
      c(r a(l r.c))
    =+  c=$(a r.a)
    ?>  ?=(^ c)
    ?:  (mor n.a n.c)
      a(r c)
    c(l a(r l.c))
  ::
  ++  rep                                               ::  reduce to product
    |*  b/_=>(~ |=({* *} +<+))
    |-
    ?~  a  +<+.b
    $(a r.a, +<+.b $(a l.a, +<+.b (b n.a +<+.b)))
  ::
  ++  run                                               ::  apply gate to values
    ~/  %run
    |*  b/gate
    =+  c=`(set _?>(?=(^ a) (b n.a)))`~
    |-  ?~  a  c
    =.  c  (~(put in c) (b n.a))
    =.  c  $(a l.a, c c)
    $(a r.a, c c)
  ::
  ++  tap                                               ::  convert to list
    =<  $
    ~/  %tap
    =+  b=`(list _?>(?=(^ a) n.a))`~
    |.  ^+  b
    ?~  a
      b
    $(a r.a, b [n.a $(a l.a)])
  ::
  ++  uni                                               ::  union
    ~/  %uni
    =+  b=a
    |@
    ++  $
      ?:  =(a b)  a
      |-  ^+  a
      ?~  b
        a
      ?~  a
        b
      ?:  =(n.b n.a)
        b(l $(a l.a, b l.b), r $(a r.a, b r.b))
      ?:  (mor n.a n.b)
        ?:  (gor n.b n.a)
          $(l.a $(a l.a, r.b ~), b r.b)
        $(r.a $(a r.a, l.b ~), b l.b)
      ?:  (gor n.a n.b)
        $(l.b $(b l.b, r.a ~), a r.a)
      $(r.b $(b r.b, l.a ~), a l.a)
    --
  ::
  ++  wyt                                               ::  size of set
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
  |@
  ++  all                                               ::  logical AND
    ~/  %all
    |*  b/$-(* ?)
    |-  ^-  ?
    ?~  a
      &
    ?&((b q.n.a) $(a l.a) $(a r.a))
  ::
  ++  any                                               ::  logical OR
    ~/  %any
    |*  b/$-(* ?)
    |-  ^-  ?
    ?~  a
      |
    ?|((b q.n.a) $(a l.a) $(a r.a))
  ::
  ++  bif                                               ::  splits a by b
    ~/  %bif
    |*  {b/* c/*}
    ^+  [l=a r=a]
    =<  +
    |-  ^+  a
    ?~  a
      [[b c] ~ ~]
    ?:  =(b p.n.a)
      ?:  =(c q.n.a)
        a
      a(n [b c])
    ?:  (gor b p.n.a)
      =+  d=$(a l.a)
      ?>  ?=(^ d)
      d(r a(l r.d))
    =+  d=$(a r.a)
    ?>  ?=(^ d)
    d(l a(r l.d))
  ::
  ++  del                                               ::  delete at key b
    ~/  %del
    |*  b/*
    |-  ^+  a
    ?~  a
      ~
    ?.  =(b p.n.a)
      ?:  (gor b p.n.a)
        a(l $(a l.a))
      a(r $(a r.a))
    |-  ^-  {$?(~ _a)}
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (mor p.n.l.a p.n.r.a)
      l.a(r $(l.a r.l.a))
    r.a(l $(r.a l.r.a))
  ::
  ++  dif                                               ::  difference
    ~/  %dif
    =+  b=a
    |@
    ++  $
      |-  ^+  a
      ?~  b
        a
      =+  c=(bif p.n.b q.n.b)
      ?>  ?=(^ c)
      =+  d=$(a l.c, b l.b)
      =+  e=$(a r.c, b r.b)
      |-  ^-  {$?(~ _a)}
      ?~  d  e
      ?~  e  d
      ?:  (mor p.n.d p.n.e)
        d(r $(d r.d))
      e(l $(e l.e))
    --
  ::
  ++  dig                                               ::  axis of b key
    |=  b/*
    =+  c=1
    |-  ^-  (unit @)
    ?~  a  ~
    ?:  =(b p.n.a)  [~ u=(peg c 2)]
    ?:  (gor b p.n.a)
      $(a l.a, c (peg c 6))
    $(a r.a, c (peg c 7))
  ::
  ++  apt                                               ::  check correctness
    =|  {l/(unit) r/(unit)}
    |-  ^-  ?
    ?~  a   &
    ?&  ?~(l & (gor p.n.a u.l))
        ?~(r & (gor u.r p.n.a))
        ?~(l.a & ?&((mor p.n.a p.n.l.a) $(a l.a, l `p.n.a)))
        ?~(r.a & ?&((mor p.n.a p.n.r.a) $(a r.a, r `p.n.a)))
    ==
  ::
  ++  gas                                               ::  concatenate
    ~/  %gas
    |*  b/(list {p/* q/*})
    =>  .(b `(list _?>(?=(^ a) n.a))`b)
    |-  ^+  a
    ?~  b
      a
    $(b t.b, a (put p.i.b q.i.b))
  ::
  ++  get                                               ::  grab value by key
    ~/  %get
    |*  b=*
    =>  .(b `_?>(?=(^ a) p.n.a)`b)
    |-  ^-  (unit _?>(?=(^ a) q.n.a))
    ?~  a
      ~
    ?:  =(b p.n.a)
      (some q.n.a)
    ?:  (gor b p.n.a)
      $(a l.a)
    $(a r.a)
  ::
  ++  got                                               ::  need value by key
    |*  b/*
    (need (get b))
  ::
  ++  gut                                               ::  fall value by key
    |*  [b=* c=*]
    (fall (get b) c)
  ::
  ++  has                                               ::  key existence check
    ~/  %has
    |*  b/*
    !=(~ (get b))
  ::
  ++  int                                               ::  intersection
    ~/  %int
    =+  b=a
    |@
    ++  $
      |-  ^+  a
      ?~  b
        ~
      ?~  a
        ~
      ?:  (mor p.n.a p.n.b)
        ?:  =(p.n.b p.n.a)
          b(l $(a l.a, b l.b), r $(a r.a, b r.b))
        ?:  (gor p.n.b p.n.a)
          %-  uni(a $(a l.a, r.b ~))  $(b r.b)
        %-  uni(a $(a r.a, l.b ~))  $(b l.b)
      ?:  =(p.n.a p.n.b)
        b(l $(b l.b, a l.a), r $(b r.b, a r.a))
      ?:  (gor p.n.a p.n.b)
        %-  uni(a $(b l.b, r.a ~))  $(a r.a)
      %-  uni(a $(b r.b, l.a ~))  $(a l.a)
    --
  ::
  ++  jab
    ~/  %jab
    |*  [key=_?>(?=(^ a) p.n.a) fun=$-(_?>(?=(^ a) q.n.a) _?>(?=(^ a) q.n.a))]
    ^+  a
    ::
    ?~  a  !!
    ::
    ?:  =(key p.n.a)
      a(q.n (fun q.n.a))
    ::
    ?:  (gor key p.n.a)
      a(l $(a l.a))
    ::
    a(r $(a r.a))
  ::
  ++  mar                                               ::  add with validation
    |*  {b/* c/(unit *)}
    ?~  c
      (del b)
    (put b u.c)
  ::
  ++  put                                               ::  adds key-value pair
    ~/  %put
    |*  {b/* c/*}
    |-  ^+  a
    ?~  a
      [[b c] ~ ~]
    ?:  =(b p.n.a)
      ?:  =(c q.n.a)
        a
      a(n [b c])
    ?:  (gor b p.n.a)
      =+  d=$(a l.a)
      ?>  ?=(^ d)
      ?:  (mor p.n.a p.n.d)
        a(l d)
      d(r a(l r.d))
    =+  d=$(a r.a)
    ?>  ?=(^ d)
    ?:  (mor p.n.a p.n.d)
      a(r d)
    d(l a(r l.d))
  ::
  ++  rep                                               ::  reduce to product
    |*  b/_=>(~ |=({* *} +<+))
    |-
    ?~  a  +<+.b
    $(a r.a, +<+.b $(a l.a, +<+.b (b n.a +<+.b)))
  ::
  ++  rib                                               ::  transform + product
    |*  {b/* c/gate}
    |-  ^+  [b a]
    ?~  a  [b ~]
    =+  d=(c n.a b)
    =.  n.a  +.d
    =+  e=$(a l.a, b -.d)
    =+  f=$(a r.a, b -.e)
    [-.f a(l +.e, r +.f)]
  ::
  ++  run                                               ::  apply gate to values
    |*  b/gate
    |-
    ?~  a  a
    [n=[p=p.n.a q=(b q.n.a)] l=$(a l.a) r=$(a r.a)]
  ::
  ++  rut                                               ::  apply gate to nodes
    |*  b/gate
    |-
    ?~  a  a
    [n=[p=p.n.a q=(b p.n.a q.n.a)] l=$(a l.a) r=$(a r.a)]
  ::
  ++  tap                                               ::  listify pairs
    =<  $
    ~/  %tap
    =+  b=`(list _?>(?=(^ a) n.a))`~
    |.  ^+  b
    ?~  a
      b
    $(a r.a, b [n.a $(a l.a)])
  ::
  ++  uni                                               ::  union, merge
    ~/  %uni
    =+  b=a
    |@
    ++  $
      |-  ^+  a
      ?~  b
        a
      ?~  a
        b
      ?:  =(p.n.b p.n.a)
        b(l $(a l.a, b l.b), r $(a r.a, b r.b))
      ?:  (mor p.n.a p.n.b)
        ?:  (gor p.n.b p.n.a)
          $(l.a $(a l.a, r.b ~), b r.b)
        $(r.a $(a r.a, l.b ~), b l.b)
      ?:  (gor p.n.a p.n.b)
        $(l.b $(b l.b, r.a ~), a r.a)
      $(r.b $(b r.b, l.a ~), a l.a)
    --
  ::
  ++  uno                                               ::  general union
    =+  b=a
    |@
    ++  $
      |=  meg/$-({_p:node _q:node _q:node} _q:node)
      |-  ^+  a
      ?~  b
        a
      ?~  a
        b
      ?:  =(p.n.b p.n.a)
        :+  [p.n.a (meg p.n.a q.n.a q.n.b)]
          $(b l.b, a l.a)
        $(b r.b, a r.a)
      ?:  (mor p.n.a p.n.b)
        ?:  (gor p.n.b p.n.a)
          $(l.a $(a l.a, r.b ~), b r.b)
        $(r.a $(a r.a, l.b ~), b l.b)
      ?:  (gor p.n.a p.n.b)
        $(l.b $(b l.b, r.a ~), a r.a)
      $(r.b $(b r.b, l.a ~), a l.a)
    --
  ::
  ::
  ++  urn                                               ::  apply gate to nodes
    |*  b/$-({* *} *)
    |-
    ?~  a  ~
    a(n n.a(q (b p.n.a q.n.a)), l $(a l.a), r $(a r.a))
  ::
  ++  wyt                                               ::  depth of map
    |-  ^-  @
    ?~(a 0 +((add $(a l.a) $(a r.a))))
  ::
  ++  key                                               ::  set of keys
    =+  b=`(set _?>(?=(^ a) p.n.a))`~
    |-  ^+  b
    ?~  a   b
    $(a r.a, b $(a l.a, b (~(put in b) p.n.a)))
  ::
  ++  val                                               ::  list of vals
    =+  b=`(list _?>(?=(^ a) q.n.a))`~
    |-  ^+  b
    ?~  a   b
    $(a r.a, b [q.n.a $(a l.a)])
  --
::                                                      ::
::::  2j: jar and jug logic                             ::
  ::                                                    ::
  ::
++  ja                                                  ::  jar engine
  =|  a/(tree (pair * (list)))  ::  (jar)
  |@
  ++  get                                               ::  gets list by key
    |*  b/*
    =+  c=(~(get by a) b)
    ?~(c ~ u.c)
  ::
  ++  add                                               ::  adds key-list pair
    |*  {b/* c/*}
    =+  d=(get b)
    (~(put by a) b [c d])
  --
++  ju                                                  ::  jug engine
  =|  a/(tree (pair * (tree)))  ::  (jug)
  |@
  ++  del                                               ::  del key-set pair
    |*  {b/* c/*}
    ^+  a
    =+  d=(get b)
    =+  e=(~(del in d) c)
    ?~  e
      (~(del by a) b)
    (~(put by a) b e)
  ::
  ++  gas                                               ::  concatenate
    |*  b/(list {p/* q/*})
    =>  .(b `(list _?>(?=({{* ^} ^} a) [p=p q=n.q]:n.a))`b)
    |-  ^+  a
    ?~  b
      a
    $(b t.b, a (put p.i.b q.i.b))
  ::
  ++  get                                               ::  gets set by key
    |*  b/*
    =+  c=(~(get by a) b)
    ?~(c ~ u.c)
  ::
  ++  has                                               ::  existence check
    |*  {b/* c/*}
    ^-  ?
    (~(has in (get b)) c)
  ::
  ++  put                                               ::  add key-set pair
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
  =|  a/(tree)  ::  (qeu)
  |@
  ++  apt                                               ::  check correctness
    |-  ^-  ?
    ?~  a  &
    ?&  ?~(l.a & ?&((mor n.a n.l.a) $(a l.a)))
        ?~(r.a & ?&((mor n.a n.r.a) $(a r.a)))
    ==
  ::
  ++  bal
    |-  ^+  a
    ?~  a  ~
    ?.  |(?=(~ l.a) (mor n.a n.l.a))
      $(a l.a(r $(a a(l r.l.a))))
    ?.  |(?=(~ r.a) (mor n.a n.r.a))
      $(a r.a(l $(a a(r l.r.a))))
    a
  ::
  ++  dep                                               ::  max depth of queue
    |-  ^-  @
    ?~  a  0
    +((max $(a l.a) $(a r.a)))
  ::
  ++  gas                                               ::  insert list to que
    |=  b/(list _?>(?=(^ a) n.a))
    |-  ^+  a
    ?~(b a $(b t.b, a (put i.b)))
  ::
  ++  get                                               ::  head-rest pair
    |-  ^+  ?>(?=(^ a) [p=n.a q=*(tree _n.a)])
    ?~  a
      !!
    ?~  r.a
      [n.a l.a]
    =+  b=$(a r.a)
    :-  p.b
    ?:  |(?=(~ q.b) (mor n.a n.q.b))
      a(r q.b)
    a(n n.q.b, l a(r l.q.b), r r.q.b)
  ::
  ++  nip                                               ::  removes root
    |-  ^+  a
    ?~  a  ~
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (mor n.l.a n.r.a)
      l.a(r $(l.a r.l.a))
    r.a(l $(r.a l.r.a))
  ::
  ++  nap                                               ::  removes root
    ?>  ?=(^ a)
    ?:  =(~ l.a)  r.a
    =+  b=get(a l.a)
    bal(n.a p.b, l.a q.b)
  ::
  ++  put                                               ::  insert new tail
    |*  b/*
    |-  ^+  a
    ?~  a
      [b ~ ~]
    bal(l.a $(a l.a))
  ::
  ++  tap                                               ::  adds list to end
    =+  b=`(list _?>(?=(^ a) n.a))`~
    |-  ^+  b
    =+  0                                               ::  hack for jet match
    ?~  a
      b
    $(a r.a, b [n.a $(a l.a)])
  ::
  ++  top                                               ::  produces head
    |-  ^-  (unit _?>(?=(^ a) n.a))
    ?~  a  ~
    ?~(r.a [~ n.a] $(a r.a))
  --
::
::::  2o: containers                                    ::
  ::                                                    ::
  ::
++  jar  |$  [key value]  (map key (list value))        ::  map of lists
++  jug  |$  [key value]  (map key (set value))         ::  map of sets
::
++  map
  |$  [key value]                                       ::  table
  $|  (tree (pair key value))
  |=(a=(tree (pair)) ~(apt by a))
::
++  qeu
  |$  [item]                                            ::  queue
  $|  (tree item)
  |=(a=(tree) ~(apt to a))
::
++  set
  |$  [item]                                            ::  set
  $|  (tree item)
  |=(a=(tree) ~(apt in a))
::
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
  ::                                                    ::
  ++  le                                                ::  construct list
    |*  a/(list)
    ^+  =<  $
      |@  ++  $  ?:(*? ~ [i=(snag 0 a) t=$])
      --
    a
  ::                                                    ::
  ++  my                                                ::  construct map
    |*  a/(list (pair))
    =>  .(a ^+((le a) a))
    (~(gas by `(map _p.i.-.a _q.i.-.a)`~) a)
  ::                                                    ::
  ++  si                                                ::  construct set
    |*  a/(list)
    =>  .(a ^+((le a) a))
    (~(gas in `(set _i.-.a)`~) a)
  ::                                                    ::
  ++  snag                                              ::  index
    |*  {a/@ b/(list)}
    ?~  b
      ~_  leaf+"snag-fail"
      !!
    ?:  =(0 a)  i.b
    $(b t.b, a (dec a))
  ::                                                    ::
  ++  weld                                              ::  concatenate
    |*  {a/(list) b/(list)}
    =>  .(a ^+((le a) a), b ^+((le b) b))
    =+  42
    |-
    ?~  a  b
    [i=i.a t=$(a t.a)]
  --
::
::::  2q: molds and mold builders                       ::
  ::                                                    ::
  ::
+$  axis  @                                             ::  tree address
+$  bean  ?                                             ::  0=&=yes, 1=|=no
+$  flag  ?
+$  char  @t                                            ::  UTF8 byte
+$  cord  @t                                            ::  UTF8, LSB first
+$  byts  [wid=@ud dat=@]                               ::  bytes, MSB first
+$  deco  ?($bl $br $un $~)                             ::  text decoration
+$  date  {{a/? y/@ud} m/@ud t/tarp}                    ::  parsed date
+$  knot  @ta                                           ::  ASCII text
+$  noun  *                                             ::  any noun
+$  path  (list knot)                                   ::  like unix path
+$  stud                                                ::  standard name
          $@  mark=@tas                                 ::  auth=urbit
          $:  auth=@tas                                 ::  standards authority
              type=path                                 ::  standard label
          ==                                            ::
+$  stub  (list (pair stye (list @c)))                  ::  styled unicode
+$  stye  (pair (set deco) (pair tint tint))            ::  decos/bg/fg
+$  styl                                                ::  cascading style
          %+  pair  (unit deco)                         ::
          (pair (unit tint) (unit tint))                ::
::                                                      ::
+$  styx  (list $@(@t (pair styl styx)))                ::  styled text
+$  tile  ::  XX: ?@(knot (pair styl knot))
          ::
          cord
+$  tint  ?($r $g $b $c $m $y $k $w $~)                 ::  text color
::
::  A `plum` is the intermediate representation for the pretty-printer. It
::  encodes hoon-shaped data with the least amount of structured needed
::  for formating.
::
::  A `plum` is either a
::
::  - `cord`: A simple cord
::  - `[%para *]`: A wrappable paragraph.
::  - `[%tree *]`: A formatted plum tree
::  - `[%sbrk *]`: An indication of a nested subexpression.
::
::  The formatter will use the tall mode unless:
::
::    - A plum has only a `wide` style.
::    - The plum is in `%sbrk` form and its subplum (`kid`), when
::      formatted in wide mode, can fit on a single line.
::
+$  plum
  $@  cord
  $%  [%para prefix=tile lines=(list @t)]
      [%tree fmt=plumfmt kids=(list plum)]
      [%sbrk kid=plum]
  ==
::
::  A `plumfmt` is a description of how to render a `plum`. A `plumfmt`
::  must include a `wide`, a `tall`, or both.
::
::  A `wide` is a description of how to render a plum in a single
::  line. The nested (`kids`) sub-plums will be interleaved with `delimit`
::  strings, and, if `enclose` is set, then the output will be enclosed
::  with `p.u.enclose` abnd `q.u.enclose`.
::
::  For example, to build a plumfmt for string literals, we could write:
::
::      [wide=[~ '' [~ '"' '"']] tall=~]
::
::  A `tall` is a description of how to render a plum across multiple
::  lines. The output will be prefixed by `intro`, suffixed by
::  `final.u.indef`, and each subplum prefixed by `sigil.u.indef`.
::
::  For example, to build a plumfmt for cores, we could write:
::
::      [wide=~ tall=`['' `['++' '--']]]
::
+$  plumfmt
  $:  wide=(unit [delimit=tile enclose=(unit (pair tile tile))])
      tall=(unit [intro=tile indef=(unit [sigil=tile final=tile])])
  ==
::
++  tang  (list tank)                                   ::  bottom-first error
++  tank  $~  [%leaf ~]                                 ::
          $%  {$leaf p/tape}                            ::  printing formats
              {$plum p/plum}                            ::
              $:  $palm                                 ::  backstep list
                  p/{p/tape q/tape r/tape s/tape}       ::
                  q/(list tank)                         ::
              ==                                        ::
              $:  $rose                                 ::  flat list
                  p/{p/tape q/tape r/tape}              ::  mid open close
                  q/(list tank)                         ::
              ==                                        ::
          ==                                            ::
++  tape  (list @tD)                                    ::  utf8 string as list
++  tour  (list @c)                                     ::  utf32 clusters
++  tarp  {d/@ud h/@ud m/@ud s/@ud f/(list @ux)}        ::  parsed time
++  term  @tas                                          ::  ascii symbol
++  wain  (list cord)                                   ::  text lines
++  wall  (list tape)                                   ::  text lines
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
++  fn  ::  float, infinity, or NaN
        ::  s=sign, e=exponent, a=arithmetic form
        ::  (-1)^s * a * 2^e
        $%  {$f s/? e/@s a/@u}
            {$i s/?}
            {$n ~}
        ==
::
++  dn  ::  decimal float, infinity, or NaN
        ::  (-1)^s * a * 10^e
        $%  {$d s/? e/@s a/@u}
            {$i s/?}
            {$n ~}
        ==
::
++  rn  ::  parsed decimal float
        ::
        $%  {$d a/? b/{c/@ {d/@ e/@} f/? i/@}}
            {$i a/?}
            {$n ~}
        ==
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
  ^|
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
::
++  fl                                                  ::  arb. precision fp
  =/  {{p/@u v/@s w/@u} r/$?($n $u $d $z $a) d/$?($d $f $i)}
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
  ::  b=bias:          added to exponent when storing
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
  ^|
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
  ^|
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
  ^|
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
  ^|
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
  ~/  %shas
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
    ~_  leaf+"rad-zero"
    ?<  =(0 b)
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
::
++  sha                                                 ::  correct byte-order
  ~%  %sha  ..sha  ~
  =>  |%
      ++  flin  |=(a=@ (swp 3 a))                       ::  flip input
      ++  flim  |=(byts [wid (rev 3 wid dat)])          ::  flip input w/ length
      ++  flip  |=(w=@u (cury (cury rev 3) w))          ::  flip output of size
      ++  meet  |=(a=@ [(met 3 a) a])                   ::  measure input size
      --
  |%
  ::
  ::  use with @
  ::
  ++  sha-1     (cork meet sha-1l)
  ++  sha-256   :(cork flin shax (flip 32))
  ++  sha-512   :(cork flin shaz (flip 64))
  ::
  ::  use with byts
  ::
  ++  sha-256l  :(cork flim shay (flip 32))
  ++  sha-512l  :(cork flim shal (flip 64))
  ::
  ++  sha-1l
    ~/  %sha1
    |=  byts
    ^-  @
    =+  [few==>(fe .(a 5)) wac=|=({a/@ b/@} (cut 5 [a 1] b))]
    =+  [sum=sum.few ror=ror.few rol=rol.few net=net.few inv=inv.few]
    =+  ral=(lsh 0 3 wid)
    =+  ^=  ful
        %+  can  0
        :~  [ral (rev 3 wid dat)]
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
  ::  +fein: conceal structure, v3.
  ::
  ::    +fein conceals planet-sized atoms.  The idea is that it should not be
  ::    trivial to tell which planet a star has spawned under.
  ::
  ++  fein
    |=  pyn/@  ^-  @
    ?:  &((gte pyn 0x1.0000) (lte pyn 0xffff.ffff))
      (add 0x1.0000 (feis (sub pyn 0x1.0000)))
    ?:  &((gte pyn 0x1.0000.0000) (lte pyn 0xffff.ffff.ffff.ffff))
      =/  lo  (dis pyn 0xffff.ffff)
      =/  hi  (dis pyn 0xffff.ffff.0000.0000)
      %+  con  hi
      $(pyn lo)
    pyn
  ::
  ::  +fynd: restore structure, v3.
  ::
  ::    Restores obfuscated values that have been enciphered with +fein.
  ::
  ++  fynd
    |=  cry/@  ^-  @
    ?:  &((gte cry 0x1.0000) (lte cry 0xffff.ffff))
      (add 0x1.0000 (tail (sub cry 0x1.0000)))
    ?:  &((gte cry 0x1.0000.0000) (lte cry 0xffff.ffff.ffff.ffff))
      =/  lo  (dis cry 0xffff.ffff)
      =/  hi  (dis cry 0xffff.ffff.0000.0000)
      %+  con  hi
      $(cry lo)
    cry
  ::
  ::  +feis: a four-round generalised Feistel cipher over the domain
  ::         [0, 2^32 - 2^16 - 1].
  ::
  ::    See: Black & Rogaway (2002), Ciphers for arbitrary finite domains.
  ::
  ++  feis
    |=  m=@
    ^-  @
    (fee 4 0xffff 0x1.0000 (mul 0xffff 0x1.0000) eff m)
  ::
  ::  +tail: reverse +feis.
  ::
  ++  tail
    |=  m=@
    ^-  @
    (feen 4 0xffff 0x1.0000 (mul 0xffff 0x1.0000) eff m)
  ::
  ::  +fee: "Fe" in B&R (2002).
  ::
  ::    A Feistel cipher given the following parameters:
  ::
  ::    r:    number of Feistel rounds
  ::    a, b: parameters such that ab >= k
  ::    k:    value such that the domain of the cipher is [0, k - 1]
  ::    prf:  a gate denoting a family of pseudorandom functions indexed by
  ::          its first argument and taking its second argument as input
  ::    m:    an input value in the domain [0, k - 1]
  ::
  ++  fee
    |=  [r=@ a=@ b=@ k=@ prf=$-([j=@ r=@] @) m=@]
    ^-  @
    =/  c  (fe r a b prf m)
    ?:  (lth c k)
      c
    (fe r a b prf c)
  ::
  ::  +feen: "Fe^-1" in B&R (2002).
  ::
  ::    Reverses a Feistel cipher constructed with parameters as described in
  ::    +fee.
  ::
  ++  feen
    |=  [r=@ a=@ b=@ k=@ prf=$-([j=@ r=@] @) m=@]
    ^-  @
    =/  c  (fen r a b prf m)
    ?:  (lth c k)
      c
    (fen r a b prf c)
  ::
  ::  +fe:  "fe" in B&R (2002).
  ::
  ::    An internal function to +fee.
  ::
  ::    Note that this implementation differs slightly from the reference paper
  ::    to support some legacy behaviour.  See urbit/arvo#1105.
  ::
  ++  fe
    |=  [r=@ a=@ b=@ prf=$-([j=@ r=@] @) m=@]
    =/  j  1
    =/  ell  (mod m a)
    =/  arr  (div m a)
    |-  ^-  @
    ::
    ?:  (gth j r)
      ?.  =((mod r 2) 0)
        (add (mul arr a) ell)
      ::
      :: Note that +fe differs from B&R (2002)'s "fe" below, as a previous
      :: implementation of this cipher contained a bug such that certain inputs
      :: could encipher to the same output.
      ::
      :: To correct these problem cases while also preserving the cipher's
      :: legacy behaviour on most inputs, we check for a problem case (which
      :: occurs when 'arr' is equal to 'a') and, if detected, use an alternate
      :: permutation instead.
      ::
      ?:  =(arr a)
        (add (mul arr a) ell)
      (add (mul ell a) arr)
    ::
    =/  f  (prf (sub j 1) arr)
    ::
    =/  tmp
      ?.  =((mod j 2) 0)
        (mod (add f ell) a)
      (mod (add f ell) b)
    ::
    $(j +(j), ell arr, arr tmp)
  ::
  ::  +fen:  "fe^-1" in B&R (2002).
  ::
  ::    Note that this implementation differs slightly from the reference paper
  ::    to support some legacy behaviour.  See urbit/arvo#1105.
  ::
  ++  fen
    |=  [r=@ a=@ b=@ prf=$-([j=@ r=@] @) m=@]
    =/  j  r
    ::
    =/  ahh
      ?.  =((mod r 2) 0)
        (div m a)
      (mod m a)
    ::
    =/  ale
      ?.  =((mod r 2) 0)
        (mod m a)
      (div m a)
    ::
    :: Similar to the comment in +fe, +fen differs from B&R (2002)'s "fe^-1"
    :: here in order to preserve the legacy cipher's behaviour on most inputs.
    ::
    :: Here problem cases can be identified by 'ahh' equating with 'a'; we
    :: correct those cases by swapping the values of 'ahh' and 'ale'.
    ::
    =/  ell
      ?:  =(ale a)
        ahh
      ale
    ::
    =/  arr
      ?:  =(ale a)
        ale
      ahh
    ::
    |-  ^-  @
    ?:  (lth j 1)
      (add (mul arr a) ell)
    =/  f  (prf (sub j 1) ell)
    ::
    ::  Note that there is a slight deviation here to avoid dealing with
    ::  negative values.  We add 'a' or 'b' to arr as appropriate and reduce
    ::  'f' modulo the same number before performing subtraction.
    ::
    =/  tmp
      ?.  =((mod j 2) 0)
        (mod (sub (add arr a) (mod f a)) a)
      (mod (sub (add arr b) (mod f b)) b)
    ::
    $(j (sub j 1), ell tmp, arr ell)
  ::
  ::  +eff: a murmur3-based pseudorandom function.  'F' in B&R (2002).
  ::
  ++  eff
    |=  [j=@ r=@]
    ^-  @
    (muk (snag j raku) 2 r)
  ::
  ::  +raku: seeds for eff.
  ::
  ++  raku
    ^-  (list @ux)
    :~  0xb76d.5eed
        0xee28.1300
        0x85bc.ae01
        0x4b38.7af7
    ==
  ::
  --
::
::::  3g: molds and mold builders
  ::
++  coin  $~  [%$ %ud 0]                                ::  print format
          $%  {$$ p/dime}                               ::
              {$blob p/*}                               ::
              {$many p/(list coin)}                     ::
          ==                                            ::
++  dime  {p/@ta q/@}                                   ::
++  edge  {p/hair q/(unit {p/* q/nail})}                ::  parsing output
++  hair  {p/@ud q/@ud}                                 ::  parsing trace
++  like  |*  a/$-(* *)                                 ::  generic edge
          |:  b=`*`[(hair) ~]                           ::
          :-  p=(hair -.b)                              ::
          ^=  q                                         ::
          ?@  +.b  ~                                    ::
          :-  ~                                         ::
          u=[p=(a +>-.b) q=[p=(hair -.b) q=(tape +.b)]] ::
++  nail  {p/hair q/tape}                               ::  parsing input
++  pint  {p/{p/@ q/@} q/{p/@ q/@}}                     ::  line+column range
++  rule  _|:($:nail $:edge)                            ::  parsing rule
++  spot  {p/path q/pint}                               ::  range in file
++  tone  $%  {$0 p/*}                                  ::  success
              {$1 p/(list)}                             ::  blocks
              {$2 p/(list {@ta *})}                     ::  error report
          ==                                            ::
++  toon  $%  {$0 p/*}                                  ::  success
              {$1 p/(list)}                             ::  blocks
              {$2 p/(list tank)}                        ::  stack trace
          ==                                            ::
++  wonk  =+  veq=$:edge                                ::  product from edge
          |@  ++  $  ?~(q.veq !! p.u.q.veq)             ::
          --                                            ::
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
  =/  yek/@ux  ~+
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
  ++  rf    `tape`[?-(a %& '&', %| '|', * !!) ~]
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
  ?.  =(%t (end 3 1 a))
    ::  XX more and better sanity
    ::
    &
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
++  ruth                                                ::  biblical sanity
  |=  {a/@ta b/*}
  ^-  @
  ?^  b  !!
  ::  ?.  ((sane a) b)  !!
  b
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
++  taft                                                ::  utf8 to utf32
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
  (rip 5 (taft (rap 3 a)))                              ::  XX horrible
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
  =+  c=(taft (end 3 b a))
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
::  This library includes `plume`, the actual pretty printing logic,
::  and a handful of utilities for constructing plums.
::
::  Generally, you'll just use `plume` like this:
::
::    ~(plume tall plum)  ::  Pretty print `plum` in tall mode.
::    ~(plume flat plum)  ::  Pretty print `plum` in wide mode.
::
::  There is probably no reason to look at the utility routines unless
::  you are writing something to generate `plum`s.
::
::  This is the pretty-printer.  Use the `flat` arm to render a plum
::  into a single line and use the `tall` arm to get a nice multi-line
::  rendering that switches to wide mode if there's enough space.
::
::  For details about how this works and what exactly it does in various
::  cases, take a look at the docs for `plum`, `plumfmt`, and at the
::  docs on the arms of this door.
::
++  plume
  |_  =plum
  ::
  ::  An line, indented by `indent` spaces.
  ::
  +$  line  [indent=@ud text=tape]
  ::
  ::  An sequence of indented lines.
  ::
  +$  block  (list line)
  ::
  ::  +flat: print as a single line
  ::
  ++  flat
    ^-  wain
    text:linear
  ::
  ::  +tall: print as multiple lines
  ::
  ++  tall
    ^-  wain
    %+  turn  window
    |=  line
    (crip (runt [indent ' '] text))
  ::
  ::  +adjust: adjust lines to right
  ::
  ++  adjust
    |=  [tab=@ud =block]  ^-  ^block
    (turn block |=([@ud tape] [(add tab +<-) +<+]))
  ::
  ::  Prepend `n` spaces to a tape.
  ::
  ++  prepend-spaces
    |=  [n=@ t=tape]  ^-  tape
    (runt [n ' '] t)
  ::
  ::  +window: print as list of tabbed lines
  ::
  ++  window
    ^-  block
    ~+                                                  ::  for random access
    ?@  plum  [0 (trip plum)]~                          ::  trivial text
    ?-  -.plum
      ::
      ::  %para: Line-wrappable paragraph. This is a stub; it should
      ::  wrap text to 40 characters.
      ::
      %para
        [0 +:linear]~
      ::
      ::  %sbrk: nested subexpression
      ::
      ::  This is an opportunity to switch to wide mode. First, try
      ::  rendered in wide mode. If that's possible and the result
      ::  isn't too big, use that. Otherwise recurse into the subplum
      ::  without switching to wide mode.
      ::
      %sbrk
        =/  sub  kid.plum
        ?+    sub
            window(plum sub)
          [%tree *]
            =/  wideresult
              ?~(wide.fmt.sub ~ [~ u=linear])
            ?:  ?&(?=(^ wideresult) (lte length.u.wideresult 40))
              [0 text.u.wideresult]~
            window(plum sub)
        ==
      ::
      ::  %tree: Try to render a text tree in tall mode.
      ::
      ::  We want to render this in tall mode. First, verify that there
      ::  the plum has a tall render (if not, fall back to `linear`
      ::  formatting), then render all the subplums, and then render
      ::  them in one of three ways:
      ::
      ::  - If the `plumfmt` contains an `indef` and that indef has
      ::    no prefix, then this is a variable-arity rune with a
      ::    terminator: Use vertical formatting.
      ::
      ::  - If the `plumfmt` contains an `indef` and that indef DOES have
      ::    a prefix, then this is something that looks like a core: Use
      ::    `core-like` formatting.
      ::
      ::  - Otherwise, this is a rune with a fixed number of arguments
      ::    Render the subplums using backstep indentation.
      ::
      ::  There's also a special case where something has exactly one sub-plum.
      ::  where something has exactly one sub-block. For example, we
      ::  want this output:
      ::
      ::      |-
      ::      foo
      ::
      %tree
        ?~  tall.fmt.plum  [0 text:linear]~
        =/  prelude  (trip intro.u.tall.fmt.plum)
        |^  =/  blocks   (turn kids.plum |=(=^plum window(plum plum)))
            =/  prelude  (trip intro.u.tall.fmt.plum)
            ?~  indef.u.tall.fmt.plum
              ?:  =(1 (lent blocks))
                [[0 prelude] (zing blocks)]
              (backstep prelude blocks)
            =/  prefix  (trip sigil.u.indef.u.tall.fmt.plum)
            =/  finale  (trip final.u.indef.u.tall.fmt.plum)
            ?~  blocks  %+  weld
                          ?~(prelude ~ [0 prelude]~)
                        ?~(finale ~ [0 finale]~)
            ?~  prefix  (running prelude blocks finale)
            (core-like prelude prefix blocks finale)
        --
    ==
  ::
  ::  Render a plum in tall-mode using backstep indentation. Here,
  ::  we are rendering things that look something like this:
  ::
  ::      :+  foo
  ::        bar
  ::      baz
  ::
  ++  backstep
    |=  [prelude=tape blocks=(list block)]
    ^-  block
    %-  zing
    =/  nkids  (lent blocks)
    =/  idx  1
    |-  ^-  (list block)
    ?~  blocks  ~
    :_  $(blocks t.blocks, idx +(idx))
    ^-  block
    =/  indent  (mul 2 (sub nkids idx))
    ?.  =(1 idx)  (adjust indent i.blocks)
    (rune-inline-with-block prelude indent i.blocks)
  ::
  ::  To make things look a bit nicer, we want to put the first
  ::  sub-block on the same line as the rune. We want this:
  ::
  ::      :-  foo
  ::      baz
  ::
  ::  Instead of this:
  ::
  ::      :-
  ::          foo
  ::      baz
  ::
  ::  This handles the "foo" case.
  ::
  ++  rune-inline-with-block
    |=  [rune=tape indent=@ blk=block]
    ^-  block
    =.  indent  (max indent (add 2 (lent rune)))
    =.  blk     (adjust indent blk)
    ?~  rune  blk
    ?~  blk   [0 rune]~
    :_  t.blk
    :-  0
    %+  weld  rune
    =/  spaces-btwn  (sub indent.i.blk (lent rune))
    (prepend-spaces spaces-btwn text.i.blk)
  ::
  ::  Render a tall hoon with running indentation. Here, we are
  ::  rendering things that look sopmething like:
  ::
  ::      :~  foo
  ::          bar
  ::          baz
  ::      ==
  ::
  ::  So, there's basically three cases here: Either the prelude
  ::  is a rune, the prelude is empty, or prelude is some other
  ::  random-ass thing.
  ::
  ::  - If there is no prelude, then just combine all of the
  ::    sub-blocks together unaltered.
  ::  - If it's a rune (two-characters wide), then combine the
  ::    rune and the first line into one line (separated by two
  ::    spaces) and indent the rest of the lines by four spaces.
  ::  - If the rune is some other random-ass thing (has a length
  ::    that isn't 0 or 2), then render the prelude alone on the
  ::    first line and then combine the sub-blocks together,
  ::    all indented by another two spaces.
  ::
  ::  Regardless, if there's a finale, stick it on the end without
  ::  any indentation.
  ::
  ++  running
    |=  [prelude=tape blocks=(list block) finale=tape]
    ^-  block
    =/  result=block  (zing blocks)
    =.  result
      ?+    (lent prelude)
          [[0 prelude] (adjust 2 result)]         ::  unusual prelude
        %0                                        ::  empty prelude
          result
        %2                                        ::  rune prelude
          (rune-inline-with-block prelude 4 result)
      ==
    ?~  finale  result
    (snoc result [0 finale])
  ::
  ::  This renders sub-blocks where each sub-block needs to be
  ::  prefixed by some tape. For example:
  ::
  ::      |%
  ::      ++  foo
  ::        bar
  ::      ++  baz
  ::        qux
  ::      --
  ::
  ++  core-like
    |=  [prelude=tape prefix=tape blocks=(list block) finale=tape]
    ^-  block
    =/  clear  (add 2 (lent prefix))
    =/  result
      ^-  block
      %-  zing
      ^-  (list block)
      %+  turn  blocks
      |=  blk=block
      ^-  block
      ^+  +<
      =*  tab  ?~(blk 0 (sub clear (min clear indent.i.blk)))
      =.  blk  (adjust tab blk)
      ?~  blk  ~
      :_  t.blk
      :-  0
      %+  weld  prefix
      (runt [(sub indent.i.blk (lent prefix)) ' '] text.i.blk)
    =.  result
      ?~  finale  result
      (snoc result [0 finale])
    ?~  prelude  result
    [[0 prelude] result]
  ::
  ::  +linear: Render a plum onto a single line, even if it only has a
  ::  wide form.
  ::
  ++  linear
    ^-  [length=@ud text=tape]
    ~+                                                  ::  ~+ for random access
    ?@  plum  [(met 3 plum) (trip plum)]                ::  Just a cord.
    ?-  -.plum
      ::
      ::  This is already in wide mode, so %sbrk nodes don't matter here.
      ::
      %sbrk
        linear(plum kid.plum)
      ::
      ::  %para: To write a wrappable text paragraph to a single line,
      ::  we just combine all the lines into one, interspersing single
      ::  spaces chars.
      ::
      %para
        |-  ^-  [length=@ud text=tape]
        ?~  lines.plum  [0 ~]
        =/  next  $(lines.plum t.lines.plum)
        =/  this  [length=(met 3 i.lines.plum) text=(trip i.lines.plum)]
        :-  (add +(length.this) length.next)
        (weld text.this `tape`[' ' text.next])
      ::
      ::  Render a text tree to a single line.
      ::
      %tree
        |^  ^-  [length=@ud text=tape]
            ?~  wide.fmt.plum  (force-wide window)
            =/  body  (render-body delimit.u.wide.fmt.plum kids.plum)
            ?~  enclose.u.wide.fmt.plum  body
            (wrap-with-enclose u.enclose.u.wide.fmt.plum body)
        ::
        ::  Given a list of subplums and a delimiter, render all the
        ::  subplums onto a single line, and combine them into a single
        ::  string by interspersing the delimiter.
        ::
        ++  render-body
           |=  [delimit=cord kids=(list ^plum)]
           =/  stop  (trip delimit)
           |-  ^-  [length=@ud text=tape]
           ?~  kids  [0 ~]
           =/  next  $(kids t.kids)
           =/  this  linear(plum i.kids)
           ?~  text.next  this
           :-  :(add length.this (lent stop) length.next)
           :(weld text.this stop text.next)
        ::
        ::  Wrap a wide-form-rendered result with the `enclose`  cords
        ::  from its `plumefmt`.
        ::
        ++  wrap-with-enclose
          |=  [clamps=(pair cord cord) body=[length=@ text=tape]]
          ^-  [length=@ud text=tape]
          ::
          =/  close  [(trip -.clamps) (trip +.clamps)]
          :-  :(add length.body (lent -.close) (lent +.close))
          :(weld -.close text.body +.close)
        ::
        ::  Given the result of rendering a plum in tall form, combine
        ::  all the lines into one by separating each by two spaces.
        ::
        ++  force-wide
          |=  render=(list [@ud text=tape])
          ^-  [length=@ud text=tape]
          ::
          ?~  render  [0 ~]
          =/  next  (force-wide t.render)
          :-  :(add (lent text.i.render) 2 length.next)
          ?~(text.next text.i.render :(weld text.i.render "  " text.next))
        --
    ==
  --
::
::  Convenience function to build a `plumfmt` for a rune with a fixed
::  number of parameters.
::
++  fixed
  |=  rune=@ta
  ^-  plumfmt
  [wide=`[' ' `[(cat 3 +< '(') ')']] tall=`[+< ~]]
::
::  Same as `fixed` but only outputs in `tall` mode.
::
++  tall-fixed
  |=  rune=cord
  ^-  (unit [cord (unit [cord cord])])
  `[rune ~]
::
::  Convenience function to build a the `tall` part of a `plumfmt` for
::  a running-style rune (one that takes a variable number of parameters
::  and has a terminator).
::
++  tall-running
  |=  [rune=cord sigil=cord term=cord]
  ^-  (unit [cord (unit [cord cord])])
  `[rune `[sigil term]]
::
::  Convenience function for rendering a rune into a plum. This takes
::  a rune, an optional tall-form terminator, optionally a short-form (if
::  you don't supply a short-form, it'll just construct the standard
::  wide-form (e.g. "?~(x x ~)") for you, and a list of sub-plums.
::
++  rune
  |=  $:  rune=cord
          term=(unit cord)
          short=(unit [cord cord cord])
          kids=(list plum)
      ==
  ^.  plum
  |^  :-  %sbrk
      :+  %tree
        :-  (rune-wide-form rune short)
        ?~  term  (tall-fixed rune)
        (tall-running rune '' u.term)
      kids
  ::
  ::  If you just give this a rune, it'll build the standard wide-form.
  ::  Otherwise, it'll just use the one that you gave it.
  ::
  ++  rune-wide-form
    |=  [rune=cord short=(unit [fst=cord mid=cord lst=cord])]
    ^-  (unit (pair cord (unit [cord cord])))
    =*  fst  (cat 3 rune '(')
    =*  std  `[' ' `[fst ')']]
    ?~  short  std
    `[mid.u.short `[fst.u.short lst.u.short]]
  --
::
::  Just a helper function for constructing a wide-form %tree plum.
::
++  simple-wide
  |=  [init=cord sep=cord end=cord kids=(list plum)]
  ^-  plum
  =/  fmt=plumfmt  [wide=[~ sep [~ init end]] tall=~]
  [%tree fmt kids]
::
::  Convenience function that builds a plum for a subexpression. The
::  `%sbrk` tells the pretty-printer that this is a valid place to
::  switch from tall mode to wide mode.
::
++  subtree
  |=  [p=plumfmt q=(list plum)]
  ^-  plum
  [%sbrk [%tree p q]]
::
::  Convenience for generating plums that look like s-expressions. Useful
::  for quickly getting decent-looking debug output.
::
++  sexp
  |=  [sym=cord kids=(list plum)]
  ^-  plum
  =/  head=cord     (cat 3 '(' sym)
  =/  headspc=cord  (cat 3 head ' ')
  =/  symcol=cord  (cat 3 sym ':')
  =/  fmt=plumfmt   [[~ ' ' [~ headspc ')']] [~ symcol [~ '' '']]]
  ?~  kids  (cat 3 '(' (cat 3 sym ')'))
  [%sbrk [%tree fmt kids]]
::
++  re
  |_  tac/tank
  ++  ram
    ^-  tape
    ?-    -.tac
        $leaf  p.tac
        $plum  ~(flat plume p.tac)
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
  ++  ug                                                ::  horrible hack
    |%
    ++  ace                                             ::  strip ctrl chars
      |=  a=tape
      ^-  tape
      ?~  a  ~
      ?:  |((lth i.a 32) =(127 `@`i.a))
        $(a t.a)
      [i.a $(a t.a)]
    ::
    ++  act                                             ::  pretend tapes
      |=  tac=tank
      ^-  tank
      ?-  -.tac
        %leaf  [%leaf (hew p.tac)]
        %plum  tac    ::  XX consider
        %palm  :+  %palm
                 [(hew p.p.tac) (hew q.p.tac) (hew r.p.tac) (hew s.p.tac)]
               (turn q.tac act)
        %rose  :+  %rose
                 [(hew p.p.tac) (hew q.p.tac) (hew r.p.tac)]
               (turn q.tac act)
      ==
    ::
    ++  fix                                             ::  restore tapes
      |=  wol=wall
      %+  turn  wol
      |=(a=tape (tufa `(list @c)``(list @)`a))
    ::
    ++  hew                                             ::  pretend tape
      |=(a=tape `tape``(list @)`(tuba (ace a)))
    --
  ::
  ++  win
    |=  {tab/@ edg/@}
    =.  tac  (act:ug tac)
    %-  fix:ug
    =+  lug=`wall`~
    |^  |-  ^-  wall
        ?-    -.tac
            $leaf  (rig p.tac)
            $plum  (turn ~(tall plume p.tac) |=(=cord (trip cord)))
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
          {s/~ c/*}
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
          :-  [%m '%h::[%d %d].[%d %d]>']
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
        ~   (show '$')
        c/@  (show c.lim)
        {%& $1}  (show '.')
        {%& c/@}
      [%leaf '+' ~(rud at c.lim)]
    ::
        {%| @ ~}  (show ',')
        {%| n/@ ~ c/@}
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
  =+  raq=|*({a/* b/*} [~ u=[a b]])
  |@
  ++  $
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
  --
::
++  comp
  ~/  %comp
  =+  raq=|*({a/* b/*} [a b])                           ::  arbitrary compose
  |@
  ++  $
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
  --
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
  |*  sam={vex/edge sab/rule}
  %.  sam
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
  |*  sam={vex/edge sab/rule}
  %.  sam
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
++  fuss
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
  =+  [hez=|=({a/pint b/*} [a b]) sef=*rule]
  |@
  ++  $
    ~/  %fun
    |=  tub/nail
    =+  vex=(sef tub)
    ?~  q.vex
      vex
    [p=p.vex q=[~ u=[p=(hez [p.tub p.q.u.q.vex] p.u.q.vex) q=q.u.q.vex]]]
  --
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
  ?:  |(?=(~ q.tub) !=((end 3 1 daf) i.q.tub))
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
  =|  {gar/* sef/_|.(*rule)}
  |@  ++  $
        |=  tub/nail
        ^-  (like _gar)
        ((sef) tub)
  --
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
        ?:  (mor p.n.yal p.n.nuc)
          [n.yal nuc r.yal]
        [n.nuc l.nuc [n.yal r.nuc r.yal]]
      =+  nuc=$(yal r.yal)
      ?>  ?=(^ nuc)
      ?:  (mor p.n.yal p.n.nuc)
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
  |*  raq/_=>(~ |*({a/* b/*} [a b]))
  |*  {bus/rule fel/rule}
  ;~((comp raq) fel (stir +<+.raq raq ;~(pfix bus fel)))
::
++  star                                                ::  0 or more times
  |*  fel/rule
  (stir `(list _(wonk *fel))`~ |*({a/* b/*} [a b]) fel)
::
++  stir
  ~/  %stir
  |*  {rud/* raq/_=>(~ |*({a/* b/*} [a b])) fel/rule}
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
++  rash  |*({naf/@ sab/rule} (scan (trip naf) sab))
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
++  ban  (just '>')
++  bar  (just '|')
++  bas  (just '\\')  ::  XX deprecated
++  bat  (just '\\')
++  buc  (just '$')   ::  XX deprecated
++  bus  (just '$')
++  cab  (just '_')
++  cen  (just '%')
++  col  (just ':')
++  com  (just ',')
++  dot  (just '.')
++  fas  (just '/')  ::  XX deprecated?
++  gal  (just '<')   ::  XX deprecated
++  gar  (just '>')   ::  XX deprecated
++  vat  (just '@')   ::  pronounced "at"
++  hax  (just '#')
++  hep  (just '-')   ::  pronounced "ep"
++  ket  (just '^')
++  leb  (just '{')
++  led  (just '<')
++  lob  (just '{')
++  lit  (just '(')
++  lac  (just '[')
++  lus  (just '+')
++  mic  (just ';')   ::  pronounced "mick"
++  net  (just '/')
++  pad  (just '&')
++  rac  (just ']')
++  reb  (just '}')
++  rob  (just '}')
++  rit  (just ')')
++  say  (just '\'')
++  sig  (just '~')
++  tar  (just '*')
++  tec  (just '`')
++  tis  (just '=')   ::  pronounced "is"
++  toc  (just '"')   ::  XX deprecated
++  yel  (just '"')
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
++  dof  ;~(plug hep gay)                               ::  - @q separator
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
++  gon  ;~(pose ;~(plug bat gay net) (easy ~))         ::  long numbers \ /
++  gul  ;~(pose (cold 2 led) (cold 3 ban))             ::  axis syntax < >
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
             ;~(less bat say prn)
             ;~(pfix bat ;~(pose bat say mes))          ::  escape chars
         ==
++  qut  ;~  simu  say                                  ::  cord
           ;~  pose
             ;~  less  soz
               (ifix [say say] (boss 256 (more gon qit)))
             ==
             =+  hed=;~(pose ;~(plug (plus ace) vul) (just '\0a'))
             %-  iny  %+  ifix
               :-  ;~(plug soz hed)
               ;~(plug (just '\0a') soz)
             (boss 256 (star qat))
           ==
         ==
++  soz  ;~(plug say say say)                           ::  delimiting '''
++  sym                                                 ::  symbol
  %+  cook
    |=(a/tape (rap 3 ^-((list @) a)))
  ;~(plug low (star ;~(pose nud low hep)))
::
++  mixed-case-symbol
  %+  cook
    |=(a/tape (rap 3 ^-((list @) a)))
  ;~(plug alf (star alp))
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
  ++  voy  ;~(pfix bat ;~(pose bat say bix))
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
  ++  fed  %+  cook  fynd:ob
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
  ++  feq  %+  cook  |=(a=(list @) (rep 4 (flop a)))
           ;~  plug
             ;~(pose hif:ab tiq:ab)
             (star ;~(pfix dof hif:ab))
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
++  co
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
          =+  sxz=(fein:ob q.p.lot)
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
            $q
          =*  val  q.p.lot
          :+  '.'  '~'
          =-  =.(rep (weld - rep) rep)
          %-  tail
          %+  roll  ?:(=(0 val) ~[0] (rip 3 val))
          |=  [q=@ s=? r=tape]
          :-  !s
          ;:  weld
            (trip (?:(s tod:po tos:po) q))
            ?:(&(s !=(r "")) "-" ~)
            r
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
      (cook |=(det/date `dime`[%da (year det)]) when)
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
      ;~(pfix hep (stag %c (cook taft urx:ab)))
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
    ;~  pose
      (stag %rh royl-rh)
      (stag %rq royl-rq)
      (stag %rd royl-rd)
      (stag %rs royl-rs)
    ==
  ::
  ++  royl-rh  (cook rylh ;~(pfix ;~(plug sig sig) (cook royl-cell royl-rn)))
  ++  royl-rq  (cook rylq ;~(pfix ;~(plug sig sig sig) (cook royl-cell royl-rn)))
  ++  royl-rd  (cook ryld ;~(pfix sig (cook royl-cell royl-rn)))
  ++  royl-rs  (cook ryls (cook royl-cell royl-rn))
  ::
  ++  royl-rn
    =/  moo
      |=  a=tape
      :-  (lent a)
      (scan a (bass 10 (plus sid:ab)))
    ;~  pose
      ;~  plug
        (easy %d)
        ;~(pose (cold | hep) (easy &))
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
      ::
      ;~  plug
        (easy %i)
        ;~  sfix
          ;~(pose (cold | hep) (easy &))
          (jest 'inf')
        ==
      ==
      ::
      ;~  plug
        (easy %n)
        (cold ~ (jest 'nan'))
      ==
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
  ++  when
    ~+
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
  ++  zust
    ~+
    ;~  pose
      (stag %is bip:ag)
      (stag %if lip:ag)
      royl
      (stag %f ;~(pose (cold & (just 'y')) (cold | (just 'n'))))
      (stag %q ;~(pfix sig feq:ag))
    ==
  --
::
::::  4m: formatting functions
  ::
++  scot
  ~/  %scot
  |=(mol/dime ~(rent co %$ mol))
++  scow
  ~/  %scow
  |=(mol/dime ~(rend co %$ mol))
++  slat  |=(mod/@tas |=(txt/@ta (slaw mod txt)))
++  slav  |=({mod/@tas txt/@ta} (need (slaw mod txt)))
++  slaw
  ~/  %slaw
  |=  {mod/@tas txt/@ta}
  ^-  (unit @)
  ?+    mod
      ::  slow fallback case to the full slay
      ::
      =+  con=(slay txt)
      ?.(&(?=({~ $$ @ @} con) =(p.p.u.con mod)) ~ [~ q.p.u.con])
  ::
      %da
    (rush txt ;~(pfix sig (cook year when:so)))
  ::
      %p
    (rush txt ;~(pfix sig fed:ag))
  ::
      %ud
    (rush txt dem:ag)
  ::
      %ux
    (rush txt ;~(pfix (jest '0x') hex:ag))
  ::
      %uv
    (rush txt ;~(pfix (jest '0v') viz:ag))
  ::
      %ta
    (rush txt ;~(pfix ;~(plug sig dot) urs:ab))
  ::
      %tas
    (rush txt sym)
  ==
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
  =+  fel=;~(pfix net (more net urs:ab))
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
++  mink  !.
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
      {$5 b/* c/*}
    =+  hed=$(fol b.fol)
    ?.  ?=($0 -.hed)  hed
    =+  tal=$(fol c.fol)
    ?.  ?=($0 -.tal)  tal
    [%0 =(p.hed p.tal)]
  ::
      {$6 b/* c/* d/*}
    =+  ben=$(fol b.fol)
    ?.  ?=($0 -.ben)  ben
    ?:  =(& p.ben)  $(fol c.fol)
    ?:  =(| p.ben)  $(fol d.fol)
    [%2 tax]
  ::
      {$7 b/* c/*}
    =+  ben=$(fol b.fol)
    ?.  ?=($0 -.ben)  ben
    $(sub p.ben, fol c.fol)
  ::
      {$8 b/* c/*}
    =+  ben=$(fol b.fol)
    ?.  ?=($0 -.ben)  ben
    $(sub [p.ben sub], fol c.fol)
  ::
      {$9 b/* c/*}
    =+  ben=$(fol c.fol)
    ?.  ?=($0 -.ben)  ben
    =.  sub  p.ben
    =+  lof=$(fol [0 b.fol])
    ?.  ?=($0 -.lof)  lof
    $(fol p.lof)
  ::
      {$10 {b/@ c/*} d/*}
    =+  bog=$(fol d.fol)
    ?.  ?=({$0 *} bog)  bog
    =+  lot=$(fol c.fol)
    ?.  ?=({$0 *} lot)  lot
    =+  [axe=b.fol big=p.bog lit=p.lot]
    ^-  tone
    :-  %0
    |-  ^-  p/*
    ?:  =(2 axe)  [lit +.big]
    ?:  =(3 axe)  [-.big lit]
    =+  mor=(mas axe)
    ?:  =(2 (cap axe))
      [$(big -.big, axe mor) +.big]
    [-.big $(big +.big, axe mor)]
  ::
      {$11 @ c/*}        $(fol c.fol)
      {$11 {b/* c/*} d/*}
    =+  ben=$(fol c.fol)
    ?.  ?=($0 -.ben)  ben
    ?:  ?=(?($hunk $hand $lose $mean $spot) b.fol)
      $(fol d.fol, tax [[b.fol p.ben] tax])
    $(fol d.fol)
  ::
      {$12 b/* c/*}
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
++  mook
  |=  ton/tone
  ^-  toon
  ?.  ?=({$2 *} ton)  ton
  :-  %2
  :: =.  p.ton  (moop p.ton)
  =+  yel=(lent p.ton)
  =.  p.ton
    ?.  (gth yel 1.024)  p.ton
    %+  weld
      (scag 512 p.ton)
    ^-  (list {@ta *})
    :_  (slag (sub yel 512) p.ton)
    :-  %lose
    %+  rap  3
    "[skipped {(scow %ud (sub yel 1.024))} frames]"
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
  (mock [gat(+< sam) %9 2 %0 1] gul)
::
++  mule                                                ::  typed virtual
  ~/  %mule
  =+  taq=|.(**)
  |@  ++  $
        =+  mud=(mute taq)
        ?-  -.mud
          %&  [%& p=$:taq]
          %|  [%| p=p.mud]
        ==
  --
::
++  mute                                                ::  untyped virtual
  |=  taq/_=>(~ ^?(|.(**)))
  ^-  (each * (list tank))
  =/  ton  (mock [taq %9 2 %0 1] |=((pair) ``.*(~ [%12 1+p 1+q])))
  ?-  -.ton
    $0  [%& p.ton]
    $1  [%| (turn p.ton |=(a/* (smyt (path a))))]
    $2  [%| p.ton]
  ==
::  +slum: slam a gate on a sample using raw nock, untyped
::
++  slum
  ~/  %slum
  |=  [gat=* sam=*]
  ^-  *
  .*(gat [%9 2 %10 [6 %1 sam] %0 1])
::
++  soft                                                ::  maybe remold
  |*  han/$-(* *)
  |=  fud/*  ^-  (unit han)
  =+  result=(mule |.((han fud)))
  ?-  -.result
    %|  ~
    %&  [~ p.result]
  ==
::
::::  4o: molds and mold builders
  ::
+$  abel  typo                                          ::  original sin: type
+$  alas  (list (pair term hoon))                       ::  alias list
+$  atom  @                                             ::  just an atom
+$  aura  @ta                                           ::  atom format
+$  base                                                ::  base mold
  $@  $?  $noun                                         ::  any noun
          $cell                                         ::  any cell
          $flag                                         ::  loobean
          $null                                         ::  ~ == 0
          $void                                         ::  empty set
      ==                                                ::
  {$atom p/aura}                                        ::  atom
::
+$  woof  $@(@ {~ p/hoon})                              ::  simple embed
+$  chum  $?  lef/term                                  ::  jet name
              {std/term kel/@}                          ::  kelvin version
              {ven/term pro/term kel/@}                 ::  vendor and product
              {ven/term pro/term ver/@ kel/@}           ::  all of the above
          ==                                            ::
+$  coil  $:  p/garb                                    ::  name, wet/dry, vary
              q/type                                    ::  context
              r/(pair seminoun (map term tome))         ::  chapters
          ==                                            ::
+$  garb  (trel (unit term) poly vair)                  ::  core
+$  poly  ?(%wet %dry)                                  ::  polarity
+$  foot  $%  {$dry p/hoon}                             ::  dry arm, geometric
              {$wet p/hoon}                             ::  wet arm, generic
          ==                                            ::
+$  link                                                ::  lexical segment
          $%  [%chat p/term]                            ::  |chapter
              [%cone p/aura q/atom]                     ::  %constant
              [%frag p/term]                            ::  .leg
              [%funk p/term]                            ::  +arm
          ==                                            ::
+$  crib  [summary=cord details=(list sect)]            ::
+$  help  [links=(list link) =crib]                     ::  documentation
+$  limb  $@  term                                      ::  wing element
          $%  {%& p/axis}                               ::  by geometry
              {%| p/@ud q/(unit term)}                  ::  by name
          ==                                            ::
            ::  XX more and better sanity
            ::
+$  null  ~                                             ::  null, nil, etc
+$  onyx  (list (pair type foot))                       ::  arm activation
+$  opal                                                ::  limb match
          $%  {%& p/type}                               ::  leg
              {%| p/axis q/(set {p/type q/foot})}       ::  arm
          ==                                            ::
+$  pica  (pair ? cord)                                 ::  & prose, | code
+$  palo  (pair vein opal)                              ::  wing trace, match
+$  plat                                                ::
          $?  %hoon                                     ::
              %type                                     ::
              %nock                                     ::
              %tank                                     ::
          ==                                            ::
+$  pock  (pair axis nock)                              ::  changes
+$  port  (each palo (pair type nock))                  ::  successful match
+$  spec                                                ::  structure definition
          $~  [%base %null]                             ::
          $%  {$base p/base}                            ::  base type
              {$dbug p/spot q/spec}                     ::  set debug
              {$leaf p/term q/@}                        ::  constant atom
              {$like p/wing q/(list wing)}              ::  reference
              {$loop p/term}                            ::  hygienic reference
              {$made p/(pair term (list term)) q/spec}  ::  annotate synthetic
              {$make p/hoon q/(list spec)}              ::  composed spec
              {$name p/term q/spec}                     ::  annotate simple
              {$over p/wing q/spec}                     ::  relative to subject
          ::                                            ::
              {$bsbn p/spec q/spec}                     ::  $>, filter: require
              {$bsbs p/spec q/(map term spec)}          ::  $$, recursion
              {$bsbr p/spec q/hoon}                     ::  $|, verify
              {$bscb p/hoon}                            ::  $_, example
              {$bscl p/{i/spec t/(list spec)}}          ::  $:, tuple
              {$bscn p/{i/spec t/(list spec)}}          ::  $%, head pick
              {$bsdt p/spec q/(map term spec)}          ::  $., read-write core
              {$bsld p/spec q/spec}                     ::  $<, filter: exclude
              {$bshp p/spec q/spec}                     ::  $-, function core
              {$bskt p/spec q/spec}                     ::  $^, cons pick
              {$bsls p/stud q/spec}                     ::  $+, standard
              {$bsnt p/spec q/(map term spec)}          ::  $/, write-only core
              {$bsmc p/hoon}                            ::  $;, manual
              {$bspd p/spec q/hoon}                     ::  $&, repair
              {$bssg p/hoon q/spec}                     ::  $~, default
              {$bstc p/spec q/(map term spec)}          ::  $`, read-only core
              {$bsts p/skin q/spec}                     ::  $=, name
              {$bsvt p/spec q/spec}                     ::  $@, atom pick
              {$bswt p/{i/spec t/(list spec)}}          ::  $?, full pick
              {$bszp p/spec q/(map term spec)}          ::  $!, opaque core
          ==                                            ::
+$  tent                                                ::  model builder
          $%  {%| p/wing q/tent r/(list spec)}          ::  ~(p q r...)
              {%& p/(list wing)}                        ::  a.b:c.d
          ==                                            ::
+$  tiki                                                ::  test case
          $%  {%& p/(unit term) q/wing}                 ::  simple wing
              {%| p/(unit term) q/hoon}                 ::  named wing
          ==                                            ::
+$  skin                                                ::  texture
          $@  =term                                     ::  name/~[term %none]
          $%  [%base =base]                             ::  base match
              [%cell =skin =skin]                       ::  pair
              [%dbug =spot =skin]                       ::  trace
              [%leaf =aura =atom]                       ::  atomic constant
              [%help =help =skin]                       ::  describe
              [%name =term =skin]                       ::  apply label
              [%over =wing =skin]                       ::  relative to
              [%spec =spec =skin]                       ::  cast to
              [%wash depth=@ud]                         ::  strip faces
          ==                                            ::
+$  tome  (pair what (map term hoon))                   ::  core chapter
+$  tope                                                ::  topographic type
  $@  $?  %&                                            ::  cell or atom
          %|                                            ::  atom
      ==                                                ::
  (pair tope tope)                                      ::  cell
++  hoot                                                ::  hoon tools
  |%
  +$  beer  $@(char {~ p/hoon})                    ::  simple embed
  +$  mane  $@(@tas {@tas @tas})                    ::  XML name+space
  +$  manx  $~([[%$ ~] ~] {g/marx c/marl})          ::  dynamic XML node
  +$  marl  (list tuna)                             ::  dynamic XML nodes
  +$  mart  (list {n/mane v/(list beer)})           ::  dynamic XML attrs
  +$  marx  $~([%$ ~] {n/mane a/mart})              ::  dynamic XML tag
  +$  mare  (each manx marl)                        ::  node or nodes
  +$  maru  (each tuna marl)                        ::  interp or nodes
  +$  tuna                                          ::  maybe interpolation
      $~  [[%$ ~] ~]
      $^  manx
      $:  ?($tape $manx $marl $call)
          p/hoon
      ==
  --                                                    ::
+$  hoon                                                ::
  $~  [%zpzp ~]
  $^  {p/hoon q/hoon}                                   ::
  $%                                                    ::
    {$$ p/axis}                                         ::  simple leg
  ::                                                    ::
    {$base p/base}                                      ::  base spec
    {$bust p/base}                                      ::  bunt base
    {$dbug p/spot q/hoon}                               ::  debug info in trace
    {$eror p/tape}                                      ::  assembly error
    {$hand p/type q/nock}                               ::  premade result
    {$note p/note q/hoon}                               ::  annotate
    {$fits p/hoon q/wing}                               ::  underlying ?=
    {$knit p/(list woof)}                               ::  assemble string
    {$leaf p/(pair term @)}                             ::  symbol spec
    {$limb p/term}                                      ::  take limb
    {$lost p/hoon}                                      ::  not to be taken
    {$rock p/term q/*}                                  ::  fixed constant
    {$sand p/term q/*}                                  ::  unfixed constant
    {$tell p/(list hoon)}                               ::  render as tape
    {$tune p/$@(term tune)}                             ::  minimal face
    {$wing p/wing}                                      ::  take wing
    {$yell p/(list hoon)}                               ::  render as tank
    {$xray p/manx:hoot}                                 ::  ;foo; templating
  ::                                            ::::::  cores
    {$brbs sample/(lest term) body/spec}                ::  |$
    {$brcb p/spec q/alas r/(map term tome)}             ::  |_
    {$brcl p/hoon q/hoon}                               ::  |:
    {$brcn p/(unit term) q/(map term tome)}             ::  |%
    {$brdt p/hoon}                                      ::  |.
    {$brkt p/hoon q/(map term tome)}                    ::  |^
    {$brhp p/hoon}                                      ::  |-
    {$brsg p/spec q/hoon}                               ::  |~
    {$brtr p/spec q/hoon}                               ::  |*
    {$brts p/spec q/hoon}                               ::  |=
    {$brvt p/(unit term) q/(map term tome)}             ::  |@
    {$brwt p/hoon}                                      ::  |?
  ::                                            ::::::  tuples
    {$clcb p/hoon q/hoon}                               ::  :_ [q p]
    {$clkt p/hoon q/hoon r/hoon s/hoon}                 ::  :^ [p q r s]
    {$clhp p/hoon q/hoon}                               ::  :- [p q]
    {$clls p/hoon q/hoon r/hoon}                        ::  :+ [p q r]
    {$clsg p/(list hoon)}                               ::  :~ [p ~]
    {$cltr p/(list hoon)}                               ::  :* p as a tuple
  ::                                            ::::::  invocations
    {$cncb p/wing q/(list (pair wing hoon))}            ::  %_
    {$cndt p/hoon q/hoon}                               ::  %.
    {$cnhp p/hoon q/hoon}                               ::  %-
    {$cncl p/hoon q/(list hoon)}                        ::  %:
    {$cntr p/wing q/hoon r/(list (pair wing hoon))}     ::  %*
    {$cnkt p/hoon q/hoon r/hoon s/hoon}                 ::  %^
    {$cnls p/hoon q/hoon r/hoon}                        ::  %+
    {$cnsg p/wing q/hoon r/(list hoon)}                 ::  %~
    {$cnts p/wing q/(list (pair wing hoon))}            ::  %=
  ::                                            ::::::  nock
    {$dtkt p/spec q/hoon}                               ::  .^  nock 11
    {$dtls p/hoon}                                      ::  .+  nock 4
    {$dttr p/hoon q/hoon}                               ::  .*  nock 2
    {$dtts p/hoon q/hoon}                               ::  .=  nock 5
    {$dtwt p/hoon}                                      ::  .?  nock 3
  ::                                            ::::::  type conversion
    {$ktbr p/hoon}                                      ::  ^|  contravariant
    {$ktcn p/hoon}                                      ::  ^%  enter test mode
    {$ktdt p/hoon q/hoon}                               ::  ^.  self-cast
    {$ktls p/hoon q/hoon}                               ::  ^+  expression cast
    {$kthp p/spec q/hoon}                               ::  ^-  structure cast
    {$ktpd p/hoon}                                      ::  ^&  covariant
    {$ktsg p/hoon}                                      ::  ^~  constant
    {$ktts p/skin q/hoon}                               ::  ^=  label
    {$ktwt p/hoon}                                      ::  ^?  bivariant
    {$kttr p/spec}                                      ::  ^*  example
    {$ktcl p/spec}                                      ::  ^:  filter
  ::                                            ::::::  hints
    {$sgbr p/hoon q/hoon}                               ::  ~|  sell on trace
    {$sgcb p/hoon q/hoon}                               ::  ~_  tank on trace
    {$sgcn p/chum q/hoon r/tyre s/hoon}                 ::  ~%  general jet hint
    {$sgnt p/chum q/hoon}                               ::  ~/  function j-hint
    {$sgld p/$@(term {p/term q/hoon}) q/hoon}           ::  ~<  backward hint
    {$sgbn p/$@(term {p/term q/hoon}) q/hoon}           ::  ~>  forward hint
    {$sgbs p/term q/hoon}                               ::  ~$  profiler hit
    {$sgls p/@ q/hoon}                                  ::  ~+  cache/memoize
    {$sgpd p/@ud q/hoon r/hoon}                         ::  ~&  printf/priority
    {$sgts p/hoon q/hoon}                               ::  ~=  don't duplicate
    {$sgwt p/@ud q/hoon r/hoon s/hoon}                  ::  ~?  tested printf
    {$sgzp p/hoon q/hoon}                               ::  ~!  type on trace
  ::                                            ::::::  miscellaneous
    {$mcts p/marl:hoot}                                 ::  ;=  list templating
    {$mccl p/hoon q/(list hoon)}                        ::  ;:  binary to nary
    {$mcnt p/hoon}                                      ::  ;/  [%$ [%$ p ~] ~]
    {$mcgl p/spec q/hoon r/hoon s/hoon}                 ::  ;<  bind
    {$mcsg p/hoon q/(list hoon)}                        ::  ;~  kleisli arrow
    {$mcmc p/spec q/hoon}                               ::  ;;  normalize
  ::                                            ::::::  compositions
    {$tsbr p/spec q/hoon}                               ::  =|  push bunt
    {$tscl p/(list (pair wing hoon)) q/hoon}            ::  =:  q w/ p changes
    {$tsnt p/skin q/hoon r/hoon}                        ::  =/  typed variable
    {$tsmc p/skin q/hoon r/hoon}                        ::  =;  =/(q p r)
    {$tsdt p/wing q/hoon r/hoon}                        ::  =.  r with p as q
    {$tswt p/wing q/hoon r/hoon s/hoon}                 ::  =?  conditional =.
    {$tsld p/hoon q/hoon}                               ::  =<  =>(q p)
    {$tshp p/hoon q/hoon}                               ::  =-  =+(q p)
    {$tsbn p/hoon q/hoon}                               ::  =>  q w/subject p
    {$tskt p/skin q/wing r/hoon s/hoon}                 ::  =^  state machine
    {$tsls p/hoon q/hoon}                               ::  =+  q w/[p subject]
    {$tssg p/(list hoon)}                               ::  =~  hoon stack
    {$tstr p/(pair term (unit spec)) q/hoon r/hoon}     ::  =*  new style
    {$tscm p/hoon q/hoon}                               ::  =,  overload p in q
  ::                                            ::::::  conditionals
    {$wtbr p/(list hoon)}                               ::  ?|  loobean or
    {$wthp p/wing q/(list (pair spec hoon))}            ::  ?-  pick case in q
    {$wtcl p/hoon q/hoon r/hoon}                        ::  ?:  if/then/else
    {$wtdt p/hoon q/hoon r/hoon}                        ::  ?.  ?:(p r q)
    {$wtkt p/wing q/hoon r/hoon}                        ::  ?^  if p is a cell
    {$wtld p/hoon q/hoon}                               ::  ?<  ?:(p !! q)
    {$wtbn p/hoon q/hoon}                               ::  ?>  ?:(p q !!)
    {$wtls p/wing q/hoon r/(list (pair spec hoon))}     ::  ?+  ?-  w/default
    {$wtpd p/(list hoon)}                               ::  ?&  loobean and
    {$wtvt p/wing q/hoon r/hoon}                        ::  ?@  if p is atom
    {$wtsg p/wing q/hoon r/hoon}                        ::  ?~  if p is null
    {$wthx p/skin q/wing}                               ::  ?#  if q matches p
    {$wtts p/spec q/wing}                               ::  ?=  if q matches p
    {$wtzp p/hoon}                                      ::  ?!  loobean not
  ::                                            ::::::  special
    {$zpcm p/hoon q/hoon}                               ::  !,
    {$zpbn p/hoon}                                      ::  !>
    {$zpld p/spec q/hoon}                               ::  !<
    {$zpmc p/hoon q/hoon}                               ::  !;
    {$zpts p/hoon}                                      ::  !=
    {$zpvt p/(list wing) q/hoon r/hoon}                 ::  !@
    {$zpwt p/$@(p/@ {p/@ q/@}) q/hoon}                  ::  !?
    {$zpzp ~}                                           ::  !!
  ==                                                    ::
+$  tyre  (list {p/term q/hoon})                        ::
+$  tyke  (list (unit hoon))                            ::
::                                                      ::::::  virtual nock
+$  nock  $^  {p/nock q/nock}                           ::  autocons
          $%  {$1 p/*}                                  ::  constant
              {$2 p/nock q/nock}                        ::  compose
              {$3 p/nock}                               ::  cell test
              {$4 p/nock}                               ::  increment
              {$5 p/nock q/nock}                        ::  equality test
              {$6 p/nock q/nock r/nock}                 ::  if, then, else
              {$7 p/nock q/nock}                        ::  serial compose
              {$8 p/nock q/nock}                        ::  push onto subject
              {$9 p/@ q/nock}                           ::  select arm and fire
              {$10 p/{p/@ q/nock} q/nock}               ::  edit
              {$11 p/$@(@ {p/@ q/nock}) q/nock}         ::  hint
              {$12 p/nock q/nock}                       ::  grab data from sky
              {$0 p/@}                                  ::  axis select
          ==                                            ::
+$  note                                                ::  type annotation
          $%  {$help p/help}                            ::  documentation
              {$know p/stud}                            ::  global standard
              {$made p/term q/(unit (list wing))}       ::  structure
          ==                                            ::
+$  type  $~  %noun                                     ::
          $@  $?  $noun                                 ::  any nouns
                  $void                                 ::  no noun
              ==                                        ::
          $%  {$atom p/term q/(unit @)}                 ::  atom / constant
              {$cell p/type q/type}                     ::  ordered pair
              {$core p/type q/coil}                     ::  object
              {$face p/$@(term tune) q/type}            ::  namespace
              {$fork p/(set type)}                      ::  union
              {$hint p/(pair type note) q/type}         ::  annotation
              {$hold p/type q/hoon}                     ::  lazy evaluation
          ==                                            ::
+$  tony                                                ::  ++tone done right
          $%  {$0 p/tine q/*}                           ::  success
              {$1 p/(set)}                              ::  blocks
              {$2 p/(list {@ta *})}                     ::  error ~_s
          ==                                            ::
+$  tine                                                ::  partial noun
          $@  ~                                         ::  open
          $%  {%& p/tine q/tine}                        ::  half-blocked
              {%| p/(set)}                              ::  fully blocked
          ==                                            ::
+$  tool  $@(term tune)                                 ::  type decoration
+$  tune                                                ::  complex
          $~  [~ ~]                                     ::
          $:  p/(map term (unit hoon))                  ::  aliases
              q/(list hoon)                             ::  bridges
          ==                                            ::
+$  typo  type                                          ::  old type
+$  vase  {p/type q/*}                                  ::  type-value pair
+$  vise  {p/typo q/*}                                  ::  old vase
+$  vial  ?($read $rite $both $free)                    ::  co/contra/in/bi
+$  vair  ?($gold $iron $lead $zinc)                    ::  in/contra/bi/co
+$  vein  (list (unit axis))                            ::  search trace
+$  sect  (list pica)                                   ::  paragraph
+$  whit                                                ::
          $:  lab/(unit term)                           ::  label
              boy/(unit (pair cord (list sect)))        ::  body
              def/(map term (pair cord (list sect)))    ::  definitions
              use/(set term)                            ::  defs used
          ==                                            ::
+$  what  (unit (pair cord (list sect)))                ::  help slogan/section
+$  wing  (list limb)                                   ::  search path
+$  worm                                                ::  compiler cache
  $:  nes/(set ^)                                       ::  ++nest
      pay/(map (pair type hoon) type)                   ::  ++play
      mit/(map (pair type hoon) (pair type nock))       ::  ++mint
  ==                                                    ::
::
::  +block: abstract identity of resource awaited
::
++  block
  path
::
::  +result: internal interpreter result
::
++  result
  $@(~ seminoun)
::
::  +thunk: fragment constructor
::
++  thunk
  $-(@ud (unit noun))
::
::  +seminoun:
::
++  seminoun
  ::  partial noun; blocked subtrees are ~
  ::
  $~  [[%full ~] ~]
  {mask/stencil data/noun}
::
::  +stencil: noun knowledge map
::
++  stencil
  $%  ::
      ::  %half: noun has partial block substructure
      ::
      [%half left=stencil rite=stencil]
      ::
      ::  %full: noun is either fully complete, or fully blocked
      ::
      [%full blocks=(set block)]
      ::
      ::  %lazy: noun can be generated from virtual subtree
      ::
      [%lazy fragment/axis resolve/thunk]
  ==
::
++  output
  ::  ~: interpreter stopped
  ::
  %-  unit
  $%  ::
      ::  %done: output is complete
      ::
      [%done p/noun]
      ::
      ::  %wait: output is waiting for resources
      ::
      [%wait p/(list block)]
  ==
:: profiling
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
    %ap    ap
    %ut    ut
  ==
|%
::
::::  5aa: new partial nock interpreter
  ::
++  musk  !.                                            ::  nock with block set
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
      done/data.noy
    ::  reduce block set to block list
    ::
    wait/~(tap in blocks)
  ::
  ++  araw
    ::  execute nock on partial subject
    ::
    |=  $:  ::  bus: subject, a partial noun
            ::  fol: formula, a complete noun
            ::
            bus/seminoun
            fol/noun
        ==
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
      [full/~ b.fol]
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
      [full/~ .?(fig)]
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
      ?^(fig ~ [full/~ +(fig)])
    ::
    ::  5; compare
    ::
        {$5 b/* c/*}
      %+  require
        $(fol b.fol)
      |=  ::  hed: left input
          ::
          hed/noun
      %+  require
        ^$(fol c.fol)
      |=  ::  tal: right input
          ::
          tal/noun
      [full/~ =(hed tal)]
    ::
    ::  6; if-then-else
    ::
        {$6 b/* c/* d/*}
      ::  semantic expansion
      ::
      %+  require
        $(fol b.fol)
      |=  ::  fig: boolean
          ::
          fig/noun
      ::  apply proper booleans
      ::
      ?:  =(& fig)  ^$(fol c.fol)
      ?:  =(| fig)  ^$(fol d.fol)
      ::  stop on bad test
      ::
      ~
    ::
    ::  7; composition
    ::
        {$7 b/* c/*}
      ::  one: input
      ::
      =+  one=$(fol b.fol)
      ::  propagate stop
      ::
      ?~  one  ~
      ::  complete composition
      ::
      $(fol c.fol, bus one)
    ::
    ::  8; introduction
    ::
        {$8 b/* c/*}
      ::  one: input
      ::
      =+  one=$(fol b.fol)
      ::  propagate stop
      ::
      ?~  one  ~
      ::  complete introduction
      ::
      $(fol c.fol, bus (combine one bus))
    ::
    ::  9; invocation
    ::
        {$9 b/* c/*}
      ::  semantic expansion
      ::
      ?^  b.fol  ~
      ::  one: core
      ::
      =+  one=$(fol c.fol)
      ::  propagate stop
      ::
      ?~  one  ~
      ::  if core is constant
      ::
      ?:  ?=([[%full ~] *] one)
        ::  then call virtual nock directly
        ::
        =+  (mack data.one [%9 b.fol %0 1])
        ::  propagate stop
        ::
        ?~  -  ~
        ::  produce result
        ::
        [[%full ~] u.-]
      ::  else complete call
      ::
      %+  require
        ::  retrieve formula
        ::
        (fragment b.fol one)
      ::  continue
      ::
      |=(noun ^$(bus one, fol +<))
    ::
    ::  10; edit
    ::
        {$10 {b/@ c/*} d/*}
      ::  tar:  target of edit
      ::
      =+  tar=$(fol d.fol)
      ::  propagate stop
      ::
      ?~  tar  ~
      ::  inn:  inner value
      ::
      =+  inn=$(fol c.fol)
      ::  propagate stop
      ::
      ?~  inn  ~
      (mutate b.fol inn tar)
    ::
    ::  11; static hint
    ::
        {$11 @ c/*}
      ::  ignore hint
      ::
      $(fol c.fol)
    ::
    ::  11; dynamic hint
    ::
        {$11 {b/* c/*} d/*}
      ::  noy: dynamic hint
      ::
      =+  noy=$(fol c.fol)
      ::  propagate stop
      ::
      ?~  noy  ~
      ::  if hint is a fully computed trace
      ::
      ?:  &(?=($spot b.fol) ?=([[%full ~] *] noy))
        ::  compute within trace
        ::
        ~_((show %o +.noy) $(fol d.fol))
      ::  else ignore hint
      ::
      $(fol d.fol)
    ==
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
    ~+
    ^-  output
    ::  simplify result
    ::
    (abet (araw bus fol))
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
    ?.  ?&  &(?=($full -.mask.hed) ?=($full -.mask.tal))
            =(=(~ blocks.mask.hed) =(~ blocks.mask.tal))
        ==
      ::  default merge
      ::
      [half/[mask.hed mask.tal] [data.hed data.tal]]
    ::  both sides total
    ::
    ?:  =(~ blocks.mask.hed)
      ::  both sides are complete
      ::
      [full/~ data.hed data.tal]
    ::  both sides are blocked
    ::
    [full/(~(uni in blocks.mask.hed) blocks.mask.tal) ~]
  ::
  ++  complete
    ::  complete any laziness
    ::
    |=  bus/seminoun
    ^-  seminoun
    ?-  -.mask.bus
      $full  bus
      $lazy  ::  fragment 1 is the whole thing
             ::
             ?:  =(1 fragment.mask.bus)
               ::  blocked; we can't get fragment 1 while compiling it
               ::
               [[%full [~ ~ ~]] ~]
             ::  execute thunk
             ::
             =+  (resolve.mask.bus fragment.mask.bus)
             ::  if product is nil
             ::
             ?~  -
               ::  then blocked
               ::
               [[%full [~ ~ ~]] ~]
             ::  else use value
             ::
             [[%full ~] u.-]
      $half  ::  recursive descent
             ::
             %+  combine
               $(bus [left.mask.bus -.data.bus])
             $(bus [rite.mask.bus +.data.bus])
    ==
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
    ::  now: top of axis (2 or 3)
    ::  lat: rest of axis
    ::
    =+  [now=(cap axe) lat=(mas axe)]
    ?-  -.mask.bus
      %lazy  ::  propagate laziness
             ::
             bus(fragment.mask (peg fragment.mask.bus axe))
    ::
      %full  ::  if fully blocked, produce self
             ::
             ?^  blocks.mask.bus  bus
             ::  descending into atom, stop
             ::
             ?@  data.bus  ~
             ::  descend into complete cell
             ::
             $(axe lat, bus [full/~ ?:(=(2 now) -.data.bus +.data.bus)])
    ::
      %half  ::  descend into partial cell
             ::
             %=  $
               axe  lat
               bus  ?:  =(2 now)
                      [left.mask.bus -.data.bus]
                    [rite.mask.bus +.data.bus]
    ==       ==
  ::
  ++  mutate
    ::  change a single axis in a seminoun
    ::
    |=  $:  ::  axe: axis within big to change
            ::  lit: (little) seminoun to insert within big at axe
            ::  big: seminoun to mutate
            ::
            axe/@
            lit/seminoun
            big/seminoun
        ==
    ^-  result
    ::  stop on zero axis
    ::
    ?~  axe  ~
    ::  edit root of big means discard it
    ::
    ?:  =(1 axe)  lit
    ::  decompose axis into path of head-tail
    ::
    |-  ^-  result
    ?:  =(2 axe)
      ::  mutate head of cell
      ::
      =+  tal=(fragment 3 big)
      ::  propagate stop
      ::
      ?~  tal  ~
      (combine lit tal)
    ?:  =(3 axe)
      ::  mutate tail of cell
      ::
      =+  hed=(fragment 2 big)
      ::  propagate stop
      ::
      ?~  hed  ~
      (combine hed lit)
    ::  deeper axis: keep one side of big and
    ::  recurse into the other with smaller axe
    ::
    =+  mor=(mas axe)
    =+  hed=(fragment 2 big)
    ::  propagate stop
    ::
    ?~  hed  ~
    =+  tal=(fragment 3 big)
    ::  propagate stop
    ::
    ?~  tal  ~
    ?:  =(2 (cap axe))
      ::  recurse into the head
      ::
      =+  mut=$(big hed, axe mor)
      ::  propagate stop
      ::
      ?~  mut  ~
      (combine mut tal)
    ::  recurse into the tail
    ::
    =+  mut=$(big tal, axe mor)
    ::  propagate stop
    ::
    ?~  mut  ~
    (combine hed mut)
  ::
  ++  require
    ::  require complete intermediate step
    ::
    |=  $:  noy/result
            yen/$-(* result)
        ==
    ^-  result
    ::  propagate stop
    ::
    ?~  noy  ~
    ::  suppress laziness
    ::
    =/  bus/seminoun  (complete noy)
    ?<  ?=($lazy -.mask.bus)
    ::  if partial block, squash blocks and stop
    ::
    ?:  ?=($half -.mask.bus)  [full/(squash mask.bus) ~]
    ::  if full block, propagate block
    ::
    ?:  ?=(^ blocks.mask.bus)  [mask.bus ~]
    ::  otherwise use complete noun
    ::
    (yen data.bus)
  ::
  ++  squash
    ::  convert stencil to block set
    ::
    |=  tyn/stencil
    ^-  (set block)
    ?-  -.tyn
      $lazy  $(tyn -:(complete tyn ~))
      $full  blocks.tyn
      $half  (~(uni in $(tyn left.tyn)) $(tyn rite.tyn))
    ==
  --
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
++  hint
  |=  {p/(pair type note) q/type}
  ^-  type
  ?:  =(%void q)  %void
  ?:  =(%noun q)  %noun
  [%hint p q]
::
++  face                                                ::  make %face type
  ~/  %face
  |=  {giz/$@(term tune) der/type}
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
    ?:  ?=({* ~ ~} lez)  n.lez
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
      {$11 *}  $(nug q.nug)
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
  ::  this optimization can remove crashes which are essential
  ::
  ::  ?:  ?=([[%0 *] [%0 *]] +<)
  ::  ?:  ?&(=(+(p.vur) p.sed) =((div p.vur 2) (div p.sed 2)))
  ::    [%0 (div p.vur 2)]
  ::  [vur sed]
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
  ?:  =(bos nif)  bos
  ?:  =([%0 0] bos)  nif
  ?:  =([%0 0] nif)  bos
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
  ?:  =([%0 0] dyr)  dyr
  [%6 dyr [%1 1] [%1 0]]
::
++  flor                                                ::  loobean  |
  ~/  %flor
  |=  {bos/nock nif/nock}
  ^-  nock
  ?:  =(bos nif)  bos
  ?:  =([%0 0] bos)  nif
  ?:  =([%0 0] nif)  bos
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
  |=  [a=axis pac=(list (pair axis nock))]
  |^  =/  rel=(map axis nock)  (roll pac insert)
      =/  ord=(list axis)      (sort ~(tap in ~(key by rel)) gth)
      |-  ^-  nock
      ?~  ord
        [%0 a]
      =/  b=axis  i.ord
      =/  c=nock  (~(got by rel) b)
      =/  d=nock  $(ord t.ord)
      [%10 [b c] d]
  ::
  ++  contains
    |=  [container=axis contained=axis]
    ^-  ?
    =/  big=@    (met 0 container)
    =/  small=@  (met 0 contained)
    ?:  (lte small big)  |
    =/  dif=@  (sub small big)
    =(container (rsh 0 dif contained))
  ::
  ++  parent
    |=  a=axis
    `axis`(rsh 0 1 a)
  ::
  ++  sibling
    |=  a=axis
    ^-  axis
    ?~  (mod a 2)
      +(a)
    (dec a)
  ::
  ++  insert
    |=  [e=[axe=axis fol=nock] n=(map axis nock)]
    ^-  (map axis nock)
    ?:  =/  a=axis  axe.e
        |-  ^-  ?
        ?:  =(1 a)  |
        ?:  (~(has by n) a)
          &
        $(a (parent a))
      ::  parent already in
      n
    =.  n
      ::  remove children
      %+  roll  ~(tap by n)
      |=  [[axe=axis fol=nock] m=_n]
      ?.  (contains axe.e axe)  m
      (~(del by m) axe)
    =/  sib  (sibling axe.e)
    =/  un   (~(get by n) sib)
    ?~  un   (~(put by n) axe.e fol.e)
    ::  replace sibling with parent
    %=  $
      n  (~(del by n) sib)
      e  :-  (parent sib)
         ?:  (gth sib axe.e)
           (cons fol.e u.un)
         (cons u.un fol.e)
    ==
  --
::
++  jock
  |=  rad/?
  |=  lot/coin  ^-  hoon
  ?-    -.lot
      ~
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
  |=  {cog/term dab/(map term hoon)}
  =+  axe=1
  |-  ^-  (unit {p/axis q/hoon})
  ?-  dab
      ~  ~
  ::
      {* ~ ~}
    ?:(=(cog p.n.dab) [~ axe q.n.dab] ~)
  ::
      {* ~ *}
    ?:  =(cog p.n.dab)
      [~ (peg axe 2) q.n.dab]
    ?:  (gor cog p.n.dab)
      ~
    $(axe (peg axe 3), dab r.dab)
  ::
      {* * ~}
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
  |=  {cog/term dom/(map term tome)}
  =+  axe=1
  |-  ^-  (unit {p/axis q/hoon})
  ?-  dom
      ~  ~
  ::
      {* ~ ~}
    %+  bind  (look cog q.q.n.dom)
    |=((pair axis hoon) [(peg axe p) q])
  ::
      {* ~ *}
    =+  yep=(look cog q.q.n.dom)
    ?^  yep
      [~ (peg (peg axe 2) p.u.yep) q.u.yep]
    $(axe (peg axe 3), dom r.dom)
  ::
      {* * ~}
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
    ?.  &(?=(%| -.tik) ?=(~ p.tik))  gen
    [%tsbn [%$ 3] gen]
  ::
  ++  teal
    |=  mod/spec
    ^-  spec
    [%over [%& 3]~ mod]
  ::
  ++  tele
    |=  syn/skin
    ^-  skin
    [%over [%& 3]~ syn]
  ::
  ++  gray
    |=  gen/hoon
    ^-  hoon
    ?-  -.tik
      %&  ?~(p.tik gen [%tstr [u.p.tik ~] [%wing q.tik] gen])
      %|  [%tsls ?~(p.tik q.tik [%ktts u.p.tik q.tik]) gen]
    ==
  ::
  ++  puce
    ^-  wing
    ?-  -.tik
      %&  ?~(p.tik q.tik [u.p.tik ~])
      %|  [[%& 2] ~]
    ==
  ::
  ++  wthp  |=  opt/(list (pair spec hoon))
            %+  gray  %wthp
            [puce (turn opt |=({a/spec b/hoon} [a (blue b)]))]
  ++  wtkt  |=({sic/hoon non/hoon} (gray [%wtkt puce (blue sic) (blue non)]))
  ++  wtls  |=  {gen/hoon opt/(list (pair spec hoon))}
            %+  gray  %wtls
            [puce (blue gen) (turn opt |=({a/spec b/hoon} [a (blue b)]))]
  ++  wtvt  |=({sic/hoon non/hoon} (gray [%wtvt puce (blue sic) (blue non)]))
  ++  wtsg  |=({sic/hoon non/hoon} (gray [%wtsg puce (blue sic) (blue non)]))
  ++  wthx  |=(syn/skin (gray [%wthx (tele syn) puce]))
  ++  wtts  |=(mod/spec (gray [%wtts (teal mod) puce]))
  --
::
++  ax
  =+  :*  ::  dom: axis to home
          ::  hay: wing to home
          ::  cox: hygienic context
          ::  bug: debug annotations
          ::  def: default expression
          ::
          dom=`axis`1
          hay=*wing
          cox=*(map term spec)
          bug=*(list spot)
          nut=*(unit note)
          def=*(unit hoon)
      ==
  |_  {fab/? mod/spec}
  ::
  ++  autoname
    ::  derive name from spec
    ::
    |-  ^-  (unit term)
    ?-  -.mod
      $base  ?.(?=([%atom *] p.mod) ~ ?:(=(%$ p.p.mod) `%atom `p.p.mod))
      $dbug  $(mod q.mod)
      $leaf  `p.mod
      $loop  `p.mod
      $like  ?~(p.mod ~ ?^(i.p.mod ?:(?=(%& -.i.p.mod) ~ q.i.p.mod) `i.p.mod))
      $make  ~(name ap p.mod)
      $made  $(mod q.mod)
      $over  $(mod q.mod)
      $name  $(mod q.mod)
    ::
      $bsbs  $(mod p.mod)
      $bsbr  $(mod p.mod)
      $bscb  ~(name ap p.mod)
      $bscl  $(mod i.p.mod)
      $bscn  $(mod i.p.mod)
      $bsdt  ~
      $bsld  $(mod q.mod)
      $bsbn  $(mod q.mod)
      $bshp  $(mod p.mod)
      $bskt  $(mod q.mod)
      $bsls  $(mod q.mod)
      $bsnt  ~
      $bsmc  ~(name ap p.mod)
      $bspd  $(mod p.mod)
      $bssg  $(mod q.mod)
      $bstc  ~
      $bsts  $(mod q.mod)
      $bsvt  $(mod q.mod)
      $bswt  $(mod i.p.mod)
      $bszp  ~
    ==
  ++  hint
    |=  not/note
    ^+  +>
    ?>(?=(~ nut) +>.$(nut `not))
  ::
  ++  function
    ::  construct a function example
    ::
    |=  {fun/spec arg/spec}
    ^-  hoon
    ::  minimal context as subject
    ::
    :+  %tsbn
      ::  context is example of both specs
      ::
      [example:clear(mod fun) example:clear(mod arg)]
    ::  produce an %iron (contravariant) core
    ::
    :-  %ktbr
    ::  make an actual gate
    ::
    :+  %brcl
      [%$ 2]
    [%$ 15]
  ::
  ++  interface
    ::  construct a core example
    ::
    |=  {variance/vair payload/spec arms/(map term spec)}
    ^-  hoon
    ::  attach proper variance control
    ::
    =-  ?-  variance
          %gold  -
          %lead  [%ktwt -]
          %zinc  [%ktpd -]
          %iron  [%ktbr -]
        ==
    ^-  hoon
    :+  %tsbn  example:clear(mod payload)
    :+  %brcn  ~
    =-  [[%$ ~ -] ~ ~]
    %-  ~(gas by *(map term hoon))
    %+  turn
      ~(tap by arms)
    |=  [=term =spec]
    ::
    ::  note that we *don't* make arm specs in an interface
    ::  hygienic -- we leave them in context, to support
    ::  maximum programmer flexibility
    ::
    [term example:clear(mod spec)]
  ::
  ++  home
    ::  express a hoon against the original subject
    ::
    |=  gen/hoon
    ^-  hoon
    =/  ,wing
        ?:  =(1 dom)
          hay
        (weld hay `wing`[[%& dom] ~])
    ?~  -  gen
    [%tsbn [%wing -] gen]
  ::
  ++  clear
    ::  clear annotations
    ^+  .
    .(bug ~, def ~, nut ~)
  ::
  ++  basal
    ::  example base case
    ::
    |=  bas/base
    ?-    bas
    ::
        {$atom *}
      ::  we may want sped
      ::
      [%sand p.bas ?:(=(%da p.bas) ~2000.1.1 0)]
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
        $flag
      ::  comparison produces boolean type
      ::
      =+([%rock %$ 0] [%ktls [%dtts - -] -])
    ::
        $null
      [%rock %n 0]
    ::
        $void
      [%zpzp ~]
    ==
  ::
  ++  unfold
    |=  [fun/hoon arg/(list spec)]
    ^-  hoon
    [%cncl fun (turn arg |=(spec ktcl/+<))]
  ::
  ++  unreel
    |=  [one/wing res/(list wing)]
    ^-  hoon
    ?~(res [%wing one] [%tsld [%wing one] $(one i.res, res t.res)])
  ::
  ++  descend
    ::  record an axis to original subject
    ::
    |=  axe/axis
    +>(dom (peg axe dom))
  ::
  ++  decorate
    ::  apply documentation to expression
    ::
    |=  gen/hoon
    ^-  hoon
    =-  ?~(nut - [%note u.nut -])
    ^-  hoon
    |-  ^-  hoon
    ?~(bug gen [%dbug i.bug $(bug t.bug)])
  ::
  ++  pieces
    ::  enumerate tuple wings
    ::
    |=  =(list term)
    ^-  (^list wing)
    (turn list |=(=term `wing`[term ~]))
  ::
  ++  spore
    ::  build default sample
    ::
    ^-  hoon
    ::  sample is always typeless
    ::
    :+  %ktls
      [%bust %noun]
    ::  consume debugging context
    ::
    %-  decorate
    ::  use home as subject
    ::
    %-  home
    ::  if default is set, use it
    ::
    ?^  def  u.def
    ::  else map structure to expression
    ::
    ~+
    |-  ^-  hoon
    ?-  mod
      {$base *}  ?:(=(%void p.mod) [%rock %n 0] (basal p.mod))
      {$bsbs *}  ::  track hygienic recursion points lexically
                 ::
                 %=  $
                   mod  p.mod
                   cox  ::  merge lexically and don't forget %$
                        ::
                        (~(put by ^+(cox (~(uni by cox) q.mod))) %$ p.mod)
                 ==
      {$dbug *}  [%dbug p.mod $(mod q.mod)]
      {$leaf *}  [%rock p.mod q.mod]
      {$loop *}  ~|([%loop p.mod] $(mod (~(got by cox) p.mod)))
      {$like *}  $(mod bsmc/(unreel p.mod q.mod))
      {$made *}  $(mod q.mod)
      {$make *}  $(mod bsmc/(unfold p.mod q.mod))
      {$name *}  $(mod q.mod)
      {$over *}  $(hay p.mod, mod q.mod)
    ::
      {$bsbr *}  $(mod p.mod)
      {$bscb *}  [%rock %n 0]
      {$bscl *}  |-  ^-  hoon
                 ?~  t.p.mod  ^$(mod i.p.mod)
                 :-  ^$(mod i.p.mod)
                 $(i.p.mod i.t.p.mod, t.p.mod t.t.p.mod)
      {$bscn *}  ::  use last entry
                 ::
                 |-  ^-  hoon
                 ?~  t.p.mod  ^$(mod i.p.mod)
                 $(i.p.mod i.t.p.mod, t.p.mod t.t.p.mod)
      {$bshp *}  ::  see under %bscb
                 ::
                 [%rock %n 0]
      {$bsld *}  $(mod q.mod)
      {$bsbn *}  $(mod q.mod)
      {$bskt *}  $(mod q.mod)
      {$bsls *}  $(mod q.mod)
      {$bsmc *}  ::  borrow sample
                 ::
                 [%tsld [%$ 6] p.mod]
      {$bspd *}  $(mod p.mod)
      {$bssg *}  [%kthp q.mod p.mod]
      {$bsts *}  [%ktts p.mod $(mod q.mod)]
      {$bsvt *}  $(mod p.mod)
      {$bswt *}  ::  use last entry
                 ::
                 |-  ^-  hoon
                 ?~  t.p.mod  ^$(mod i.p.mod)
                 $(i.p.mod i.t.p.mod, t.p.mod t.t.p.mod)
      {$bsdt *}  [%rock %n 0]
      {$bsnt *}  [%rock %n 0]
      {$bstc *}  [%rock %n 0]
      {$bszp *}  [%rock %n 0]
    ==
  ::
  ++  example
    ::  produce a correctly typed default instance
    ::
    ~+
    ^-  hoon
    ?+  mod
      ::  in the general case, make and analyze a spore
      ::
      :+  %tsls
        spore
      ~(relative analyze:(descend 3) 2)
    ::
      {$base *}  (decorate (basal p.mod))
      {$dbug *}  example(mod q.mod, bug [p.mod bug])
      {$leaf *}  (decorate [%rock p.mod q.mod])
      {$like *}  example(mod bsmc/(unreel p.mod q.mod))
      {$loop *}  [%limb p.mod]
      {$made *}  example(mod q.mod, nut `made/[p.p.mod `(pieces q.p.mod)])
      {$make *}  example(mod bsmc/(unfold p.mod q.mod))
      {$name *}  example(mod q.mod, nut `made/[p.mod ~])
      {$over *}  example(hay p.mod, mod q.mod)
    ::
      {$bscb *}  (decorate (home p.mod))
      {$bscl *}  %-  decorate
                 |-  ^-  hoon
                 ?~  t.p.mod
                   example:clear(mod i.p.mod)
                 :-  example:clear(mod i.p.mod)
                 example:clear(i.p.mod i.t.p.mod, t.p.mod t.t.p.mod)
      {$bshp *}  (decorate (function:clear p.mod q.mod))
      {$bsmc *}  (decorate (home [%tsld [%limb %$] p.mod]))
      {$bssg *}  [%ktls example(mod q.mod) (home p.mod)]
      {$bsls *}  (decorate example(mod q.mod))
      {$bsts *}  (decorate [%ktts p.mod example:clear(mod q.mod)])
      {$bsdt *}  (decorate (home (interface %gold p.mod q.mod)))
      {$bsnt *}  (decorate (home (interface %iron p.mod q.mod)))
      {$bszp *}  (decorate (home (interface %lead p.mod q.mod)))
      {$bstc *}  (decorate (home (interface %zinc p.mod q.mod)))
    ==
  ::
  ++  factory
    ::  make a normalizing gate (mold)
    ::
    ^-  hoon
    ::  process annotations outside construct, to catch default
    ::
    ?:  ?=($dbug -.mod)  factory(mod q.mod, bug [p.mod bug])
    ?:  ?=($bssg -.mod)  factory(mod q.mod, def `[%kthp q.mod p.mod])
    ^-  hoon
    ::  if we recognize an indirection
    ::
    ?:  &(=(~ def) ?=(?(%bsmc %like %loop %make) -.mod))
      ::  then short-circuit it
      ::
      %-  decorate
      %-  home
      ?-  -.mod
        %bsmc  p.mod
        %like  (unreel p.mod q.mod)
        %loop  [%limb p.mod]
        %make  (unfold p.mod q.mod)
      ==
    ::  else build a gate
    ::
    :+  %brcl
      [%ktsg spore]
    ~(relative analyze:(descend 7) 6)
  ::
  ++  analyze
    ::  normalize a fragment of the subject
    ::
    |_  $:  ::  axe: axis to fragment
            ::
            axe/axis
        ==
    ++  basic
      |=  bas/base
      ^-  hoon
      ?-    bas
          {%atom *}
        :+  %ktls  example
        ^-  hoon
        :^    %zpvt
            [[[%| 0 `%ruth] ~] ~]
          [%cnls [%limb %ruth] [%sand %ta p.bas] fetch]
        [%wtvt fetch-wing fetch [%zpzp ~]]
      ::
          $cell
        :+  %ktls  example
        =+  fetch-wing
        :-  [%wing [[%& %2] -]]
            [%wing [[%& %3] -]]
      ::
          $flag
        :^    %wtcl
            [%dtts [%rock %$ &] [%$ axe]]
          [%rock %f &]
        :+  %wtbn
          [%dtts [%rock %$ |] [%$ axe]]
        [%rock %f |]
      ::
          $noun
        fetch
      ::
          $null
        :+  %wtbn
          [%dtts [%bust %noun] [%$ axe]]
        [%rock %n ~]
      :::
          $void
        [%zpzp ~]
      ==
    ++  clear
      .(..analyze ^clear)
    ::
    ++  fetch
      ::  load the fragment
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
              one/spec
              rep/(list spec)
          ==
      ^-  hoon
      ::  if no other choices, construct head
      ::
      ?~  rep  relative:clear(mod one)
      ::  build test
      ::
      :^    %wtcl
          ::  if we fit the type of this choice
          ::
          [%fits example:clear(mod one) fetch-wing]
        ::  build with this choice
        ::
        relative:clear(mod one)
      ::  continue through loop
      ::
      $(one i.rep, rep t.rep)
    ::
    ++  switch
      |=  $:  ::  one: first format
              ::  two: more formats
              ::
              one/spec
              rep/(list spec)
          ==
      |-  ^-  hoon
      ::  if no other choices, construct head
      ::
      ?~  rep  relative:clear(mod one)
      ::  fin: loop completion
      ::
      =/  fin/hoon  $(one i.rep, rep t.rep)
      ::  interrogate this instance
      ::
      :^    %wtcl
          ::  test if the head matches this wing
          ::
          :+  %fits
            [%tsld [%$ 2] example:clear(mod one)]
          fetch-wing(axe (peg axe 2))
        ::  if so, use this form
        ::
        relative:clear(mod one)
      ::  continue in the loop
      ::
      fin
    ::
    ++  relative
      ::  local constructor
      ::
      ~+
      ^-  hoon
      ?-    mod
      ::
      ::  base
      ::
          {$base *}
        (decorate (basic:clear p.mod))
      ::
      ::  debug
      ::
          {$dbug *}
        relative(mod q.mod, bug [p.mod bug])
      ::
      ::  constant
      ::
          {$leaf *}
        %-  decorate
        :+  %wtbn
          [%dtts fetch [%rock %$ q.mod]]
        [%rock p.mod q.mod]
      ::
      ::  composite
      ::
          {$make *}
        relative(mod bsmc/(unfold p.mod q.mod))
      ::
      ::  indirect
      ::
          {$like *}
        relative(mod bsmc/(unreel p.mod q.mod))
      ::
      ::  loop
      ::
          {$loop *}
        (decorate [%cnhp [%limb p.mod] fetch])
      ::
      ::  simple named structure
      ::
          {$name *}
        relative(mod q.mod, nut `made/[p.mod ~])
      ::
      ::  synthetic named structure
      ::
          {$made *}
        relative(mod q.mod, nut `made/[p.p.mod `(pieces q.p.mod)])
      ::
      ::  subjective
      ::
          {$over *}
        relative(hay p.mod, mod q.mod)
      ::
      ::  recursive, $$
      ::
          {$bsbs *}
        ::
        ::  apply semantically
        ::
        :+  %brkt
          relative(mod p.mod, dom (peg 3 dom))
        =-  [[%$ ~ -] ~ ~]
        %-  ~(gas by *(map term hoon))
        ^-  (list (pair term hoon))
        %+  turn
          ~(tap by q.mod)
        |=  [=term =spec]
        [term relative(mod spec, dom (peg 3 dom))]
      ::
      ::  normalize, $&
      ::
          {$bspd *}
        ::  push the raw result
        ::
        :+  %tsls  relative(mod p.mod)
        ::  push repair function
        ::
        :+  %tsls
          [%tsbn $/3 q.mod]
        ::  push repaired product
        ::
        :+  %tsls
          [%cnhp $/2 $/6]
        ::  sanity-check repaired product
        ::
        :+  %wtbn
          ::  either
          ::
          :~  %wtbr
              ::  the repair did not change anything
              ::
              [%dtts $/14 $/2]
              ::  when we fix it again, it stays fixed
              ::
              [%dtts $/2 [%cnhp $/6 $/2]]
          ==
        $/2
      ::
      ::  verify, $|
      ::
          {$bsbr *}
        ^-  hoon
        ::  push the raw product
        ::
        :+  %tsls  relative(mod p.mod)
        ^-  hoon
        ::  assert
        ::
        :+  %wtbn
          ::  run the verifier
          ::
          [%cnhp [%tsbn $/3 q.mod] $/2]
        ::  produce verified product
        ::
        $/2
      ::
      ::  special, $_
      ::
          {$bscb *}
        (decorate (home p.mod))
      ::
      ::  switch, $%
      ::
          {$bscn *}
        (decorate (switch i.p.mod t.p.mod))
      ::
      ::  tuple, $:
      ::
          {$bscl *}
        %-  decorate
        |-  ^-  hoon
        ?~  t.p.mod
          relative:clear(mod i.p.mod)
        :-  relative:clear(mod i.p.mod, axe (peg axe 2))
        %=  relative
          i.p.mod  i.t.p.mod
          t.p.mod  t.t.p.mod
          axe      (peg axe 3)
        ==
      ::
      ::  exclude, $<
      ::
          {$bsld *}
        :+  %tsls
          relative:clear(mod q.mod)
        :+  %wtld
          [%wtts [%over ~[&/3] p.mod] ~[&/4]]
        $/2
      ::
      ::  require, $>
      ::
          {$bsbn *}
        :+  %tsls
          relative:clear(mod q.mod)
        :+  %wtbn
          [%wtts [%over ~[&/3] p.mod] ~[&/4]]
        $/2
      ::
      ::  function
      ::
          {$bshp *}
        %-  decorate
        =/  fun  (function:clear p.mod q.mod)
        ?^  def
          [%ktls fun u.def]
        fun
      ::
      ::  bridge, $^
      ::
          {$bskt *}
        %-  decorate
        :^    %wtcl
            [%dtwt fetch(axe (peg axe 2))]
          relative:clear(mod p.mod)
        relative:clear(mod q.mod)
      ::
      ::  synthesis, $;
      ::
          {$bsmc *}
        (decorate [%cncl (home p.mod) fetch ~])
      ::
      ::  default
      ::
          {$bssg *}
        relative(mod q.mod, def `[%kthp q.mod p.mod])
      ::
      ::  choice, $?
      ::
          {$bswt *}
        (decorate (choice i.p.mod t.p.mod))
      ::
      ::  name, $=
      ::
          {$bsts *}
        [%ktts p.mod relative(mod q.mod)]
      ::
      ::  branch, $@
      ::
          {$bsvt *}
        %-  decorate
        :^    %wtcl
            [%dtwt fetch]
          relative:clear(mod q.mod)
        relative:clear(mod p.mod)
      ::
        {$bsls *}  relative(mod q.mod)
        {$bsdt *}  (decorate (home (interface %gold p.mod q.mod)))
        {$bsnt *}  (decorate (home (interface %iron p.mod q.mod)))
        {$bszp *}  (decorate (home (interface %lead p.mod q.mod)))
        {$bstc *}  (decorate (home (interface %zinc p.mod q.mod)))
      ==
    --
  --
::
++  ap                                                  ::  hoon engine
  ~%    %ap
      +>+
    ==
      %open  open
      %rake  rake
    ==
  =+  fab=`?`&
  |_  gen/hoon
  ::
  ++  grip
    |=  =skin
    =|  rel/wing
    |-  ^-  hoon
    ?-    skin
        @
      [%tsld [%tune skin] gen]
        [%base *]
      ?:  ?=(%noun base.skin)
        gen
      [%kthp skin gen]
    ::
        [%cell *]
      =+  haf=~(half ap gen)
      ?^  haf
        :-  $(skin skin.skin, gen p.u.haf)
        $(skin ^skin.skin, gen q.u.haf)
      :+  %tsls
        gen
      :-  $(skin skin.skin, gen [%$ 4])
      $(skin ^skin.skin, gen [%$ 5])
    ::
        [%dbug *]
      [%dbug spot.skin $(skin skin.skin)]
    ::
        [%leaf *]
      [%kthp skin gen]
    ::
        [%help *]
      [%note [%help help.skin] $(skin skin.skin)]
    ::
        [%name *]
      [%tsld [%tune term.skin] $(skin skin.skin)]
    ::
        [%over *]
      $(skin skin.skin, rel (weld wing.skin rel))
    ::
        [%spec *]
      :+  %kthp
        ?~(rel spec.skin [%over rel spec.skin])
      $(skin skin.skin)
    ::
        [%wash *]
      :+  %tsld
        :-  %wing
        |-  ^-  wing
        ?:  =(0 depth.skin)  ~
        [[%| 0 ~] $(depth.skin (dec depth.skin))]
      gen
    ==
  ::
  ++  name
    |-  ^-  (unit term)
    ?+  gen  ~
      {$wing *}  ?~  p.gen  ~
                 ?^  i.p.gen
                   ?:(?=(%& -.i.p.gen) ~ q.i.p.gen)
                 `i.p.gen
      {$limb *}  `p.gen
      {$dbug *}  $(gen ~(open ap gen))
      {$tsld *}  $(gen ~(open ap gen))
      {$tsbn *}  $(gen q.gen)
    ==
  ::
  ++  feck
    |-  ^-  (unit term)
    ?-  gen
      {$sand $tas @}  [~ q.gen]
      {$dbug *}       $(gen q.gen)
      *               ~
    ==
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
::
  ++  half
    |-  ^-  (unit (pair hoon hoon))
    ?+  gen  ~
      {^ *}       `[p.gen q.gen]
      {$dbug *}   $(gen q.gen)
      {$clcb *}   `[q.gen p.gen]
      {$clhp *}   `[p.gen q.gen]
      {$clkt *}   `[p.gen %clls q.gen r.gen s.gen]
      {$clsg *}   ?~(p.gen ~ `[i.p.gen %clsg t.p.gen])
      {$cltr *}   ?~  p.gen  ~
                  ?~(t.p.gen $(gen i.p.gen) `[i.p.gen %cltr t.p.gen])
    ==
::::
  ::  +flay: hoon to skin
  ::
  ++  flay
    |-  ^-  (unit skin)
    ?+    gen
      =+(open ?:(=(- gen) ~ $(gen -)))
    ::
        [^ *]
      =+  [$(gen p.gen) $(gen q.gen)]
      ?~(-< ~ ?~(-> ~ `[%cell -<+ ->+]))
    ::
        [%base *]
      `gen
    ::
        [%rock *]
      ?@(q.gen `[%leaf p.gen q.gen] ~)
    ::
        [%cnts [@ ~] ~]
      `i.p.gen
    ::
        [%tsbn *]
      %+  biff  reek(gen p.gen)
      |=  =wing
      (bind ^$(gen q.gen) |=(=skin [%over wing skin]))
    ::
        [%limb @]
      `p.gen
    ::
      ::  [%rock *]
      ::  [%spec %leaf q.gen q.gen]
    ::
        [%note [%help *] *]
      (bind $(gen q.gen) |=(=skin [%help p.p.gen skin]))
    ::
        [%wing *]
      ?:  ?=([@ ~] p.gen)
        `i.p.gen
      =/  depth  0
      |-  ^-  (unit skin)
      ?~  p.gen  `[%wash depth]
      ?.  =([%| 0 ~] i.p.gen)  ~
      $(p.gen t.p.gen)
    ::
        [%kttr *]
      `[%spec p.gen %base %noun]
    ::
        [%ktts *]
      %+  biff  $(gen q.gen)
      |=  =skin
      ?@  p.gen  `[%name p.gen skin]
      ?.  ?=([%name @ [%base %noun]] p.gen)  ~
      `[%name term.p.gen skin]
    ==
  ::
  ++  open
    ^-  hoon
    ?-    gen
        {~ *}     [%cnts [[%& p.gen] ~] ~]
    ::
        {$base *}  ~(factory ax fab `spec`gen)
        {$bust *}  ~(example ax fab %base p.gen)
        {$ktcl *}  ~(factory ax fab p.gen)
        {$dbug *}   q.gen
        {$eror *}  ~>(%slog.[0 leaf/p.gen] !!)
    ::
        {$knit *}                                       ::
      :+  %tsbn  [%ktts %v %$ 1]                        ::  =>  v=.
      :-  %brhp                                         ::  |-
      :+  %ktls                                         ::  ^+
        :-  %brhp                                       ::  |-
        :^    %wtcl                                     ::  ?:
            [%bust %flag]                               ::  ?
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
      :+  %tsls                                         ::
        :-  :+  %ktts                                   ::  ^=
              %a                                        ::  a
            :+  %ktls                                   ::  ^+
              [%limb %$]                                ::  $
            [%tsbn [%limb %v] p.i.p.gen]                ::  =>(v {p.i.p.gen})
        [%ktts %b res]                                  ::  b={res}
      ^-  hoon                                          ::
      :-  %brhp                                         ::  |-
      :^    %wtvt                                       ::  ?@
          [%a ~]                                        ::  a
        [%limb %b]                                      ::  b
      :-  [%tsld [%$ 2] [%limb %a]]                     ::  :-  -.a
      :+  %cnts                                         ::  %=
        [%$ ~]                                          ::  $
      [[[%a ~] [%tsld [%$ 3] [%limb %a]]] ~]            ::  a  +.a
    ::
        {$leaf *}  ~(factory ax fab `spec`gen)
        {$limb *}  [%cnts [p.gen ~] ~]
        {$tell *}  [%cncl [%limb %noah] [%zpbn [%cltr p.gen]] ~]
        {$wing *}  [%cnts p.gen ~]
        {$yell *}  [%cncl [%limb %cain] [%zpbn [%cltr p.gen]] ~]
        {$note *}  q.gen
    ::
        {$brbs *}  =-  ?~  -  !!
                       [%brtr [%bscl -] [%ktcl body.gen]]
                   %+  turn  `(list term)`sample.gen
                   |=  =term
                   ^-  spec
                   =/  tar  [%base %noun]
                   [%bsts term [%bssg tar [%bshp tar tar]]]
        {$brcb *}  :+  %tsls  [%kttr p.gen]
                   :+  %brcn  ~
                   %-  ~(run by r.gen)
                   |=  =tome
                   :-  p.tome
                   %-  ~(run by q.tome)
                   |=  =hoon
                   ?~  q.gen  hoon
                   [%tstr [p.i.q.gen ~] q.i.q.gen $(q.gen t.q.gen)]
        {$brcl *}  [%tsls p.gen [%brdt q.gen]]
        {$brdt *}  :+  %brcn  ~
                   =-  [[%$ ~ -] ~ ~]
                   (~(put by *(map term hoon)) %$ p.gen)
        {$brkt *}  :+  %tsld  [%limb %$]
                   :+  %brcn  ~
                   =+  zil=(~(get by q.gen) %$)
                   ?~  zil
                     %+  ~(put by q.gen)  %$
                     [*what [[%$ p.gen] ~ ~]]
                   %+  ~(put by q.gen)  %$
                   [p.u.zil (~(put by q.u.zil) %$ p.gen)]
        {$brhp *}  [%tsld [%limb %$] [%brdt p.gen]]
        {$brsg *}  [%ktbr [%brts p.gen q.gen]]
        {$brtr *}  :+  %tsls  [%kttr p.gen]
                   :+  %brvt  ~
                   =-  [[%$ ~ -] ~ ~]
                   (~(put by *(map term hoon)) %$ q.gen)
        {$brts *}  :+  %brcb  p.gen
                   =-  [~ [[%$ ~ -] ~ ~]]
                   (~(put by *(map term hoon)) %$ q.gen)
        {$brwt *}  [%ktwt %brdt p.gen]
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
        {$kttr *}  [%ktsg ~(example ax fab p.gen)]
        {$cncb *}  [%ktls [%wing p.gen] %cnts p.gen q.gen]
        {$cndt *}  [%cncl q.gen [p.gen ~]]
        {$cnkt *}  [%cncl p.gen q.gen r.gen s.gen ~]
        {$cnls *}  [%cncl p.gen q.gen r.gen ~]
        {$cnhp *}  [%cncl p.gen q.gen ~]
        ::  this probably should work, but doesn't
        ::
        ::  {$cncl *}  [%cntr [%$ ~] p.gen [[[[%& 6] ~] [%cltr q.gen]] ~]]
        {$cncl *}  [%cnsg [%$ ~] p.gen q.gen]
        {$cnsg *}
      ::  this complex matching system is a leftover from the old
      ::  "electroplating" era.  %cnsg should be removed and replaced
      ::  with the commented-out %cncl above.  but something is broken.
      ::
      :^  %cntr  p.gen  q.gen
      =+  axe=6
      |-  ^-  (list {wing hoon})
      ?~  r.gen  ~
      ?~  t.r.gen  [[[[%| 0 ~] [%& axe] ~] i.r.gen] ~]
      :-  [[[%| 0 ~] [%& (peg axe 2)] ~] i.r.gen]
      $(axe (peg axe 3), r.gen t.r.gen)
    ::
        {$cntr *}
      ?:  =(~ r.gen)
        [%tsbn q.gen [%wing p.gen]]
      :+  %tsls
        q.gen
      :+  %cnts
        (weld p.gen `wing`[[%& 2] ~])
      (turn r.gen |=({p/wing q/hoon} [p [%tsbn [%$ 3] q]]))
    ::
        {$ktdt *}  [%ktls [%cncl p.gen q.gen ~] q.gen]
        {$kthp *}  [%ktls ~(example ax fab p.gen) q.gen]
        {$ktts *}  (grip(gen q.gen) p.gen)
    ::
        {$sgbr *}
      :+  %sgbn
        :-  %mean
        =+  fek=~(feck ap p.gen)
        ?^  fek  [%rock %tas u.fek]
        [%brdt [%cncl [%limb %cain] [%zpbn [%tsbn [%$ 3] p.gen]] ~]]
      q.gen
    ::
        {$sgcb *}  [%sgbn [%mean [%brdt p.gen]] q.gen]
        {$sgcn *}
      :+  %sgld
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
        {$sgnt *}  [%sgcn p.gen [%$ 7] ~ q.gen]
        {$sgld *}  [%tsld [%sgbn p.gen [%$ 1]] q.gen]
        {$sgbs *}  [%sgbn [%live [%rock %$ p.gen]] q.gen]
        {$sgls *}  [%sgbn [%memo %rock %$ p.gen] q.gen]
        {$sgpd *}
      :+  %sgbn
        [%slog [%sand %$ p.gen] [%cncl [%limb %cain] [%zpbn q.gen] ~]]
      r.gen
    ::
        {$sgts *}  [%sgbn [%germ p.gen] q.gen]
        {$sgwt *}
      :+  %tsls  [%wtdt q.gen [%bust %null] [[%bust %null] r.gen]]
      :^  %wtsg  [%& 2]~
        [%tsbn [%$ 3] s.gen]
      [%sgpd p.gen [%$ 5] [%tsbn [%$ 3] s.gen]]
    ::
        {$mcts *}
      |-
      ?~  p.gen  [%bust %null]
      ?-  -.i.p.gen
        ^      [[%xray i.p.gen] $(p.gen t.p.gen)]
        $manx  [p.i.p.gen $(p.gen t.p.gen)]
        $tape  [[%mcnt p.i.p.gen] $(p.gen t.p.gen)]
        $call  [%cncl p.i.p.gen [$(p.gen t.p.gen)]~]
        $marl  =-  [%cndt [p.i.p.gen $(p.gen t.p.gen)] -]
               ^-  hoon
               :+  %tsbr  [%base %cell]
               :+  %brvt  ~
               ^-  (map term tome)
               =-  [[%$ ~ -] ~ ~]
               ^-  (map term hoon)
               :_  [~ ~]
               =+  sug=[[%& 12] ~]
               :-  %$
               :^  %wtsg  sug
                 [%cnts sug [[[[%& 1] ~] [%$ 13]] ~]]
               [%cnts sug [[[[%& 3] ~] [%cnts [%$ ~] [[sug [%$ 25]] ~]]] ~]]
      ==
    ::
        {$mccl *}
      ?-    q.gen
          ~      [%zpzp ~]
          {* ~}  i.q.gen
          ^
        :+  %tsls
          p.gen
        =+  yex=`(list hoon)`q.gen
        |-  ^-  hoon
        ?-  yex
          {* ~}  [%tsbn [%$ 3] i.yex]
          {* ^}   [%cncl [%$ 2] [%tsbn [%$ 3] i.yex] $(yex t.yex) ~]
          ~      !!
        ==
      ==
    ::
        {$mcnt *}  =+(zoy=[%rock %ta %$] [%clsg [zoy [%clsg [zoy p.gen] ~]] ~])
        {$mcgl *}
      :^    %cnls
          :+  %cnhp
            q.gen
          [%ktcl p.gen]
        r.gen
      :+  %brts
        p.gen
      s.gen
    ::
        {$mcsg *}                                       ::                  ;~
      |-  ^-  hoon
      ?-  q.gen
          ~      ~_(leaf+"open-mcsg" !!)
          ^
        :+  %tsbn  [%ktts %v %$ 1]                      ::  =>  v=.
        |-  ^-  hoon                                    ::
        ?:  ?=(~ t.q.gen)                               ::
          [%tsbn [%limb %v] i.q.gen]                    ::  =>(v {i.q.gen})
        :+  %tsls  [%ktts %a $(q.gen t.q.gen)]          ::  =+  ^=  a
        :+  %tsls                                       ::    {$(q.gen t.q.gen)}
          [%ktts %b [%tsbn [%limb %v] i.q.gen]]         ::  =+  ^=  b
        :+  %tsls                                       ::    =>(v {i.q.gen})
          :+  %ktts  %c                                 ::  =+  c=,.+6.b
          :+  %tsld                                     ::
            [%wing [%| 0 ~] [%& 6] ~]                   ::
          [%limb %b]                                    ::
        :-  %brdt                                       ::  |.
        :^    %cnls                                     ::  %+
            [%tsbn [%limb %v] p.gen]                    ::      =>(v {p.gen})
          [%cncl [%limb %b] [%limb %c] ~]               ::    (b c)
        :+  %cnts  [%a ~]                               ::  a(,.+6 c)
        [[[[%| 0 ~] [%& 6] ~] [%limb %c]] ~]            ::
      ==                                                ::
    ::
        {$mcmc *}                                       ::                  ;;
      [%cnhp ~(factory ax fab p.gen) q.gen]
    ::
        {$tsbr *}
      [%tsls ~(example ax fab p.gen) q.gen]
    ::
        {$tstr *}
      :+  %tsld
        r.gen
      [%tune [[p.p.gen ~ ?~(q.p.gen q.gen [%kthp u.q.p.gen q.gen])] ~ ~] ~]
    ::
        {$tscl *}
      [%tsbn [%cncb [[%& 1] ~] p.gen] q.gen]
    ::
        {$tsnt *}
      [%tsls [%ktts p.gen q.gen] r.gen]
    ::
        {$tsmc *}  [%tsnt p.gen r.gen q.gen]
        {$tsdt *}
      [%tsbn [%cncb [[%& 1] ~] [[p.gen q.gen] ~]] r.gen]
        {$tswt *}                                       ::                  =?
      [%tsdt p.gen [%wtcl q.gen r.gen [%wing p.gen]] s.gen]
    ::
        {$tskt *}                                       ::                  =^
      =+  wuy=(weld q.gen `wing`[%v ~])                 ::
      :+  %tsbn  [%ktts %v %$ 1]                        ::  =>  v=.
      :+  %tsls  [%ktts %a %tsbn [%limb %v] r.gen]      ::  =+  a==>(v \r.gen)
      :^  %tsdt  wuy  [%tsld [%$ 3] [%limb %a]]
      :+  %tsbn  :-  :+  %ktts  [%over [%v ~] p.gen]
                     [%tsld [%$ 2] [%limb %a]]
                 [%limb %v]
      s.gen
    ::
        {$tsld *}  [%tsbn q.gen p.gen]
        {$tsls *}  [%tsbn [p.gen [%$ 1]] q.gen]
        {$tshp *}  [%tsls q.gen p.gen]
        {$tssg *}
      |-  ^-  hoon
      ?~  p.gen    [%$ 1]
      ?~  t.p.gen  i.p.gen
      [%tsbn i.p.gen $(p.gen t.p.gen)]
    ::
        {$wtbr *}
      |-
      ?~(p.gen [%rock %f 1] [%wtcl i.p.gen [%rock %f 0] $(p.gen t.p.gen)])
    ::
        {$wtdt *}   [%wtcl p.gen r.gen q.gen]
        {$wtld *}   [%wtcl p.gen [%zpzp ~] q.gen]
        {$wtbn *}   [%wtcl p.gen q.gen [%zpzp ~]]
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
        {$wtpd *}
      |-
      ?~(p.gen [%rock %f 0] [%wtcl i.p.gen $(p.gen t.p.gen) [%rock %f 1]])
    ::
        {$xray *}
      |^  :-  [(open-mane n.g.p.gen) %clsg (turn a.g.p.gen open-mart)]
          [%mcts c.p.gen]
      ::
      ++  open-mane
        |=  a/mane:hoot
        ?@(a [%rock %tas a] [[%rock %tas -.a] [%rock %tas +.a]])
      ::
      ++  open-mart
        |=  {n/mane:hoot v/(list beer:hoot)}
        [(open-mane n) %knit v]
      --
    ::
        {$wtvt *}   [%wtcl [%wtts [%base %atom %$] p.gen] q.gen r.gen]
        {$wtsg *}   [%wtcl [%wtts [%base %null] p.gen] q.gen r.gen]
        {$wtts *}   [%fits ~(example ax fab p.gen) q.gen]
        {$wtzp *}   [%wtcl p.gen [%rock %f 1] [%rock %f 0]]
        {$zpbn *}
      [%cncl [%limb %onan] [%zpmc [%kttr [%bsmc %limb %abel]] p.gen] ~]
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
  ++  rake  ~>(%mean.'rake-hoon' (need reek))
  ++  reek
    ^-  (unit wing)
    ?+  gen  ~
      {~ *}        `[[%& p.gen] ~]
      {$limb *}     `[p.gen ~]
      {$wing *}     `p.gen
      {$cnts * ~}  `p.gen
      {$dbug *}     reek(gen q.gen)
    ==
  ++  rusk
    ^-  term
    =+  wig=rake
    ?.  ?=({@ ~} wig)
      ~>(%mean.'rusk-hoon' !!)
    i.wig
  --
::
::::  5c: compiler backend and prettyprinter
  ::
++  ut
  ~%    %ut
      +>+
    ==
      %ar     ar
      %fan    fan
      %rib    rib
      %vet    vet
      %fab    fab
      %blow   blow
      %burp   burp
      %busk   busk
      %buss   buss
      %crop   crop
      %duck   duck
      %dune   dune
      %dunk   dunk
      %epla   epla
      %emin   emin
      %emul   emul
      %feel   feel
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
      %mile   mile
      %mine   mine
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
  ++  clip
    |=  ref/type
    ?>  ?|(!vet (nest(sut ref) & sut))
    ref
  ::
  ::  +ar: texture engine
  ::
  ++  ar  !:
    ~%    %ar
        +>
      ==
        %fish  fish
        %gain  gain
        %lose  lose
      ==
    |_  [ref=type =skin]
    ::
    ::  =fish: make a $nock that tests a .ref at .axis for .skin
    ::
    ++  fish
      |=  =axis
      ^-  nock
      ?@  skin  [%1 &]
      ?-    -.skin
      ::
          %base
        ?-  base.skin
          %cell      $(skin [%cell [%base %noun] [%base %noun]])
          %flag      ?:  (~(nest ut bool) | ref)
                       [%1 &]
                     %+  flan
                       $(skin [%base %atom %$])
                     %+  flor
                       [%5 [%0 axis] [%1 &]]
                     [%5 [%0 axis] [%1 |]]
          %noun      [%1 &]
          %null      $(skin [%leaf %n ~])
          %void      [%1 |]
          [%atom *]  ?:  (~(nest ut [%atom %$ ~]) | ref)
                       [%1 &]
                     ?:  (~(nest ut [%cell %noun %noun]) | ref)
                       [%1 |]
                     (flip [%3 %0 axis])
        ==
      ::
          %cell
        ?:  (~(nest ut [%atom %$ ~]) | ref)  [%1 |]
        %+  flan
          ?:  (~(nest ut [%cell %noun %noun]) | ref)
            [%1 &]
          [%3 %0 axis]
        %+  flan
          $(ref (peek(sut ref) %free 2), skin skin.skin)
        $(ref (peek(sut ref) %free 3), skin ^skin.skin)
      ::
          %leaf
        ?:  (~(nest ut [%atom %$ `atom.skin]) | ref)
          [%1 &]
        [%5 [%1 atom.skin] [%0 axis]]
      ::
          %dbug  $(skin skin.skin)
          %help  $(skin skin.skin)
          %name  $(skin skin.skin)
          %over  $(skin skin.skin)
          %spec  $(skin skin.skin)
          %wash  [%1 1]
      ==
    ::
    ::  -gain: make a $type by restricting .ref to .skin
    ::
    ++  gain
      |-  ^-  type
      ?@  skin  [%face skin ref]
      ?-    -.skin
      ::
          %base
        ?-    base.skin
            %cell      $(skin [%cell [%base %noun] [%base %noun]])
            %flag      (fork $(skin [%leaf %f &]) $(skin [%leaf %f |]) ~)
            %null      $(skin [%leaf %n ~])
            %void      %void
            %noun      ?:((~(nest ut %void) | ref) %void ref)
            [%atom *]
          =|  gil=(set type)
          |-  ^-  type
          ?-    ref
            %void      %void
            %noun      [%atom p.base.skin ~]
            [%atom *]  ?.  (fitz p.base.skin p.ref)
                          ~>(%mean.'atom-mismatch' !!)
                       :+  %atom
                         (max p.base.skin p.ref)
                       q.ref
            [%cell *]  %void
            [%core *]  %void
            [%face *]  (face p.ref $(ref q.ref))
            [%fork *]  (fork (turn ~(tap in p.ref) |=(=type ^$(ref type))))
            [%hint *]  (hint p.ref $(ref q.ref))
            [%hold *]  ?:  (~(has in gil) ref)  %void
                       $(gil (~(put in gil) ref), ref repo(sut ref))
          ==
        ==
      ::
          %cell
        =|  gil=(set type)
        |-  ^-  type
        ?-    ref
            %void      %void
            %noun      [%cell %noun %noun]
            [%atom *]  %void
            [%cell *]  =+  ^$(skin skin.skin, ref p.ref)
                       ?:  =(%void -)  %void
                       (cell - ^$(skin ^skin.skin, ref q.ref))
            [%core *]  =+  ^$(skin skin.skin, ref p.ref)
                       ?:  =(%void -)  %void
                       ?.  =(%noun ^skin.skin)
                         (cell - ^$(skin ^skin.skin, ref %noun))
                       [%core - q.ref]
            [%face *]  (face p.ref $(ref q.ref))
            [%fork *]  (fork (turn ~(tap in p.ref) |=(=type ^$(ref type))))
            [%hint *]  (hint p.ref $(ref q.ref))
            [%hold *]  ?:  (~(has in gil) ref)  %void
                       $(gil (~(put in gil) ref), ref repo(sut ref))
        ==
      ::
          %leaf
        =|  gil=(set type)
        |-  ^-  type
        ?-  ref
          %void      %void
          %noun      [%atom aura.skin `atom.skin]
          [%atom *]  ?:  &(?=(^ q.ref) !=(atom.skin u.q.ref))
                       %void
                     ?.  (fitz aura.skin p.ref)
                        ~>(%mean.'atom-mismatch' !!)
                     :+  %atom
                       (max aura.skin p.ref)
                     `atom.skin
          [%cell *]  %void
          [%core *]  %void
          [%face *]  (face p.ref $(ref q.ref))
          [%fork *]  (fork (turn ~(tap in p.ref) |=(=type ^$(ref type))))
          [%hint *]  (hint p.ref $(ref q.ref))
          [%hold *]  ?:  (~(has in gil) ref)  %void
                     $(gil (~(put in gil) ref), ref repo(sut ref))
        ==
      ::
          %dbug  $(skin skin.skin)
          %help  (hint [sut %help help.skin] $(skin skin.skin))
          %name  (face term.skin $(skin skin.skin))
          %over  $(skin skin.skin, sut (~(play ut sut) %wing wing.skin))
          %spec  =/  yon  $(skin skin.skin)
                 =/  hit  (~(play ut sut) ~(example ax fab spec.skin))
                 ?>  (~(nest ut hit) & yon)
                 hit
          %wash  =-  $(ref (~(play ut ref) -))
                 :-  %wing
                 |-  ^-  wing
                 ?:  =(0 depth.skin)  ~
                 [[%| 0 ~] $(depth.skin (dec depth.skin))]
      ==
    ::
    ::  -lose: make a $type by restricting .ref to exclude .skin
    ::
    ++  lose
      |-  ^-  type
      ?@  skin  [%face skin ref]
      ?-    -.skin
      ::
          %base
        ?-    base.skin
            %cell      $(skin [%cell [%base %noun] [%base %noun]])
            %flag      $(skin [%base %atom %f])
            %null      $(skin [%leaf %n ~])
            %void      ref
            %noun      %void
            [%atom *]
          =|  gil=(set type)
          |-  ^-  type
          ?-    ref
            %void      %void
            %noun      [%cell %noun %noun]
            [%atom *]  %void
            [%cell *]  ref
            [%core *]  ref
            [%face *]  (face p.ref $(ref q.ref))
            [%fork *]  (fork (turn ~(tap in p.ref) |=(=type ^$(ref type))))
            [%hint *]  (hint p.ref $(ref q.ref))
            [%hold *]  ?:  (~(has in gil) ref)  %void
                       $(gil (~(put in gil) ref), ref repo(sut ref))
          ==
        ==
      ::
          %cell
        =|  gil=(set type)
        |-  ^-  type
        ?-    ref
            %void      %void
            %noun      [%atom %$ ~]
            [%atom *]  ref
            [%cell *]  =+  ^$(skin skin.skin, ref p.ref)
                       ?:  =(%void -)  %void
                       (cell - ^$(skin ^skin.skin, ref q.ref))
            [%core *]  =+  ^$(skin skin.skin, ref p.ref)
                       ?:  =(%void -)  %void
                       ?.  =(%noun ^skin.skin)
                         (cell - ^$(skin ^skin.skin, ref %noun))
                       [%core - q.ref]
            [%face *]  (face p.ref $(ref q.ref))
            [%fork *]  (fork (turn ~(tap in p.ref) |=(=type ^$(ref type))))
            [%hint *]  (hint p.ref $(ref q.ref))
            [%hold *]  ?:  (~(has in gil) ref)  %void
                       $(gil (~(put in gil) ref), ref repo(sut ref))
        ==
      ::
          %leaf
        =|  gil=(set type)
        |-  ^-  type
        ?-  ref
          %void      %void
          %noun      %noun
          [%atom *]  ?:  =(q.ref `atom.skin)
                       %void
                     ref
          [%cell *]  ref
          [%core *]  ref
          [%face *]  (face p.ref $(ref q.ref))
          [%fork *]  (fork (turn ~(tap in p.ref) |=(=type ^$(ref type))))
          [%hint *]  (hint p.ref $(ref q.ref))
          [%hold *]  ?:  (~(has in gil) ref)  %void
                     $(gil (~(put in gil) ref), ref repo(sut ref))
        ==
      ::
          %dbug  $(skin skin.skin)
          %help  $(skin skin.skin)
          %name  $(skin skin.skin)
          %over  $(skin skin.skin)
          %spec  $(skin skin.skin)
          %wash  ref
      ==
    --
  ::
  ++  bleu
    |=  {gol/type gen/hoon}
    ^-  {type nock}
    =+  pro=(mint gol gen)
    =+  jon=(apex:musk bran q.pro)
    ?:  |(?=(~ jon) ?=($wait -.u.jon))
      ?:  &(!fab vet)
        ~&  %bleu-fail
        !!
      [p.pro q.pro]
    [p.pro %1 p.u.jon]
  ::
  ++  blow
    |=  {gol/type gen/hoon}
    ^-  {type nock}
    =+  pro=(mint gol gen)
    =+  jon=(apex:musk bran q.pro)
    ?:  |(?=(~ jon) ?=($wait -.u.jon))
      [p.pro q.pro]
    [p.pro %1 p.u.jon]
  ::
  ++  bran
    ~+
    =+  gil=*(set type)
    |-  ~+  ^-  seminoun:musk
    ?-    sut
      $noun      [full/[~ ~ ~] ~]
      $void      [full/[~ ~ ~] ~]
      {$atom *}  ?~(q.sut [full/[~ ~ ~] ~] [full/~ u.q.sut])
      {$cell *}  (combine:musk $(sut p.sut) $(sut q.sut))
      {$core *}  %+  combine:musk
                   p.r.q.sut
                 $(sut p.sut)
      {$face *}  $(sut repo)
      {$fork *}  [full/[~ ~ ~] ~]
      {$hint *}  $(sut repo)
      {$hold *}  ?:  (~(has in gil) sut)
                   [full/[~ ~ ~] ~]
                 $(sut repo, gil (~(put in gil) sut))
    ==
  ::
  ++  burp
    ::  expel undigested seminouns
    ::
    ^-  type
    ~+
    ~=  sut
    ?+  sut      sut
      [%cell *]  [%cell burp(sut p.sut) burp(sut q.sut)]
      [%core *]  :+  %core
                   burp(sut p.sut)
                 :*  p.q.sut
                     burp(sut q.q.sut)
                     :_  q.r.q.sut
                     ?:  ?=([[%full ~] *] p.r.q.sut)
                       p.r.q.sut
                     [[%full ~ ~ ~] ~]
                  ==
      [%face *]  [%face p.sut burp(sut q.sut)]
      [%fork *]  [%fork (~(run in p.sut) |=(type burp(sut +<)))]
      [%hint *]  (hint p.sut burp(sut q.sut))
      [%hold *]  [%hold burp(sut p.sut) q.sut]
    ==
  ::
  ++  busk
    ~/  %busk
    |=  gen/hoon
    ^-  type
    [%face [~ [gen ~]] sut]
  ::
  ++  buss
    ~/  %buss
    |=  {cog/term gen/hoon}
    ^-  type
    [%face [[[cog ~ gen] ~ ~] ~] sut]
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
          {$hint *}  (hint p.sut dext(sut q.sut))
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
        {$hint *}  dext(ref repo(sut ref))
        {$hold *}  dext(ref repo(sut ref))
      ==
    --
  ::
  ++  cool
    |=  {pol/? hyp/wing ref/type}
    ^-  type
    =+  fid=(find %both hyp)
    ?-  -.fid
      %|  sut
      %&  =<  q
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
    ?:  ?=(%& -.q.lop)
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
    ?:  ?=(%& -.q.lop)
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
    ?:  ?=(%& -.q.p.lop)
      ?>  ?=(%& -.q.q.lop)
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
    ?>  ?=(%| -.q.q.lop)
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
      |@  ++  $
      =>  +<
      |%
      ++  pord  |*(* (form +< *nock))                   ::  wrap mint formula
      ++  rosh  |*(* (form +< *(list pock)))            ::  wrap mint changes
      ++  fleg  _(pord $:bath)                          ::  legmatch + code
      ++  fram  _(pord $:claw)                          ::  armmatch +
      ++  foat  _(rosh $:bath)                          ::  leg with changes
      ++  fult  _(rosh $:claw)                          ::  arm with changes
      --  --
    ::
    ++  lib
      |%
      ++  deft
        =>  (def deft:arc)
        |%
        ++  halp  ^|(|:($:hoon $:fleg))
        ++  vant
          |%  ++  trep  ^|(|:($:{bath wing bath} $:{axis bath}))
              ++  tasp  ^|(|:($:{{axis bath} fleg foat} $:foat))
              ++  tyle  ^|(|:($:foat $:foat))
          --
        ++  vunt
          |%  ++  trep  ^|(|:($:{claw wing bath} $:{axis claw}))
              ++  tasp  ^|(|:($:{{axis claw} fleg fult} $:fult))
              ++  tyle  ^|(|:($:fult $:foat))
        --  --
      ::
      ++  make
        =>  (def make:arc)
        |%
        ++  halp  |~  a/hoon
                  ^-  fleg
                  (mint %noun a)
        ++  vant
          |%  ++  trep  |:  $:{a/type b/wing c/type}
                        ^-  {axis type}
                        (tack(sut a) b c)
              ++  tasp  |:  $:{a/(pair axis type) b/fleg c/foat}
                        ^-  foat
                        [q.a [[p.a (skin b)] (skin c)]]
              ++  tyle  |:($:foat +<)
          --
        ++  vunt
          |%  ++  trep  |:  $:{a/claw b/wing c/bath}
                        ^-  (pair axis claw)
                        (toss b c a)
              ++  tasp  |:  $:{a/(pair axis claw) b/fleg c/fult}
                        ^-  fult
                        [q.a [[p.a (skin b)] (skin c)]]
              ++  tyle  |:  $:fult
                        ^-  foat
                        [(fire +<-) +<+]
      --  --  --
    ::
    ++  bin
      =+  deft:lib
      |@  ++  $
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
        |@  ++  $
        =>  +<
        |:  $:{rum/clom rig/(list (pair wing hoon))}
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
    |@  ++  $
    =>  inc
    |%
    ++  echo
      |:  $:{rum/bath rig/(list (pair wing hoon))}
      (ecbo rum rig)
    ::
    ++  ecmo
      |:  $:{hag/claw rig/(list (pair wing hoon))}
      (eclo hag rig)
    --  --
  ::
  ++  etco
    |=  {lop/palo rig/(list (pair wing hoon))}
    ^-  (pair type nock)
    =+  cin=(oc (bin:ad make:lib:ad))
    =.  rig  (flop rig)         ::  XX this unbreaks, void order in devulc
    =+  axe=(tend p.lop)
    ?:  ?=(%& -.q.lop)
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
      ?:  ?=(%| -.lug)  ~>(%mean.'hoon' ?>(?=(~ rig) p.p.lug))
      (elbo p.lug rig)
    ::
    ++  mint
      |=  gol/type
      ^-  (pair type nock)
      =+  lug=(find %read hyp)
      ?:  ?=(%| -.lug)  ~>(%mean.'hoon' ?>(?=(~ rig) p.lug))
      =-  ?>(?|(!vet (nest(sut gol) & p.-)) -)
      (etco p.lug rig)
    ::
    ++  mull
      |=  {gol/type dox/type}
      ^-  {type type}
      =+  lug=[p=(find %read hyp) q=(find(sut dox) %read hyp)]
      ?:  ?=(%| -.p.lug)
        ?>   &(?=(%| -.q.lug) ?=(~ rig))
        [p.p.p.lug p.p.q.lug]
      ?>  ?=(%& -.q.lug)
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
      %&  p.lap
      %|  %-  fork
          %+  turn  ~(tap in q.lap)
          |=  [a=type *]
          ?>  ?=([%core *] a)
          [%core q.q.a q.a]
    ==
  ::                                                    ::
  ++  feel                                              ::  detect existence
    |=  rot/(list wing)
    ^-  ?
    =.  rot  (flop rot)
    |-  ^-  ?
    ?~  rot  &
    =/  yep  (fond %free i.rot)
    ?~  yep  |
    ?-    -.yep
      %&  %=  $
            rot  t.rot
            sut  p:(fine %& p.yep)
          ==
      %|  ?-  -.p.yep
            %&  |
            %|  %=  $
                  rot  t.rot
                  sut  p:(fine %| p.p.yep)
                ==
    ==    ==
  ::
  ++  fond
    ~/  %fond
    |=  {way/vial hyp/wing}
    =>  |%
        ++  pony                                        ::  raw match
                  $@  ~                                 ::  void
                  %+  each                              ::  natural/abnormal
                    palo                                ::  arm or leg
                  %+  each                              ::  abnormal
                    @ud                                 ::  unmatched
                  (pair type nock)                      ::  synthetic
        --
    ^-  pony
    ?~  hyp
      [%& ~ %& sut]
    =+  mor=$(hyp t.hyp)
    ?-    -.mor
        %|
      ?-    -.p.mor
          %&  mor
          %|
        =+  fex=(mint(sut p.p.p.mor) %noun [%wing i.hyp ~])
        [%| %| p.fex (comb q.p.p.mor q.fex)]
      ==
    ::
        %&
      =.  sut  (felt q.p.mor)
      =>  :_  +
          :*  axe=`axis`1
              lon=p.p.mor
              heg=?^(i.hyp i.hyp [%| p=0 q=(some i.hyp)])
          ==
      ?:  ?=(%& -.heg)
        [%& [`p.heg lon] %& (peek way p.heg)]
      =|  gil/(set type)
      =<  $
      |%  ++  here  ?:  =(0 p.heg)
                      [%& [~ `axe lon] %& sut]
                    [%| %& (dec p.heg)]
          ++  lose  [%| %& p.heg]
          ++  stop  ?~(q.heg here lose)
          ++  twin  |=  {hax/pony yor/pony}
                    ^-  pony
                    ~_  leaf+"find-fork"
                    ?:  =(hax yor)  hax
                    ?~  hax  yor
                    ?~  yor  hax
                    ?:  ?=(%| -.hax)
                      ?>  ?&  ?=(%| -.yor)
                              ?=(%| -.p.hax)
                              ?=(%| -.p.yor)
                              =(q.p.p.hax q.p.p.yor)
                          ==
                      :+  %|
                        %|
                      [(fork p.p.p.hax p.p.p.yor ~) q.p.p.hax]
                    ?>  ?=(%& -.yor)
                    ?>  =(p.p.hax p.p.yor)
                    ?:  &(?=(%& -.q.p.hax) ?=(%& -.q.p.yor))
                      :+  %&  p.p.hax
                      [%& (fork p.q.p.hax p.q.p.yor ~)]
                    ?>  &(?=(%| -.q.p.hax) ?=(%| -.q.p.yor))
                    ?>  =(p.q.p.hax p.q.p.yor)
                    =+  wal=(~(uni in q.q.p.hax) q.q.p.yor)
                    :+  %&  p.p.hax
                    [%| p.q.p.hax wal]
          ++  $
            ^-  pony
            ?-    sut
                $void       ~
                $noun       stop
                {$atom *}   stop
                {$cell *}
              ?~  q.heg  here
              =+  taf=$(axe (peg axe 2), sut p.sut)
              ?~  taf  ~
              ?:  |(?=(%& -.taf) ?=(%| -.p.taf))
                taf
              $(axe (peg axe 3), p.heg p.p.taf, sut q.sut)
            ::
                {$core *}
              ?~  q.heg  here
              =^  zem  p.heg
                  =+  zem=(loot u.q.heg q.r.q.sut)
                  ?~  zem  [~ p.heg]
                  ?:(=(0 p.heg) [zem 0] [~ (dec p.heg)])
              ?^  zem
                :+  %&
                  [`axe lon]
                =/  zut  ^-  foot
                         ?-  q.p.q.sut
                           %wet  [%wet q.u.zem]
                           %dry  [%dry q.u.zem]
                         ==
                [%| (peg 2 p.u.zem) [[sut zut] ~ ~]]
              =+  pec=(peel way r.p.q.sut)
              ?.  sam.pec  lose
              ?:  con.pec  $(sut p.sut, axe (peg axe 3))
              $(sut (peek(sut p.sut) way 2), axe (peg axe 6))
            ::
                {$hint *}
              $(sut repo)
            ::
                {$face *}
              ?:  ?=(~ q.heg)  here(sut q.sut)
              =*  zot  p.sut
              ?@  zot
                ?:(=(u.q.heg zot) here(sut q.sut) lose)
              =<  main
              |%
              ++  main
                ^-  pony
                =+  tyr=(~(get by p.zot) u.q.heg)
                ?~  tyr
                  next
                ?~  u.tyr
                  $(sut q.sut, lon [~ lon], p.heg +(p.heg))
                ?.  =(0 p.heg)
                  next(p.heg (dec p.heg))
                =+  tor=(fund way u.u.tyr)
                ?-  -.tor
                  %&  [%& (weld p.p.tor `vein`[~ `axe lon]) q.p.tor]
                  %|  [%| %| p.p.tor (comb [%0 axe] q.p.tor)]
                ==
              ++  next
                |-  ^-  pony
                ?~  q.zot
                  ^$(sut q.sut, lon [~ lon])
                =+  tiv=(mint(sut q.sut) %noun i.q.zot)
                =+  fid=^$(sut p.tiv, lon ~, axe 1, gil ~)
                ?~  fid  ~
                ?:  ?=({%| %& *} fid)
                  $(q.zot t.q.zot, p.heg p.p.fid)
                =/  vat/(pair type nock)
                    ?-    -.fid
                      %&  (fine %& p.fid)
                      %|  (fine %| p.p.fid)
                    ==
                [%| %| p.vat (comb (comb [%0 axe] q.tiv) q.vat)]
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
          %&  [%& p.-]
          %|  ?-  -.p.-
                %|  [%| p.p.-]
                %&  !!
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
      %|  p.tor
      %&  =+  axe=(tend p.p.tor)
          ?-  -.q.p.tor
            %&  [`type`p.q.p.tor %0 axe]
            %|  [(fire ~(tap in q.q.p.tor)) [%9 p.q.p.tor %0 axe]]
    ==    ==
  ::
  ++  fire
    |=  hag/(list {p/type q/foot})
    ^-  type
    ?:  ?=({{* {$wet ~ $1}} ~} hag)
      p.i.hag
    %-  fork
    %+  turn
      hag.$
    |=  {p/type q/foot}
    ?.  ?=({$core *} p)
      ~_  (dunk %fire-type)
      ~_  leaf+"expected-fork-to-be-core"
      ~_  (dunk(sut p) %fork-type)
      ~>(%mean.'fire-core' !!)
    :-  %hold
    =+  dox=[%core q.q.p q.p(r.p %gold)]
    ?:  ?=($dry -.q)
      ::  ~_  (dunk(sut [%cell q.q.p p.p]) %fire-dry)
      ?>  ?|(!vet (nest(sut q.q.p) & p.p))
      [dox p.q]
    ?>  ?=($wet -.q)
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
        {$core *}   ~>(%mean.'fish-core' !!)
        {$face *}   $(sut q.sut)
        {$fork *}   =+  yed=~(tap in p.sut)
                    |-  ^-  nock
                    ?~(yed [%1 1] (flor ^$(sut i.yed) $(yed t.yed)))
        {$hint *}   $(sut q.sut)
        {$hold *}
      ?:  (~(has in vot) sut)
        ~>(%mean.'fish-loop' !!)
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
        {$hint *}  (hint p.sut $(sut q.sut))
        {$hold *}
      ?:  (~(has in bix) [sut ref])
        ~>(%mean.'fuse-loop' !!)
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
  ++  hemp
    ::  generate formula from foot
    ::
    |=  [hud/poly gol/type gen/hoon]
    ^-  nock
    ~+
    =+  %hemp-141
    ?-  hud
      $dry  q:(mint gol gen)
      $wet  q:(mint(vet |) gol gen)
    ==
  ::
  ++  laze
    ::  produce lazy core generator for static execution
    ::
    |=  [nym=(unit term) hud=poly dom=(map term tome)]
    ::  only one layer of fabrication analysis
    ::
    =.  fab  &
    ~+
    ^-  seminoun
    =+  %hemp-141
    ::  tal: map from battery axis to foot
    ::
    =;  tal/(map @ud hoon)
      ::  produce lazy battery
      ::
      :_  ~
      :+  %lazy  1
      |=  axe/@ud
      ^-  (unit noun)
      %+  bind  (~(get by tal) axe)
      |=  gen/hoon
      %.  [hud %noun gen]
      hemp(sut (core sut [nym hud %gold] sut [[%lazy 1 ..^$] ~] dom))
    ::
    %-  ~(gas by *(map @ud hoon))
    =|  yeb/(list (pair @ud hoon))
    =+  axe=1
    |^  ?-  dom
          ~        yeb
          [* ~ ~]  (chapter q.q.n.dom)
          [* * ~]  %=  $
                     dom  l.dom
                     axe  (peg axe 3)
                     yeb  (chapter(axe (peg axe 2)) q.q.n.dom)
                   ==
          [* ~ *]  %=  $
                     dom  r.dom
                     axe  (peg axe 3)
                     yeb  (chapter(axe (peg axe 2)) q.q.n.dom)
                   ==
          [* * *]  %=  $
                     dom  r.dom
                     axe  (peg axe 7)
                     yeb  %=  $
                            dom  l.dom
                            axe  (peg axe 6)
                            yeb  (chapter(axe (peg axe 2)) q.q.n.dom)
        ==         ==     ==
    ++  chapter
      |=  dab/(map term hoon)
      ^+  yeb
      ?-  dab
        ~        yeb
        [* ~ ~]  [[axe q.n.dab] yeb]
        [* * ~]  %=  $
                   dab  l.dab
                   axe  (peg axe 3)
                   yeb  [[(peg axe 2) q.n.dab] yeb]
                 ==
        [* ~ *]  %=  $
                   dab  r.dab
                   axe  (peg axe 3)
                   yeb  [[(peg axe 2) q.n.dab] yeb]
                 ==
        [* * *]  %=  $
                   dab  r.dab
                   axe  (peg axe 7)
                   yeb  %=  $
                          dab  l.dab
                          axe  (peg axe 6)
                          yeb  [[(peg axe 2) q.n.dab] yeb]
      ==         ==     ==
    --
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
      (cool how q.gen (play ~(example ax fab p.gen)))
    ?:  ?=({$wthx *} gen)
      =+  (play %wing q.gen)
      ~>  %slog.[0 [%leaf "chipping"]]
      ?:  how
        =-  ~>  %slog.[0 (dunk(sut +<) 'chip: gain: ref')]
            ~>  %slog.[0 (dunk(sut -) 'chip: gain: gain')]
            -
        ~(gain ar - p.gen)
      ~(lose ar - p.gen)
    ?:  ?&(how ?=({$wtpd *} gen))
      |-(?~(p.gen sut $(p.gen t.p.gen, sut ^$(gen i.p.gen))))
    ?:  ?&(!how ?=({$wtbr *} gen))
      |-(?~(p.gen sut $(p.gen t.p.gen, sut ^$(gen i.p.gen))))
    =+  neg=~(open ap gen)
    ?:(=(neg gen) sut $(gen neg))
  ::
  ++  bake
    |=  [dox/type hud/poly dab/(map term hoon)]
    ^-  *
    ?:  ?=(~ dab)
      ~
    =+  ^=  dov
        ::  this seems wrong but it's actually right
        ::
        ?-  hud
          $dry  (mull %noun dox q.n.dab)
          $wet  ~
        ==
    ?-  dab
      {* ~ ~}  dov
      {* ~ *}  [dov $(dab r.dab)]
      {* * ~}  [dov $(dab l.dab)]
      {* * *}  [dov $(dab l.dab) $(dab r.dab)]
    ==
  ::
  ++  balk
    |=  [dox/type hud/poly dom/(map term tome)]
    ^-  *
    ?:  ?=(~ dom)
      ~
    =+  dov=(bake dox hud q.q.n.dom)
    ?-    dom
      {* ~ ~}   dov
      {* ~ *}   [dov $(dom r.dom)]
      {* * ~}   [dov $(dom l.dom)]
      {* * *}   [dov $(dom l.dom) $(dom r.dom)]
    ==
  ::
  ++  mile
    ::  mull all chapters and feet in a core
    ::
    |=  [dox=type mel=vair nym=(unit term) hud=poly dom=(map term tome)]
    ^-  (pair type type)
    =+  yet=(core sut [nym hud %gold] sut (laze nym hud dom) dom)
    =+  hum=(core dox [nym hud %gold] dox (laze nym hud dom) dom)
    =+  (balk(sut yet) hum hud dom)
    [yet hum]
  ::
  ++  mine
    ::  mint all chapters and feet in a core
    ::
    |=  [gol/type mel/vair nym/(unit term) hud/poly dom/(map term tome)]
    ^-  (pair type nock)
    |^
    =/  log  (chapters-check (core-check gol))
    =/  dog  (get-tomes log)
    =-  :_  [%1 dez]
        (core sut [nym hud mel] sut [[%full ~] dez] dom)
    ^=  dez
    =.  sut  (core sut [nym hud %gold] sut (laze nym hud dom) dom)
    |-  ^-  ?(~ ^)
    ?:  ?=(~ dom)
      ~
    =/  dov/?(~ ^)
      =/  dab/(map term hoon)  q.q.n.dom
      =/  dag  (arms-check dab (get-arms dog p.n.dom))
      |-  ^-  ?(~ ^)
      ?:  ?=(~ dab)
        ~
      =/  gog  (get-arm-type log dag p.n.dab)
      =+  vad=(hemp hud gog q.n.dab)
      ?-    dab
        {* ~ ~}   vad
        {* ~ *}   [vad $(dab r.dab)]
        {* * ~}   [vad $(dab l.dab)]
        {* * *}   [vad $(dab l.dab) $(dab r.dab)]
      ==
    ?-    dom
      {* ~ ~}   dov
      {* ~ *}   [dov $(dom r.dom)]
      {* * ~}   [dov $(dom l.dom)]
      {* * *}   [dov $(dom l.dom) $(dom r.dom)]
    ==
    ::
    ::  all the below arms are used for gol checking and should have no
    ::  effect other than giving more specific errors
    ::
    ::  all the possible types we could be expecting.
    ::
    +$  gol-type
      $~  %noun
      $@  %noun
      $%  [%cell p=type q=type]
          [%core p=type q=coil]
          [%fork p=(set gol-type)]
      ==
    ::  check that we're looking for a core
    ::
    ++  core-check
      |=  log=type
      |-  ^-  gol-type
      ?+    log  $(log repo(sut log))
          %noun      (nice log &)
          %void      (nice %noun |)
          [%atom *]  (nice %noun |)
          [%cell *]  (nice log (nest(sut p.log) & %noun))
          [%core *]  (nice log(r.p.q %gold) &)
          [%fork *]
        =/  tys  ~(tap in p.log)
        :-  %fork
        |-  ^-  (set gol-type)
        ?~  tys
          ~
        =/  a  ^$(log i.tys)
        =/  b  $(tys t.tys)
        (~(put in b) a)
      ==
    ::  check we have the expected number of chapters
    ::
    ++  chapters-check
      |=  log=gol-type
      |-  ^-  gol-type
      ?-    log
          %noun      (nice log &)
          [%cell *]  (nice log &)
          [%core *]  ~_  leaf+"core-number-of-chapters"
                     (nice log =(~(wyt by dom) ~(wyt by q.r.q.log)))
          [%fork *]
        =/  tys  ~(tap in p.log)
        |-  ^-  gol-type
        ?~  tys
          log
        =/  a  ^$(log i.tys)
        =/  b  $(tys t.tys)
        log
      ==
    ::  get map of tomes if exists
    ::
    ++  get-tomes
      |=  log=gol-type
      ^-  (unit (map term tome))
      ?-    log
          %noun      ~
          [%cell *]  ~
          [%fork *]  ~  ::  maybe could be more aggressive
          [%core *]  `q.r.q.log
      ==
    ::  get arms in tome
    ::
    ++  get-arms
      |=  [dog=(unit (map term tome)) nam=term]
      ^-  (unit (map term hoon))
      %+  bind  dog
      |=  a/(map term tome)
      ~_  leaf+"unexpcted-chapter.{(trip nam)}"
      q:(~(got by a) nam)
    ::  check we have the expected number of arms
    ::
    ++  arms-check
      |=  [dab=(map term hoon) dag=(unit (map term hoon))]
      ?~  dag
        dag
      =/  a
        =/  exp  ~(wyt by u.dag)
        =/  hav  ~(wyt by dab)
        ~_  =/  expt  (scow %ud exp)
            =/  havt  (scow %ud hav)
            leaf+"core-number-of-arms.exp={expt}.hav={havt}"
        ~_  =/  missing  ~(tap in (~(dif in ~(key by u.dag)) ~(key by dab)))
            leaf+"missing.{<missing>}"
        ~_  =/  extra  ~(tap in (~(dif in ~(key by dab)) ~(key by u.dag)))
            leaf+"extra.{<extra>}"
        ~_  =/  have  ~(tap in ~(key by dab))
            leaf+"have.{<have>}"
        (nice dag =(exp hav))
      a
    ::  get expected type of this arm
    ::
    ++  get-arm-type
      |=  [log=gol-type dag=(unit (map term hoon)) nam=term]
      ^-  type
      %-  fall  :_  %noun
      %+  bind  dag
      |=  a=(map term hoon)
      =/  gen=hoon
        ~_  leaf+"unexpected-arm.{(trip nam)}"
        (~(got by a) nam)
      (play(sut log) gen)
    ::
    ++  nice
      |*  [typ=* gud=?]
      ?:  gud
        typ
      ~_  leaf+"core-nice"
      !!
    --
  ::
  ++  mint
    ~/  %mint
    |=  {gol/type gen/hoon}
    ^-  {p/type q/nock}
    ::~&  %pure-mint
    |^  ^-  {p/type q/nock}
    ?:  ?&(=(%void sut) !?=({$dbug *} gen))
      ?.  |(!vet ?=({$lost *} gen) ?=({$zpzp *} gen))
        ~>(%mean.'mint-vain' !!)
      [%void %0 0]
    ?-    gen
    ::
        {^ *}
      =+  hed=$(gen p.gen, gol %noun)
      =+  tal=$(gen q.gen, gol %noun)
      [(nice (cell p.hed p.tal)) (cons q.hed q.tal)]
    ::
        {$ktcn *}  $(fab |, gen p.gen)
        {$brcn *}  (grow %gold p.gen %dry [%$ 1] q.gen)
        {$brvt *}  (grow %gold p.gen %wet [%$ 1] q.gen)
    ::
        {$cnts *}  (~(mint et p.gen q.gen) gol)
    ::
        {$dtkt *}
      =+  nef=$(gen [%kttr p.gen])
      [p.nef [%12 [%1 %151 p.nef] q:$(gen q.gen, gol %noun)]]
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
        {$ktbr *}  =+(vat=$(gen p.gen) [(nice (wrap(sut p.vat) %iron)) q.vat])
    ::
        {$ktls *}
      =+(hif=(nice (play p.gen)) [hif q:$(gen q.gen, gol hif)])
    ::
        {$ktpd *}  =+(vat=$(gen p.gen) [(nice (wrap(sut p.vat) %zinc)) q.vat])
        {$ktsg *}  (blow gol p.gen)
        {$tune *}  [(face p.gen sut) [%0 %1]]
        {$ktwt *}  =+(vat=$(gen p.gen) [(nice (wrap(sut p.vat) %lead)) q.vat])
    ::
        {$note *}
      =+  hum=$(gen q.gen)
      [(hint [sut p.gen] p.hum) q.hum]
    ::
        {$sgzp *}  ~_(duck(sut (play p.gen)) $(gen q.gen))
        {$sgbn *}
      =+  hum=$(gen q.gen)
      :: ?:  &(huz !?=(%|(@ [?(%sgcn %sgls) ^]) p.gen))
      ::  hum
      :-  p.hum
      :+  %11
        ?-    p.gen
            @   p.gen
            ^   [p.p.gen q:$(gen q.p.gen, gol %noun)]
        ==
      q.hum
    ::
        {$tsbn *}
      =+  fid=$(gen p.gen, gol %noun)
      =+  dov=$(sut p.fid, gen q.gen)
      [p.dov (comb q.fid q.dov)]
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
        {$wthx *}
      :-  (nice bool)
      =+  fid=(find %read [[%& 1] q.gen])
      ~>  %mean.'mint-fragment'
      ?>  &(?=(%& -.fid) ?=(%& -.q.p.fid))
      (~(fish ar `type`p.q.p.fid `skin`p.gen) (tend p.p.fid))
    ::
        {$fits *}
      :-  (nice bool)
      =+  ref=(play p.gen)
      =+  fid=(find %read q.gen)
      ~|  [%test q.gen]
      |-  ^-  nock
      ?-  -.fid
        %&  ?-  -.q.p.fid
              %&  (fish(sut ref) (tend p.p.fid))
              %|  $(fid [%| (fine fid)])
            ==
        %|  [%7 q.p.fid (fish(sut ref) 1)]
      ==
    ::
        {$dbug *}
      ~_  (show %o p.gen)
      =+  hum=$(gen q.gen)
      [p.hum [%11 [%spot %1 p.gen] q.hum]]
    ::
        {$zpcm *}   [(nice (play p.gen)) [%1 q.gen]]    ::  XX validate!
        {$lost *}
      ?:  vet
        ~_  (dunk(sut (play p.gen)) 'lost')
        ~>(%mean.'mint-lost' !!)
      [%void [%0 0]]
    ::
        {$zpmc *}
      =+  vos=$(gol %noun, gen q.gen)
      =+  ref=p:$(gol %noun, gen p.gen)
      [(nice (cell ref p.vos)) (cons [%1 burp(sut p.vos)] q.vos)]
    ::
        {$zpld *}
      =/  typ  (nice (play [%kttr p.gen]))
      =/  val
        =<  q
        %_    $
            gol  %noun
            gen
          :^    %wtcl
              :+  %cncl  [%limb %levi]
              :~  [%tsbn [%zpbn [%kttr p.gen]] [%$ 2]]
                  [%tsbn q.gen [%$ 2]]
              ==
            [%tsbn q.gen [%$ 3]]
          [%zpzp ~]
        ==
      [typ val]
    ::
        {$zpts *}   [(nice %noun) [%1 q:$(vet |, gen p.gen)]]
        {$zpvt *}   ?:((feel p.gen) $(gen q.gen) $(gen r.gen))
    ::
        {$zpzp ~}  [%void [%0 0]]
        *
      =+  doz=~(open ap gen)
      ?:  =(doz gen)
        ~_  (show [%c 'hoon'] [%q gen])
        ~>(%mean.'mint-open' !!)
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
      |=  {mel/vair nym/(unit term) hud/poly ruf/hoon dom/(map term tome)}
      ^-  {p/type q/nock}
      =+  dan=^$(gen ruf, gol %noun)
      =+  pul=(mine gol mel nym hud dom)
      [(nice p.pul) (cons q.pul q.dan)]
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
      {$fork *}  (levy ~(tap in p.sut) |=(type ^$(sut +<)))
      {$hint *}  $(sut q.sut)
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
      ~>(%mean.'mull-none' !!)
    ?-    gen
    ::
        {^ *}
      =+  hed=$(gen p.gen, gol %noun)
      =+  tal=$(gen q.gen, gol %noun)
      [(nice (cell p.hed p.tal)) (cell q.hed q.tal)]
    ::
        {$ktcn *}  $(fab |, gen p.gen)
        {$brcn *}  (grow %gold p.gen %dry [%$ 1] q.gen)
        {$brvt *}  (grow %gold p.gen %wet [%$ 1] q.gen)
        {$cnts *}  (~(mull et p.gen q.gen) gol dox)
        {$dtkt *}  =+($(gen q.gen, gol %noun) $(gen [%kttr p.gen]))
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
        {$dtwt *}  =+($(gen p.gen, gol %noun) (beth bool)) ::  XX  =|
        {$hand *}  [p.gen p.gen]
        {$ktbr *}
      =+(vat=$(gen p.gen) [(wrap(sut p.vat) %iron) (wrap(sut q.vat) %iron)])
    ::
        {$ktls *}
      =+  hif=[p=(nice (play p.gen)) q=(play(sut dox) p.gen)]
      =+($(gen q.gen, gol p.hif) hif)
    ::
        {$ktpd *}
      =+(vat=$(gen p.gen) [(wrap(sut p.vat) %zinc) (wrap(sut q.vat) %zinc)])
    ::
        {$tune *}
      [(face p.gen sut) (face p.gen dox)]
    ::
        {$ktwt *}
      =+(vat=$(gen p.gen) [(wrap(sut p.vat) %lead) (wrap(sut q.vat) %lead)])
    ::
        {$note *}
      =+  vat=$(gen q.gen)
      [(hint [sut p.gen] p.vat) (hint [dox p.gen] q.vat)]
    ::
        {$ktsg *}  $(gen p.gen)
        {$sgzp *}  ~_(duck(sut (play p.gen)) $(gen q.gen))
        {$sgbn *}  $(gen q.gen)
        {$tsbn *}
      =+  lem=$(gen p.gen, gol %noun)
      $(gen q.gen, sut p.lem, dox q.lem)
    ::
        {$tscm *}
      =/  boc  (busk p.gen)
      =/  nuf  (busk(sut dox) p.gen)
      $(gen q.gen, sut boc, dox nuf)
    ::
        {$wtcl *}
      =+  nor=$(gen p.gen, gol bool)
      =+  ^=  hiq  ^-  {p/type q/type}
          =+  fex=[p=(gain p.gen) q=(gain(sut dox) p.gen)]
          ?:  =(%void p.fex)
            :-  %void
            ?:  =(%void q.fex)
              %void
            ~>(%mean.'if-z' (play(sut q.fex) q.gen))
          ?:  =(%void q.fex)
            ~>(%mean.'mull-bonk-b' !!)
          $(sut p.fex, dox q.fex, gen q.gen)
      =+  ^=  ran  ^-  {p/type q/type}
          =+  wux=[p=(lose p.gen) q=(lose(sut dox) p.gen)]
          ?:  =(%void p.wux)
            :-  %void
            ?:  =(%void q.wux)
              %void
            ~>(%mean.'if-a' (play(sut q.wux) r.gen))
          ?:  =(%void q.wux)
            ~>(%mean.'mull-bonk-c' !!)
          $(sut p.wux, dox q.wux, gen r.gen)
      [(nice (fork p.hiq p.ran ~)) (fork q.hiq q.ran ~)]
    ::
        {$fits *}
      =+  waz=[p=(play p.gen) q=(play(sut dox) p.gen)]
      =+  ^=  syx  :-  p=(cove q:(mint %noun [%wing q.gen]))
                   q=(cove q:(mint(sut dox) %noun [%wing q.gen]))
      =+  pov=[p=(fish(sut p.waz) p.syx) q=(fish(sut q.waz) q.syx)]
      ?.  &(=(p.syx q.syx) =(p.pov q.pov))
        ~>(%mean.'mull-bonk-a' !!)
      (beth bool)
    ::
        {$wthx *}
      ~>  %mean.'mull-bonk-x'
      =+  :-  =+  (find %read [[%& 1] q.gen])
              ?>  &(?=(%& -.-) ?=(%& -.q.p.-))
              new=[type=p.q.p.- axis=(tend p.p.-)]
          =+  (find(sut dox) %read [%& 1] q.gen)
          ?>  &(?=(%& -.-) ?=(%& -.q.p.-))
          old=[type=p.q.p.- axis=(tend p.p.-)]
      ?>  =(axis.old axis.new)
      ?>  (nest(sut type.old) & type.new)
      (beth bool)
    ::
        {$dbug *}  ~_((show %o p.gen) $(gen q.gen))
        {$zpcm *}  [(nice (play p.gen)) (play(sut dox) p.gen)]
        {$lost *}
      ?:  vet
        ::  ~_  (dunk(sut (play p.gen)) 'also')
        ~>(%mean.'mull-skip' !!)
      (beth %void)
    ::
        {$zpts *}  (beth %noun)
    ::
        {$zpmc *}
      =+  vos=$(gol %noun, gen q.gen)       ::  XX validate!
      [(nice (cell (play p.gen) p.vos)) (cell (play(sut dox) p.gen) q.vos)]
    ::
        {$zpld *}
      ::  XX is this right?
      (beth (play [%kttr p.gen]))
    ::
        {$zpvt *}
      =+  [(feel p.gen) (feel(sut dox) p.gen)]
      ?.  =(-< ->)
        ~>(%mean.'mull-bonk-f' !!)
      ?:  -<
        $(gen q.gen)
      $(gen r.gen)
    ::
        {$zpzp *}  (beth %void)
        *
      =+  doz=~(open ap gen)
      ?:  =(doz gen)
        ~_  (show [%c 'hoon'] [%q gen])
        ~>(%mean.'mull-open' !!)
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
      |=  {mel/vair nym/(unit term) hud/poly ruf/hoon dom/(map term tome)}
      ::  make al
      ~_  leaf+"mull-grow"
      ^-  {p/type q/type}
      =+  dan=^$(gen ruf, gol %noun)
      =+  yaz=(mile(sut p.dan) q.dan mel nym hud dom)
      [(nice p.yaz) q.yaz]
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
        {$hint *}  dext(sut q.sut)
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
    ~%  %nest-in  ..$  ~
    |%
    ++  deem
      |=  {mel/vair ram/vair}
      ^-  ?
      ?.  |(=(mel ram) =(%lead mel) =(%gold ram))  |
      ?-  mel
        %lead  &
        %gold  meet
        %iron  dext(sut (peek(sut ref) %rite 2), ref (peek %rite 2))
        %zinc  dext(sut (peek %read 2), ref (peek(sut ref) %read 2))
      ==
    ::
    ++  deep
      |=  $:  dom/(map term tome)
              vim/(map term tome)
          ==
      ^-  ?
      ?:  ?=(~ dom)  =(vim ~)
      ?:  ?=(~ vim)  |
      ?&  =(p.n.dom p.n.vim)
          $(dom l.dom, vim l.vim)
          $(dom r.dom, vim r.vim)
      ::
          =+  [dab hem]=[q.q.n.dom q.q.n.vim]
          |-  ^-  ?
          ?:  ?=(~ dab)  =(hem ~)
          ?:  ?=(~ hem)  |
          ?&  =(p.n.dab p.n.hem)
              $(dab l.dab, hem l.hem)
              $(dab r.dab, hem r.hem)
              %=  dext
                sut  (play q.n.dab)
                ref  (play(sut ref) q.n.hem)
      ==  ==  ==
    ::
    ++  dext
      =<  $
      ~%  %nest-dext  +  ~
      |.
      ^-  ?
      =-  ?:  -  &
          ?.  tel  |
          ::  ~_  (dunk %need)
          ::  ~_  (dunk(sut ref) %have)
          ~>  %mean.'nest-fail'
          !!
      ?:  =(sut ref)  &
      ?-  sut
        $void      sint
        $noun      &
        {$atom *}  ?.  ?=({$atom *} ref)  sint
                   ?&  (fitz p.sut p.ref)
                       |(?=(~ q.sut) =(q.sut q.ref))
                   ==
        {$cell *}  ?.  ?=({$cell *} ref)  sint
                   ?&  dext(sut p.sut, ref p.ref, seg ~, reg ~)
                       dext(sut q.sut, ref q.ref, seg ~, reg ~)
                   ==
        {$core *}  ?.  ?=({$core *} ref)  sint
                   ?:  =(q.sut q.ref)  dext(sut p.sut, ref p.ref)
                   ?&  =(q.p.q.sut q.p.q.ref)  ::  same wet/dry
                       meet(sut q.q.sut, ref p.sut)
                       dext(sut q.q.ref, ref p.ref)
                       (deem(sut q.q.sut, ref q.q.ref) r.p.q.sut r.p.q.ref)
                       ?:  =(%wet q.p.q.sut)  =(q.r.q.sut q.r.q.ref)
                       ?|  (~(has in gil) [sut ref])
                           %.  [q.r.q.sut q.r.q.ref]
                           %=  deep
                             gil  (~(put in gil) [sut ref])
                             sut  sut(p q.q.sut, r.p.q %gold)
                             ref  ref(p q.q.ref, r.p.q %gold)
                       ==  ==
                   ==
        {$face *}  dext(sut q.sut)
        {$fork *}  ?.  ?=(?({$atom *} $noun {$cell *} {$core *}) ref)  sint
                   (lien ~(tap in p.sut) |=(type dext(tel |, sut +<)))
        {$hint *}  dext(sut q.sut)
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
        {$hint *}   dext(ref q.ref)
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
      =+  pec=(peel way r.p.q.sut)
      =/  tow
        ?:  =(1 lat)  1
        (cap lat)
      %=    ^$
          axe  lat
          sut
        ?:  ?|  =([& &] pec)
                &(sam.pec =(tow 2))
                &(con.pec =(tow 3))
            ==
          p.sut
        ~_  leaf+"payload-block"
        ?.  =(way %read)  !!
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
      {$brcn *}  (core sut [p.gen %dry %gold] sut *seminoun q.gen)
      {$brvt *}  (core sut [p.gen %wet %gold] sut *seminoun q.gen)
      {$cnts *}  ~(play et p.gen q.gen)
      {$dtkt *}  $(gen [%kttr p.gen])
      {$dtls *}  [%atom %$ ~]
      {$rock *}  |-  ^-  type
                 ?@  q.gen  [%atom p.gen `q.gen]
                 [%cell $(q.gen -.q.gen) $(q.gen +.q.gen)]
      {$sand *}  ?@  q.gen
                   ?:  =(%n p.gen)  ?>(=(0 q.gen) [%atom p.gen `q.gen])
                   ?:  =(%f p.gen)  ?>((lte q.gen 1) bool)
                   [%atom p.gen ~]
                 $(-.gen %rock)
      {$tune *}  (face p.gen sut)
      {$dttr *}  %noun
      {$dtts *}  bool
      {$dtwt *}  bool
      {$hand *}  p.gen
      {$ktbr *}  (wrap(sut $(gen p.gen)) %iron)
      {$ktls *}  $(gen p.gen)
      {$ktpd *}  (wrap(sut $(gen p.gen)) %zinc)
      {$ktsg *}  $(gen p.gen)
      {$ktwt *}  (wrap(sut $(gen p.gen)) %lead)
      {$note *}  (hint [sut p.gen] $(gen q.gen))
      {$sgzp *}  ~_(duck(sut ^$(gen p.gen)) $(gen q.gen))
      {$sgbn *}  $(gen q.gen)
      {$tsbn *}  $(gen q.gen, sut $(gen p.gen))
      {$tscm *}  $(gen q.gen, sut (busk p.gen))
      {$wtcl *}  =+  [fex=(gain p.gen) wux=(lose p.gen)]
                 %-  fork  :~
                   ?:(=(%void fex) %void $(sut fex, gen q.gen))
                   ?:(=(%void wux) %void $(sut wux, gen r.gen))
                 ==
      {$fits *}  bool
      {$wthx *}  bool
      {$dbug *}  ~_((show %o p.gen) $(gen q.gen))
      {$zpcm *}  $(gen p.gen)
      {$lost *}  %void
      {$zpmc *}  (cell $(gen p.gen) $(gen q.gen))
      {$zpld *}  (play [%kttr p.gen])
      {$zpts *}  %noun
      {$zpvt *}  ?:((feel p.gen) $(gen q.gen) $(gen r.gen))
      {$zpzp *}  %void
      *          =+  doz=~(open ap gen)
                 ?:  =(doz gen)
                   ~_  (show [%c 'hoon'] [%q gen])
                   ~>  %mean.'play-open'
                   !!
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
      ?.  ?=({* ~ ~} wec)
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
        dext(hos [p.sut hos], sut q.sut)
      ::
          {$hint *}
        ::  work through hint
        ::
        (hint p.sut dext(sut q.sut))
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
      $(lov t.lov, sut (face i.lov sut))
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
          {$hint *}  $(ref q.ref)
          {$face *}
        ::  extend all stacks in set
        ::
        %=  $
          ref  q.ref
          wec  (~(run in wec) |=((list tool) [p.ref +<]))
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
      {$hint *}   q.sut
      {$hold *}   (rest [[p.sut q.sut] ~])
      $noun       (fork [%atom %$ ~] [%cell %noun %noun] ~)
      *           ~>(%mean.'repo-fltt' !!)
    ==
  ::
  ++  rest
    ~/  %rest
    |=  leg/(list {p/type q/hoon})
    ^-  type
    ?:  (lien leg |=({p/type q/hoon} (~(has in fan) [p q])))
      ~>(%mean.'rest-loop' !!)
    =>  .(fan (~(gas in fan) leg))
    %-  fork
    %~  tap  in
    %-  ~(gas in *(set type))
    (turn leg |=({p/type q/hoon} (play(sut p) q)))
  ::
  ++  take
    |=  {vit/vein duz/$-(type type)}
    ^-  (pair axis type)
    :-  (tend vit)
    =.  vit  (flop vit)
    |-  ^-  type
    ?~  vit  (duz sut)
    ?~  i.vit
      |-  ^-  type
      ?+  sut      ^$(vit t.vit)
        {$face *}  (face p.sut ^$(vit t.vit, sut q.sut))
        {$hint *}  (hint p.sut ^$(sut q.sut))
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
      {$hint *}  (hint p.sut $(sut q.sut))
      {$hold *}  ?:  (~(has in vil) sut)
                   %void
                 $(sut repo, vil (~(put in vil) sut))
    ==
  ::
  ++  tack
    |=  {hyp/wing mur/type}
    ~_  (show [%c %tack] %l hyp)
    =+  fid=(find %rite hyp)
    ?>  ?=(%& -.fid)
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
      {$core *}  ?>(|(=(%gold r.p.q.sut) =(%lead yoz)) sut(r.p.q yoz))
      {$face *}  (face p.sut $(sut q.sut))
      {$fork *}  (fork (turn ~(tap in p.sut) |=(type ^$(sut +<))))
      {$hint *}  (hint p.sut $(sut q.sut))
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
                    {$bswt p/(list wine)}               ::
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
          {$bswt *}
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
  ++  dish  !:
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
      =+  cis=;;(tank .*(vol [%9 2 %0 1]))
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
      [[%leaf (trip ;;(@ -.lum))] $(lum +.lum)]
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
        {$bswt *}
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
      ?:  ?=({* ~} p.q.ham)
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
      ?>  ?=({~ $leaf ^} fox)
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
      ?:  ?&  ?=  {$bswt {$pear $n $0} {$plot {$pear $n $0} {$face *} ~} ~}
                q.ham
              =(1 (met 3 p.i.t.p.i.t.p.q.ham))
          ==
        [%unit =<([p q] i.t.p.i.t.p.q.ham)]
      q.ham
    =+  may=(~(get by p.ham) p.q.ham)
    ?~  may
      q.ham
    =+  nul=[%pear %n 0]
    ?.  ?&  ?=({$bswt *} u.may)
            ?=({* * ~} p.u.may)
            |(=(nul i.p.u.may) =(nul i.t.p.u.may))
        ==
      q.ham
    =+  din=?:(=(nul i.p.u.may) i.t.p.u.may i.p.u.may)
    ?:  ?&  ?=({$plot {$face *} {$face * $stop *} ~} din)
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
                    {{$face * $stop *} ~}
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
        %-  ~(rep by (~(run by q.r.q.sut) |=(tome ~(wyt by q.+<))))
        |=([[@ a=@u] b=@u] (add a b))
      %^  cat  3
        ?-(r.p.q.sut $gold '.', $iron '|', $lead '?', $zinc '&')
      =+  gum=(mug q.r.q.sut)
      %+  can  3
      :~  [1 (add 'a' (mod gum 26))]
          [1 (add 'a' (mod (div gum 26) 26))]
          [1 (add 'a' (mod (div gum 676) 26))]
      ==
    ::
        {$hint *}
      $(sut q.sut)
    ::
        {$face *}
      =+  yad=$(sut q.sut)
      ?^(p.sut yad [p.yad [%face p.sut q.yad]])
    ::
        {$fork *}
      =+  yed=(sort ~(tap in p.sut) aor)
      =-  [p [%bswt q]]
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
++  levi                                                ::  $-([type type] ?)
  |=  [a=type b=type]
  (~(nest ut a) & b)
::
++  text                                                ::  tape pretty-print
  |=  vax/vase  ^-  tape
  ~(ram re (sell vax))
::
++  seem  |=(toy/typo `type`toy)                        ::  promote typo
++  seer  |=(vix/vise `vase`vix)                        ::  promote vise
::
::  +sell Pretty-print a vase to a tank using `deal`.
::
++  sell
  ~/  %sell
  |=  vax/vase
  ^-  tank
  ~|  %sell
  (~(deal us p.vax) q.vax)
::
::  These are the public types for the `xray` library.  Analysing a type
::  yields an `ximage`, and everything else here is just some structure
::  within that.
::
::  `ximage`s can be printed as specs (hoon syntax for types), and can
::  be used to pretty-print typed data.
::
::  |%
::
::  An `xtable` is a graph of types referenced by the top-level type,
::  and the `root` `key` points to the node which corresponds to the
::  type under analysis.
::
+$  ximage  [root=xkey =xtable]
::
::  A `xkey` is just an identifier for a node in the xray graph.
::
+$  xkey  @
::
::  An `xtable` is the xray graph itself. It contains one node for for
::  the type that was analyzed and one node for every type referenced
::  within that type.
::
::  The `next` field is the the next available xkey (used when inserting
::  new xrays), `xrays` maps keys to graph nodes and `type-map` gives
::  the xkey corresponding to a type.
::
::  The `type-map` is basically just the reverse of the `xrays` map. It
::  doesn't contain any new information, but is needed for performance
::  reasons.
::
+$  xtable  [next=xkey xrays=(map xkey xray) =type=(map type xkey)]
::
::  An `xray` is a node in the `ximage` graph. It contains everything
::  we know about a certain `type`. `key` is its identifier in the graph,
::  `type` is the type that it's an xray of, and `xdat` is the basic
::  information we derived about the type.  The basic references to other
::  nodes are inside the `xdat` structure, though some of the other
::  fields may contain references as well.
::
::  - `xshape` is some more information about the xshape of data within
::     a cell.
::  - `xrole` expands on `xshape`, adding further information about the
::     xrole that a node has within a fork.
::  - `pats` is used for printing data: we want to know if this type
::    can be printed as a list, as json, as a tape literal, etc.
::  - `recipes` contains information about how a type was
::     constructed. It's used to get much nicer output when printing types.
::  - `studs` contains "standards names". I actually don't know what this is.
::  - `helps` contains all the documentation about a type.
::  - `loop` indicates whether or not a node references itself. The list
::    type is cyclical, for example. This is used when printing an
::    `ximage`.
::
+$  xray
  $:  =xkey
      =type
      xdat=(unit xdat)
      xrole=(unit xrole)
      pats=(unit xpat)
      studs=(set stud)
      recipes=(set recipe)
      helps=(set help)
      xshape=(unit xshape)
      loop=(unit ?)
  ==
::
::  - `%void` -- impossible to create.
::  - `%noun` -- could be any noun.
::  - `%atom` -- An atom of some aura, possibly constant
::  - `%cell` -- A cell with a head and a tail.
::  - `%core` -- A core, its garb, its context type, and the types of
::     each of its arms.
::  - `%face` -- A face on another type.
::  - `%fork` -- Could be one or more other types.
::  - `%pntr` -- This is an internal hack, it should never survive
::     analysis; ignore.
::
+$  xdat
  $@  ?(%noun %void)
  $%  [%atom =aura constant=(unit @)]
      [%cell head=xkey tail=xkey]
      [%core =garb xray=xkey batt=xbat]
      [%face face=$@(term tune) xray=xkey]
      [%fork =(set xkey)]
      [%pntr xray=xkey]
  ==
::
::  The basic xshape of a type:
::
::  - `%void` -- impossible to create.
::  - `%noun` -- could be any noun.
::  - `%atom` -- always some type of atom; never a cell
::  - `%cell` -- always some type of cell; never an atom.
::  - `%junc` -- is a fork of a cell type and an atom type.
::
+$  xshape  ?(%void %noun %atom %cell %junc)
::
::  A `xrole` is the of a type, including a more refined understanding
::  of what xrole it plays within a fork.
::
::  Nodes referenced within a `xrole` often do not actually exist in the
::  original type, since we need to reorganize forks in order to make
::  them more coherent.
::
::  - `%void` -- impossible to create.
::  - `%noun` -- could be any noun.
::  - `%atom` -- always some type of atom; never a cell
::  - `%constant` -- a cell type whose head is a constant atom.
::  - `%tall` -- a cell type whose head is an atom.
::  - `%wide` -- a cell type whose head is also a cell
::  - `%instance` -- a cell type whose head is a constant atom.
::  - `%option` -- a union of types which are all constant atoms.
::  - `%union` -- a union of types which are all instances (cells whose
::    head is a constant atom).
::  - `%junction` -- a union of an atom type and a cell type.
::  - `%conjunction` -- a union of two cell types, one of them %wide
::     and the other %tall.
::  - `%misjunction` -- any other union type. There's no efficient way
::    to tell which branch to take when analyzing a fork which is a
::    %misjunction, and the type is probably improperly constructed.
::
+$  xrole
  $@  $?  %void  %noun  %atom  %tall  %wide  ==
  $%  [%constant =atom]
      [%instance =atom]
      [%option =(map atom xkey)]
      [%union =(map atom xkey)]
      [%junction flat=xkey deep=xkey]
      [%conjunction wide=xkey tall=xkey]
      [%misjunction one=xkey two=xkey]
  ==
::
::  This is just a utility type, it encodes the "battery" structure
::  within a core.
::
::  It's a map from chapter names to the documentation and arms within
::  that chapter.
::
+$  xbat  (map term (pair what (map term xkey)))
::
::  A recipe tells us how a type was constructed.
::
::  - `%direct` is a simple type like `term`, or `xray`.
::  - `%synthetic` is a constructed type, like `(list @)`.
::
+$  recipe
  $%  [%direct =term]
      [%synthetic =term =(list xkey)]
  ==
::
::  A `xpat` is high-level information about the shape of a type. This
::  is used for printing data.
::
::  This is fairly heuristic. [%a %b %c ~] is recognized as a `path`,
::  `[3 ~[4 5 6]]` is recognized as a list, etc.
::
::  Most of the xpats have names that make their purpose obvious:
::  for example, the %tape xpat means that data of type type can be
::  printed as if it had the `tape` type. However, `%gear` and `%gate`
::  might not be entirely obvious.
::
::  - The %gear xpat is any core with a cell subject.
::  - The %gate xpat is a core that looks like a gate.
::
+$  xpat
  $@  ?(%hoon %manx %json %nock %path %plum %skin %spec %tape %tour %type %vase)
  $%  [%gate sample=xkey product=xkey]
      [%gear sample=xkey context=xkey batt=xbat]
      [%list item=xkey]
      [%tree item=xkey]
      [%unit item=xkey]
  ==
::
::  Left-fold over a list.
::
::  This is `roll`, but with explicit type parameters.
::
++  fold
   |*  [state=mold elem=mold]
   |=  [[st=state xs=(list elem)] f=$-([state elem] state)]
   ^-  state
   |-
   ?~  xs  st
   $(xs t.xs, st (f st i.xs))
::
::  This is basically a `mapM` over a list using the State monad.
::
::  Another way to think about this is that it is the same as `turn`,
::  except that a state variable `st` is threaded through the
::  execution. The list is processed from left to right.
::
::  This is `spin`, but with explicit type parameters.
::
++  traverse
  |*  [state=mold in=mold out=mold]
  |=  [[st=state xs=(list in)] f=$-([state in] [out state])]
  ^-  [(list out) state]
  ?~  xs  [~ st]
  =^  r   st  (f st i.xs)
  =^  rs  st  $(xs t.xs, st st)
  [[r rs] st]
::
::  `traverse` over a set.
::
++  traverse-set
  |*  [state=mold input=mold out=mold]
  |=  [[st=state xs=(set input)] f=$-([state input] [out state])]
  ^-  [(set out) state]
  ::
  =^  elems  st  ((traverse state input out) [st ~(tap in xs)] f)
  :_  st  (~(gas in *(set out)) elems)
::
::  `traverse` over a map, also passing the key to the folding function.
::
++  traverse-map
  |*  [state=mold key=mold in=mold out=mold]
  |=  [[st=state dict=(map key in)] f=$-([state key in] [out state])]
  ^-  [(map key out) state]
  ::
  =^  pairs=(list (pair key out))  st
    %+  (traverse state (pair key in) (pair key out))
      [st ~(tap by dict)]
    |=  [st=state k=key x=in]
    ^-  [(pair key out) state]
    =^  v  st  (f st k x)
    [[k v] st]
  ::
  :_  st
  (~(gas by *(map key out)) pairs)
::
::  Given a map, return its inverse: For each value, what are the set
::  of associated keys?
::
++  reverse-map
  |*  [key=mold val=mold]
  |=  tbl=(map key val)
  =/  init  *(map val (set key))
  ^-  _init
  %+  (fold _init (pair key val))
    [init ~(tap by tbl)]
  |=  [acc=_init k=key v=val]
  ^-  _init
  =/  mb-keys         (~(get by acc) v)
  =/  keys=(set key)  ?~(mb-keys ~ u.mb-keys)
  (~(put by acc) v (~(put in keys) k))
::
++  json                                                ::  normal json value
  $@  ~                                                 ::  null
  $%  {$a p/(list json)}                                ::  array
      {$b p/?}                                          ::  boolean
      {$o p/(map @t json)}                              ::  object
      {$n p/@ta}                                        ::  number
      {$s p/@t}                                         ::  string
  ==                                                    ::
::
::  +skol  $-(type tank) using `duck`.
::
++  skol
  |=  typ/type
  ^-  tank
  ~(duck ut typ)
::
++  slam                                                ::  slam a gate
  |=  {gat/vase sam/vase}  ^-  vase
  =+  :-  ^=  typ  ^-  type
          [%cell p.gat p.sam]
      ^=  gen  ^-  hoon
      [%cnsg [%$ ~] [%$ 2] [%$ 3] ~]
  =+  gun=(~(mint ut typ) %noun gen)
  [p.gun (slum q.gat q.sam)]
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
      {$hint *}  $(typ ~(repo ut typ))
      {$core *}
    |-  ^-  ?
    ?~  q.r.q.typ  |
    ?|  (~(has by q.q.n.q.r.q.typ) cog)
        $(q.r.q.typ l.q.r.q.typ)
        $(q.r.q.typ r.q.r.q.typ)
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
    %+  turn  ~(tap by q.r.q.typ)
      |=  {* b/tome}
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
++  sped                                                ::  reconstruct type
  |=  vax/vase
  ^-  vase
  :_  q.vax
  ?@  q.vax  (~(fuse ut p.vax) [%atom %$ ~])
  ?@  -.q.vax
    ^=  typ
    %-  ~(play ut p.vax)
    [%wtbn [%wtts [%leaf %tas -.q.vax] [%& 2]~] [%$ 1]]
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
            (more net limp)
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
              (ifix [lac rac] wide)
              (stag %cncl (ifix [lit rit] (most ace wide)))
              (stag %sand (stag %tas (cold %$ bus)))
              (stag %sand (stag %t qut))
              %+  cook
                |=(a/coin [%sand ?:(?=({~ $tas *} a) %tas %ta) ~(rent co a)])
              nuck:so
            ==
  ++  limp  %+  cook
              |=  {a/(list) b/tyke}
              ?~  a  b
              $(a t.a, b [`[%sand %tas %$] b])
            ;~(plug (star net) gasp)
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
        ;~  plug                                        :: can duplicate ::
          (into ;~(pfix (punt ;~(plug null col ban step)) line))
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
    ::  into: :: and indent to end of line, consuming following space.
    ::  indo: :: to end of line, consuming following space.
    ::  exit: :: to end of line, not consuming following space.
    ::
    ++  step  ;~(plug ace ace)
    ++  into  |*(bod/rule (indo ;~(pfix step bod)))
    ::
    ++  indo
      |*  bod/rule
      ;~(pfix col ban ;~(sfix bod (just `@`10) (punt gap)))
    ::
    ++  exit
      |*  bod/rule
      ;~(pfix (star ace) col led step bod)
    ::
    ::  fill: full definition
    ::
    ++  fill
      %+  cook  |=({{a/term b/cord} c/(list sect) (unit ~)} [a b c])
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
    =+  wod=|=({a/tape b/(list hoon)} ^+(b ?~(a b [[%mcnt %knit (flop a)] b])))
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
      ;~(pfix net gash)
    ==
  ::
  ++  rump
    %+  sear
      |=  {a/wing b/(unit hoon)}  ^-  (unit hoon)
      ?~(b [~ %wing a] ?.(?=({@ ~} a) ~ [~ [%rock %tas i.a] u.b]))
    ;~(plug rope ;~(pose (stag ~ wede) (easy ~)))
  ::
  ++  rood
    ;~  pfix  net
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
        |=  tum/(each manx:hoot marl:hoot)  ^-  hoon
        ?-  -.tum
          %&  [%xray p.tum]
          %|  [%mcts p.tum]
        ==
      top-level
    ::
    ++  top-level                                       ::  entry-point
      ;~(pfix mic ?:(in-tall-form tall-top wide-top))
    ::
    ++  inline-embed                                    ::  brace interpolation
      %+  cook  |=(a/tuna:hoot a)
      ;~  pose
        ;~(pfix mic bracketed-elem(in-tall-form |))
        ;~(plug tuna-mode sump)
        (stag %tape sump)
      ==
    ::
    ++  script-or-style                                 ::  script or style
      %+  cook  |=(a/marx:hoot a)
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
      %+  knee  *(each manx:hoot marl:hoot)  |.  ~+
      ;~  pose
        (stag %| wide-quote)
        (stag %| wide-paren-elems)
        (stag %& ;~(plug tag-head wide-tail))
      ==
    ::
    ++  wide-inner-top                                  ::  wide inner top
      %+  knee  *(each tuna:hoot marl:hoot)  |.  ~+
      ;~  pose
        wide-top
        (stag %& ;~(plug tuna-mode wide))
      ==
    ::
    ++  wide-attrs                                      ::  wide attributes
      %+  cook  |=(a/(unit mart:hoot) (fall a ~))
      %-  punt
      %+  ifix  [lit rit]
      %+  more  (jest ', ')
      ;~((glue ace) a-mane hopefully-quote)
    ::
    ++  wide-tail                                       ::  wide elements
      %+  cook  |=(a/marl:hoot a)
      ;~(pose ;~(pfix col wrapped-elems) (cold ~ mic) (easy ~))
    ::
    ++  wide-elems                                      ::  wide elements
      %+  cook  |=(a/marl:hoot a)
      %+  cook  join-tops
      (star ;~(pfix ace wide-inner-top))
    ::
    ++  wide-paren-elems                                ::  wide flow
      %+  cook  |=(a/marl:hoot a)
      %+  cook  join-tops
      (ifix [lit rit] (more ace wide-inner-top))
    ::
    ::+|
    ::
    ++  drop-top
      |=  a/(each tuna:hoot marl:hoot)  ^-  marl:hoot
      ?-  -.a
        %&  [p.a]~
        %|  p.a
      ==
    ::
    ++  join-tops
      |=  a/(list (each tuna:hoot marl:hoot))  ^-  marl:hoot
      (zing (turn a drop-top))
    ::
    ::+|
    ::
    ++  wide-quote                                      ::  wide quote
      %+  cook  |=(a/marl:hoot a)
      ;~  pose
        ;~  less  (jest '"""')
          (ifix [yel yel] (cook collapse-chars quote-innards))
        ==
      ::
        %-  inde
        %+  ifix  [(jest '"""\0a') (jest '\0a"""')]
        (cook collapse-chars quote-innards(lin |))
      ==
    ::
    ++  quote-innards                                   ::  wide+tall flow
      %+  cook  |=(a/(list $@(@ tuna:hoot)) a)
      %-  star
      ;~  pose
        ;~(pfix bas ;~(pose (mask "-+*%;\{") bas yel bix:ab))
        inline-embed
        ;~(less bas lob ?:(in-tall-form fail toc) prn)
        ?:(lin fail ;~(less (jest '\0a"""') (just '\0a')))
      ==
    ::
    ++  bracketed-elem                                  ::  bracketed element
      %+  ifix  [lob rob]
      ;~(plug tag-head wide-elems)
    ::
    ++  wrapped-elems                                   ::  wrapped tuna
      %+  cook  |=(a/marl:hoot a)
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
      ;~  plug
        mixed-case-symbol
        ;~  pose
          %+  stag  ~
            ;~(pfix cab mixed-case-symbol)
          (easy ~)
        ==
      ==
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
        |=  {a/mane:hoot b/mart:hoot c/mart:hoot}
        ^-  marx:hoot
        [a (weld b c)]
      ;~  plug
        a-mane
      ::
        %+  cook
          |=  a/(list (unit {term (list beer:hoot)}))
          ^-  (list {term (list beer:hoot)})
          :: discard nulls
          (murn a same)
        ;~  plug
          (punt ;~(plug (cold %id hax) (cook trip sym)))
          (cook en-class (star ;~(plug (cold %class dot) sym)))
          (punt ;~(plug ;~(pose (cold %href net) (cold %src vat)) soil))
          (easy ~)
        ==
      ::
        wide-attrs
      ==
    ::
    ::+|
    ::
    ++  tall-top                                        ::  tall top
      %+  knee  *(each manx:hoot marl:hoot)  |.  ~+
      ;~  pose
        (stag %| ;~(pfix (plus ace) (cook collapse-chars quote-innards)))
        (stag %& ;~(plug script-or-style script-style-tail))
        (stag %& tall-elem)
        (stag %| wide-quote)
        (stag %| ;~(pfix tis tall-tail))
        (stag %& ;~(pfix ban gap (stag [%div ~] cram)))
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
        |=  {a/{p/mane:hoot q/mart:hoot} b/mart:hoot c/marl:hoot}
        ^-  manx:hoot
        [[p.a (weld q.a b)] c]
      ;~(plug tag-head tall-attrs tall-tail)
    ::
    ::+|
    ::
    ::REVIEW is there a better way to do this?
    ++  hopefully-quote                                 :: prefer "quote" form
      %+  cook  |=(a/(list beer:hoot) a)
      %+  cook  |=(a/hoon ?:(?=($knit -.a) p.a [~ a]~))
      wide
    ::
    ++  script-style-tail                               ::  unescaped tall tail
      %+  cook  |=(a/marl:hoot a)
      %+  ifix  [gap ;~(plug gap duz)]
      %+  most  gap
      ;~  pfix  mic
        %+  cook  |=(a/tape ;/(a))
        ;~  pose
          ;~(pfix ace (star prn))
          (easy "\0a")
        ==
      ==
    ::
    ++  tall-tail                                       ::  tall tail
      ?>  in-tall-form
      %+  cook  |=(a/marl:hoot a)
      ;~  pose
        (cold ~ mic)
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
      |=  reb/(list $@(@ tuna:hoot))
      ^-  marl:hoot
      =|  {sim/(list @) tuz/marl:hoot}
      |-  ^-  marl:hoot
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
        ++  item  (pair mite marl:hoot)                 ::  xml node generator
        ++  colm  @ud                                   ::  column
        ++  tarp  marl:hoot                             ::  node or generator
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
              {$mage p/tape q/tape}                     ::  image
              {$expr p/tuna:hoot}                       ::  interpolated hoon
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
        =/  nex/@ud  cur-indent  ::  REVIEW code and poem blocks are
                                 ::  handled elsewhere
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
        ?:  ?=({~ @ $end ?($stet $dent)} saw)           ::  stop on == or dedent
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
          %-  zing
          %+  turn  (flop q.u.par)
          |=  a/tape
          (runt [(dec inr.ind) ' '] "{a}\0a")
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
        ?:  ?|  ?=(~ par)                          :: if after a paragraph or
                ?&  ?=(?($down $lime $bloc) p.cur)  :: unspaced new container
                    |(!=(%old -.sty.saw) (gth col.saw inr.ind))
            ==  ==
          =>  .(..$ close-par)
            ::
          ::  if column has retreated, adjust stack
          =.  ..$  (back col.saw)
            ::
          =^  col-ok  sty.saw
            ?+  (sub col.saw inr.ind)  [| sty.saw]      :: columns advanced
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
            |=({a/pint b/?(~ trig-style)} ?~(b ~ `[q.p.a b]))
          ;~  pose
            (cold ~ (just `@`10))                       ::  blank line
          ::
            (full (easy [%end %done]))                  ::  end of input
            (cold [%end %stet] duz)                     ::  == end of markdown
          ::
            (cold [%one %rule] ;~(plug hep hep hep))    ::  --- horizontal ruler
            (cold [%one %fens] ;~(plug tec tec tec))    ::  ``` code fence
            (cold [%one %expr] mic)                     ::  ;sail expression
          ::
            (cold [%new %head] ;~(plug (star hax) ace)) ::  # heading
            (cold [%new %lint] ;~(plug hep ace))        ::  - line item
            (cold [%new %lite] ;~(plug lus ace))        ::  + line item
            (cold [%new %bloc] ;~(plug ban ace))        ::  > block-quote
          ::
            (easy [%old %text])                         ::  anything else
          ==
        ==
      ::
      ::
      ++  calf                                          ::  cash but for tec tec
        |*  tem=rule
        %-  star
        ;~  pose
          ;~(pfix bas tem)
          ;~(less tem prn)
        ==
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
        %+  cook
          |=  a/$%(graf [%list (list graf)])
          ^-  (list graf)
          ?:(?=(%list -.a) +.a [a ~])
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
          (stag %bold (ifix [tar tar] (cool (cash tar) werk)))
        ::
        ::  _italic literal_
        ::
          (stag %talc (ifix [cab cab] (cool (cash cab) werk)))
        ::
        ::  "quoted text"
        ::
          (stag %quod (ifix [yel yel] (cool (cash yel) werk)))
        ::
        ::  `classic markdown quote`
        ::
          (stag %code (ifix [tec tec] (calf tec)))
        ::
        ::  ++arm, +-arm, +$arm, +*arm, ++arm:core, ...
        ::
          %+  stag  %code
          ;~  plug
            lus  ;~(pose lus hep bus tar)
            low  (star ;~(pose nud low hep col))
          ==
        ::
        ::  [arbitrary *content*](url)
        ::
          %+  stag  %link
          ;~  (glue (punt whit))
            (ifix [lac rac] (cool (cash rac) werk))
            (ifix [lit rit] (cash rit))
          ==
        ::
        ::  ![alt text](url)
        ::
          %+  stag  %mage
          ;~  pfix  zap
            ;~  (glue (punt whit))
              (ifix [lac rac] (cash rac))
              (ifix [lit rit] (cash rit))
            ==
          ==
        ::
        ::  #hoon
        ::
          %+  stag  %list
          ;~  plug
            (stag %text ;~(pose (cold " " whit) (easy-sol ~)))
            (stag %code ;~(pfix hax (echo wide)))
            ;~(simu whit (easy ~))
          ==
        ::
        ::  direct hoon constant
        ::
          %+  stag  %list
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
              ;~(pfix cen ;~(pose sym bus pad bar qut nuck:so))
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
      ++  werk  (cook zing (star word))                 ::  indefinite tarp
      ::
      ++  down                                          ::  parse inline tarp
        %+  knee  *tarp  |.  ~+
        =-  (cook - werk)
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
          ^-  tarp  ::CHECK can be tuna:hoot?
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
            $mage  [[%img [%src q.nex] ?~(p.nex ~ [%alt p.nex]~)] ~]~
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
        (cook drop-top top-level)                       ::  list of tags
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
        |=  a/(list tuna:hoot)  ^-  tape
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
              {{$$ {$$ *} ~} ~}                       ::  text node contents
            (murn v.i.a.g.i.a |=(a/beer:hoot ?^(a ~ (some a))))
              {^ *}  $(a c.i.a)                         ::  concatenate children
              {@ *}  ~                                  ::  ignore interpolation
          ==
        $(a t.a)
      --
    --
  ::
  ++  scad
    %+  knee  *spec  |.  ~+
    %-  stew
    ^.  stet  ^.  limo
    :~
      :-  '_'
        ;~(pfix cab (stag %bscb wide))
      :-  ','
        ;~(pfix com (stag %bsmc wide))
      :-  '$'
        ;~  pose
          ;~  pfix  bus
            ;~  pose
            ::  XX all three deprecated
            ::
              (stag %leaf (stag %tas (cold %$ bus)))
              (stag %leaf (stag %t qut))
              (stag %leaf (sear |=(a/coin ?:(?=($$ -.a) (some +.a) ~)) nuck:so))
            ==
          ==
          (stag %like (most col rope))
        ==
      :-  '%'
        ;~  pose
          ;~  pfix  cen
            ;~  pose
              (stag %leaf (stag %tas (cold %$ bus)))
              (stag %leaf (stag %f (cold & pad)))
              (stag %leaf (stag %f (cold | bar)))
              (stag %leaf (stag %t qut))
              (stag %leaf (sear |=(a/coin ?:(?=($$ -.a) (some +.a) ~)) nuck:so))
            ==
          ==
        ==
      :-  '('
        %+  cook  |=(spec +<)
        %+  stag  %make
        %+  ifix  [lit rit]
        ;~  plug
          wide
          ;~(pose ;~(pfix ace (most ace wyde)) (easy ~))
        ==
      :-  '{'
        ::  XX deprecated
        ::
        (stag %bscl (ifix [lob rob] (most ace wyde)))
      :-  '['
        (stag %bscl (ifix [lac rac] (most ace wyde)))
      :-  '*'
        (cold [%base %noun] tar)
      :-  '/'
        ;~(pfix net (stag %loop ;~(pose (cold %$ bus) sym)))
      :-  '@'
        ;~(pfix vat (stag %base (stag %atom mota)))
      :-  '?'
        ;~  pose
          %+  stag  %bswt
          ;~(pfix wut (ifix [lit rit] (most ace wyde)))
        ::
          (cold [%base %flag] wut)
        ==
      :-  '~'
        (cold [%base %null] sig)
      :-  '!'
        (cold [%base %void] ;~(plug zap zap))
      :-  '^'
        ;~  pose
          (stag %like (most col rope))
          (cold [%base %cell] ket)
        ==
      :-  '='
        ;~  pfix  tis
          %+  sear
            |=  [=(unit term) =spec]
            %+  bind
              ~(autoname ax & spec)
            |=  =term
            =*  name  ?~(unit term (cat 3 u.unit (cat 3 '-' term)))
            [%bsts name spec]
          ;~  pose
            ;~(plug (stag ~ ;~(sfix sym tis)) wyde)
            (stag ~ wyde)
          ==
        ==
      :-  ['a' 'z']
        ;~  pose
          (stag %bsts ;~(plug sym ;~(pfix ;~(pose net tis) wyde)))
          (stag %like (most col rope))
        ==
    ==
  ::
  ++  scat
    %+  knee  *hoon  |.  ~+
    %-  stew
    ^.  stet  ^.  limo
    :~
      :-  ','
        ;~  pose
          (stag %ktcl ;~(pfix com wyde))
          (stag %wing rope)
        ==
      :-  '!'
        ;~  pose
          (stag %wtzp ;~(pfix zap wide))
          (stag %zpzp (cold ~ ;~(plug zap zap)))
        ==
      :-  '_'
        ;~(pfix cab (stag %ktcl (stag %bscb wide)))
      :-  '$'
        ;~  pose
          ;~  pfix  bus
            ;~  pose
              ::  XX: these are all obsolete in hoon 142
              ::
              (stag %leaf (stag %tas (cold %$ bus)))
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
            (stag %rock (stag %tas (cold %$ bus)))
            (stag %rock (stag %f (cold & pad)))
            (stag %rock (stag %f (cold | bar)))
            (stag %rock (stag %t qut))
            (cook (jock &) nuck:so)
            (stag %clsg (sear |=(a/(list) (posh ~ ~ (lent a) ~)) (star cen)))
          ==
        ==
      :-  '&'
        ;~  pose
          (cook |=(a/wing [%cnts a ~]) rope)
          (stag %wtpd ;~(pfix pad (ifix [lit rit] (most ace wide))))
          ;~(plug (stag %rock (stag %f (cold & pad))) wede)
          (stag %sand (stag %f (cold & pad)))
        ==
      :-  '\''
        (stag %sand (stag %t qut))
      :-  '('
        (stag %cncl (ifix [lit rit] (most ace wide)))
      :-  '{'
        (stag %ktcl (stag %bscl (ifix [lob rob] (most ace wyde))))
      :-  '*'
        ;~  pose
          (stag %kttr ;~(pfix tar wyde))
          (cold [%base %noun] tar)
        ==
      :-  '@'
        ;~(pfix vat (stag %base (stag %atom mota)))
      :-  '+'
        ;~  pose
          (stag %dtls ;~(pfix lus (ifix [lit rit] wide)))
        ::
          %+  cook
            |=  a/(list (list woof))
            :-  %mcnt
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
            (stag %mccl (ifix [lit rit] (most ace wide)))
            ;~(pfix net (stag %mcnt wide))
          ==
        ==
      :-  '='
        ;~  pfix  tis
          ;~  pose
            (stag %dtts (ifix [lit rit] ;~(glam wide wide)))
          ::
            %+  sear
              ::  mainly used for +skin formation
              ::
              |=  =spec
              ^-  (unit hoon)
              %+  bind  ~(autoname ax & spec)
              |=(=term `hoon`[%ktts term %kttr spec])
            wyde
          ==
        ==
      :-  '?'
        ;~  pose
          %+  stag  %ktcl
          (stag %bswt ;~(pfix wut (ifix [lit rit] (most ace wyde))))
        ::
          (cold [%base %flag] wut)
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
            ;~(pfix vat ;~(plug mota ;~(pfix tec wide)))
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
          (stag %wtbr ;~(pfix bar (ifix [lit rit] (most ace wide))))
          ;~(plug (stag %rock (stag %f (cold | bar))) wede)
          (stag %sand (stag %f (cold | bar)))
        ==
      :-  '~'
        ;~  pose
          rupl
        ::
          ;~  pfix  sig
            ;~  pose
              (stag %clsg (ifix [lac rac] (most ace wide)))
            ::
              %+  stag  %cnsg
              %+  ifix
                [lit rit]
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
        (ifix [led ban] (stag %tell (most ace wide)))
      :-  '>'
        (ifix [ban led] (stag %yell (most ace wide)))
    ==
  ++  soil
    ;~  pose
      ;~  less  (jest '"""')
        %+  ifix  [yel yel]
        %-  star
        ;~  pose
          ;~(pfix bas ;~(pose bas yel lob bix:ab))
          ;~(less yel bas lob prn)
          (stag ~ sump)
        ==
      ==
    ::
      %-  iny  %+  ifix
        [(jest '"""\0a') (jest '\0a"""')]
      %-  star
      ;~  pose
        ;~(pfix bas ;~(pose bas lob bix:ab))
        ;~(less bas lob prn)
        ;~(less (jest '\0a"""') (just `@`10))
        (stag ~ sump)
      ==
    ==
  ++  sump  (ifix [lob rob] (stag %cltr (most ace wide)))
  ++  norm                                              ::  rune regular form
    |=  tol/?
    |%
    ++  structure
      %-  stew
      ^.  stet  ^.  limo
      :~  :-  '$'
            ;~  pfix  bus
              %-  stew
              ^.  stet  ^.  limo
              :~  [':' (rune col %bscl exqs)]
                  ['%' (rune cen %bscn exqs)]
                  ['<' (rune led %bsld exqb)]
                  ['>' (rune ban %bsbn exqb)]
                  ['^' (rune ket %bskt exqb)]
                  ['~' (rune sig %bssg exqd)]
                  ['|' (rune bar %bsbr exqc)]
                  ['&' (rune pad %bspd exqc)]
                  ['@' (rune vat %bsvt exqb)]
                  ['_' (rune cab %bscb expa)]
                  ['-' (rune hep %bshp exqb)]
                  ['=' (rune tis %bsts exqg)]
                  ['?' (rune wut %bswt exqs)]
                  [';' (rune mic %bsmc expa)]
              ==
            ==
        :-  '%'
          ;~  pfix  cen
            %-  stew
            ^.  stet  ^.  limo
            :~  :-  '^'
                %+  cook
                  |=  [%cnkt a/hoon b/spec c/spec d/spec]
                  [%make a b c d ~]
                (rune ket %cnkt exqy)
            ::
                :-  '+'
                %+  cook
                  |=  [%cnls a/hoon b/spec c/spec]
                  [%make a b c ~]
                (rune lus %cnls exqx)
            ::
                :-  '-'
                %+  cook
                  |=  [%cnhp a/hoon b/spec]
                  [%make a b ~]
                (rune hep %cnhp exqd)
            ::
                :-  ':'
                %+  cook
                  |=  [%cncl a/hoon b/(list spec)]
                  [%make a b]
                (rune col %cncl exqz)
            ==
          ==
      ==
    ++  expression
      %-  stew
      ^.  stet  ^.  limo
      :~  :-  '|'
            ;~  pfix  bar
              %-  stew
              ^.  stet  ^.  limo
              :~  ['_' (rune cab %brcb exqr)]
                  ['%' (runo cen %brcn ~ expe)]
                  ['@' (runo vat %brvt ~ expe)]
                  [':' (rune col %brcl expb)]
                  ['.' (rune dot %brdt expa)]
                  ['-' (rune hep %brhp expa)]
                  ['^' (rune ket %brkt expx)]
                  ['~' (rune sig %brsg exqc)]
                  ['*' (rune tar %brtr exqc)]
                  ['=' (rune tis %brts exqc)]
                  ['?' (rune wut %brwt expa)]
                  ['$' (rune bus %brbs exqe)]
              ==
            ==
          :-  '$'
            ;~  pfix  bus
              %-  stew
              ^.  stet  ^.  limo
              :~  ['@' (stag %ktcl (rune vat %bsvt exqb))]
                  ['_' (stag %ktcl (rune cab %bscb expa))]
                  [':' (stag %ktcl (rune col %bscl exqs))]
                  ['%' (stag %ktcl (rune cen %bscn exqs))]
                  ['<' (stag %ktcl (rune led %bsld exqb))]
                  ['>' (stag %ktcl (rune ban %bsbn exqb))]
                  ['|' (stag %ktcl (rune bar %bsbr exqc))]
                  ['&' (stag %ktcl (rune pad %bspd exqc))]
                  ['^' (stag %ktcl (rune ket %bskt exqb))]
                  ['~' (stag %ktcl (rune sig %bssg exqd))]
                  ['-' (stag %ktcl (rune hep %bshp exqb))]
                  ['=' (stag %ktcl (rune tis %bsts exqg))]
                  ['?' (stag %ktcl (rune wut %bswt exqs))]
                  ['.' (rune dot %kttr exqa)]
                  [',' (rune com %ktcl exqa)]
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
                  ['-' (rune hep %cnhp expb)]
                  [':' (rune col %cncl expi)]
                  ['~' (rune sig %cnsg expn)]
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
                  ['&' (rune pad %ktpd expa)]
                  ['~' (rune sig %ktsg expa)]
                  ['=' (rune tis %ktts expj)]
                  ['?' (rune wut %ktwt expa)]
                  ['%' (rune cen %ktcn expa)]
                  ['*' (rune tar %kttr exqa)]
                  [':' (rune col %ktcl exqa)]
              ==
            ==
          :-  '~'
            ;~  pfix  sig
              %-  stew
              ^.  stet  ^.  limo
              :~  ['|' (rune bar %sgbr expb)]
                  ['$' (rune bus %sgbs expf)]
                  ['_' (rune cab %sgcb expb)]
                  ['%' (rune cen %sgcn hind)]
                  ['/' (rune net %sgnt hine)]
                  ['<' (rune led %sgld hinb)]
                  ['>' (rune ban %sgbn hinb)]
                  ['+' (rune lus %sgls hinc)]
                  ['&' (rune pad %sgpd hinf)]
                  ['?' (rune wut %sgwt hing)]
                  ['=' (rune tis %sgts expb)]
                  ['!' (rune zap %sgzp expb)]
              ==
            ==
          :-  ';'
            ;~  pfix  mic
              %-  stew
              ^.  stet  ^.  limo
              :~  [':' (rune col %mccl expi)]
                  ['/' (rune net %mcnt expa)]
                  ['<' (rune gal %mcgl exp1)]
                  ['~' (rune sig %mcsg expi)]
                  [';' (rune mic %mcmc exqc)]
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
                  ['/' (rune net %tsnt expo)]
                  [';' (rune mic %tsmc expo)]
                  ['<' (rune led %tsld expb)]
                  ['>' (rune ban %tsbn expb)]
                  ['-' (rune hep %tshp expb)]
                  ['*' (rune tar %tstr expg)]
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
                  ['<' (rune led %wtld expb)]
                  ['>' (rune ban %wtbn expb)]
                  ['-' ;~(pfix hep (toad txhp))]
                  ['^' ;~(pfix ket (toad tkkt))]
                  ['=' ;~(pfix tis (toad txts))]
                  ['#' ;~(pfix hax (toad txhx))]
                  ['+' ;~(pfix lus (toad txls))]
                  ['&' (rune pad %wtpd exps)]
                  ['@' ;~(pfix vat (toad tkvt))]
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
                  [';' (rune mic %zpmc expb)]
                  ['>' (rune ban %zpbn expa)]
                  ['<' (rune led %zpld exqc)]
                  ['@' (rune vat %zpvt expy)]
                  ['=' (rune tis %zpts expa)]
                  ['?' (rune wut %zpwt hinh)]
              ==
            ==
      ==
    ::
    ++  boog  !:                                        ::  core arms
      %+  knee  [p=*term q=*hoon]  |.  ~+
      ;~  pose
        ;~  pfix  ;~  pose
                    (jest '++')
                    (jest '+-')   ::  XX deprecated
                  ==
          ;~  plug
            ;~(pfix gap ;~(pose (cold %$ bus) sym))
            ;~(pfix gap loaf)
          ==
        ==
      ::
        %+  cook
          |=  {b/term d/spec}
          [b [%ktcl [%name b d]]]
        ;~  pfix  ;~(pose (jest '+=') (jest '+$'))
          ;~  plug
            ;~(pfix gap sym)
            ;~(pfix gap loan)
          ==
        ==
      ::
        %+  cook
          |=  [b=term c=(list term) e=spec]
          ^-  [term hoon]
          :-  b
          :+  %brtr
            :-  %bscl
            =-  ?>(?=(^ -) -)
            ::  for each .term in .c, produce $=(term $~(* $-(* *)))
            ::  ie {term}=mold
            ::
            %+  turn  c
            |=  =term
            ^-  spec
            =/  tar  [%base %noun]
            [%bsts term [%bssg tar [%bshp tar tar]]]
          [%ktcl [%made [b c] e]]
        ;~  pfix  (jest '+*')
          ;~  plug
            ;~(pfix gap sym)
            ;~(pfix gap (ifix [lac rac] (most ace sym)))
            ;~(pfix gap loan)
          ==
        ==
      ==
   ::  parses a or [a b c] or a  b  c  ==
   ++  lynx
      =/  wid  (ifix [lac rac] (most ace sym))
      =/  tal
        ;~  sfix
          (most gap sym)
          ;~(plug gap duz)
        ==
      =/  one
        %-  cook  :_  sym
        |=  a=term
        `(list term)`~[a]
      %-  cook
      :_  ;~(pose (runq wid tal) one)
      ::  lestify
      |=  a=(list term)
      ?~(a !! a)
    ++  whap  !:                                        ::  chapter
      %+  cook
        |=  a=(list (pair term hoon))
        |-  ^-  (map term hoon)
        ?~  a  ~
        =+  $(a t.a)
        %+  ~(put by -)
          p.i.a
        ?:  (~(has by -) p.i.a)
          [%eror (weld "duplicate arm: +" (trip p.i.a))]
        q.i.a
      (most muck boog)
    ::
    ++  whip                                            ::  chapter declare
      ;~  plug
        (ifix [cen gap] sym)
        whap
      ==
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
    ++  wisp  !:                                        ::  core tail
      ?.  tol  fail
      %+  cook
        |=  a=(list (pair term (map term hoon)))
        ^-  (map term tome)
        =<  p
        |-  ^-  (pair (map term tome) (map term hoon))
        ?~  a  [~ ~]
        =/  mor  $(a t.a)
        =.  q.i.a
          %-  ~(urn by q.i.a)
          |=  b=(pair term hoon)  ^+  +.b
          ?.  (~(has by q.mor) p.b)  +.b
          [%eror (weld "duplicate arm: +" (trip p.b))]
        :_  (~(uni by q.mor) q.i.a)
        %+  ~(put by p.mor)
          p.i.a
        :-  *what
        ?.  (~(has by p.mor) p.i.a)
          q.i.a
        [[%$ [%eror (weld "duplicate chapter: |" (trip p.i.a))]] ~ ~]
      ::
      ;~  pose
        dun
        ;~  sfix
          ;~  pose
            (most muck ;~(pfix (jest '+|') ;~(pfix gap whip)))
            ;~(plug (stag %$ whap) (easy ~))
          ==
          gap
          dun
        ==
      ==
    ::
    ++  toad                                            ::  untrap parser exp
      =+  har=expa
      |@  ++  $
            =+  dur=(ifix [lit rit] $:har(tol |))
            ?:(tol ;~(pose ;~(pfix gap $:har(tol &)) dur) dur)
      --
    ::
    ++  rune                                            ::  build rune
      =+  [dif=*rule tuq=** har=expa]
      |@  ++  $
            ;~(pfix dif (stag tuq (toad har)))
      --
    ::
    ++  runo                                            ::  rune plus
      =+  [dif=*rule hil=** tuq=** har=expa]
      |@  ++  $
            ;~(pfix dif (stag hil (stag tuq (toad har))))
      --
    ++  runq                                            ::  wide or tall if tol
      |*  [wid/rule tal/rule]                           ::  else wide
      ?.  tol
        wid
      ;~(pose wid tal)
    ::
    ++  glop  ~+((glue mash))                           ::  separated by space
    ++  gunk  ~+((glue muck))                           ::  separated list
    ++  butt  |*  zor/rule                              ::  closing == if tall
              ?:(tol ;~(sfix zor ;~(plug gap duz)) zor)
    ++  ulva  |*  zor/rule                              ::  closing -- and tall
              ?.(tol fail ;~(sfix zor ;~(plug gap dun)))
    ++  hank  (most muck loaf)                          ::  gapped hoons
    ++  hunk  (most muck loan)                          ::  gapped specs
    ++  lore  (sear |=(=hoon ~(flay ap hoon)) loaf)     ::  skin
    ++  loaf  ?:(tol tall wide)                         ::  tall/wide hoon
    ++  loan  ?:(tol till wyde)                         ::  tall/wide spec
    ++  lomp  ;~(plug sym (punt ;~(pfix tis wyde)))     ::  typeable name
    ++  mash  ?:(tol gap ;~(plug com ace))              ::  list separator
    ++  muck  ?:(tol gap ace)                           ::  general separator
    ++  teak  %+  knee  *tiki  |.  ~+                   ::  wing or hoon
              =+  ^=  gub
                  |=  {a/term b/$%({%& p/wing} {%| p/hoon})}
                  ^-  tiki
                  ?-(-.b %& [%& [~ a] p.b], %| [%| [~ a] p.b])
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
    ++  ruck  (most mash ;~(gunk loan loaf))            ::  list [spec hoon]
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
    ++  expg  |.(;~(gunk lomp loaf loaf))               ::  term/spec, two hoons
    ++  exph  |.((butt ;~(gunk rope rick)))             ::  wing, [spec hoon]s
    ++  expi  |.((butt ;~(gunk loaf hank)))             ::  one or more hoons
    ++  expj  |.(;~(gunk lore loaf))                    ::  skin and hoon
    ++  expk  |.(;~(gunk loaf ;~(plug loaf (easy ~))))  ::  list of two hoons
    ++  expl  |.(;~(gunk sym loaf loaf))                ::  term, two hoons
    ++  expm  |.((butt ;~(gunk rope loaf rick)))        ::  several [spec hoon]s
    ++  expn  |.  ;~  gunk  rope  loaf                  ::  wing, hoon,
                    ;~(plug loaf (easy ~))              ::  list of one hoon
                  ==                                    ::
    ++  expo  |.(;~(gunk wise loaf loaf))               ::  =;
    ++  expp  |.(;~(gunk (butt rick) loaf))             ::  [wing hoon]s, hoon
    ++  expq  |.(;~(gunk rope loaf loaf))               ::  wing and two hoons
    ++  expr  |.(;~(gunk loaf wisp))                    ::  hoon and core tail
    ++  exps  |.((butt hank))                           ::  closed gapped hoons
    ++  expt  |.(;~(gunk wise rope loaf loaf))          ::  =^
    ++  expu  |.(;~(gunk rope loaf (butt hank)))        ::  wing, hoon, hoons
    ++  expv  |.((butt rick))                           ::  just changes
    ++  expw  |.(;~(gunk rope loaf loaf loaf))          ::  wing and three hoons
    ++  expx  |.(;~(gunk loaf wisp))                    ::  hoon and core tail
    ++  expy  |.(;~(gunk ropa loaf loaf))               ::  wings and two hoons
    ++  expz  |.(loaf(bug &))                           ::  hoon with tracing
    ++  exp1  |.(;~(gunk loan loaf loaf loaf))          ::  spec and three hoons
    ::    spec contents
    ::
    ++  exqa  |.(loan)                                  ::  one hoon
    ++  exqb  |.(;~(gunk loan loan))                    ::  two specs
    ++  exqc  |.(;~(gunk loan loaf))                    ::  spec then hoon
    ++  exqd  |.(;~(gunk loaf loan))                    ::  hoon then spec
    ++  exqe  |.(;~(gunk lynx loan))                    ::  list of names then spec
    ++  exqs  |.((butt hunk))                           ::  closed gapped specs
    ++  exqg  |.(;~(gunk sym loan))                     ::  term and spec
    ++  exqk  |.(;~(gunk loaf ;~(plug loan (easy ~))))  ::  hoon with one spec
    ++  exqr  |.(;~(gunk loan ;~(plug wasp wisp)))      ::  spec/aliases?/tail
    ++  exqn  |.(;~(gunk loan (stag %cltr (butt hank))))::  autoconsed hoons
    ++  exqw  |.(;~(gunk loaf loan))                    ::  hoon and spec
    ++  exqx  |.(;~(gunk loaf loan loan))               ::  hoon, two specs
    ++  exqy  |.(;~(gunk loaf loan loan loan))          ::  hoon, three specs
    ++  exqz  |.(;~(gunk loaf (butt hunk)))             ::  hoon, n specs
    ::
    ::    tiki expansion for %wt runes
    ::
    ++  txhp  |.  %+  cook  |=  {a/tiki b/(list (pair spec hoon))}
                            (~(wthp ah a) b)
                  (butt ;~(gunk teak ruck))
    ++  tkkt  |.  %+  cook  |=  {a/tiki b/hoon c/hoon}
                            (~(wtkt ah a) b c)
                  ;~(gunk teak loaf loaf)
    ++  txls  |.  %+  cook  |=  {a/tiki b/hoon c/(list (pair spec hoon))}
                            (~(wtls ah a) b c)
                  (butt ;~(gunk teak loaf ruck))
    ++  tkvt  |.  %+  cook  |=  {a/tiki b/hoon c/hoon}
                            (~(wtvt ah a) b c)
                  ;~(gunk teak loaf loaf)
    ++  tksg  |.  %+  cook  |=  {a/tiki b/hoon c/hoon}
                            (~(wtsg ah a) b c)
                  ;~(gunk teak loaf loaf)
    ++  txts  |.  %+  cook  |=  {a/spec b/tiki}
                            (~(wtts ah b) a)
                  ;~(gunk loan teak)
    ++  txhx  |.  %+  cook  |=  {a/skin b/tiki}
                            (~(wthx ah b) a)
                  ;~(gunk lore teak)
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
        ;~(gunk (cook lent (stun [1 3] ban)) loaf loaf)
        (stag 0 ;~(gunk loaf loaf))
      ==
    ++  hing  |.                                        ::  0-3 >s, three hoons
      ;~  pose
        ;~(gunk (cook lent (stun [1 3] ban)) loaf loaf loaf)
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
            (ifix [lac rac] ;~(plug dem ;~(pfix ace dem)))
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
          ?:(tol [;~(plug duz gap) ;~(plug gap duz)] [lit rit])
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
            {$lit p/(list (pair wing hoon))}
        ==
    ==
  ::
  ++  lung
    ~+
    %-  bend
    |:  $:lang
    ^-  (unit hoon)
    ?-    -.vil
      $col  ?:(=([%base %flag] ros) ~ [~ %tsld ros p.vil])
      $lit  (bind ~(reek ap ros) |=(hyp/wing [%cnts hyp p.vil]))
      $ket  [~ ros p.vil]
      $tis  =+  rud=~(flay ap ros)
            ?~(rud ~ `[%ktts u.rud p.vil])
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
        ;~  plug
          (easy %lit)
          (ifix [lit rit] lobo)
        ==
      ==
    ==
  ::
  ++  lobo  (most ;~(plug com ace) ;~(glam rope wide))
  ++  loon  (most ;~(plug com ace) ;~(glam wide wide))
  ++  lute                                              ::  tall [] noun
    ~+
    %+  cook  |=(hoon +<)
    %+  stag  %cltr
    %+  ifix
      [;~(plug lac gap) ;~(plug gap rac)]
    (most gap tall)
  ::
  ++  ropa  (most col rope)
  ++  rope                                              ::  wing form
    %+  knee  *wing
    |.  ~+
    %+  (slug |=({a/limb b/wing} [a b]))
      dot
    ;~  pose
      (cold [%| 0 ~] com)
      %+  cook
        |=({a/(list) b/term} ?~(a b [%| (lent a) `b]))
      ;~(plug (star ket) ;~(pose sym (cold %$ bus)))
    ::
      %+  cook
        |=(a/axis [%& a])
      ;~  pose
        ;~(pfix lus dim:ag)
        ;~(pfix pad (cook |=(a/@ ?:(=(0 a) 0 (mul 2 +($(a (dec a)))))) dim:ag))
        ;~(pfix bar (cook |=(a/@ ?:(=(0 a) 1 +((mul 2 $(a (dec a)))))) dim:ag))
        ven
        (cold 1 dot)
      ==
    ==
  ::
  ++  wise
    ;~  pose
      ;~  pfix  tis
        %+  sear
          |=  =spec
          ^-  (unit skin)
          %+  bind  ~(autoname ax & spec)
          |=  =term
          [%name term %spec spec %base %noun]
        wyde
      ==
    ::
      %+  cook
        |=  [=term =(unit spec)]
        ^-  skin
        ?~  unit
          term
        [%name term %spec u.unit %base %noun]
      ;~  plug  sym
        ::  XX: net deprecated
        ::
        (punt ;~(pfix ;~(pose net tis) wyde))
      ==
    ::
      %+  cook
        |=  =spec
        ^-  skin
        [%spec spec %base %noun]
      wyde
    ==
  ++  tall                                              ::  full tall form
    %+  knee  *hoon
    |.(~+((wart ;~(pose expression:(norm &) long lute apex:(sail &)))))
  ++  till                                              ::  mold tall form
    %+  knee  *spec
    |.(~+((wert ;~(pose structure:(norm &) scad))))
  ++  wede                                              ::  wide bulb
    ::  XX: lus deprecated
    ::
    ;~(pfix ;~(pose lus net) wide)
  ++  wide                                              ::  full wide form
    %+  knee  *hoon
    |.(~+((wart ;~(pose expression:(norm |) long apex:(sail |)))))
  ++  wyde                                              ::  mold wide form
    %+  knee  *spec
    |.(~+((wert ;~(pose structure:(norm |) scad))))
  ++  wart
    |*  zor/rule
    %+  here
      |=  {a/pint b/hoon}
      ?:(bug [%dbug [wer a] b] b)
    zor
  ++  wert
    |*  zor/rule
    %+  here
      |=  {a/pint b/spec}
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
  ~>  %slog.[0 leaf/"ride-parsing"]
  =/  gen  (ream txt)
  ~>  %slog.[0 leaf/"ride-compiling"]
  =-  ~>  %slog.[0 leaf/"ride-compiled"]
      -
  (~(mint ut typ) %noun gen)
::
::::  5e: caching compiler
  ::
++  wa                                                  ::  cached compile
  |_  worm
  ++  nell  |=(ref/type (nest [%cell %noun %noun] ref)) ::  nest in cell
  ++  nest                                              ::  nest:ut, cached
    |=  {sut/type ref/type}
    ^-  {? worm}
    ?:  (~(has in nes) [sut ref])  [& +>+<]
    ?.  (~(nest ut sut) | ref)
      ~&  %nest-failed
      =+  foo=(skol ref)
      =+  bar=(skol sut)
      ~&  %nest-need
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
         %&  (nest p.mes p.p.som)
         %|  (nets p.mes -.p.som)
      ==
    ?>  hip
    [[p.dor q.dor(+6 +7.som)] +>+<.$]
  ::
  ++  neat                                              ::  type compliance
    |=  {typ/type som/(each vase ^)}
    ^-  worm
    =^  hip  +>+<.$
      ?-  -.som
        %&  (nest typ p.p.som)
        %|  (nets typ -.p.som)
      ==
    ?>  hip
    +>+<.$
  ::
  ++  nets                                              ::  typeless nest
    |=  {sut/* ref/*}
    ^-  {? worm}
    ?:  (~(has in nes) [sut ref])  [& +>+<]
    =+  gat=|=({a/type b/type} (~(nest ut a) | b))
    ?.  (? (slum gat [sut ref]))
      ~&  %nets-failed
      =+  tag=`*`skol
      =+  foo=(tank (slum tag ref))
      =+  bar=(tank (slum tag sut))
      ~&  %nets-need
      ~>  %slog.[0 bar]
      ~&  %nets-have
      ~>  %slog.[0 foo]
      [| +>+<.$]
    [& +>+<.$(nes (~(put in nes) [sut ref]))]
  ::  +play: +play:ut, cached
  ::
  ++  play
    |=  {sut/type gen/hoon}
    ^-  {type worm}
    =+  old=(~(get by pay) [sut gen])
    ?^  old  [u.old +>+<.$]
    =+  new=(~(play ut sut) gen)
    [new +>+<.$(pay (~(put by pay) [sut gen] new))]
  ::  +mint: +mint:ut to noun, cached
  ::
  ++  mint
    |=  {sut/type gen/hoon}
    ^-  {(pair type nock) worm}
    =+  old=(~(get by mit) [sut gen])
    ?^  old  [u.old +>+<.$]
    =+  new=(~(mint ut sut) %noun gen)
    [new +>+<.$(mit (~(put by mit) [sut gen] new))]
  ::  +slam: +slam:ut, cached
  ::
  ++  slam
    |=  [gat=vase sam=vase]
    =/  sut=type  [%cell p.gat p.sam]
    =/  gen=hoon  [%cnsg [%$ ~] [%$ 2] [%$ 3] ~]
    =^  new=type  +>+<.$  (play sut gen)
    [[new (slum q.gat q.sam)] +>+<.$]
  ::  +slap: +slap:ut, cached
  ::
  ++  slap
    |=  {vax/vase gen/hoon}
    ^-  {vase worm}
    =^  gun  +>+<  (mint p.vax gen)
    [[p.gun .*(q.vax q.gun)] +>+<.$]
  ::  +slot: +slot:ut, cached
  ::
  ++  slot
    |=  {axe/@ vax/vase}
    ^-  {vase worm}
    =^  gun  +>+<  (mint p.vax [%$ axe])
    [[p.gun .*(q.vax [0 axe])] +>+<.$]
  ::  +slym: +slym:ut, cached
  ::
  ++  slym
    |=  {gat/vase sam/*}
    ^-  [vase worm]
    (slap gat(+<.q sam) [%limb %$])
  ::
  ++  sped                                              ::  specialize vase
    |=  vax/vase
    ^-  {vase worm}
    =+  ^=  gen  ^-  hoon
      ?@  q.vax    [%wtts [%base [%atom %$]] [%& 1]~]
      ?@  -.q.vax  [%wtts [%leaf %tas -.q.vax] [%& 2]~]
      [%wtts [%base %cell] [%& 1]~]
    =^  typ  +>+<.$  (play p.vax [%wtbn gen [%$ 1]])
    [[typ q.vax] +>+<.$]
  ::
  ++  spot                                              ::  slot then sped
    |=  {axe/@ vax/vase}
    ^-  {vase worm}
    =^  xav  +>+<  (slot axe vax)
    (sped xav)
  ::
  ++  stop                                              ::  sped then slot
    |=  {axe/@ vax/vase}
    ^-  {vase worm}
    =^  xav  +>+<  (sped vax)
    (slot axe xav)
  --
::
::::  5f: molds and mold builders
  ::
++  mane  $@(@tas {@tas @tas})                          ::  XML name+space
++  manx  $~([[%$ ~] ~] {g/marx c/marl})                ::  dynamic XML node
++  marl  (list manx)                                   ::  XML node list
++  mars  {t/{n/$$ a/{i/{n/$$ v/tape} t/~}} c/~}        ::  XML cdata
++  mart  (list {n/mane v/tape})                        ::  XML attributes
++  marx  $~([%$ ~] {n/mane a/mart})                    ::  dynamic XML tag
++  mite  (list @ta)                                    ::  mime type
++  monk  (each ship {p/@tas q/@ta})                    ::  general identity
++  pass  @                                             ::  public key
++  ring  @                                             ::  private key
++  ship  @p                                            ::  network identity
++  shop  (each ship (list @ta))                        ::  urbit/dns identity
++  spur  path                                          ::  ship desk case spur
++  time  @da                                           ::  galactic time
::
::::  5g: profiling support (XX move)
  ::
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
