!.
=>  %a50
~%  %a.50  ~  ~
|%
::  Types
::
+$  ship  @p
+$  life  @ud
+$  rift  @ud
+$  pass  @
+$  bloq  @
+$  step  _`@u`1
+$  bite  $@(bloq [=bloq =step])
+$  octs  [p=@ud q=@]
++  unit  |$  [item]  $@(~ [~ u=item])
++  list  |$  [item]  $@(~ [i=item t=(list item)])
++  lest  |$  [item]  [i=item t=(list item)]
++  tree  |$  [node]  $@(~ [n=node l=(tree node) r=(tree node)])
++  pair  |$  [head tail]  [p=head q=tail]
++  map
  |$  [key value]
  $|  (tree (pair key value))
  |=(a=(tree (pair)) ?:(=(~ a) & ~(apt by a)))
::
++  set
  |$  [item]
  $|  (tree item)
  |=(a=(tree) ?:(=(~ a) & ~(apt in a)))
::
++  jug   |$  [key value]  (map key (set value))
::
::  Biblical
::
++  ruth  |=([a=* b=*] ?>(?=(@ b) b))
::
::  Bits
::
++  dec                                                 ::  decrement
  ~/  %dec
  |=  a=@
  ~_  leaf+"decrement-underflow"
  ?<  =(0 a)
  =+  b=0
  |-  ^-  @
  ?:  =(a +(b))  b
  $(b +(b))
::
++  add                                                 ::  plus
  ~/  %add
  |=  [a=@ b=@]
  ^-  @
  ?:  =(0 a)  b
  $(a (dec a), b +(b))
::
++  sub                                                 ::  subtract
  ~/  %sub
  |=  [a=@ b=@]
  ~_  leaf+"subtract-underflow"
  ::  difference
  ^-  @
  ?:  =(0 b)  a
  $(a (dec a), b (dec b))
::
++  mul                                                 ::  multiply
  ~/  %mul
  |:  [a=`@`1 b=`@`1]
  ^-  @
  =+  c=0
  |-
  ?:  =(0 a)  c
  $(a (dec a), c (add b c))
::
++  div                                                 ::  divide
  ~/  %div
  |:  [a=`@`1 b=`@`1]
  ^-  @
  ~_  leaf+"divide-by-zero"
  ?<  =(0 b)
  =+  c=0
  |-
  ?:  (lth a b)  c
  $(a (sub a b), c +(c))
::
++  mod                                                 ::  modulus
  ~/  %mod
  |:  [a=`@`1 b=`@`1]
  ^-  @
  ?<  =(0 b)
  (sub a (mul b (div a b)))
::
++  bex                                                 ::  binary exponent
  ~/  %bex
  |=  a=bloq
  ^-  @
  ?:  =(0 a)  1
  (mul 2 $(a (dec a)))
::
++  lsh                                                 ::  left-shift
  ~/  %lsh
  |=  [a=bite b=@]
  =/  [=bloq =step]  ?^(a a [a *step])
  (mul b (bex (mul (bex bloq) step)))
::
++  rsh                                                 ::  right-shift
  ~/  %rsh
  |=  [a=bite b=@]
  =/  [=bloq =step]  ?^(a a [a *step])
  (div b (bex (mul (bex bloq) step)))
::
++  con                                                 ::  binary or
  ~/  %con
  |=  [a=@ b=@]
  =+  [c=0 d=0]
  |-  ^-  @
  ?:  ?&(=(0 a) =(0 b))  d
  %=  $
    a   (rsh 0 a)
    b   (rsh 0 b)
    c   +(c)
    d   %+  add  d
          %+  lsh  [0 c]
          ?&  =(0 (end 0 a))
              =(0 (end 0 b))
          ==
  ==
::
++  dis                                                 ::  binary and
  ~/  %dis
  |=  [a=@ b=@]
  =|  [c=@ d=@]
  |-  ^-  @
  ?:  ?|(=(0 a) =(0 b))  d
  %=  $
    a   (rsh 0 a)
    b   (rsh 0 b)
    c   +(c)
    d   %+  add  d
          %+  lsh  [0 c]
          ?|  =(0 (end 0 a))
              =(0 (end 0 b))
          ==
  ==
::
++  mix                                                 ::  binary xor
  ~/  %mix
  |=  [a=@ b=@]
  ^-  @
  =+  [c=0 d=0]
  |-
  ?:  ?&(=(0 a) =(0 b))  d
  %=  $
    a   (rsh 0 a)
    b   (rsh 0 b)
    c   +(c)
    d   (add d (lsh [0 c] =((end 0 a) (end 0 b))))
  ==
::
++  lth                                                 ::  less
  ~/  %lth
  |=  [a=@ b=@]
  ^-  ?
  ?&  !=(a b)
      |-
      ?|  =(0 a)
          ?&  !=(0 b)
              $(a (dec a), b (dec b))
  ==  ==  ==
::
++  lte                                                 ::  less or equal
  ~/  %lte
  |=  [a=@ b=@]
  |(=(a b) (lth a b))
::
++  gte                                                 ::  greater or equal
  ~/  %gte
  |=  [a=@ b=@]
  ^-  ?
  !(lth a b)
::
++  gth                                                 ::  greater
  ~/  %gth
  |=  [a=@ b=@]
  ^-  ?
  !(lte a b)
::
++  swp                                                 ::  naive rev bloq order
  ~/  %swp
  |=  [a=bloq b=@]
  (rep a (flop (rip a b)))
::
++  met                                                 ::  measure
  ~/  %met
  |=  [a=bloq b=@]
  ^-  @
  =+  c=0
  |-
  ?:  =(0 b)  c
  $(b (rsh a b), c +(c))
::
++  end                                                 ::  tail
  ~/  %end
  |=  [a=bite b=@]
  =/  [=bloq =step]  ?^(a a [a *step])
  (mod b (bex (mul (bex bloq) step)))
::
++  cat                                                 ::  concatenate
  ~/  %cat
  |=  [a=bloq b=@ c=@]
  (add (lsh [a (met a b)] c) b)
::
++  cut                                                 ::  slice
  ~/  %cut  ::  TODO: jet
  |=  [a=bloq [b=step c=step] d=@]
  (end [a c] (rsh [a b] d))
::
++  dad                                                 ::  concatenate fixed
  ~/  %dad
  |=  [=bite a=@ b=@]
  (add a (lsh bite b))
::
++  can                                                 ::  assemble
  ~/  %can
  |=  [a=bloq b=(list [p=step q=@])]
  ^-  @
  ?~  b  0
  (add (end [a p.i.b] q.i.b) (lsh [a p.i.b] $(b t.b)))
::
++  cad                                                 ::  assemble specific
  ~/  %cad
  |=  [a=bloq b=(list [p=step q=@])]
  ^-  [=step @]
  :_  (can a b)
  |-
  ?~  b
    0
  (add p.i.b $(b t.b))
::
++  rep                                                 ::  assemble fixed
  ~/  %rep
  |=  [a=bite b=(list @)]
  =/  [=bloq =step]  ?^(a a [a *step])
  =|  i=@ud
  |-  ^-  @
  ?~  b   0
  %+  add  $(i +(i), b t.b)
  (lsh [bloq (mul step i)] (end [bloq step] i.b))
::
++  rip                                                 ::  disassemble
  ~/  %rip
  |=  [a=bite b=@]
  ^-  (list @)
  ?:  =(0 b)  ~
  [(end a b) $(b (rsh a b))]
::
::
::  Lists
::
++  lent                                                ::  length
  ~/  %lent
  |=  a=(list)
  ^-  @
  =+  b=0
  |-
  ?~  a  b
  $(a t.a, b +(b))
::
++  slag                                                ::  suffix
  ~/  %slag
  |*  [a=@ b=(list)]
  |-  ^+  b
  ?:  =(0 a)  b
  ?~  b  ~
  $(b t.b, a (dec a))
::
++  snag                                                ::  index
  ~/  %snag
  |*  [a=@ b=(list)]
  |-  ^+  ?>(?=(^ b) i.b)
  ?~  b
    ~_  leaf+"snag-fail"
    !!
  ?:  =(0 a)  i.b
  $(b t.b, a (dec a))
::
++  homo                                                ::  homogenize
  |*  a=(list)
  ^+  =<  $
    |@  ++  $  ?:(*? ~ [i=(snag 0 a) t=$])
    --
  a
::
++  flop                                                ::  reverse
  ~/  %flop
  |*  a=(list)
  =>  .(a (homo a))
  ^+  a
  =+  b=`_a`~
  |-
  ?~  a  b
  $(a t.a, b [i.a b])
::
++  welp                                                ::  concatenate
  ~/  %welp
  =|  [* *]
  |@
  ++  $
    ?~  +<-
      +<-(. +<+)
    +<-(+ $(+<- +<->))
  --
::
++  turn                                                ::  transform
  ~/  %turn
  |*  [a=(list) b=$-(* *)]
  =>  .(a (homo a))
  ^-  (list _?>(?=(^ a) (b i.a)))
  |-
  ?~  a  ~
  [i=(b i.a) t=$(a t.a)]
::
++  levy                                                ::  all of
  ~/  %levy
  |*  [a=(list) b=$-(* ?)]
  |-  ^-  ?
  ?~  a  &
  ?.  (b i.a)  |
  $(a t.a)
::
++  reap                                                ::  replicate
  ~/  %reap
  |*  [a=@ b=*]
  |-  ^-  (list _b)
  ?~  a  ~
  [b $(a (dec a))]
::
::  Modular arithmetic
::
++  fe                                                  ::  modulo bloq
  |_  a=bloq
  ++  rol  |=  [b=bloq c=@ d=@]  ^-  @                  ::  roll left
           =+  e=(sit d)
           =+  f=(bex (sub a b))
           =+  g=(mod c f)
           (sit (con (lsh [b g] e) (rsh [b (sub f g)] e)))
  ++  sum  |=([b=@ c=@] (sit (add b c)))                ::  wrapping add
  ++  sit  |=(b=@ (end a b))                            ::  enforce modulo
  --
::
::  Hashes
::
++  muk                                                 ::  standard murmur3
  ~%  %muk  ..muk  ~
  =+  ~(. fe 5)
  |=  [syd=@ len=@ key=@]
  =.  syd      (end 5 syd)
  =/  pad      (sub len (met 3 key))
  =/  data     (welp (rip 3 key) (reap pad 0))
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
      %3  =.  k1  (mix k1 (lsh [0 16] (snag 2 tail)))
          =.  k1  (mix k1 (lsh [0 8] (snag 1 tail)))
          =.  k1  (mix k1 (snag 0 tail))
          =.  k1  (sit (mul k1 c1))
          =.  k1  (rol 0 15 k1)
          =.  k1  (sit (mul k1 c2))
          (mix h1 k1)
      %2  =.  k1  (mix k1 (lsh [0 8] (snag 1 tail)))
          =.  k1  (mix k1 (snag 0 tail))
          =.  k1  (sit (mul k1 c1))
          =.  k1  (rol 0 15 k1)
          =.  k1  (sit (mul k1 c2))
          (mix h1 k1)
      %1  =.  k1  (mix k1 (snag 0 tail))
          =.  k1  (sit (mul k1 c1))
          =.  k1  (rol 0 15 k1)
          =.  k1  (sit (mul k1 c2))
          (mix h1 k1)
    ==
  =.  h1  (mix h1 len)
  |^  (fmix32 h1)
  ++  fmix32
    |=  h=@
    =.  h  (mix h (rsh [0 16] h))
    =.  h  (sit (mul h 0x85eb.ca6b))
    =.  h  (mix h (rsh [0 13] h))
    =.  h  (sit (mul h 0xc2b2.ae35))
    =.  h  (mix h (rsh [0 16] h))
    h
  --
::
++  mug                                                 ::  mug with murmur3
  ~/  %mug
  |=  a=*
  |^  ?@  a  (mum 0xcafe.babe 0x7fff a)
      =/  b  (cat 5 $(a -.a) $(a +.a))
      (mum 0xdead.beef 0xfffe b)
  ::
  ++  mum
    |=  [syd=@uxF fal=@F key=@]
    =/  wyd  (met 3 key)
    =|  i=@ud
    |-  ^-  @F
    ?:  =(8 i)  fal
    =/  haz=@F  (muk syd wyd key)
    =/  ham=@F  (mix (rsh [0 31] haz) (end [0 31] haz))
    ?.(=(0 ham) ham $(i +(i), syd +(syd)))
  --
::
++  gor                                                 ::  mug order
  ~/  %gor
  |=  [a=* b=*]
  ^-  ?
  =+  [c=(mug a) d=(mug b)]
  ?:  =(c d)
    (dor a b)
  (lth c d)
::
++  mor                                                 ::  more mug order
  ~/  %mor
  |=  [a=* b=*]
  ^-  ?
  =+  [c=(mug (mug a)) d=(mug (mug b))]
  ?:  =(c d)
    (dor a b)
  (lth c d)
::
++  dor                                                 ::  tree order
  ~/  %dor
  |=  [a=* b=*]
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
::  Maps
::
++  by
  ~/  %by
  =|  a=(tree (pair))  ::  (map)
  =*  node  ?>(?=(^ a) n.a)
  |@
  ++  get
    ~/  %get
    |*  b=*
    =>  .(b `_?>(?=(^ a) p.n.a)`b)
    |-  ^-  (unit _?>(?=(^ a) q.n.a))
    ?~  a
      ~
    ?:  =(b p.n.a)
      `q.n.a
    ?:  (gor b p.n.a)
      $(a l.a)
    $(a r.a)
  ::
  ++  has
    ~/  %has
    |*  b=*
    !=(~ (get b))
  ::
  ++  put
    ~/  %put
    |*  [b=* c=*]
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
  ++  del
    ~/  %del
    |*  b=*
    |-  ^+  a
    ?~  a
      ~
    ?.  =(b p.n.a)
      ?:  (gor b p.n.a)
        a(l $(a l.a))
      a(r $(a r.a))
    |-  ^-  [$?(~ _a)]
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (mor p.n.l.a p.n.r.a)
      l.a(r $(l.a r.l.a))
    r.a(l $(r.a l.r.a))
  ::
  ++  apt
    =<  $
    ~/  %apt
    =|  [l=(unit) r=(unit)]
    |.  ^-  ?
    ?~  a   &
    ?&  ?~(l & &((gor p.n.a u.l) !=(p.n.a u.l)))
        ?~(r & &((gor u.r p.n.a) !=(u.r p.n.a)))
        ?~  l.a   &
        &((mor p.n.a p.n.l.a) !=(p.n.a p.n.l.a) $(a l.a, l `p.n.a))
        ?~  r.a   &
        &((mor p.n.a p.n.r.a) !=(p.n.a p.n.r.a) $(a r.a, r `p.n.a))
    ==
  --
::
::  Sets
::
++  in
  ~/  %in
  =|  a=(tree)  :: (set)
  |@
  ++  put
    ~/  %put
    |*  b=*
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
  ++  del
    ~/  %del
    |*  b=*
    |-  ^+  a
    ?~  a
      ~
    ?.  =(b n.a)
      ?:  (gor b n.a)
        a(l $(a l.a))
      a(r $(a r.a))
    |-  ^-  [$?(~ _a)]
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (mor n.l.a n.r.a)
      l.a(r $(l.a r.l.a))
    r.a(l $(r.a l.r.a))
  ::
  ++  apt
    =<  $
    ~/  %apt
    =|  [l=(unit) r=(unit)]
    |.  ^-  ?
    ?~  a   &
    ?&  ?~(l & (gor n.a u.l))
        ?~(r & (gor u.r n.a))
        ?~(l.a & ?&((mor n.a n.l.a) $(a l.a, l `n.a)))
        ?~(r.a & ?&((mor n.a n.r.a) $(a r.a, r `n.a)))
    ==
  --
::
::  Jugs
::
++  ju
  =|  a=(tree (pair * (tree)))  ::  (jug)
  |@
  ++  get
    |*  b=*
    =+  c=(~(get by a) b)
    ?~(c ~ u.c)
  ::
  ++  del
    |*  [b=* c=*]
    ^+  a
    =+  d=(get b)
    =+  e=(~(del in d) c)
    ?~  e
      (~(del by a) b)
    (~(put by a) b e)
  ::
  ++  put
    |*  [b=* c=*]
    ^+  a
    =+  d=(get b)
    (~(put by a) b (~(put in d) c))
  --
::  $mk-item: constructor for +ordered-map item type
::
++  mk-item  |$  [key val]  [key=key val=val]
::  +ordered-map: treap with user-specified horizontal order
::
::    Conceptually smaller items go on the left, so the item with the
::    smallest key can be popped off the head. If $key is `@` and
::    .compare is +lte, then the numerically smallest item is the head.
::
::  WARNING: ordered-map will not work properly if two keys can be
::  unequal under noun equality but equal via the compare gate
::
++  ordered-map
  |*  [key=mold val=mold]
  =>  |%
      +$  item  (mk-item key val)
      --
  ::  +compare: item comparator for horizontal order
  ::
  |=  compare=$-([key key] ?)
  |%
  ::  +check-balance: verify horizontal and vertical orderings
  ::
  ++  check-balance
    =|  [l=(unit key) r=(unit key)]
    |=  a=(tree item)
    ^-  ?
    ::  empty tree is valid
    ::
    ?~  a  %.y
    ::  nonempty trees must maintain several criteria
    ::
    ?&  ::  if .n.a is left of .u.l, assert horizontal comparator
        ::
        ?~(l %.y (compare key.n.a u.l))
        ::  if .n.a is right of .u.r, assert horizontal comparator
        ::
        ?~(r %.y (compare u.r key.n.a))
        ::  if .a is not leftmost element, assert vertical order between
        ::  .l.a and .n.a and recurse to the left with .n.a as right
        ::  neighbor
        ::
        ?~(l.a %.y &((mor key.n.a key.n.l.a) $(a l.a, l `key.n.a)))
        ::  if .a is not rightmost element, assert vertical order
        ::  between .r.a and .n.a and recurse to the right with .n.a as
        ::  left neighbor
        ::
        ?~(r.a %.y &((mor key.n.a key.n.r.a) $(a r.a, r `key.n.a)))
    ==
  ::  +put: ordered item insert
  ::
  ++  put
    |=  [a=(tree item) =key =val]
    ^-  (tree item)
    ::  base case: replace null with single-item tree
    ::
    ?~  a  [n=[key val] l=~ r=~]
    ::  base case: overwrite existing .key with new .val
    ::
    ?:  =(key.n.a key)  a(val.n val)
    ::  if item goes on left, recurse left then rebalance vertical order
    ::
    ?:  (compare key key.n.a)
      =/  l  $(a l.a)
      ?>  ?=(^ l)
      ?:  (mor key.n.a key.n.l)
        a(l l)
      l(r a(l r.l))
    ::  item goes on right; recurse right then rebalance vertical order
    ::
    =/  r  $(a r.a)
    ?>  ?=(^ r)
    ?:  (mor key.n.a key.n.r)
      a(r r)
    r(l a(r l.r))
  ::  +peek: produce head (smallest item) or null
  ::
  ++  peek
    |=  a=(tree item)
    ^-  (unit item)
    ::
    ?~  a    ~
    ?~  l.a  `n.a
    $(a l.a)
  ::
  ::  +pop: produce .head (smallest item) and .rest or crash if empty
  ::
  ++  pop
    |=  a=(tree item)
    ^-  [head=item rest=(tree item)]
    ::
    ?~  a    !!
    ?~  l.a  [n.a r.a]
    ::
    =/  l  $(a l.a)
    :-  head.l
    ::  load .rest.l back into .a and rebalance
    ::
    ?:  |(?=(~ rest.l) (mor key.n.a key.n.rest.l))
      a(l rest.l)
    rest.l(r a(r r.rest.l))
  ::  +del: delete .key from .a if it exists, producing value iff deleted
  ::
  ++  del
    |=  [a=(tree item) =key]
    ^-  [(unit val) (tree item)]
    ::
    ?~  a  [~ ~]
    ::  we found .key at the root; delete and rebalance
    ::
    ?:  =(key key.n.a)
      [`val.n.a (nip a)]
    ::  recurse left or right to find .key
    ::
    ?:  (compare key key.n.a)
      =+  [found lef]=$(a l.a)
      [found a(l lef)]
    =+  [found rig]=$(a r.a)
    [found a(r rig)]
  ::  +nip: remove root; for internal use
  ::
  ++  nip
    |=  a=(tree item)
    ^-  (tree item)
    ::
    ?>  ?=(^ a)
    ::  delete .n.a; merge and balance .l.a and .r.a
    ::
    |-  ^-  (tree item)
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (mor key.n.l.a key.n.r.a)
      l.a(r $(l.a r.l.a))
    r.a(l $(r.a l.r.a))
  ::  +traverse: stateful partial inorder traversal
  ::
  ::    Mutates .state on each run of .f.  Starts at .start key, or if
  ::    .start is ~, starts at the head (item with smallest key).  Stops
  ::    when .f produces .stop=%.y.  Traverses from smaller to larger
  ::    keys.  Each run of .f can replace an item's value or delete the
  ::    item.
  ::
  ++  traverse
    |*  state=mold
    |=  $:  a=(tree item)
            =state
            f=$-([state item] [(unit val) ? state])
        ==
    ^+  [state a]
    ::  acc: accumulator
    ::
    ::    .stop: set to %.y by .f when done traversing
    ::    .state: threaded through each run of .f and produced by +abet
    ::
    =/  acc  [stop=`?`%.n state=state]
    =<  abet  =<  main
    |%
    ++  this  .
    ++  abet  [state.acc a]
    ::  +main: main recursive loop; performs a partial inorder traversal
    ::
    ++  main
      ^+  this
      ::  stop if empty or we've been told to stop
      ::
      ?:  =(~ a)  this
      ?:  stop.acc  this
      ::  inorder traversal: left -> node -> right, until .f sets .stop
      ::
      =.  this  left
      ?:  stop.acc  this
      =^  del  this  node
      =?  this  !stop.acc  right
      =?  a  del  (nip a)
      this
    ::  +node: run .f on .n.a, updating .a, .state, and .stop
    ::
    ++  node
      ^+  [del=*? this]
      ::  run .f on node, updating .stop.acc and .state.acc
      ::
      ?>  ?=(^ a)
      =^  res  acc  (f state.acc n.a)
      ?~  res
        [del=& this]
      [del=| this(val.n.a u.res)]
    ::  +left: recurse on left subtree, copying mutant back into .l.a
    ::
    ++  left
      ^+  this
      ?~  a  this
      =/  lef  main(a l.a)
      lef(a a(l a.lef))
    ::  +right: recurse on right subtree, copying mutant back into .r.a
    ::
    ++  right
      ^+  this
      ?~  a  this
      =/  rig  main(a r.a)
      rig(a a(r a.rig))
    --
  ::  +tap: convert to list, smallest to largest
  ::
  ++  tap
    |=  a=(tree item)
    ^-  (list item)
    ::
    =|  b=(list item)
    |-  ^+  b
    ?~  a  b
    ::
    $(a l.a, b [n.a $(a r.a)])
  ::  +bap: convert to list, largest to smallest
  ::
  ++  bap
    |=  a=(tree item)
    ^-  (list item)
    ::
    =|  b=(list item)
    |-  ^+  b
    ?~  a  b
    ::
    $(a r.a, b [n.a $(a l.a)])
  ::  +gas: put a list of items
  ::
  ++  gas
    |=  [a=(tree item) b=(list item)]
    ^-  (tree item)
    ::
    ?~  b  a
    $(b t.b, a (put a i.b))
  ::  +uni: unify two ordered maps
  ::
  ::    .b takes precedence over .a if keys overlap.
  ::
  ++  uni
    |=  [a=(tree item) b=(tree item)]
    ^-  (tree item)
    ::
    ?~  b  a
    ?~  a  b
    ?:  =(key.n.a key.n.b)
      ::
      [n=n.b l=$(a l.a, b l.b) r=$(a r.a, b r.b)]
    ::
    ?:  (mor key.n.a key.n.b)
      ::
      ?:  (compare key.n.b key.n.a)
        $(l.a $(a l.a, r.b ~), b r.b)
      $(r.a $(a r.a, l.b ~), b l.b)
    ::
    ?:  (compare key.n.a key.n.b)
      $(l.b $(b l.b, r.a ~), a r.a)
    $(r.b $(b r.b, l.a ~), a l.a)
  ::
  ::  +get: get val at key or return ~
  ::
  ++  get
    |=  [a=(tree item) b=key]
    ^-  (unit val)
    ?~  a  ~
    ?:  =(b key.n.a)
      `val.n.a
    ?:  (compare b key.n.a)
      $(a l.a)
    $(a r.a)
  ::
  ::  +subset: take a range excluding start and/or end and all elements
  ::  outside the range
  ::
  ++  subset
    |=  $:  tre=(tree item)
            start=(unit key)
            end=(unit key)
        ==
    ^-  (tree item)
    |^
    ?:  ?&(?=(~ start) ?=(~ end))
      tre
    ?~  start
      (del-span tre %end end)
    ?~  end
      (del-span tre %start start)
    ?>  (compare u.start u.end)
    =.  tre  (del-span tre %start start)
    (del-span tre %end end)
    ::
    ++  del-span
      |=  [a=(tree item) b=?(%start %end) c=(unit key)]
      ^-  (tree item)
      ?~  a  a
      ?~  c  a
      ?-  b
          %start
        ::  found key
        ?:  =(key.n.a u.c)
          (nip a(l ~))
        ::  traverse to find key
        ?:  (compare key.n.a u.c)
          ::  found key to the left of start
          $(a (nip a(l ~)))
        ::  found key to the right of start
        a(l $(a l.a))
      ::
          %end
        ::  found key
        ?:  =(u.c key.n.a)
          (nip a(r ~))
        ::  traverse to find key
        ?:  (compare key.n.a u.c)
          :: found key to the left of end
          a(r $(a r.a))
        :: found key to the right of end
        $(a (nip a(r ~)))
      ==
    --
  --
::
+$  mold
  ::    normalizing gate
  ::
  ::  a gate that accepts any noun, and validates its shape, producing the
  ::  input if it fits or a default value if it doesn't.
  ::
  ::  examples: * @ud ,[p=time q=?(%a %b)]
  $~(* $-(* *))
::
++  aor
  ~/  %aor
  |=  [a=* b=*]
  ^-  ?
  ?:  =(a b)  &
  ?.  ?=(@ a)
    ?:  ?=(@ b)  |
    ?:  =(-.a -.b)
      $(a +.a, b +.b)
    $(a -.a, b -.b)
  ?.  ?=(@ b)  &
  |-
  =+  [c=(end 3 a) d=(end 3 b)]
  ?:  =(c d)
    $(a (rsh 3 a), b (rsh 3 b))
  (lth c d)
--
