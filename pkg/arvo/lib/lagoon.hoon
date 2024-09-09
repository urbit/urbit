/-  lagoon
=+  lagoon
::                                                    ::
::::                    ++la                          ::  (2v) vector/matrix ops
~%  %non  ..part  ~  :: nest non in hex for now
|%
::  +lake: set +la params
::
::    rnd: rounding mode
::
+$  rounding-mode  ?(%n %u %d %z)
++  lake
  |=  [inrnd=rounding-mode]
  %*(. la rnd inrnd)
::
++  la
  ^|
  =+  [rnd=*rounding-mode]
  ~/  %lagoon
  |%
  ::
  ::  Utilities
  ::
  ++  print  |=(a=ray ~>(%slog.1^(to-tank (ravel a) shape.meta.a kind.meta.a) ~))
  ::
  ++  slog   |=(a=ray (^slog (to-tank (ravel a) shape.meta.a kind.meta.a) ~))
  ::
  ::
  ++  to-tank
    |=  [dat=(list @) shape=(list @) =kind]
    ^-  tank
    ::  1D vector case
    ?:  =(1 (lent shape))
      :+  %rose  [" " "[" "]"]
      %+  turn  dat
      |=  i=@
      ^-  tank
      (sell [%atom kind ~] i)
    ::  general tensor case
    =/  width  (^div (lent dat) -.shape)
    :+  %rose  [" " "[" "]\0a"]
    ^-  (list tank)
    %+  turn  (gulf 0 (dec -.shape))
    |=  i=@
    (to-tank (swag [(^mul width i) width] dat) +.shape kind)
  ::
  ++  get-term
    |=  =meta
    ?-    kind.meta
        %uint
      %ud
      ::
        %int2
      !!
      ::
        %i754
      ?+    bloq.meta  ~|(bloq.meta !!)
        %7  %rq
        %6  %rd
        %5  %rs
        %4  %rh
      ==
    ==
  ::
  ++  squeeze  |*(a=* `(list _a)`~[a])
  ++  submatrix
    ::
    ::  +submatrix: grab a submatrix using numpy slice notation, except indices are inclusive.
    ::  If you aren't slicing the last few dimensions, you can omit them.
    ::
    ~/  %submatrix
    |=  [sli=(list slice) a=ray]
    ::
    ::  example: sli=~[`[`1 `3] `[`1 ~] ~] is equivalent to a[1:3,1:,:]
    ::
    ^-  ray
    ::
    ::  pad slice with sigs if necessary
    =?  sli  (^lth (lent sli) (lent shape.meta.a))
      (weld sli (reap (^sub (lent shape.meta.a) (lent sli)) *(unit [(unit @) (unit @)])))
    ::
    ::  calculate indices to grab using cartesian product
    =/  out-indices=(list (list (list @)))
    %+  turn
      (gulf 0 (dec (lent shape.meta.a)))
    |=  i=@
    =/  s  (snag i sli)
    =/  dim  (snag i shape.meta.a)
    ?~  s
      (turn (gulf 0 (dec dim)) squeeze)
    =/  s2=[(unit @) (unit @)]  (need s)
    =/  c=^  [(fall -.s2 ~) (fall +.s2 ~)]
    ^-  (list (list @))
    ?+    c  !!
        [j=@ k=~]  (turn (gulf j.c (dec dim)) squeeze)
        [j=@ k=@]  (turn (gulf j.c k.c) squeeze)
        [j=~ k=@]  (turn (gulf 0 k.c) squeeze)
        [~ ~]  (turn (gulf 0 (dec dim)) squeeze)
    ==
    ::
    ::  calculate the shape of the result
    =/  out-shape=(list @)
    %+  turn
      out-indices
    |=(inds=(list (list @)) (lent inds))
    :: 
    ::  grab submatrix entries from cartesian product
    =/  new-dat=@ux
    %+  rep  bloq.meta.a
    %+  turn
      (gather out-indices)
    |=  dex=(list @)
    (get-item a dex)
    ::
    ::  construct new ray
    %-  spac
    =,  meta.a
    :-  [out-shape bloq kind ~]
    new-dat
  ::
  ++  product  ::  cartesian product
    |*  [a=(list) b=(list)]
    ?~  a
      b
    %-  zing
    %+  turn  a
    |=  ai=_-.a
    (turn b |=(bi=_-.b (welp ai bi)))
  ::
  ++  gather
    |=  [a=(list (list (list @)))]
    ^-  (list (list @))
    =/  i  0
    =|  c=(list (list @)) 
    |-
    ?:  =(i (lent a))
      c
    $(i +(i), c `(list (list @))`(product c (snag i a)))
  ::
  ++  get-item  ::  extract item at index .dex
    |=  [=ray dex=(list @)]
    ^-  @ux
    =/  len  (^sub (roll shape.meta.ray ^mul) 1)
    %^    cut
        bloq.meta.ray
      :: [(^sub len (get-bloq-offset meta.ray dex)) 1]
      [(get-bloq-offset meta.ray dex) 1]
    data.ray
  ::
  ++  set-item  ::  set item at index .dex to .val
    |=  [=ray dex=(list @) val=@]
    ^+  ray
    =/  len  (^sub (roll shape.meta.ray ^mul) 1)
    :-  meta.ray
    %^    sew
        bloq.meta.ray
      :: [(^sub len (get-bloq-offset meta.ray dex)) 1 val]
      [(get-bloq-offset meta.ray dex) 1 val]
    data.ray
  ::
  ++  get-row
    |=  [a=ray dex=(list @)]
    ^-  ray
    =,  meta.a
    ?:  =(1 (lent shape))
      (spac [~[1] bloq kind ~] (get-item a dex))
    ?>  =(+((lent dex)) (lent shape))
    =/  res
      %-  zeros
      :*  ~[1 (snag 0 (flop shape))]
          bloq
          kind
          ~
      ==
    =/  idx  0
    |-  ^-  ray
    ?:  =((snag 0 (flop shape.meta.res)) idx)  res
    =/  val  (get-item a (weld dex ~[idx]))
    $(idx +(idx), res (set-item res ~[0 idx] val))
  ::
  ++  set-row
    |=  [a=ray dex=(list @) row=ray]
    ^-  ray
    ?:  &(=(1 (lent dex)) =(1 (lent shape.meta.row)))  (set-item a dex (get-item row ~[0]))
    ?>  =(+((lent dex)) (lent shape.meta.a))
    ?>  =((lent shape.meta.row) 2)
    ?>  =(1 (snag 0 shape.meta.row))
    ?>  =((snag 1 shape.meta.row) (snag 0 (flop shape.meta.a)))
    =/  idx  0
    |-  ^-  ray
    ?:  =((snag 1 shape.meta.row) idx)  a
    %=  $
      idx  +(idx)
      a    (set-item a (weld dex ~[idx]) (get-item row ~[0 idx]))
    ==
  :: only works for 2D
  ++  get-col
    |=  [a=ray dex=(list @)]
    ^-  ray
    (get-row (transpose a) dex)
  ::
  ++  set-col
    |=  [a=ray dex=(list @) col=ray]
    ^-  ray
    (transpose (set-row (transpose a) dex col))
  ::
  ++  get-bloq-offset  ::  get bloq offset of n-dimensional index
    |=  [=meta dex=(list @)]
    ^-  @
    (get-item-number shape.meta dex)
  ::
  ++  get-item-number  ::  convert n-dimensional index to scalar index
    |=  [sap=(list @) dex=(list @)]
    ^-  @
    =/  cof  1
    =/  ret  0
    =.  sap  (flop sap)
    =.  dex  (flop dex)
    |-  ^+  ret
    ?~  sap  ret :: out of indices, return
    ?~  dex  !!  :: no indices past size
    ?>  (^lth i.dex i.sap)  :: index must be less than size
    %=  $
      sap  t.sap
      dex  t.dex
      cof  (^mul cof i.sap)
      ret  (^add ret (^mul i.dex cof))
    ==
  ::  Return the stride in each dimension:  row, col, layer, &c.
  ::  The stride is reported in units of bits.
  ::
  ++  strides
    |=  =meta
    ^-  (list @)
    =/  idx  0
    =|  res=(list @)
    |-  ^-  (list @)
    ?~  shape.meta  (flop res)
    =/  stride  (roll (scag idx `(list @)`shape.meta) ^mul)
    %=  $
      idx         +(idx)
      shape.meta  t.shape.meta
      res         [(^mul (pow 2 bloq.meta) stride) res]
    ==
  ::
  ++  get-dim  :: convert scalar index to n-dimensional index
    |=  [shape=(list @) ind=@]
    =/  shape  (flop shape)
    =/  i=@  0
    =|  res=(list @)
    ?>  (^lth ind (roll shape ^mul))
    |-  ^-  (list @)
    ?:  (^gte i (lent shape))  (flop res)
    %=    $
      res  `(list @)`(snoc res (^mod ind (snag i shape)))
      ind  (^div ind (snag i shape))
      i    (^add i 1)
    ==
  ::
  ++  get-item-index
    |=  [shape=(list @) num=@]
    ^-  @
    =/  len  (roll shape ^mul)
    =-  (roll - ^add)
    ^-  (list @)
    %+  turn  shape
    |=  wid=@
    (^mod (^div len wid) num)
  ::
  ++  ravel
    :: ~/  %ravel
    |=  a=ray
    ^-  (list @)
    (snip (rip bloq.meta.a data.a))
  ::
  ++  en-ray    :: baum to ray
    |=  =baum
    ^-  ray
    =/  a=ray  (zeros meta.baum)
    =/  i  0
    =/  n  (roll shape.meta.a ^mul)
    |-
    ?:  =(i n)  a
    %=  $
      i  +(i)
      data.a
        %+  con
          data.a
        %+  lsh
          [bloq.meta.a i]
        (get-item-baum baum (get-dim shape.meta.a i))
    ==
  ::
  ++  de-ray    :: ray to baum
    |=  =ray
    ^-  baum
    |^
    :-  meta.ray
    ^-  ndray
    ::
    =,  meta.ray
    ?:  =(1 (lent shape))
      ::  Snip off tail which is the pinned 0x1 MSB
      (snip (rip bloq data.ray))
    ::
    ?:  =(2 (lent shape))
      =/  dims  (flop shape)
      =|  fin=(list ndray)
      =|  els=ndray
      |-
      ?:  =(0x1 data.ray)  (welp ;;((list ndray) fin) ~[;;((list ndray) els)])
      %=  $
        els   (snip (rip bloq (cut bloq [0 +((snag 0 dims))] data:(spac `^ray`[[~[(snag 0 dims) 1] bloq kind tail] `@ux`data.ray]))))
        fin   ?~  els  fin  :: skip on first row
              ?~  fin  `(list (list ndray))`~[;;((list ndray) els)]
              (welp ;;((list (list ndray)) fin) ~[;;((list ndray) els)])
        data.ray  (rsh [bloq (snag 0 dims)] data.ray)
      ==
    !!
    ::  cut off end
    ++  rip
      |=  [a=bite b=@]
      ^-  (list @)
      =/  cnt  ?@(a 1 +.a)
      ?~  (^rip a b)  (reap cnt 0)
      (^rip a b)
    --
  ::
  ++  check
    |=  =ray
    ^-  ?
    .=  (roll shape.meta.ray ^mul)
    (dec (met bloq.meta.ray data.ray))
  ::
  ++  get-item-baum
    |=  [=baum dex=(list @)]
    ^-  @
    =/  a=ndray  data.baum
    |-
    ?~  a  !!
    ?@  (snag -.dex ;;((list ndray) a))
      ;;(@ (snag -.dex ((list ndray) a)))
    %=  $
      dex  +.dex
      a    (snag -.dex ;;((list ndray) a))
    ==
  ::
  ++  fill
    |=  [=meta x=@]
    ^-  ray
    =/  len  (roll shape.meta ^mul)
    :-  meta
    (con +:(zeros meta) (fil bloq.meta len x))
  ::
  ++  spac
    |=  =ray
    ^-  ^ray
    :-  meta.ray
    (con data:(zeros meta.ray) data.ray)
  ::
  ++  unspac
    |=  =ray
    ^-  ^ray
    :-  meta.ray
    (cut bloq.meta.ray [0 (roll shape.meta.ray ^mul)] data.ray)
  ::
  ++  scalar-to-ray
    |=  [=meta data=@]
    ^-  ray
    =.  shape.meta  (reap (lent shape.meta) 1)
    %-  spac
    [meta data]
  ::
  ++  change
    |=  [=ray =kind =bloq]
    ^-  ^ray
    ?+    kind.meta.ray  !!
        %uint
      ?+    kind  !!
          :: %uint -> %uint
          %uint
        %-  en-ray
        :-  [shape.meta.ray bloq %uint tail.meta.ray]
        data:(de-ray ray)
          :: %uint -> %i754
          %i754
        %-  en-ray
        :-  [shape.meta.ray bloq %i754 tail.meta.ray]
        %+  turn  (ravel ray)
        ?+  bloq  !!
          %7  ~(sun rq rnd)
          %6  ~(sun rd rnd)
          %5  ~(sun rs rnd)
          %4  ~(sun rh rnd)
        ==
      ==
      ::
        %i754
      ?+    kind  !!
          :: %i754 -> %uint
          :: XXX will incorrectly convert negative values to %uint
          %uint
        %-  en-ray
        :-  [shape.meta.ray bloq %uint tail.meta.ray]
        %+  turn  (ravel ray)
        ?+  bloq.meta.ray  !!
          %7  |=(a=@rq ^-(@u (^div (need (~(toi rq rnd) a)) 2)))
          %6  |=(a=@rd ^-(@u (^div (need (~(toi rd rnd) a)) 2)))
          %5  |=(a=@rs ^-(@u (^div (need (~(toi rs rnd) a)) 2)))
          %4  |=(a=@rh ^-(@u (^div (need (~(toi rh rnd) a)) 2)))
        ==
          :: %i754 -> %i754
          %i754
        ?>  &((^gte bloq %4) (^lte bloq %7))
        %-  en-ray
        :-  [shape.meta.ray bloq %i754 tail.meta.ray]
        data:(de-ray ray)
      ==
    ==
  ::
  ::  Builders
  ::
  ::
  ++  eye      ::  produces identity matrix of shape nxn.
    |=  =meta
    ^-  ray
    ~_  leaf+"lagoon-fail"
    ?>  =(2 (lent shape.meta))
    ?>  =((snag 0 shape.meta) (snag 1 shape.meta))
    =/  n  (snag 0 shape.meta)
    =<  +
    %^    spin
        (gulf 0 (dec n))
      ^-  ray  (zeros [~[n n] bloq.meta kind.meta ~])
    |=  [i=@ r=ray]
    :: [i (set-item r ~[i i] 1)]
    :-  i
    %^  set-item
        r
      ~[i i]
    ^-  @
    ?-    kind.meta
        %uint  `@`1
      ::
        %int2  !!
      ::
        %i754
      ?+  bloq.meta  ~|(bloq.meta !!)
        %7  .~~~1
        %6  .~1
        %5  .1
        %4  .~~1
      ==
    ==
  ::    Zeroes
  ++  zeros
    |=  =meta  ^-  ray
    ~_  leaf+"lagoon-fail"
    :-  meta
    (lsh [bloq.meta (roll shape.meta ^mul)] 1)
  ::    Ones
  ++  ones
    |=  =meta  ^-  ray
    ~_  leaf+"lagoon-fail"
    =/  one
      ?-    kind.meta
          %uint  `@`1
        ::
          %int2  !!
        ::
          %i754
        ?+  bloq.meta  !!
          %7  .~~~1
          %6  .~1
          %5  .1
          %4  .~~1
        ==
      ==
    (fill meta one)
  ::  Produce a 1-dimensional index array.
  ::  Only produces %uint.
  ::  Note that this runs from 0 to n-1.  The point of ++iota is to be an index,
  ::  so it needs to pattern-match the context rather than slavishly follow APL.
  ::
  ++  iota
    |=  =meta
    ^-  ray
    ?>  =((lent shape.meta) 1)
    =/  n  (snag 0 shape.meta)
    =.  kind.meta  %uint
    (en-ray meta (gulf 0 (dec n)))
  ::  Produce a magic square in nD.
  ++  magic
    |=  =meta
    ^-  ray
    =/  n  (roll shape.meta ^mul)
    %+  reshape
      (en-ray [~[n] bloq.meta %uint ~] (gulf 0 (dec n)))
    shape.meta
  ::  Produce a 1-dimensional range along one dimension
  ::  as [a, b) with interval d.
  ::  Only produces %i754.
  ::
  ++  range
    ~/  %range
    |=  [=meta [a=@ b=@] d=@]
    ^-  ray
    =.  kind.meta  %i754
    %-  spac
    %-  en-ray
    ::
    ?+    bloq.meta  !!
        %7
      =/  ba  (~(sub rq rnd) b a)
      =/  bad  `(list @)`~[a]
      |-  ^-  baum
      ?:  (?:((~(lth rq rnd) ba .~~~0) ~(lte rq rnd) ~(gte rq rnd)) (~(add rq rnd) (snag 0 bad) d) b)
        =.  shape.meta  ~[(lent bad)]
        [meta (flop bad)]
      $(bad [(~(add rq rnd) (snag 0 bad) d) bad])
      ::
        %6
      =/  ba  (~(sub rd rnd) b a)
      =/  bad  `(list @)`~[a]
      |-  ^-  baum
      ?:  (?:((~(lth rd rnd) ba .~0) ~(lte rd rnd) ~(gte rd rnd)) (~(add rd rnd) (snag 0 bad) d) b)
        =.  shape.meta  ~[(lent bad)]
        [meta (flop bad)]
      $(bad [(~(add rd rnd) (snag 0 bad) d) bad])
      ::
        %5
      =/  ba  (~(sub rs rnd) b a)
      =/  bad  `(list @)`~[a]
      |-  ^-  baum
      ?:  (?:((~(lth rs rnd) ba .0) ~(lte rs rnd) ~(gte rs rnd)) (~(add rs rnd) (snag 0 bad) d) b)
        =.  shape.meta  ~[(lent bad)]
        [meta (flop bad)]
      $(bad [(~(add rs rnd) (snag 0 bad) d) bad])
      ::
        %4
      =/  ba  (~(sub rh rnd) b a)
      =/  bad  `(list @)`~[a]
      |-  ^-  baum
      ?:  (?:((~(lth rh rnd) ba .~~0) ~(lte rh rnd) ~(gte rh rnd)) (~(add rh rnd) (snag 0 bad) d) b)
        [meta (flop bad)]
      $(bad [(~(add rh rnd) (snag 0 bad) d) bad])
    ==
  ::  Produce a 1-dimensional range along one dimension
  ::  as [a b] with number of steps n.
  ::  Only produces %i754.
  ::
  ++  linspace
    ~/  %linspace
    |=  [=meta [a=@ b=@] n=@ud]
    ^-  ray
    =.  shape.meta  ~[n]
    =.  kind.meta  %i754
    ?<  =(n 0)
    ?:  =(n 1)  (en-ray meta ~[a])
    %-  en-ray
    :-  meta
    ::
    ?+    bloq.meta  !!
        %7
      =/  ba  (~(sub rq rnd) b a)
      =/  d  (~(div rq rnd) ba (~(sun rq rnd) (dec n)))
      =|  bad=(list @)
      =|  i=@ud
      |-  ^-  ndray
      ?:  (^lte n +(i))  (flop [b bad])
      $(i +(i), bad [(~(add rq rnd) a (~(mul rq rnd) (~(sun rq rnd) i) d)) bad])
      ::
        %6
      =/  ba  (~(sub rd rnd) b a)
      =/  d  (~(div rd rnd) ba (~(sun rd rnd) (dec n)))
      =|  bad=(list @)
      =|  i=@ud
      |-  ^-  ndray
      ?:  (^lte n +(i))  (flop [b bad])
      $(i +(i), bad [(~(add rd rnd) a (~(mul rd rnd) (~(sun rd rnd) i) d)) bad])
      ::
        %5
      =/  ba  (~(sub rs rnd) b a)
      =/  d  (~(div rs rnd) ba (~(sun rs rnd) (dec n)))
      =|  bad=(list @)
      =|  i=@ud
      |-  ^-  ndray
      ?:  (^lte n +(i))  (flop [b bad])
      $(i +(i), bad [(~(add rs rnd) a (~(mul rs rnd) (~(sun rs rnd) i) d)) bad])
      ::
        %4
      =/  ba  (~(sub rh rnd) b a)
      =/  d  (~(div rh rnd) ba (~(sun rh rnd) (dec n)))
      =|  bad=(list @)
      =|  i=@ud
      |-  ^-  ndray
      ?:  (^lte n +(i))  (flop [b bad])
      $(i +(i), bad [(~(add rh rnd) a (~(mul rh rnd) (~(sun rh rnd) i) d)) bad])
    ==
  ::  Coerce 1D array along specified dimension with given overall
  ::  dimensionality.
  ::
  ++  urge
    |=  [=ray [i=@ud n=@ud]]
    ^-  ^ray
    ?>  =(1 (lent shape.meta.ray))
    =.  shape.meta.ray  `(list @)`(zing (reap n ~[1]))
    =.  shape.meta.ray  (snap shape.meta.ray i n)
    |-
    ray
  ::  Produce an n-dimensional array containing a single value.
  ::
  ++  scale
    |=  [=meta data=@]
    ^-  ray
    =.  shape.meta  `(list @)`(zing (reap (lent shape.meta) ~[1]))
    ?-    kind.meta
        %uint
      (spac [meta `@ux`data])
      ::
        %int2
      (spac [meta `@ux`data])
      ::
        %i754
      ::  convert date to fl to @r XXX TODO REVISIT whether we want to specify input type
      =/  fin
        ?+    bloq.meta  !!
          %7  (bit:ma:rq (sea:ma:rq data))
          %6  (bit:ma:rd (sea:ma:rd data))
          %5  (bit:ma:rs (sea:ma:rs data))
          %4  (bit:ma:rh (sea:ma:rh data))
        ==
      (spac [meta `@ux`fin])
    ==
  ::
  ::  Operators
  ::
  ++  max
    ~/  %max
    |=  a=ray
    ?>  (check a)
    =/  fun
      |:  [b=1 c=-:(ravel a)] 
      ?.  =(((fun-scalar meta.a %gth) b c) 0)
        b  c 
    (scalar-to-ray meta.a (reel (ravel a) fun))
  ::
  ++  argmax :: Only returns first match
    ~/  %argmax
    |=  a=ray
    ^-  @ud
    ?>  (check a)
    +:(find ~[(get-item (max a) ~[0 0])] (ravel a))
  ::
  ++  min
    ~/  %min
    |=  a=ray
    ?>  (check a)
    =/  fun
      |:  [b=1 c=-:(ravel a)] 
      ?.  =(((fun-scalar meta.a %lth) b c) 0)
        b  c 
    (scalar-to-ray meta.a (reel (ravel a) fun))
  ::
  ++  argmin :: Only returns first match
    ~/  %argmin
    |=  a=ray
    ^-  @ud
    ?>  (check a)
    +:(find ~[(get-item (min a) ~[0 0])] (ravel a))
  ::
  ++  cumsum
    ~/  %cumsum
    |=  a=ray
    ^-  ray
    ?>  (check a)
    %+  scalar-to-ray  meta.a
    (reel (ravel a) |=([b=@ c=@] ((fun-scalar meta.a %add) b c)))
  ::
  ++  prod
    |=  a=ray
    ^-  ray
    ?>  (check a)
    %+  scalar-to-ray  meta.a
    (reel (ravel a) |=([b=_1 c=_1] ((fun-scalar meta.a %mul) b c)))
  ::
  ++  reshape
    |=  [a=ray shape=(list @)]
    ^-  ray
    ?>  (check a)
    =/  in-cnt  (reel shape.meta.a ^mul)
    =/  out-cnt  (reel shape ^mul)
    ?>  =(in-cnt out-cnt)
    =.  shape.meta.a  shape
    a
  ::  stack along dimension (0 row, 1 col, 2 lay, etc.)
  ++  stack
    ~/  %stack
    |=  [a=ray b=ray dim=@ud]
    ^-  ray
    ?>  (check a)
    ?>  (check b)
    ::  check same dims overall
    ?>  =((lent shape.meta.a) (lent shape.meta.b))
    ::  check same dims other than target dim
    ?>  =/  idx  0
      |-  ^-  ?
      ?:  =(idx (lent shape.meta.a))  %.y
      ?:  =(dim idx)  $(idx +(idx))
      ?.  =((snag idx shape.meta.a) (snag idx shape.meta.b))
        %.n
      $(idx +(idx))
    ::  TODO revisit this assumption/requirement
    ?>  (^lte dim (lent shape.meta.a))
    =|  c=ray
    ?:  =(0 dim)
      =.  meta.c  meta.a
      =.  shape.meta.c
        :-  (^add (snag dim shape.meta.a) (snag dim shape.meta.b))
            +.shape.meta.a
      =.  data.c  (con (lsh [bloq.meta.a (roll shape.meta.a ^mul)] data.a) data:(unspac b))
      c
    ?:  =(1 dim)
      =.  meta.c  meta.a
      =.  shape.meta.c
        (snap shape.meta.c dim (^add (snag dim shape.meta.a) (snag dim shape.meta.b)))
      =/  c  (zeros meta.c)
      =/  idx  0
      |-  ^-  ray
      ?:  =((snag 0 (flop shape.meta.a)) idx)  c
      =/  off  (weld (snip (snip shape.meta.a)) ~[idx])
      =/  row-a  (get-row a off)
      =/  row-b  (get-row b off)
      =/  data-c  (con (lsh [bloq.meta.a (snag 1 shape.meta.row-a)] data.row-a) data:(unspac row-b))
      =/  meta-c=meta  meta.row-a
      =.  shape.meta-c  (snap shape.meta-c dim (^add (snag dim shape.meta.row-a) (snag dim shape.meta.row-b)))
      =/  row-c=ray  (spac [meta-c data-c])
      %=  $
        idx  +(idx)
        c    (set-row c off row-c)
      ==
    !!
  ::
  ++  hstack
    |=  [a=ray b=ray]
    ^-  ray
    (stack a b 1)
  ::
  ++  vstack
    |=  [a=ray b=ray]
    ^-  ray
    (stack a b 0)
  ::
  ::
  ++  transpose
    ~/  %transpose
    |=  a=ray  ^-  ray
    ?>  (check a)
    =,  meta.a
    ?>  =(2 (lent shape))
    =/  i  0
    =/  j  0
    =/  shape=(list @)  ~[(snag 1 shape) (snag 0 shape)]
    =/  prod=ray  (zeros [shape bloq kind ~])
    |-
      ?:  =(i (snag 0 shape.meta.a))
        prod
      %=  $
        i  +(i)
        prod
      |-
        ?:  =(j (snag 1 shape.meta.a))
          prod
        %=  $
          j  +(j)
          prod  (set-item prod ~[j i] (get-item a ~[i j]))
        ==
    ==
  ::  Returns diagonal of an array.
  ::
  ++  diag
    ~/  %diag
    |=  a=ray
    ^-  ray
    ?>  (check a)
    =,  meta.a
    ?>  =(2 (lent shape))
    ?>  =(-.shape +<.shape)
    ^-  ray
    %-  en-ray
    ^-  baum
    :-  `meta`[~[-.shape 1] bloq kind tail]
    ^-  ndray
    %+  turn
      `(list @)`(flop (gulf 0 (dec -.shape)))
    |=(i=@ (get-item a ~[i i]))
  ::
  ++  trace
    ~/  %trace
    |=  a=ray
    ^-  ray
    (cumsum (diag a))
  ::
  ++  dot
    ~/  %dot
    |=  [a=ray b=ray]
    ^-  ray
    ?>  =(shape.meta.a shape.meta.b)
    (cumsum (mul a b))
  ::
  ++  mmul
    ~/  %mmul
    |=  [a=ray b=ray]
    ?>  (check a)
    ?>  (check b)
    =/  i  0
    =/  j  0
    =/  k  0
    =/  shape=(list @)  ~[(snag 0 shape.meta.a) (snag 1 shape.meta.b)]
    =/  prod=ray  =,(meta.a (zeros [^shape bloq kind ~]))
    ::  
    ::  multiplication conditions
    ?>
    ?&  =(2 (lent shape.meta.b))
        =(2 (lent shape.meta.a))
        =((snag 1 shape.meta.a) (snag 0 shape.meta.b))
    ==
    |-
      ?:   =(i (snag 0 shape.meta.prod))
        prod
      %=    $
        i  +(i)
        prod
      |-
        ?:  =(j (snag 1 shape.meta.prod))
          prod
        =/  cume  0
        %=    $
            j  +(j)
            prod
          |-  
          ?:   =(k (snag 1 shape.meta.a))
            (set-item prod `(list @)`~[i j] cume)
          %=    $
              k  +(k)
              cume
            %+  (fun-scalar meta.a %add)
              cume
            %+  (fun-scalar meta.a %mul)
              (get-item a ~[i k])
            (get-item b ~[k j])
          ==
        ==
      ==
::
  ++  abs
    ~/  %abs
    |=  a=ray
    ^-  ray
    (el-wise-op a (trans-scalar bloq.meta.a kind.meta.a %abs))
::
  ++  add-scalar
    ~/  %add-scal
    |=  [a=ray n=@]
    ^-  ray
    =/  b=ray  (fill meta.a n)
    (add a b)
  ::
  ++  sub-scalar
    ~/  %sub-scal
    |=  [a=ray n=@]
    ^-  ray
    =/  b=ray  (fill meta.a n)
    (sub a b)
  ::
  ++  mul-scalar
    ~/  %mul-scal
    |=  [a=ray n=@]
    ^-  ray
    =/  b=ray  (fill meta.a n)
    (mul a b)
  ::
  ++  div-scalar
    ~/  %div-scal
    |=  [a=ray n=@]
    ^-  ray
    =/  b=ray  (fill meta.a n)
    (div a b)
  ::
  ++  mod-scalar
    ~/  %mod-scal
    |=  [a=ray n=@]
    ^-  ray
    =/  b=ray  (fill meta.a n)
    (mod a b)
  ::
  ++  add
    ~/  %add-rays
    |=  [a=ray b=ray]
    ^-  ray
    (bin-op a b (fun-scalar meta.a %add))
  ::
  ++  sub
    ~/  %sub-rays
    |=  [a=ray b=ray]
    ^-  ray
    (bin-op a b (fun-scalar meta.a %sub))
  ::
  ++  mul
    ~/  %mul-rays
    |=  [a=ray b=ray]
    ^-  ray
    (bin-op a b (fun-scalar meta.a %mul))
  ::
  ++  div
    ~/  %div-rays
    |=  [a=ray b=ray]
    ^-  ray
    (bin-op a b (fun-scalar meta.a %div))
  ::
  ++  mod
    ~/  %mod-rays
    |=  [a=ray b=ray]
    ^-  ray
    (bin-op a b (fun-scalar meta.a %mod))
  ::
  ++  pow-n
    |=  [a=ray n=@ud]
    ^-  ray
    ?>  (check a)
    ?~  =(0 n)  (ones meta.a)
    =/  b=ray   a
    |-  ^-  ray
    ?~  =(1 n)  b
    $(b (mul a b), n (dec n))
  ::
  ++  gth
    ~/  %gth
    |=  [a=ray b=ray]
    ^-  ray
    (bin-op a b (fun-scalar meta.a %gth))
  ::
  ++  gte
    ~/  %gte
    |=  [a=ray b=ray]
    ^-  ray
    (bin-op a b (fun-scalar meta.a %gte))
  ::
  ++  lth
    ~/  %lth
    |=  [a=ray b=ray]
    ^-  ray
    (bin-op a b (fun-scalar meta.a %lth))
  ::
  ++  lte
    ~/  %lte
    |=  [a=ray b=ray]
    ^-  ray
    (bin-op a b (fun-scalar meta.a %lte))
  ::
  ++  equ
    :: ~/  %equ
    |=  [a=ray b=ray]
    ^-  ray
    (bin-op a b (fun-scalar meta.a %equ))
  ::
  ++  neq
    :: ~/  %equ
    |=  [a=ray b=ray]
    ^-  ray
    (bin-op a b (fun-scalar meta.a %neq))
  ::
  ++  mpow-n
    |=  [a=ray n=@ud]
    ^-  ray
    ?~  =(0 n)  (ones meta.a)
    =/  b=ray   a
    |-  ^-  ray
    ?~  =(1 n)  b
    $(b (mmul a b), n (dec n))
  ::  proximity check [abs rel]
  ::
  ++  is-close
    |=  [a=ray b=ray tol=[@ @]]
    ^-  ray
    ?>  =(shape.meta.a shape.meta.b)
    =/  atol  (fill meta.a data:(scale meta.a -.tol))
    =/  rtol  (fill meta.a data:(scale meta.a +.tol))
    (lte (abs (sub a b)) (add atol (mul rtol (abs b))))
  ::
  ++  any
    |=  [a=ray]
    ^-  ?(%.y %.n)
    (^lth (get-item (cumsum a) ~[0]) (roll shape.meta.a ^mul))
  ::
  ++  all
    |=  [a=ray]
    ^-  ?(%.y %.n)
    =((get-item (cumsum a) ~[0]) 0)
  ::
  +$  ops   $?  %add
                %sub
                %mul
                %div
                %mod
                %pow
                %gth
                %gte
                %lth
                %lte
                %equ
                %neq
                %abs
            ==
  ::
  ++  fun-scalar
    |=  [=meta fun=ops]
    ^-  $-([@ @] @)
    =,  meta
    ?-    `^kind`kind
        %uint
      ?+  fun  !!
        %add  ~(sum fe bloq)
        %sub  ~(dif fe bloq)
        %mul  |=([b=_1 c=_1] (~(sit fe bloq) (^mul b c)))
        %div  |=([b=_1 c=_1] (~(sit fe bloq) (^div b c)))
        %mod  |=([b=@ c=@] (~(sit fe bloq) (^mod b c)))
        %pow  |=([b=@ c=@] (~(sit fe bloq) (pow b c)))
        ::%exp  |=([b=@ c=@] (~(sit fe bloq) (^pow b c)))
        ::%log  |=([b=@ c=@] (~(sit fe bloq) (^pow b c)))
        %gth  |=([b=@ c=@] !(^gth b c))
        %gte  |=([b=@ c=@] !(^gte b c))
        %lth  |=([b=@ c=@] !(^lth b c))
        %lte  |=([b=@ c=@] !(^lte b c))
        %equ  |=([b=@ c=@] ?:(.=(b c) 1 0))
        %neq  |=([b=@ c=@] ?:(.=(b c) 0 1))
      ==
      ::
        %int2  !!
      ::
        %i754
      ?+    `^bloq`bloq  !!
          %7
        ?+  fun  !!
          %add  ~(add rq rnd)
          %sub  ~(sub rq rnd)
          %mul  ~(mul rq rnd)
          %div  ~(div rq rnd)
          %mod  |=([a=@rq b=@rq] (~(sub rq rnd) a (~(mul rq rnd) b (~(san rq rnd) (need (~(toi rq rnd) (~(div rq rnd) a b)))))))
          %gth  |=([a=@rq b=@rq] ?:((~(gth rq rnd) a b) .~~~1 .~~~0))
          %gte  |=([a=@rq b=@rq] ?:((~(gte rq rnd) a b) .~~~1 .~~~0))
          %lth  |=([a=@rq b=@rq] ?:((~(lth rq rnd) a b) .~~~1 .~~~0))
          %lte  |=([a=@rq b=@rq] ?:((~(lte rq rnd) a b) .~~~1 .~~~0))
          %equ  |=([a=@rq b=@rq] ?:(.=(a b) .~~~1 .~~~0))
          %neq  |=([a=@rq b=@rq] ?:(.=(a b) .~~~0 .~~~1))
        ==
          %6
        ?+  fun  !!
          %add  ~(add rd rnd)
          %sub  ~(sub rd rnd)
          %mul  ~(mul rd rnd)
          %div  ~(div rd rnd)
          %mod  |=([a=@rd b=@rd] (~(sub rd rnd) a (~(mul rd rnd) b (~(san rd rnd) (need (~(toi rd rnd) (~(div rd rnd) a b)))))))
          %gth  |=([a=@rd b=@rd] ?:((~(gth rd rnd) a b) .~1 .~0))
          %gte  |=([a=@rd b=@rd] ?:((~(gte rd rnd) a b) .~1 .~0))
          %lth  |=([a=@rd b=@rd] ?:((~(lth rd rnd) a b) .~1 .~0))
          %lte  |=([a=@rd b=@rd] ?:((~(lte rd rnd) a b) .~1 .~0))
          %equ  |=([a=@rd b=@rd] ?:(.=(a b) .~1 .~0))
          %neq  |=([a=@rd b=@rd] ?:(.=(a b) .~0 .~1))
        ==
          %5
        ?+  fun  !!
          %add  ~(add rs rnd)
          %sub  ~(sub rs rnd)
          %mul  ~(mul rs rnd)
          %div  ~(div rs rnd)
          %mod  |=([a=@rs b=@rs] (~(sub rs rnd) a (~(mul rs rnd) b (~(san rs rnd) (need (~(toi rs rnd) (~(div rs rnd) a b)))))))
          %gth  |=([a=@rs b=@rs] ?:((~(gth rs rnd) a b) .1 .0))
          %gte  |=([a=@rs b=@rs] ?:((~(gte rs rnd) a b) .1 .0))
          %lth  |=([a=@rs b=@rs] ?:((~(lth rs rnd) a b) .1 .0))
          %lte  |=([a=@rs b=@rs] ?:((~(lte rs rnd) a b) .1 .0))
          %equ  |=([a=@rs b=@rs] ?:(.=(a b) .1 .0))
          %neq  |=([a=@rs b=@rs] ?:(.=(a b) .0 .1))
        ==
          %4
        ?+  fun  !!
          %add  ~(add rh rnd)
          %sub  ~(sub rh rnd)
          %mul  ~(mul rh rnd)
          %div  ~(div rh rnd)
          %mod  |=([a=@rh b=@rh] (~(sub rh rnd) a (~(mul rh rnd) b (~(san rh rnd) (need (~(toi rh rnd) (~(div rh rnd) a b)))))))
          %gth  |=([a=@rh b=@rh] ?:((~(gth rh rnd) a b) .~~1 .~~0))
          %gte  |=([a=@rh b=@rh] ?:((~(gte rh rnd) a b) .~~1 .~~0))
          %lth  |=([a=@rh b=@rh] ?:((~(lth rh rnd) a b) .~~1 .~~0))
          %lte  |=([a=@rh b=@rh] ?:((~(lte rh rnd) a b) .~~1 .~~0))
          %equ  |=([a=@rh b=@rh] ?:(.=(a b) .~~1 .~~0))
          %neq  |=([a=@rh b=@rh] ?:(.=(a b) .~~0 .~~1))
        ==
      ==  :: bloq real
    ==  :: kind
  ::
  ++  trans-scalar
    |=  [=bloq =kind fun=ops]
    ^-  $-(@ @)
    ?-    kind
        %uint  
      ?+  fun  !!
        %abs  |=(b=@ b)
      ==
      ::
        %int2  !!
      ::
        %i754
      ?+    bloq  !!
          %7
        ?+  fun  !!
          %abs  |=(b=@ ?:((~(gte rq rnd) b .~~~0) b (~(mul rq rnd) b .~~~-1)))
        ==
          %6
        ?+  fun  !!
          %abs  |=(b=@ ?:((~(gte rd rnd) b .~0) b (~(mul rd rnd) b .~-1)))
        ==
          %5
        ?+  fun  !!
          %abs  |=(b=@ ?:((~(gte rs rnd) b .0) b (~(mul rs rnd) b .-1)))
        ==
          %4
        ?+  fun  !!
          %abs  |=(b=@ ?:((~(gte rh rnd) b .~~0) b (~(mul rh rnd) b .~~-1)))
        ==
      ==
    ==
  ::
  ++  el-wise-op
    |=  [a=ray fun=$-(@ @)]
    ^-  ray
    ?>  (check a)
    %-  spac
    :-  meta.a
    =/  ali  (flop (ravel a))  :: compensate for LSB
    %+  rep  bloq.meta.a
    %+  turn
      ali
    |=(e=@ (fun e))
 :: 
  ++  bin-op
    |=  [a=ray b=ray op=$-([@ @] @)]
    ^-  ray
    ?>  =(meta.a meta.b)
    ?>  (check a)
    ?>  (check b)
    %-  spac
    :-  meta.a
    =/  ali  (ravel a)
    =/  bob  (ravel b)
    %+  rep  bloq.meta.a
    %+  turn
      (gulf 0 (dec (lent ali)))
    |=  i=@
    (op (snag i ali) (snag i bob))
  ::
  ++  ter-op
    |=  [a=ray b=ray c=ray op=$-([@ @ @] @)]
    ^-  ray
    ?>  =(meta.a meta.b)
    ?>  =(meta.c meta.b)
    ?>  (check a)
    ?>  (check b)
    ?>  (check c)
    %-  spac:la
    :-  meta.a
    =/  ali  (ravel:la a)
    =/  bob  (ravel:la b)
    =/  car  (ravel:la c)
    %+  rep  bloq.meta.a
    %+  turn
      (gulf 0 (dec (lent ali)))
    |=  i=@
    (op (snag i ali) (snag i bob) (snag i car))
  --
--
