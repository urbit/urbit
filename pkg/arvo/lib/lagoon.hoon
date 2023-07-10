::                                                    ::
::::                    ++la                          ::  (2v) vector/matrix ops
|%
++  la
  ^|
  !:
  |_  r=$?(%n %u %d %z)   :: round nearest, round up, round down, round to zero
  ::
  ::  Metadata
  ::
  +$  ray  ::     $ray:  n-dimensional array
    $:  =meta     ::  descriptor
        data=@ux  ::  data, row-major order
    ==
  ::
  +$  meta  ::          $meta:  metadata for a $ray
    $:  shape=(list @)  ::  list of dimension lengths
        =bloq           ::  logarithm of bitwidth
        aura=@tas       ::  name of data type
    ==
  ::
  +$  baum  ::          $baum:  ndray with metadata
    $:  =meta  
        data=ndray
    ==
  ::
  +$  ndray  ::        $ndray:  n-dimensional array as a nested list
      $@  @        ::  single item
      (lest ndray)  ::  nonempty list of children, in row-major order
  ::
  ::  Utilities
  ::
  ++  print
    |=  a=ray
    ~>  %slog.1^(to-tank a)
    ~
  ++  slog  |=(a=ray (^slog (to-tank a) ~))
  ++  to-tank  ::  TODO nest dimensions
    |=  a=ray
    ^-  tank
    :+  %rose  [" " "[" "]"]
    %+  turn  (ravel a)
    |=  i=@
    ^-  tank
    (sell [%atom aura.meta.a ~] i)
  ::
  ++  get-item  ::  extract item at index .dex
    |=  [=ray dex=(list @)]
    ^-  @ux
    =/  len  (^sub (roll shape.meta.ray ^mul) 1)
    %^    cut
        bloq.meta.ray 
      [(^sub len (get-bloq-offset -.ray dex)) 1] 
    data.ray
  ::
  ++  set-item  ::  set item at index .dex to .val
    |=  [=ray dex=(list @) val=@]
    ^+  ray
    =/  len  (^sub (roll shape.meta.ray ^mul) 1)
    :-  -.ray
    %^    sew 
        bloq.meta.ray
      [(^sub len (get-bloq-offset -.ray dex)) 1 val] 
    data.ray
  ::
  ++  get-bloq-offset  ::  get bloq offset of n-dimensional index
    |=  [=meta dex=(list @)]
    ^-  @
    (get-item-number shape.meta dex)
  ::
  ++  get-item-number  ::  convert n-dimensional index to scalar index
    |=  [shape=(list @) dex=(list @)]
    ^-  @
    =.  dex  (flop dex)
    =/  sap  (flop shape)
    =/  cof  1
    =/  ret  0
    |-  ^+  ret
    ?~  sap  ret
    ?~  dex  !!
    ?>  (lth i.dex i.sap)
    %=  $
      sap  t.sap
      dex  t.dex
      cof  (^mul cof i.sap)
      ret  (^add ret (^mul i.dex cof))
    ==
  ::
  ::
  ++  get-dim  :: convert scalar index to n-dimensional index
    |=  [shape=(list @) ind=@]
    =/  sap  (flop shape)
    =/  i=@  0
    =|  dex=(list @)
    ^-  (list @)
    |-
    ?:  (gte i (lent sap))
      (flop dex)
    %=    $
      dex  `(list @)`(snoc dex (^mod ind (snag i sap)))
      ind  (^div ind (snag i sap))
      i    (^add i 1)
    ==
  ::
  ++  get-item-index
    |=  [shape=(list @) num=@]
    ^-  @
    =/  len  (roll shape ^mul)  :: TODO will shadow
    =-  (roll - ^add)
    ^-  (list @)
    %+  turn  shape
    |=  wid=@
    (^mod (^div len wid) num)
  ::
  ++  ravel
    |=  a=ray
    ^-  (list @)
    +:(flop (rip bloq.meta.a data.a))
  ::
  ++  en-ray    :: baum to ray
  |=  =baum
  ^-  ray
  =/  a=ray  [meta.baum `@ux`1]
  =/  i  0
  =/  n  (roll shape.meta.a ^mul)
  |-
  ?:  =(i n)
    a
  %=    $
      i  +(i)
      data.a
    %+  ^add 
      (get-item-baum baum (get-dim shape.meta.a i))
    %+  lsh 
      bloq.meta.a 
    data.a 
  ==
  ::
  ++  get-item-baum
  |=  [=baum dex=(list @)]
  ^-  @
  =/  a=ndray  data.baum
  =/  i  0
  |-
  ?@  a
    a
  %=    $
      i  +(i)
      a
    (snag (snag i dex) `(list ndray)`a)
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
  ::  Builders
  ::
  ::
  ++  eye      ::  produces identity matrix of shape nxn.
    |=  [=bloq =aura n=@]
    ^-  ray
    ~_  leaf+"lagoon-fail"
    =<  +
    %^    spin
        (gulf 0 (^sub n 1))
      ^-  ray  (zeros [~[n n] bloq aura])
    |=  [i=@ r=ray]
    [i (set-item r ~[i i] 1)]
 :: 
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
      ?+    aura.meta  ~|(aura.meta !!)
          ?(%u %ub %ux %ud %uv %uw)  `@`1
          ?(%s %sb %sx %sd %sv %sw)  `@`--1
          %r
        ?+  bloq.meta  !!
          %7  .~~~1
          %6  .~1
          %5  .1
          %4  .~~1
        ==
      ==
    (fill meta one)
  ::
  ++  iota
    |=  [=meta n=@ud]
    ^-  ray
    =.  shape.meta  ~[n]
    %-  spac
    :-  meta 
    (rap bloq.meta (gulf 1 n))
  ::
  ::  Operators
  ::
  ++  max
    |=  a=ray
    ^-  @ux
    (reel (ravel a) |:([a=1 b=(min a)] (^max a b)))
  ::
  ++  argmax :: Only returns first match
    |=  a=ray
    ^-  @ud
    +:(find ~[(max a)] (ravel a))
  ::
  ++  min
    |=  a=ray
    ^-  @ux
    (reel (ravel a) |:([a=1 b=(cumsum a)] (^min a b)))
  ::
  ++  argmin :: Only returns first match
    |=  a=ray
    ^-  @ud
    +:(find ~[(min a)] (ravel a))
  ::
  ++  cumsum
    |=  a=ray  
    ^-  @ux
    (reel (ravel a) |=([b=@ c=@] ((fun-scalar bloq.meta.a aura.meta.a %add) b c)))
  ::
  ++  prod
    |=  a=ray
    ^-  @ux
    =/  fun  (fun-scalar bloq.meta.a aura.meta.a %mul)
    =/  ali  +:(ravel a)
    =/  p  -:(ravel a)
    |-  ^+  p
    ?~  ali  p
    $(ali +.ali, p (fun p -.ali))
  ::
  ++  matmul-2d
  |=  [a=ray b=ray]
  =/  ar  (ravel a)
  =/  br  (ravel b)
  =/  i  0
  =/  j  0
  =/  k  0
  =/  shape=(list @)  ~[(snag 0 shape.meta.a) (snag 1 shape.meta.b)]
  =/  prod=ray  (zeros [shape bloq.meta.a aura.meta.a])
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
          %+  (fun-scalar bloq.meta:a aura.meta:a %add)
            cume 
          %+  (fun-scalar bloq.meta:a aura.meta:a %mul)
            (snag (get-bloq-offset meta.a `(list @)`~[i k]) ar)
          (snag (get-bloq-offset meta.b `(list @)`~[k j]) br)
        ==
      ==
    ==
  ++  add
    |=  [a=ray b=ray]
    ^-  ray
    ?>  =(meta.a meta.b)
    %-  spac
    :-  meta.a
    =/  ali  (ravel a)
    =/  bob  (ravel b)
    %+  rep  bloq.meta.a
    =|  res=(list @)
    %-  flop
    |-  ^+  res
    ?@  ali  res
    ?@  bob  res
    =/  sum  ((fun-scalar bloq.meta:a aura.meta:a %add) i.ali i.bob)
    $(ali t.ali, bob t.bob, res [sum res])
  ::
  ++  sub
    |=  [a=ray b=ray]
    ^-  ray
    ?>  =(meta.a meta.b)
    %-  spac
    :-  meta.a
    =/  ali  (ravel a)
    =/  bob  (ravel b)
    %+  rep  bloq.meta.a
    =|  res=(list @)
    %-  flop
    |-  ^+  res
    ?@  ali  res
    ?@  bob  res
    =/  dif  ((fun-scalar bloq.meta:a aura.meta:a %sub) i.ali i.bob)
    $(ali t.ali, bob t.bob, res [dif res])
  ::
  ++  mul
    |=  [a=ray b=ray]
    ^-  ray
    ?>  =(meta.a meta.b)
    %-  spac
    :-  meta.a
    =/  ali  (ravel a)
    =/  bob  (ravel b)
    %+  rep  bloq.meta.a
    =|  res=(list @)
    %-  flop
    |-  ^+  res
    ?@  ali  res
    ?@  bob  res
    =/  pro  ((fun-scalar bloq.meta:a aura.meta:a %mul) i.ali i.bob)
    $(ali t.ali, bob t.bob, res [pro res])
  ::
  ++  div
    |=  [a=ray b=ray]
    ^-  ray
    ?>  =(meta.a meta.b)
    %-  spac
    :-  meta.a
    =/  ali  (ravel a)
    =/  bob  (ravel b)
    %+  rep  bloq.meta.a
    =|  res=(list @)
    %-  flop
    |-  ^+  res
    ?@  ali  res
    ?@  bob  res
    =/  fra  ((fun-scalar bloq.meta:a aura.meta:a %div) i.ali i.bob)
    $(ali t.ali, bob t.bob, res [fra res])
  ::
  ++  mod
    |=  [a=ray b=ray]
    ^-  ray
    ?>  =(meta.a meta.b)
    %-  spac
    :-  meta.a
    =/  ali  (ravel a)
    =/  bob  (ravel b)
    %+  rep  bloq.meta.a
    =|  res=(list @)
    %-  flop
    |-  ^+  res
    ?@  ali  res
    ?@  bob  res
    =/  rem  ((fun-scalar bloq.meta:a aura.meta:a %mod) i.ali i.bob)
    $(ali t.ali, bob t.bob, res [rem res])
   ::
  +$  ops  ?(%add %sub %mul %div %mod)
  ::
  ++  fun-scalar
    |=  [=bloq aura=@tas fun=ops]
    ^-  $-([@ @] @)
    ?+    aura  ~|(aura !!)
        ?(%u %ub %ux %ud %uv %uw)  
        ?-  fun
          %add  ~(sum fe bloq)
          %sub  ~(dif fe bloq)
          %mul  |=([b=@ c=@] (~(sit fe bloq) (^mul b c)))
          %div  |=([b=@ c=@] (~(sit fe bloq) (^div b c)))
          %mod  |=([b=@ c=@] (~(sit fe bloq) (^mod b c)))
        ==
        ::?(%s %sb %sx %sd %sv %sw)  sum:si
        %r
      ?+  bloq  !!
        %7
        ?+  fun  !!
          %add  ~(add rq r)
          %sub  ~(sub rq r)
          %mul  ~(mul rq r)
          %div  ~(div rq r)
        ==
        %6
        ?+  fun  !!
          %add  ~(add rd r)
          %sub  ~(sub rd r)
          %mul  ~(mul rd r)
          %div  ~(div rd r)
        ==
        %5
        ?+  fun  !!
          %add  ~(add rs r)
          %sub  ~(sub rs r)
          %mul  ~(mul rs r)
          %div  ~(div rs r)
        ==
        %4
        ?+  fun  !!
          %add  ~(add rh r)
          %sub  ~(sub rh r)
          %mul  ~(mul rh r)
          %div  ~(div rh r)
        ==
      ==
    ::
        ::  TODO signed integers -- add new 2's complement aura?
    ==
  --
--
