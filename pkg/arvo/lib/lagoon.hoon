::                                                    ::
::::                    ++la                          ::  (2v) vector/matrix ops
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
  +$  baum  ::   $baum:  n-dimensional array as a nested list
    $@  @        ::  single item
    (lest baum)  ::  nonempty list of children, in row-major order
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
    (cut bloq.meta.ray [(get-bloq-offset -.ray dex) 1] data.ray)
  ::
  ++  set-item  ::  set item at index .dex to .val
    |=  [=ray dex=(list @) val=@]
    ^+  ray
    :-  -.ray
    (sew bloq.meta.ray [(get-bloq-offset -.ray dex) 1 val] data.ray)
  ::
  ++  get-bloq-offset  ::  get bloq offset of n-dimensional index
    |=  [=meta dex=(list @)]
    ^-  @
    +((get-item-number shape.meta dex))
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
      cof  (mul cof i.sap)
      ret  (^add ret (mul i.dex cof))
    ==
  ::
  ++  get-item-index
    |=  [shape=(list @) num=@]
    ^-  (list @)
    =/  len  (roll shape mul)  :: TODO will shadow
    ~
  ::
  ++  ravel
    |=  a=ray
    ^-  (list @)
    =/  len  (lent shape.meta.a)
    %+  turn  (gulf 0 (dec len))
    |=  i=@
    (get-item a (get-item-index shape.meta.a i))
  ::
  ++  fill
    |=  [=meta x=@]  ^-  ray
    =/  len  (roll shape.meta mul)
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
  ::    Zeroes
  ++  zeros
    |=  =meta  ^-  ray
    ~_  leaf+"lagoon-fail"
    :-  meta
        (lsh [bloq.meta (roll shape.meta mul)] 1)
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
  ::  Operators
  ::
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
    =/  sum  ((add-scalar [bloq.meta aura.meta]:a) i.ali i.bob)
    $(ali t.ali, bob t.bob, res [sum res])
  ::
  ++  add-scalar
    |=  [=bloq aura=@tas]
    ^-  $-([@ @] @)
    ?+    aura  ~|(aura !!)
        ?(%u %ub %ux %ud %uv %uw)  ~(sum fe bloq)
        ::?(%s %sb %sx %sd %sv %sw)  sum:si
        %r
      ?+  bloq  !!
        %7  ~(add rq %n)
        %6  ~(add rd %n)
        %5  ~(add rs %n)
        %4  ~(add rh %n)
      ==
    ::
        ::  TODO signed integers -- add new 2's complement aura?
    ==
  --
