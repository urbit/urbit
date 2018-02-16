::
::::
  ::
:-  %say
|=  {^ {{typ/type gen/hoon $~} $~}}
=<  :-  %noun
    =+  pro=(~(mint ut typ) %noun gen)
    ~_  (~(dunk ut typ) 'blow-subject')
    =+  bus=(bran:musk typ)
    ~&  [%subject-mask mask.bus]
    =+  jon=(apex:musk bus q.pro)
    ?~  jon
      ~&  %constant-stopped
      !!
    ?.  ?=($& -.u.jon)
      ~&  %constant-blocked
      !!
    ::  [p.pro [%1 p.u.jon]]
    p.u.jon
|%
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
  ++  bran
    |=  sut/type
    =+  gil=*(set type)
    |-  ^-  seminoun
    ?-    sut
      $noun      [&+[~ ~ ~] ~]
      $void      [&+[~ ~ ~] ~]
      {$atom *}  ?~(q.sut [&+[~ ~ ~] ~] [&+~ u.q.sut])
      {$cell *}  (combine $(sut p.sut) $(sut q.sut))
      {$core *}  %+  combine:musk 
                   ?~  p.s.q.sut  [&+[~ ~ ~] ~]
                   [&+~ p.s.q.sut]
                 $(sut p.sut) 
      {$face *}  $(sut ~(repo ut sut))
      {$fork *}  [&+[~ ~ ~] ~]
      {$help *}  $(sut ~(repo ut sut))
      {$hold *}  ?:  (~(has in gil) sut)
                   [&+[~ ~ ~] ~]
                 $(sut ~(repo ut sut), gil (~(put in gil) sut))
    ==
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
    ::  ~&  [%apex-fol fol]
    ::  ~&  [%apex-mac mask.bus]
    ::  =-  ~&  [%apex-pro-mac ?@(foo ~ ~!(foo mask.foo))]
    ::      foo
    ::  ^=  foo
    ::  ^-  result
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
    |-  ^-  result
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
--
