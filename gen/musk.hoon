|%
::
++  musk                                                ::  nock with block set
  =>  |%
      ++  block  
        ::  identity of resource awaited
        ::
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
        (pair stencil noun)
      ::
      ++  stencil  
        ::  noun knowledge map
        ::
        %+  each 
          ::  yes; noun is either fully complete, or fully blocked
          ::
          (set block) 
        ::  no; noun has partial block substructure
        ::
        (pair stencil stencil)
      ::
      ++  output
        ::  optional partial result
        ::
        %-  unit
        ::  complete noun or block set
        ::
        (each noun (set block))
      -- 
  ::  execute nock on partial subject
  ::
  |=  $:  ::  sub: subject, a partial noun
          ::  fol: formula, a complete noun
          ::
          sub/seminoun
          fol/noun
      ==
  ^-  output
  =<  apex
  |%  
  ++  abet
    ::  simplify raw result
    ::
    |=  $:  ::  noy: raw result
            ::
            noy/result
        ==
    ^-  output
    ::  interpreter stopped
    ::
    ?~  noy  ~
    ::  simplify internal result
    ::
    [~ =+((squash p.noy) ?~(- [%& q.noy] [%| ~(tap in -)]))]
  ::
  ++  apex
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
      ?:  ?=(~ -.hed)  ~
      ::  tal: interpret tail
      ::
      =+  tal=$(fol +.fol)
      ::  propagate stop
      ::
      ?:  ?=(~ -.tal)  ~
      ::  combine 
      ::
      `(combine +.hed +.tal)
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
      (fragment b.fol)
    ::
    ::  1; constant
    ::
        {$1 b/*}
      ::  constant is complete
      ::
      `[[%& ~] b.fol]
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
      =+  lub=$(fol b.fol)
      ::  propagate stop
      ::
      ?~  lub  ~
      ::  recurse
      ::
      $(fol ryf, sub lub)
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
      `[[%& ~] .?(fig)]
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
      ?^(fig ~ `[[%& ~] +(fig))
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
      ?@(fig ~ `[[%& ~] =(-.fig +.fig))
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
      $(fol =>(fol [7 [[0 1] b] c]))
    ::
    ::  9; invocation
    ::
        {$9 b/* c/*}       
      ::  use standard macro expansion (slow)
      ::
      $(fol =>(fol [7 c 0 b]))
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
    --
  ::  require complete result
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
    ?:  ?=($| -.p.noy)  `[[%& (squash p.noy)] ~]
    ::  if full block, propagate block
    ::
    ?:  ?=(^ p.p.noy)  `[[%& p.p.noy] ~]
    ::  otherwise use complete noun
    ::
    (yen q.noy)
  ::
  ++  squash
    ::  convert stencil to block set
    ::
    |=  tyn/stencil
    ^-  (set block)
    ?-  -.tyn
      $&  p.tyn
      $|  (~(uni in $(tyn p.tyn)) $(tyn q.tyn))
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
    ?:  &(?=($& p.hed) ?=($& p.tal))
      ::  yin: merged block set
      ::
      =/  yin  (~(uni in p.p.hed) p.p.tal
      :-  [%& yin]
      ::  don't accumulate stubs
      ::
      ?~(yin ~ [q.hed q.tal])
    ::  partial cell
    ::
    [[%| p.hed p.tal] [q.hed q.tal]]
  ::
  ++  fragment
    ::  seek to an axis in a seminoun
    ::
    |=  $:  ::  axe: tree address of subtree
            ::
            axe/axis
        ==
    ^-  result
    ::  1 is the root
    ::
    ?:  =(1 axe)  sub
    ::  now: 2 or 3, top of axis
    ::  lat: rest of axis
    ::
    =+  [now=(cap axe) lat=(mas axe)]
    ?-  -.p.sub
    ::  subject is fully blocked or complete
    ::
      $&  ::  if fully blocked, produce self
          ::
          ?^  p.p.sub  sub
          ::  descending into atom, stop
          ::
          ?@  q.sub  ~
          ::  descend into complete cell
          ::
          $(axe lat, sub [[%& ~] ?:(=(2 now) -.q.sub +.q.sub))
    ::  subject is partly blocked
    ::
      $|  ::  descend into partial cell
          ::
          $(axe lat, sub ?:(=(2 now) [p.p.sub -.q.sub] [q.p.sub +.q.sub]))
    ==
  --
