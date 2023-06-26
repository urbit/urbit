|_  [eny=@uv runs=@ud]
++  give
  |$  [sam]
  $-([@ud _og] sam)
++  fin
  |=  [runs=@ drop=@]
  ~&  [success-runs+runs drops+drop]
  %.y
++  check
  |*  [vax=vase give=(unit (give)) alts=(unit $-(vase (list vase)))]
  =?  runs  =(0 runs)  100
  =+  size=1
  =+  sax=(slot 6 vax)
  =+  run-i=0
  =+  rng=~(. og eny)
  =+  drop=0
  =+  tried=*(set noun)
  =+  tries=0
  |-  ^-  ?
  ?:  =(run-i runs)
    (fin runs drop)
  =+  [fill-rng next-rng]=(split-rng rng)
  =/  sam=vase
    ?~  give
      (~(fill quiz [size fill-rng]) sax)
    [p=p.sax q=(u.give size fill-rng)]
  =+  new-size=(add +(size) (div (mul size 2) 21))
  ?:  (~(has in tried) q.sam)
    =.  tries  +(tries)
    ?.  =(tries 1.000)
      $(rng next-rng, size new-size)
    ~&  %tired
    (fin run-i drop)
  =.  tried  (~(put in tried) sam)
  =+  res=(slum q.vax q.sam)
  =?  drop  =(res %drop)  +(drop)
  ?:  |(=(res %drop) =(res %.y))
    :: Arbitrarily chosen growth pace.
    $(run-i +(run-i), rng next-rng, size new-size)
  ?>  =(res %.n)
  =+  sink=?~(alts sink u.alts)
  =+  sunk=(sink sam)
  =/  simp=vase
    |-
    ?~  sunk
      sam
    =+  res=(slam vax i.sunk)
    ?:  |(=(+:res %.y) =(+:res %drop))
      $(sunk t.sunk)
    =.  sam  i.sunk
    $(sunk (sink i.sunk))
  ~&  [fail-with-sam+(noah simp) drops+drop]
  %.n
++  sink
  |=  sax=vase
  ^-  (list vase)
  ?+  p.sax  ~
    %noun            ?^  q.sax
                       ~[(slot 2 sax) (slot 3 sax)]
                     ?:  =(q.sax 0)
                       ~
                     ~[sax(q (div q.sax 2)) sax(q (dec q.sax))]
    [%atom p=* q=~]  ?>  ?=(@ q.sax)
                     ?:  =(q.sax 0)
                       ~
                     ~[sax(q (div q.sax 2)) sax(q (dec q.sax))]
    [%atom *]        ~
    [%cell p=* q=*]  =+  p=(slot 2 sax)
                     =+  q=(slot 3 sax)
                     =+  ps=(sink p)
                     =+  qs=(sink q)
                     ?~  qs
                       (turn ps |=(p=vase (slop p q)))
                     |-
                     ^-  (list vase)
                     ?~  ps
                       (turn qs |=(q=vase (slop p q)))
                     =+  cs=(turn qs |=(q=vase (slop i.ps q)))
                     (weld cs $(ps t.ps))
    [%face p=* q=*]  %+  turn
                       (sink [q.p.sax q.sax])
                     |=  q=vase
                     sax(q q.q)
    [%hint p=* q=*]  %+  turn
                       (sink [q.p.sax q.sax])
                     |=  q=vase
                     sax(q q.q)
  ==
++  quiz
  |_  [size=@ud rng=_og]
  ++  fill
    |=  sax=vase
    ^+  sax
    =+  new-rng=+:(rads:rng 1)
    ?+  p.sax  ~&(warn-unfill-sam+`type`p.sax sax)
      %noun            !>  (noun:givers size new-rng)
      [%atom p=* q=~]  !>  ((atom:givers @) size new-rng)
      [%atom *]        sax(q (need q.p.sax))
      :: TODO use cell:givers
      [%cell *]        =+  [rng-1 rng-2]=(split-rng rng)
                       %+  slop  (fill(rng rng-1) (slot 2 sax))
                                 (fill(rng rng-2) (slot 3 sax))
      [%face *]        sax(q q:(fill [p=q.p.sax q=q.sax]))
      [%fork *]        =+  ts=~(tap in p.p.sax)
                       =^  ran  new-rng  (rads:new-rng (lent ts))
                       =+  new-type=(snag ran ts)
                       :: Note: by assigning a specific type, we may create an evil vase.
                       (fill(rng new-rng) sax(p new-type, q q.sax))
      [%hint *]        sax(q q:(fill [p=q.p.sax q=q.sax]))
      [%hold *]        =+  nex=(~(mint ut p.p.sax) %noun q.p.sax)
                       (fill nex)
    ==
  --
++  split-rng
  |=  [rng=_og]
  ^+  [og og]
  =+  bit-size=256
  =^  bits-1  rng  (raws:rng bit-size)
  [~(. og bits-1) ~(. og (raw:rng bit-size))]
++  givers
  |%
  :: value givers
  ++  atom
    |*  [a=mold]
    ^-  (give a)
    |=  [size=@ud rng=_og]
    (rad:rng size)
  ++  noun
    ^-  (give *)
    |=  [size=@ud rng=_og]
    =+  start-size=size
    |-
    ^-  ^noun
    ?:  (lte size 1)
      (rad:rng start-size)  :: leafs should be able to make large atoms.
    =^  ran  rng  (rads:rng 3)
    ?:  =(0 ran) :: 1/3 chance for a leaf.
      (rad:rng start-size)
    ?:  =(1 ran)  :: 1/3 chance for a identical subtrees.
      =+  subtree=$(size (div size 2))
      [subtree subtree]
    =+  [rng-1 rng-2]=(split-rng rng)  :: 1/3 chance for different subtrees.
    :-  $(size (div size 2), rng rng-1)
    $(size (div size 2), rng rng-2)
  :: combinators
  ++  const
    |*  a=mold
    |=  x=a
    ^-  (give a)
    |=  [=@ud =_og]
    x
  ++  cell
    |*  [a=mold b=mold]
    |=  [givp=(give a) givq=(give b)]
    ^-  (give $:(a b))
    |=  [size=@ud rng=_og]
    =+  [rng-1 rng-2]=(split-rng rng)
    [(givp size rng-1) (givq size rng-2)]
  ++  freq
    |*  a=mold
    |=  b=(^list (pair @ (give a)))
    ^-  (give a)
    =+  tot=(roll (turn b head) add)
    |=  [size=@ud rng=_og]
    =^  ran  rng  (rads:rng tot)
    |-
    ?~  b  !!  :: impossible
    =/  [f=@ p=(give a)]  i.b
    ?:  (lth ran f)
      (p size rng)
    $(b t.b, ran (sub ran f))
  ++  pick
    |*  a=mold
    |=  b=(^list (give a))
    ^-  (give a)
    =+  c=(turn b |=(b=(give a) [1 b]))
    ((freq a) c)
  ++  list
    |*  a=mold
    |=  give=(give a)
    ^-  (^give (^list a))
    |=  [size=@ud rng=_og]
    =+  stop=size
    |-
    ^-  (^list a)
    =^  ran  rng  (rads:rng stop)
    ?:  =(ran 0)
      ~
    =^  ran  rng  (rads:rng size)
    =+  head=(give size rng)
    =^  ran  rng  (rads:rng size)
    =+  tail=$(stop (div stop 2), rng rng)
    [i=head t=tail]
  --
--