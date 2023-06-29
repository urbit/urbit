:: specify starting input entropy and the number of different samples
:: the check should try.
::
|_  [eny=@uv runs=@ud]
++  norn                             :: gate for creating inputs
  |$  [sam]
  $-([@ud _og] sam)
++  fin                              ::  successful ending
  |=  [runs=@ drop=@]
  ~&  [success-runs+runs drops+drop]
  %.y
++  run
  |=  [vax=vase sam=vase]
  =/  tres=toon  (mong [q.vax q.sam] |=(^ ~))
  ?-  tres
    [%0 *]  p.tres            :: executed successfully
    [%1 *]  ~&  %why-scry  |  :: fate tried scrying
    :: fate crashed. print trace and report failure.
    ::
    [%2 *]  ~&  err+(turn p.tres |=(tank ~(ram re +6)))  |
  ==
++  check                            :: main test runner
  |*  [vax=vase norn=(unit (norn)) alts=(unit $-(vase (list vase)))]
  =?  runs  =(0 runs)  100   :: default to 100 runs
  =+  run-i=0                :: counter of which run we are on
  =+  size=1                 :: this will only grow
  =+  sax=(slot 6 vax)       :: vase of the sample of the fate
  =+  rng=~(. og eny)
  =+  drop=0                 :: counter of how many times we dropped
  =+  tried=*(set noun)      :: all samples we have tried
  =+  tries=0                :: how many times we have had sample collisions
  ::
  :: main loop
  ::
  |-  ^-  ?
  ?:  =(run-i runs)
    (fin runs drop)
  =+  [fill-rng next-rng]=(split-rng rng)
  =/  sam=vase
    ?~  norn
      :: in case no norn is given, we will use the default (fill).
      :: fill is not a norn, because it acts on a vase and changes its value.
      ::
      (~(fill quiz [size fill-rng]) sax)
    :: in case a norn is given, use that.
    ::
    [p=p.sax q=(u.norn size fill-rng)]
  :: arbitrarily chosen growth pace.
  ::
  =+  new-size=(add +(size) (div (mul size 2) 21))
  :: if we have tried this sample before, count the collision, keep going.
  :: unless there are too many collisions, then we give up (%tired)
  ::
  ?:  (~(has in tried) q.sam)
    =.  tries  +(tries)
    ?.  =(tries 1.000)
      $(rng next-rng, size new-size)
    ~&  %tired
    (fin run-i drop)
  =.  tried  (~(put in tried) sam)
  :: slam the fate with the random sample.
  :: virtualized slam to catch and report crashes.
  ::
  =+  res=(run vax sam)
  =?  drop  =(res %drop)  +(drop)
  ?:  |(=(res %drop) =(res %.y))
    $(run-i +(run-i), rng next-rng, size new-size)
  ?>  =(res %.n)
  =+  sink=?~(alts sink u.alts)
  =+  sunk=(sink sam)
  =/  simp=vase
    |-
    ?~  sunk
      sam
    =+  res=(run vax i.sunk)
    ?:  |(=(res %.y) =(res %drop))
      $(sunk t.sunk)
    =.  sam  i.sunk
    $(sunk (sink i.sunk))
  ~&  [defy-with-sam+(noah simp) drops+drop]
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
      %noun            !>  (noun:norns size new-rng)
      [%atom p=* q=~]  =+  new=((atom:norns @) size new-rng)
                       sax(q new)
      [%atom *]        sax(q (need q.p.sax))
      :: TODO: use cell:norns
      ::
      [%cell *]        =+  [rng-1 rng-2]=(split-rng rng)
                       %+  slop  (fill(rng rng-1) (slot 2 sax))
                                 (fill(rng rng-2) (slot 3 sax))
      [%face *]        sax(q q:(fill [p=q.p.sax q=q.sax]))
      [%fork *]        =+  ts=~(tap in p.p.sax)
                       =^  ran  new-rng  (rads:new-rng (lent ts))
                       =+  new-type=(snag ran ts)
                       :: note: by assigning a specific type, we may create an
                       :: evil vase.
                       ::
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
++  norns
  |%
  ::
  :: value norns
  ::
  ++  atom
    :: TODO: differentiate by aura. some values won't be valid depending on aura.
    ::
    |*  [a=mold]
    ^-  (norn a)
    |=  [size=@ud rng=_og]
    (rad:rng size)
  ++  noun
    ^-  (norn *)
    |=  [size=@ud rng=_og]
    =+  start-size=size
    |-
    ^-  ^noun
    ?:  (lte size 1)
      :: leafs should be able to make large atoms.
      ::
      (rad:rng start-size)
    =^  ran  rng  (rads:rng 3)
    ?:  =(0 ran)                        :: 1/3 chance for a leaf.
      (rad:rng start-size)
    ?:  =(1 ran)                        :: 1/3 chance for a identical subtrees.
      =+  subtree=$(size (div size 2))
      [subtree subtree]
    =+  [rng-1 rng-2]=(split-rng rng)   :: 1/3 chance for different subtrees.
    :-  $(size (div size 2), rng rng-1)
    $(size (div size 2), rng rng-2)
  ::
  :: combinators
  ::
  ++  const
    |*  a=mold
    |=  x=a
    ^-  (norn a)
    |=  [=@ud =_og]
    x
  ++  cell
    |*  [a=mold b=mold]
    |=  [givp=(norn a) givq=(norn b)]
    ^-  (norn $:(a b))
    |=  [size=@ud rng=_og]
    =+  [rng-1 rng-2]=(split-rng rng)
    [(givp size rng-1) (givq size rng-2)]
  ++  freq
    |*  a=mold
    |=  b=(^list (pair @ (norn a)))
    ^-  (norn a)
    =+  tot=(roll (turn b head) add)
    |=  [size=@ud rng=_og]
    =^  ran  rng  (rads:rng tot)
    |-
    ?~  b  ~|  %impossible-bug-in-quiz-please-report  !!
    =/  [f=@ p=(norn a)]  i.b
    ?:  (lth ran f)
      (p size rng)
    $(b t.b, ran (sub ran f))
  ++  pick
    |*  a=mold
    |=  b=(^list (norn a))
    ^-  (norn a)
    =+  c=(turn b |=(b=(norn a) [1 b]))
    ((freq a) c)
  ++  list
    |*  a=mold
    |=  norn=(norn a)
    ^-  (^norn (^list a))
    |=  [size=@ud rng=_og]
    =+  stop=size
    |-
    ^-  (^list a)
    =^  ran  rng  (rads:rng stop)
    ?:  =(ran 0)
      ~
    =^  ran  rng  (rads:rng size)
    =+  head=(norn size rng)
    =^  ran  rng  (rads:rng size)
    =+  tail=$(stop (div stop 2), rng rng)
    [i=head t=tail]
  --
--