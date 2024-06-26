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
  =?  drop  =(res %drop)  +(drop)    :: count number of %drop results.
  ?:  |(=(res %drop) =(res %.y))     :: %drop and %.y means we try again.
    $(run-i +(run-i), rng next-rng, size new-size)
  ?>  =(res %.n)
  :: fate either crashed or returned %.n.
  :: attempt shrinking.
  ::
  =+  sink=?~(alts sink u.alts)  :: user-supplied alts or default
  =+  sunk=(sink sam)
  =/  simp=vase
    |-
    :: in the loop we
    :: 1. try each alternative result.
    :: 2. if it doesn't fail, keep going.
    :: 3. if it fails, take it as the new smallest fail case and recurse.
    ::
    ?~  sunk
      sam
    =+  res=(run vax i.sunk)
    ?:  |(=(res %.y) =(res %drop))
      $(sunk t.sunk)
    =.  sam  i.sunk
    $(sunk (sink i.sunk))
  :: report the smallest sample we found.
  ::
  ~&  [defy-with-sam+(noah simp) drops+drop]
  %.n
++  sink
  :: automatically try to shrink the sample.
  ::
  |=  sax=vase
  ^-  (list vase)
  ?+  p.sax  ~
    %noun            ?^  q.sax
                     :: if the noun is a cell, try both subtrees separately.
                     ::
                       ~[(slot 2 sax) (slot 3 sax)]
                     ?:  =(q.sax 0)
                       ~
                     :: if the noun is an atom (not 0), try dividing it by 2
                     :: and decrementing it.
                     ::
                     ~[sax(q (div q.sax 2)) sax(q (dec q.sax))]
    [%atom p=* q=~]  ?>  ?=(@ q.sax)
                     ?:  =(q.sax 0)
                       ~
                     :: if the noun is an atom (not 0), try dividing it by 2
                     :: and decrementing it.
                     ::
                     ~[sax(q (div q.sax 2)) sax(q (dec q.sax))]
    [%atom *]        :: if it is a singleton type atom, no shrinking can be done
                     ::
                     ~
    [%cell p=* q=*]  :: if it is a cell, take all the alts of each element
                     :: try all combinations.
                     ::
                     =+  p=(slot 2 sax)
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
    [%face p=* q=*]  :: strip the face, shrink, put the results back.
                     %+  turn
                       (sink [q.p.sax q.sax])
                     |=  q=vase
                     sax(q q.q)
    [%hint p=* q=*]  :: strip the face, shrink, put the results back.
                     %+  turn
                       (sink [q.p.sax q.sax])
                     |=  q=vase
                     sax(q q.q)
  ==
++  quiz
  :: this core is for construction random samples when no norn was given.
  :: it decunstructs the type in the vase and uses simple heuristics to come up
  :: with a sample.
  ::
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
  :: take an og core, return two new ones.
  :: each has 128 bits entropy (assuming the original og is truly)
  :: pseudorandom.
  ::
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
    :: create a random, sized atom.
    :: TODO: differentiate by aura. some values won't be valid depending on aura.
    ::
    |*  [a=mold]
    ^-  (norn a)
    |=  [size=@ud rng=_og]
    (rad:rng size)
  ++  noun
    :: create a random, sized noun.
    ::
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
  ++  const
    :: always return the same constant.
    ::
    |*  a=mold
    |=  x=a
    ^-  (norn a)
    |=  [=@ud =_og]
    x
  ::
  :: combinators
  ::
  ++  cell
    :: create a cell norn from two norns, one for each element.
    ::
    |*  [a=mold b=mold]
    |=  [givp=(norn a) givq=(norn b)]
    ^-  (norn $:(a b))
    |=  [size=@ud rng=_og]
    =+  [rng-1 rng-2]=(split-rng rng)
    [(givp size rng-1) (givq size rng-2)]
  ++  freq
    :: supply a list of pairs of atoms and norns.
    :: take the atom to represent frequency.
    :: pick each norn according to its frequency.
    :: for example, given ~[[1 norn-a] [2 norn-b] [1 norn-c]]
    :: there is a 50% chance of picking norn-b, and 25% each chance
    :: to pick norn-a or norn-c.
    ::
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
    :: like the freq norn, but each norn has equal chance of being picked.
    ::
    |*  a=mold
    |=  b=(^list (norn a))
    ^-  (norn a)
    =+  c=(turn b |=(b=(norn a) [1 b]))
    ((freq a) c)
  ++  list
    :: creates a norn for a list by taking a norn as input.
    :: the input norn is used to create each element of the list.
    :: the list norn combinator tries many different list sizes.
    ::
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