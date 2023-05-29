::  %hugo
!:
!?  164
::
=,  hugo
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    ::
    +$  hugo-state
      $:  %0
          files=(trie octs)  ::  files
          ducts=(set duct)   ::  listeners
      ==
    --
=>  :: ~%  %hugo  ..part  ~
    |%
    ++  of                                             ::  trie engine, see +axal
      =|  fat=(trie)
      |@
      ++  del
        |=  pax=path
        ^+  fat
        ?~  pax  [~ dir.fat]
        =/  kid  (~(get by dir.fat) i.pax)
        ?~  kid  fat
        fat(dir (~(put by dir.fat) i.pax $(fat u.kid, pax t.pax)))
      ::  Descend to the trie at this path
      ::
      ++  dip
        |=  pax=path
        ^+  fat
        ?~  pax  fat
        =/  kid  (~(get by dir.fat) i.pax)
        ?~  kid  [~ ~]
        $(fat u.kid, pax t.pax)
      ::
      ++  gas
        |=  lit=(list (pair path _?>(?=(^ fil.fat) u.fil.fat)))
        ^+  fat
        ?~  lit  fat
        $(fat (put p.i.lit q.i.lit), lit t.lit)
      ::
      ++  get
        |=  pax=path
        fil:(dip pax)
      ::  Fetch file at longest existing prefix of the path
      ::
      ++  fit
        |=  pax=path
        ^+  [pax fil.fat]
        ?~  pax  [~ fil.fat]
        =/  kid  (~(get by dir.fat) i.pax)
        ?~  kid  [pax fil.fat]
        =/  low  $(fat u.kid, pax t.pax)
        ?~  +.low
          [pax fil.fat]
        low
      ::
      ++  has
        |=  pax=path
        !=(~ (get pax))
      ::  Delete subtree
      ::
      ++  lop
        |=  pax=path
        ^+  fat
        ?~  pax  fat
        |-
        ?~  t.pax  fat(dir (~(del by dir.fat) i.pax))
        =/  kid  (~(get by dir.fat) i.pax)
        ?~  kid  fat
        fat(dir (~(put by dir.fat) i.pax $(fat u.kid, pax t.pax)))
      ::
      ++  put
        |*  [pax=path dat=*]
        =>  .(dat `_?>(?=(^ fil.fat) u.fil.fat)`dat, pax `path`pax)
        |-  ^+  fat
        ?~  pax  fat(fil `dat)
        =/  kid  (~(gut by dir.fat) i.pax ^+(fat [~ ~]))
        fat(dir (~(put by dir.fat) i.pax $(fat kid, pax t.pax)))
      ::
      ++  tap
        =|  pax=path
        =|  out=(list (pair path _?>(?=(^ fil.fat) u.fil.fat)))
        |-  ^+   out
        =?  out  ?=(^ fil.fat)  :_(out [pax u.fil.fat])
        =/  dir  ~(tap by dir.fat)
        |-  ^+   out
        ?~  dir  out
        %=  $
          dir  t.dir
          out  ^$(pax (weld pax /[p.i.dir]), fat q.i.dir)
        ==
      ::  Serialize to map
      ::
      ++  tar
        (~(gas by *(map path _?>(?=(^ fil.fat) u.fil.fat))) tap)
      --
    --
=|  hugo-state
=*  state  -
|=  [now=@da eny=@uvJ rof=roof]
=*  hugo-gate  .
^?
|%
::  +call: handle a +task:hugo request
::
++  call
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task)
      ==
  ^-  [(list move) _hugo-gate]
  =/  =task  ((harden task) wrapped-task)
  =^  moves  state
    ?-  -.task 
      %fill  `state(files tie.task) :: TODO listeners
    == 
  [moves hugo-gate]
::  +load: migrate an old state to a new hugo version
::
++  load
  |=  old=hugo-state
  ^+  hugo-gate
  hugo-gate(state old)
::  +scry: view hugo state
::
++  scry
  ^-  roon
  |=  [lyc=gang car=term bem=beam]
  ^-  (unit (unit cage))
  ::
  ::  only respond for the local identity, %$ desk, current timestamp
  ::
  ?.  ?&  =(our p.bem)
          =([%$ %da now] r.bem)
          =(%$ q.bem)
      ==
    ~
  ::  /bx scries
  ?.  ?=(%x car)  ~
  ?+  s.bem  [~ ~]
    [%test ~]  ``noun+!>(~)
  ==
::
++  stay  state
++  take
  |=  [=wire =duct dud=(unit goof) sign=*]
  ^-  [(list move) _hugo-gate]
  ?^  dud
    ~|(%hugo-take-dud (mean tang.u.dud))
  ::
  :: ?>  ?=([%drip @ ~] wire)
  :: =/  event-core  (per-event [now duct] state)
  :: =^  moves  state
  ::   abet:(take-drip:event-core (slav %ud i.t.wire) error.sign)
  [~ hugo-gate]
--
