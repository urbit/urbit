/@  node
/@  task-diff
:-  [%node %task-diff]
|=  nod=node
^-  task-diff
=/  head  (@tas (crip (~(got by (malt a.g.nod)) %head)))
%-  task-diff
?+  head
  ~|  [%unknown-head head]
  !!
    %become
  =/  path-el  (snag 0 c.nod)
  ~&  path-el
  =/  path  (stab (crip (~(got by (malt a.g.path-el)) %value)))
  [head (pave:neo path)]
::
    %nest
  =/  name-el  (snag 0 c.nod)
  =/  name  (@tas (crip (~(got by (malt a.g.name-el)) %value)))
  [head name '' | ~]
::
    %prep
  =/  name-el  (snag 0 c.nod)
  =/  name  (@tas (crip (~(got by (malt a.g.name-el)) %value)))
  [head name '' | ~]
::
    %oust
  =/  path  (stab (crip (~(got by (malt a.g.nod)) %pith)))
  [head (pave:neo path)]
::
    %edit
  =/  done-label  (snag 0 c.nod)
  =/  done-el  (snag 0 c.done-label)
  =/  text-el  (snag 1 c.nod)
  =/  text  (crip (~(got by (malt a.g.text-el)) %value))
  =/  done  (~(has by (malt a.g.done-el)) %checked)
  [head text done]
::
    %kid-done
  =/  path  (stab (crip (~(got by (malt a.g.nod)) %pith)))
  [head (pave:neo path)]
::
    %reorder
  =/  piths
    %+  turn  c.nod
    |=  =manx
    =/  here  (~(get by (malt a.g.manx)) %here)
    ?~  here
      ~&  >>>  [%bad-here manx]
      !!
    (pave:neo /[(rear (stab (crip (need here))))])
  [head piths]
==
