/@  node
/@  task-diff
/-  manx-utils
:-  [%node %$ %task-diff]
|=  nod=node
^-  task-diff
=/  mu  ~(. manx-utils nod)
=/  head  (@tas (got:mu %head))
%-  task-diff
?+  head
  ~|  [%unknown-head head]
  !!
    %new
  [head '' | ~]
::
    %edit
  =/  text  (vol:mu "text")
  =/  done-el  (need (named:mu "done"))
  =/  done  (~(has by (malt a.g.done-el)) %checked)
  [head text done]
::
    %oust
  =/  path  (stab (got:mu %pith))
  [head (pave:neo path)]
::
    %reorder
  =/  piths
    %+  turn  c.nod
    |=  =manx
    =/  here  (get:mu %here)
    ?~  here
      ~&  >>>  [%bad-here manx]
      !!
    (pave:neo /[(rear (stab (crip (need here))))])
  [head piths]
==
