/@  node
/@  task-diff
/-  _/manx-utils
:-  [%node %task-diff]
|=  nod=node
^-  task-diff
=/  head  (@tas (crip (need (~(get-attribute manx-utils nod) %head))))
%-  task-diff
?+  head
  ~|  [%unknown-head head]
  !!
    %become
  =/  path  (stab (crip (need (~(value manx-utils nod) "path"))))
  [head (pave:neo path)]
::
    %nest
  =/  name  (@tas (crip (need (~(value manx-utils nod) "name"))))
  [head name '' | ~]
::
    %prep
  =/  name  (@tas (crip (need (~(value manx-utils nod) "name"))))
  [head name '' | ~]
::
    %oust
  =/  path  (stab (crip (need (~(get-attribute manx-utils nod) %pith))))
  [head (pave:neo path)]
::
    %edit
  =/  text  (crip (need (~(value manx-utils nod) "text")))
  =/  done-el  (need (~(named manx-utils nod) "done"))
  =/  done  (~(has by (malt a.g.done-el)) %checked)
  [head text done]
::
    %kid-done
  =/  path  (stab (crip (need (~(get-attribute manx-utils nod) %pith))))
  [head (pave:neo path)]
::
    %reorder
  =/  piths
    %+  turn  c.nod
    |=  =manx
    =/  here  (~(get-attribute manx-utils nod) %here)
    ?~  here
      ~&  >>>  [%bad-here manx]
      !!
    (pave:neo /[(rear (stab (crip (need here))))])
  [head piths]
==
