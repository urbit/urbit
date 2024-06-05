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
  =/  text  (vol:mu "text")
  =/  prepend  (vol:mu "prepend")
  ?:  =(prepend 'prepend')
    [head [text | ~] &]
  [head [text | ~] |]
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
    =/  mu-reorder  ~(. manx-utils manx)
    =/  here  (get:mu-reorder %here)
    ?~  here
      ~&  >>>  [%bad-here manx]
      !!
    (pave:neo /[(rear (stab (crip (need here))))])
  [head piths]
==
