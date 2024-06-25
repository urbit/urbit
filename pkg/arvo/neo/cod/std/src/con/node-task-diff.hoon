/@  node  ::  manx
::
::  $task-diff
::  $%  [%new =task prepend=?]
::      [%edit text=cord done=?]
::      [%oust =pith]
::      [%reorder order=(list pith)]
::  ==
/@  task-diff
::  import lib/manx-utils
/-  manx-utils
::
::  declare that this is a conversion from a
::  dynamic XML node to task-diff
:-  [%node %$ %task-diff]
|=  nod=node
^-  task-diff
=/  mu  ~(. manx-utils nod)
::  extract head attribute from XML node
=/  head  (@tas (got:mu %head))
%-  task-diff
?+  head
  ~|  [%unknown-head head]
  !!
    %new
  ::  extract text and prepend attributes from XML node
  =/  text     (vol:mu "text")
  =/  prepend  (vol:mu "prepend")
  ?:  =(prepend 'prepend')
  ::  construct the task-diff
    [head [text | & ~] &]
  [head [text | & ~] |]
::
    %edit
  ::  extract text attribute from XML node
  =/  text     (vol:mu "text")
  ::  extract checked attribute from done element in XML node
  =/  done-el  (need (named:mu "done"))
  =/  done     (~(has by (malt a.g.done-el)) %checked)
  ::  construct the task-diff
  [head text done]
::
    %oust
  ::  extract pith attribute from XML node
  =/  path  (stab (got:mu %pith))
  ::  construct the task-diff
  [head (pave:neo path)]
::
    %reorder
  =/  piths
    ::
    ::  extracting here attribute from each node in XML
    ::  list and return as last element of here path
    %+  turn  c.nod
    |=  =manx
    =/  mu-reorder  ~(. manx-utils manx)
    =/  here        (get:mu-reorder %here)
    ?~  here
      ~&  >>>  [%bad-here manx]
      !!
    (pave:neo /[(rear (stab (crip (need here))))])
  ::  construct the task-diff
  [head piths]
==
