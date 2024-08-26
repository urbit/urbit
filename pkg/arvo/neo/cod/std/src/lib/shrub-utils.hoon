|%  
::
::  get list of kids piths from lore
::
++  kids-at-pith
  |=  [lore=(axal:neo *) =pith:neo]
  ^-  (list pith:neo) 
  %~  tap  in 
  %~  key  by
  %-  %~  kid  of:neo
      lore
  pith
::
::  list of kids piths from lore as care %z
::
++  kidz-at-pith
  |=  [=pith:neo =lore:neo]
  =|  i=@
  ^-  (list pith:neo)
  =+  piths=(full-pith pith lore)
  |-
  ?:  =(i (lent piths))  piths
    =/  p=pith:neo  (snag i piths)
    ?~  (~(kid of:neo lore) p)  
      $(i +(i))
    =/  grand-kids  (full-pith p lore)
    $(i +(i), piths (welp piths grand-kids))
::
::  list of kids piths from lore with full pith
::
++  full-pith
  |=  [parent=pith:neo =lore:neo]
  ^-  (list pith:neo)
  ?~  (~(kid of:neo lore) parent)  ~
  %+  turn  ~(tap in ~(key by (~(kid of:neo lore) parent)))  
    |=  p=pith:neo 
    %+  welp  parent  p
::
::  (unit pail) from lore 
::
++  get-pail-by-pith
  |=  [=lore:neo =pith:neo]
  ^-  (unit pail:neo)
  =/  idea=(unit idea:neo)  (~(get of:neo lore) pith)
  ?~  idea  ~
  `pail:(need idea)
::
++  got-pail-by-pith
  |=  [=lore:neo =pith:neo]
  ^-  pail:neo
  =/  =idea:neo  (~(got of:neo lore) pith)
  pail.idea
::
++  get-pail-saga-by-pith
  |=  [=lore:neo =pith:neo]
  ^-  (unit pail:neo)
  =/  idea=(unit idea:neo)  (~(get of:neo lore) pith)
  ?~  idea  ~
  `q.saga:(need idea)
::
++  got-pail-saga-by-pith
  |=  [=lore:neo =pith:neo]
  ^-  pail:neo
  =/  idea=idea:neo  (~(got of:neo lore) pith)
  q.saga:idea
::
++  get-vase-by-pith
  |=  [=lore:neo =pith:neo]
  ^-  (unit vase)
  =/  idea=(unit idea:neo)  (~(get of:neo lore) pith)
  ?~  idea  ~
  `q.pail:(need idea)
::
++  got-vase-by-pith
  |=  [=lore:neo =pith:neo]
  ^-  vase
  =/  =idea:neo  (~(got of:neo lore) pith)
  q.pail:idea
::
++  get-vase-saga-by-pith
  |=  [=lore:neo =pith:neo]
  ^-  (unit vase)
  =/  idea=(unit idea:neo)  (~(get of:neo lore) pith)
  ?~  idea  ~
  `q.q.saga:(need idea)
::
++  got-vase-saga-by-pith
  |=  [=lore:neo =pith:neo]
  ^-  vase
  =/  idea=idea:neo  (~(got of:neo lore) pith)
  q.q.saga:idea
::
++  en-pith
  |=  =cord
  ^-  pith
  %-  pave:neo
  ;;  path
  %-  stab  cord
::
::  produce list of values in axal
::
++  val
|=  axal=(axal:neo *)
^-  (list *)
~(val by ~(tar of:neo axal))
::
--