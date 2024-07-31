|%  
::
::  get list of kids piths from lore
::
++  kids-at-pith
  |=  [=lore:neo =pith:neo]
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
++  kids-full-pith
  |=  [parent=pith:neo =lore:neo]
  ^-  (list pith:neo)
  ?~  (~(kid of:neo lore) parent)  ~
  %+  turn  ~(tap in ~(key by (~(kid of:neo lore) parent)))  
    |=  p=pith:neo 
    %+  welp  parent  p
::
::  (unit pail) from lore 
::
++  pail-from-lore
  |=  [=lore:neo =pith:neo]
  ^-  (unit pail:neo)
  =/  id=(unit idea:neo)  (~(get of:neo lore) pith)
  ?~  id  ~
  `q.saga:(need id)
::
::  get pail.saga from idea 
::  
:: ++  pail-from-idea
::   |=  =idea:neo
::   ^-  pail:neo
::   q.saga:(need idea)
--