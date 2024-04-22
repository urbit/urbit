/@  accel-node
::
^-  firm:neo
|%
++  state  %accel-node
++  poke  (sy %rely ~)
++  kids  *kids:neo
++  deps
  %-  ~(gas by *deps:neo)
  =/  q 
    :-  req=|  
    :-  [%sig %sig]
    `[%x ~]
  :~  [%a q]  [%b q]  [%c q]
      [%d q]  [%e q]  [%f q]
      [%g q]  [%h q]  [%i q]
  ==
::
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(%rely stud)
    =+  !<([=term =stem:neo] vax)
    ~&  >  [term stem]
    ?>  ?=(%x -.q.stem)
    =/  old  !<(accel-node state-vase)
    =/  new  (~(put by old) term q.pail.q.stem)
    [~ !>(new)]
  ++  init
    |=  old=(unit vase)
    ::=/  d  ~(val by deps.bowl)
    ::~&  >  pail:q:(snag 0 d)
    `(need old)
  --
--
