/-  neo
=> 
|%
++  card  card:neo
--
^-  firm:neo
|%
++  state  %ford-out
++  poke   *(set stud:neo)
++  kids  ~
++  deps  ~
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  +*  sta  !<([cache=(unit vase) ~] state-vase)
  ++  poke
    |=  =pail:neo  
    ^-  (quip card:neo vase)
    `state-vase
  ::
  ++  init
    |=  old=(unit vase)
    ^-  (quip card:neo vase)
    =+  !<(ref=vase (need old))
    `!>(`[cache=(unit vase) ~]`[`ref ~])
  --
--
