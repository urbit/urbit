/-  neo
=> 
|%
++  card  card:neo
--
^-  firm:neo
|%
+$  state  [cache=(unit vase) ~]
+$  poke  ~
++  kids  ~
++  deps  ~
++  form
  ^-  form:neo
  |_  [=bowl:neo case=@ud state-vase=vase *]
  +*  sta  !<(state state-vase)
  ++  call
    |=  [old-state=vase act=*]
    *(list card:neo)
  ++  reduce
    |=  pok=*
    ^-  vase
    state-vase
  ++  init
    |=  old=(unit vase)
    =+  !<(ref=vase (need old))
    !>(`state`[`ref ~])
  ++  born  *(list card:neo)
  ++  echo
    |=  [=pith val=*]
    *(list card:neo)
  ++  take
    |=  =sign:neo
    *(list card:neo)
  --
--