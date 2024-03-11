/-  neo
=> 
|%
++  card  card:neo
--
^-  firm:neo
|%
+$  state  [%0 p=@ud]
+$  poke
  $%  [%val p=@ud]
  ==
++  kids  ~
++  deps  ~
++  form
  ^-  form:neo
  |_  [=bowl:neo case=@ud state-vase=vase *]
  +*  sta  !<(state state-vase)
  ++  call
    |=  [old-state=vase act=*]
    *(list card)
  ++  reduce
    |=  pok=*
    ^-  vase
    =+  ;;(=poke pok)
    =/  sta  sta
    !>(sta(p p.poke))
  ++  init
    |=  old=(unit vase)
    !>(*state)
  ++  born  *(list card:neo)
  ++  echo
    |=  [=pith val=*]
    *(list card:neo)
  ++  take
    |=  =sign:neo
    *(list card:neo)
  --
--
