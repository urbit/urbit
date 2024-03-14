/-  neo
=> 
|%
++  card  card:neo
--
^-  firm:neo
|%
+$  state  @tas
+$  poke   @tas
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
    !>(`state`poke)
  ++  init
    |=  old=(unit vase)
    ?~  old
      !>(*state)
    ~&  u.old
    u.old
  ++  born  *(list card:neo)
  ++  echo
    |=  [=pith val=*]
    *(list card:neo)
  ++  take
    |=  =sign:neo
    *(list card:neo)
  --
--
