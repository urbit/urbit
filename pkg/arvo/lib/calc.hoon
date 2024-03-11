/-  neo
=> 
|%
++  card  card:neo
+$  state-0
  $:  exp=hoon
      calc=(each @ud tang)
  ==
++  reef  ^~(!>(..zuse))
++  update
  |=  [exp=hoon deps=(pair @ud @ud)]
  %-  mule  |.  
  %.  deps
  !<($-([@ud @ud] @ud) (slap reef exp))
++  pull-dep
  |=  [=bowl:neo =term]
  ~|  deps/deps.bowl
  =<  p
  !<([%0 p=@ud] +:(~(got by deps.bowl) term))
++  pull-deps
  |=  =bowl:neo
  ^-  (pair @ud @ud)
  [(pull-dep bowl %p) (pull-dep bowl %q)]
--
^-  firm:neo
|%
+$  state  [%0 state-0]
+$  poke
  $%  [%exp p=hoon]
      [%dep ~]
  ==
++  kids  ~
++  deps
  =<  apex
  |%
  ++  apex
    %-  ~(gas by *deps:neo)
    :~  p/dep
        q/dep
    ==
  ++  dep
    [& ,? ,?]
  --

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
    =.  sta
      ?-    -.poke
          %exp  sta(exp p.poke)
          %dep  ?>(=(src our):bowl sta)
      ==
    =.  calc.sta  (update exp.sta (pull-deps bowl))
    !>(sta)
  ++  init
    |=  old=(unit vase)
    !>(*state)
  ++  born
    ~&  born/bowl
    *(list card:neo)
  ++  echo
    |=  [=pith val=*]
    *(list card:neo)
  ++  take
    |=  =sign:neo
    ^-  (list card:neo)
    ?.  ?=([%neo %conf %val @] sign)
      !!
    =-  ~[-]
    [%neo were.bowl %poke %dep ~]
  --
--
