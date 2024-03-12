/-  neo
=> 
|%
++  card  card:neo
++  get-sut
  |=  =bowl:neo
  ^-  (unit vase)
  =+  !<([cac=(unit vase) *] q:(~(got by deps.bowl) %sut))
  cac
++  build
  |=  [=bowl:neo =hoon]
  ^-  (unit vase)
  ?~  sut=(get-sut bowl)
    ~
  `(slap u.sut hoon)

--
^-  firm:neo
|%
+$  state  [cache=(unit vase) =hoon]
+$  poke
  $%  [%dep ~]
      [%hoon =hoon]
  ==
++  kids  ~
++  deps
  =<  apex
  |%
  ++  apex
    %-  ~(gas by *deps:neo)
    :~  sut/sut
    ==
  ++  sut
    [& ,[cache=(unit vase) *] ,*]
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
    =?  hoon.sta  ?=(%hoon -.poke)
      hoon.poke
    =.  cache.sta  (build bowl hoon.sta)
    !>(sta)
  ++  init
    |=  vax=(unit vase)
    ?~  vax  !>(*state)
    =+  !<([cac=(unit vase) =hoon] u.vax)
    !>(`state`[~ hoon])
  ++  born
    =-  ~[-]
    [%neo were.bowl %poke %dep ~]
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
