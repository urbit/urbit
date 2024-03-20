/-  neo
=> 
|%
++  card  card:neo
++  get-src
  |=  =bowl:neo
  ^-  (unit vase)
  =+  !<([cac=(unit vase) *] q:(~(got by deps.bowl) %src))
  cac
+$  state  [cache=(unit vase) ~]
+$  poke   [%dep ~]
--
^-  firm:neo
|%
+$  poke    ^poke
+$  state   ^state
++  kids  ~
++  deps
  =<  apex
  |%
  ++  apex
    %-  ~(gas by *deps:neo)
    :~  src/src
    ==
  ++  src
    [%& %x ,[cache=(unit vase) *] ,*]
  --
++  form
  ^-  form:neo
  |_  [=bowl:neo case=@ud state-vase=vase *]
  +*  sta  !<(^state state-vase)
  ++  call
    |=  [old-state=vase act=*]
    *(list card)
  ++  reduce
    |=  pok=*
    ^-  vase
    =+  ;;(poke=^poke pok)
    =/  sta  sta
    =.  cache.sta  (get-src bowl)
    ~&  ford-same/[were.bowl !=(~ cache.sta)]
    !>(sta)
  ++  init
    |=  vax=(unit vase)
    !>(*^state)
  ++  born
    =-  ~[-]
    [were.bowl %poke %dep ~]
  ++  echo
    |=  [=pith val=*]
    *(list card:neo)
  ++  take  (rerun:ford:neo bowl)
  --
--
