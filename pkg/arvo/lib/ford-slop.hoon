/-  neo
=> 
|%
++  card  card:neo
++  get-sut
  |=  [sid=?(%a %b) =bowl:neo]
  ^-  (unit vase)
  =+  !<([cac=(unit vase) *] q:(~(got by deps.bowl) sid))
  cac
++  build
  |=  =bowl:neo
  ^-  (unit vase)
  ?~  a=(get-sut %a bowl)
    ~&  missing-a/were.bowl
    ~
  ?~  b=(get-sut %b bowl)
    ~&  missing-b/were.bowl
    ~
  `(slop u.a u.b)
+$  state  [cache=(unit vase) ~]
+$  poke
  $%  [%dep ~]
  ==
--
^-  firm:neo
|%
+$  state  ^state
+$  poke   ^poke
++  kids  ~
++  deps
  =<  apex
  |%
  ++  apex
    %-  ~(gas by *deps:neo)
    :~  a/sut
        b/sut
    ==
  ++  sut
    [& %x ,[cache=(unit vase) *] ,*]
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
    =+  ;;(=^poke pok)
    =/  sta  sta
    =.  cache.sta  (build bowl)
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
  ++  take
    |=  =sign:neo
    ^-  (list card:neo)
    ?.  ?=([%neo %conf %val @] sign)
      !!
    =-  ~[-]
    [were.bowl %poke %dep ~]
  --
--
