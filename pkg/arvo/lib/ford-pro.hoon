/-  neo
=> 
|%
++  card  card:neo
++  get-face
  |=  =bowl:neo
  ^-  @tas
  !<(@tas q:(~(got by deps.bowl) %face))
++  get-sut
  |=  =bowl:neo
  ^-  (unit (pair pith vase))
  =/  sut  (~(got by deps.bowl) %sut)
  =+  !<([cac=(unit vase) *] q.sut)
  ?~  cac
    ~
  `[p.sut u.cac]
++  build
  |=  =bowl:neo
  ^-  (unit vase)
  ?~  sut=(get-sut bowl)
    ~
  =/  pit=vase  !>(p=p.u.sut)
  =/  cor=vase  q.u.sut(p [%face %q p.q.u.sut])
  =/  res=vase  (slop pit cor)
  `res(p [%face (get-face bowl) p.res])
--
^-  firm:neo
|%
+$  state  [cache=(unit vase) ~]
+$  poke
  $%  [%dep ~]
  ==
++  kids  ~
++  deps
  =<  apex
  |%
  ++  apex
    %-  ~(gas by *deps:neo)
    :~  sut/sut
        face/face
    ==
  ++  sut
    [& ,[cache=(unit vase) *] ,*]
  ++  face
    [& ,@tas ,*]
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
    =.  cache.sta  (build bowl)
    !>(sta)
  ++  init
    |=  vax=(unit vase)
    !>(*state)
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
