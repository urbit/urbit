/-  neo
=*  card  card:neo
^-  firm:neo
|%
::  $state: state for chat container
::    .who: set of ships allowed to poke
::    .title: human-readable title of chat
::
+$  state  [%0 who=(set ship) title=@t]
::  $poke: update for chat container
::
::    %title: 
::    %add: add .ship to .who in $state
::    %del: remove .ship from .who in $state
::
+$  poke
  $%  [%title title=@t]   :: update
      [%add =ship]
      [%del =ship]
      [%dbug ~]
  ==
++  deps
  =<  apex
  |%
  ++  apex
    %-  ~(gas by *deps:neo)
    :~  open/open
    ==
  ++  open
    [| ,? ,?]
  --
++  form
  ^-  form:neo
  |_  [=bowl:neo untyped-state=* *]
  +*  sta  ;;(state untyped-state)
  ++  call
    |=  [old-state=* act=*]
    =+  ;;(=poke act)
    ~&  call/act
    ?:  ?=(%dbug -.act)
      ~&  dbug/bowl
      *(list card)
    *(list card)
  ++  reduce
    |=  pok=*
    ^-  *
    =+  ;;(=poke pok)
    =/  sta  sta
    ?.  ;;(? +:(~(gut by deps.bowl) %open [*pith &]))
      ~&(dropping-poke/poke sta)
    ?-  -.poke
      %title  sta(title +.poke)
      %add    sta(who (~(put in who.sta) ship.poke))
      %del    sta(who (~(del in who.sta) ship.poke))
      %dbug   sta
    ==
  ++  init
    |=  old=(unit *)
    *state
  ++  born  *(list card:neo)
  ++  echo
    |=  [=pith val=*]
    *(list card:neo)
  ++  take
    |=  =sign:neo
    *(list card:neo)
  --
--
