/-  neo
/+  message
=*  card  card:neo
=> 
|%
++  msg-loc
  |=  =bowl:neo
  ^-  path
  /(scot %p our.bowl)/base/(scot %da now.bowl)/lib/message/hoon
--
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
      [%msg msg=state:message]
      [%dbug ~]
  ==
++  kids
  =<  apex
  |%
  ++  apex
    %-  ~(gas by *kids:neo)
    :~  messages
    ==
  ++  messages
    :-  ~[&/%messages |/%da]
    [state:message poke:message]
  --

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
    ?:  ?=(%dbug -.poke)
      ~&  dbug/bowl
      *(list card)
    ?.  ?=(%msg -.poke)
      *(list card)
    [%neo (welp were.bowl ~[da/now.bowl]) %make (msg-loc bowl) `msg.poke ~]^~
  ++  reduce
    |=  pok=*
    ^-  *
    =+  ;;(=poke pok)
    =/  sta  sta
    ?.  ;;(? +:(~(gut by deps.bowl) %open [*pith &]))
      ~&(dropping-poke/poke sta)
    ?>  (~(has in who.sta) src.bowl)
    ?-  -.poke
      %title  sta(title +.poke)
      %add    sta(who (~(put in who.sta) ship.poke))
      %del    sta(who (~(del in who.sta) ship.poke))
      ?(%dbug %msg)   sta
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
