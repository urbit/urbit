/@  message
/@  chat-diff
/@  chat
/@  sig
=> 
|%
++  state  chat
++  msg-loc
  |=  =bowl:neo
  ^-  path
  /(scot %p our.bowl)/base/(scot %da now.bowl)/lib/message/hoon
++  poke  chat-diff
--
^-  firm:neo
|%
+$  state  %chat
+$  poke  %chat-diff
++  kids
  =<  apex
  |%
  ++  apex
    %-  ~(gas by *kids:neo)
    :~  messages
    ==
  ++  messages
    :-  ~[&/%messages |/%da]
    [message sig]
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
  |_  [=bowl:neo case=@ud state-vase=vase *]
  +*  sta  !<(chat state-vase)
  ++  call
    |=  [old-state=vase act=*]
    =+  ;;(=poke act)
    ?:  ?=(%dbug -.poke)
      ~&  dbug/bowl
      *(list card)
    ?.  ?=(%msg -.poke)
      *(list card)
    [%neo (welp were.bowl ~[da/now.bowl]) %make %message `!>(msg.poke) ~]^~
  ++  reduce
    |=  pok=*
    ^-  vase
    =+  ;;(=poke pok)
    =/  sta  sta
    ?.  ;;(? +:(~(gut by deps.bowl) %open [*pith &]))
      ~&(dropping-poke/poke !>(sta))
    ?>  |(=(our src):bowl (~(has in who.sta) src.bowl))
    =-  !>(-)
    ^-  state
    ?-  -.poke
      %title  !! :: sta(title title.poke)
      %add    sta(who (~(put in who.sta) ship.poke))
      %del    sta(who (~(del in who.sta) ship.poke))
      ?(%dbug %msg)   sta
    ==
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
