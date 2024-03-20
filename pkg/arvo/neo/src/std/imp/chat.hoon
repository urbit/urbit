/@  message :: message:/~zod/desk/1 <- [p=stud q=*]
/@  chat-diff
/@  chat
/@  sig
=> 
|%
++  state  chat
++  poke  chat-diff
++  card  card:neo
--
^-  firm:neo
|%
++  state  %chat
++  poke  %chat-diff
++  kids
  =<  apex
  |%
  ++  apex
    %-  ~(gas by *kids:neo)
    :~  messages
    ==
  ++  messages 
    ::  /messages/[date=@da]
    ::  /messages/~2023.
    :-  ~[&/%messages |/%da]
    [%message %sig]
  --

++  deps
  =<  apex
  |%
  ++  apex
    %-  ~(gas by *deps:neo)
    :~  open/open
    ==
  ++  open
    [required=| %x %bool %sig]
  --
++  form
  ^-  form:neo
  |_  [=bowl:neo case=@ud state-vase=vase *]
  +*  sta  !<(chat state-vase)
  ++  call
    |=  [old-state=vase act=*]
    =+  ;;(=^poke act)
    ?:  ?=(%dbug -.poke)
      ~&  dbug/bowl
      *(list card)
    ?.  ?=(%msg -.poke)
      *(list card)
    =-  ~[-]
    ^-  card:neo
    :-  (welp were.bowl ~[da/now.bowl])
    ^-  note:neo
    [%make stud/%message `!>(msg.poke) ~]
  ++  reduce
    |=  pok=* :: XX: vaseify
    ^-  vase
    =+  ;;(=^poke pok)
    =/  sta  sta
    ?.  ;;(? +:(~(gut by deps.bowl) %open [*pith &]))
      ~&(dropping-poke/poke !>(sta))
    ?>  |(=(our ship.src):bowl (~(has in who.sta) ship.src.bowl))
    =-  !>(-)
    ^-  ^state
    ?-  -.poke
      %title  sta(title title.poke)
      %add    sta(who (~(put in who.sta) ship.poke))
      %del    sta(who (~(del in who.sta) ship.poke))
      ?(%dbug %msg)   sta
    ==
  ++  init
    |=  old=(unit vase)
    !>(*^state)
  ++  born  *(list card:neo)
  ++  echo
    :: (echo /messages/~2023 )
    |=  [=pith val=*]
    *(list card:neo)
  ++  take
    |=  =sign:neo
    *(list card:neo)
  --
--
