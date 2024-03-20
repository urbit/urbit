/-  neo
::  sender ship namesapce (~bus)
::  /messages/1
::  host ship namespace
::  /chat/foo
::  /chat/foo/messages/1 :: symlink to /+bus/messages/1
::
::  /~zod/chat/foo/messages/1 <- []
::  /~zod/chat/foo/messages/1 <-
::  /~zod/chat/foo/messages/1 <-
::
::  /~bus/subs/foo -> /~zod/chat/foo
::
::  [/~bus/subs/foo/messages/1 %make ]
::  possibly optimisticaly update, then forward note to foreign ship
::
::  %make
:: ^-  firm:neo
=>
|%
++  behn
  (pave //sys/behn)
--
|%
+$  state  [count=@ud last=@da freq=@dr]
+$  poke
  $%  [%freq freq=@dr]
      [%last last=@da]
  ==
  
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo case=@ud state-vase=vase *]
  +*  sta  !<(state state-vase)
  ++  call
    |=  [ole=vase act=*]
    ^-  (list card:neo)
    =/  old-state  !<(state ole)
    =/  sta  sta
    ?:  =(freq.old-state freq.sta)
      ~
    =/  behn=pith
      (pave //vane/behn)
    =/  wait=req:behn:neo
      [%rest (add [last freq]:old-state)]
    =/  rest=req:behn:neo
      [%wait (add now.bowl freq.sta)]
    :~  [behn %poke rest]
        [behn %poke wait]
    ==
  ++  reduce
    |=  act=*
    ^-  vase
    =+  ;;(=poke act)
    =/  sta  sta
    !>  ^-  state
    ?-  -.poke
      %freq  sta(freq freq.poke)
      %last  sta(last last.poke)
    ==
  ++  init
    |=  old=(unit vase)
    ?>  ?=(^ old)
    =+  !<(sta=state u.old)
    u.old
  ++  echo
    |=  [=pith val=*]
    *(list card:neo)
  ++  born
    =/  sta  sta
    [behn %poke %wait (add now.bowl freq.sta)]^~
  ::
  ++  take
    |=  =sign:neo
    ^-  (list card:neo)
    ~&  now/now.bowl
    ?.  ?=([%arvo %behn %wake *] sign)
      ~
    :~  [were.bowl %poke %last now.bowl]
        [behn %poke %wait (add now.bowl freq.sta)]
    ==
  --
--
