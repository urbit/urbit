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
  |=  our=@p
  ^-  pith:neo
  [p/our #/$/behn]
--
|%
+$  state  [count=@ud last=@da freq=@dr]
+$  poke
  $%  [%freq freq=@dr]
      [%last last=@da]
      [%wake ~]
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
    ?:  &(=(freq.old-state freq.sta) =(last.old-state last.state))
      ~
    =/  behn=pith  (behn our.bowl)
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
      %wake  sta(last now.bowl, count +(count.sta))
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
    [(behn our.bowl) %poke %wait (add now.bowl freq.sta)]^~
  ::
  ++  take
    |=  =sign:neo
    ^-  (list card:neo)
    ~&  now/now.bowl
    ?.  ?=([%arvo %behn %wake *] sign)
      ~
    :~  [were.bowl %poke %last now.bowl]
        [(behn our.bowl) %poke %wait (add now.bowl freq.sta)]
    ==
  --
--
