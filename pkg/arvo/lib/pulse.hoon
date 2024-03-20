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
  |_  [=bowl:neo untyp-sta=* *]
  +*  sta  ;;(state untyp-sta)
  ++  call
    |=  [ole=* act=*]
    ^-  (list card:neo)
    =/  old-state  ;;(state ole)
    =/  sta  sta
    ?:  =(freq.old-state freq.sta)
      ~
    :~  [%arvo %b %rest (add [last freq]:old-state)]
        [%arvo %b %wait (add now.bowl freq.sta)]
    ==
  ++  reduce
    |=  act=*
    ^-  *
    =+  ;;(=poke act)
    =/  sta  sta
    ?-  -.poke
      %freq  sta(freq freq.poke)
      %last  sta(last last.poke)
    ==
  ++  init
    |=  old=(unit *)
    ?>  ?=(^ old)
    =+  ;;(sta=state u.old)
    sta
  ++  echo
    |=  [=pith val=*]
    *(list card:neo)
  ++  born
    =/  sta  sta
    [%arvo %b %wait (add now.bowl freq.sta)]^~
  ::
  ++  take
    |=  =sign:neo
    ^-  (list card:neo)
    ~&  now/now.bowl
    ?.  ?=([%arvo %behn %wake *] sign)
      ~
    :~  [%neo were.bowl %poke %last now.bowl]
        [%arvo %b %wait (add now.bowl freq.sta)]
    ==
  --
--
