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
^-  firm:neo
|%
+$  state  [from=ship now=time message=@t]
+$  poke   ~
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo sta=* *]
  +*  state  ;;(state-0 sta)
  ++  call
    |=  [old-state=* act=*]
    ::  =+  ;;(=action act)
    ::  ~&  call/act
    *(list card)
  ++  reduce
    |=  act=*
    ^-  *
    state
  ++  init
    |=  old=(unit *)
    ?>  ?=(^ old)
    =+  ;;(sta=state-0 u.old)
    sta
  ++  echo
    |=  [=pith val=*]
    *(list card:neo)
  ++  born
    *(list card:neo)
  ++  take
    |=  =sign:neo
    *(list card:neo)
  --
