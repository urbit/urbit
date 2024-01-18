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
|%
+$  state-0  [%0 from=ship now=time message=@t]
+$  card     card:neo
+$  action   ~
--
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
++  take
  |=  =sign:neo
  *(list card:neo)
--
