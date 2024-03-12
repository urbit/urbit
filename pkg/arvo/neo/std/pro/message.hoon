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
+$  state  [from=ship now=time message=@t]
+$  poke   ~
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo case=@ud vase-state=vase *]
  +*  sta  !<(state vase-state)
  ++  call
    |=  [old-state=* act=*]
    ::  =+  ;;(=action act)
    ::  ~&  call/act
    *(list card:neo)
  ++  reduce
    |=  act=*
    ^-  vase
    !>(sta)
  ++  init
    |=  vas=(unit vase)
    ?>  ?=(^ vas)
    =+  !<(=state u.vas)
    !>(state)
  ++  echo
    |=  [=pith val=*]
    *(list card:neo)
  ++  born
    *(list card:neo)
  ++  take
    |=  =sign:neo
    *(list card:neo)
  --
--
