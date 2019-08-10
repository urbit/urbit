=*  point  point:able:kale
::
=>  |%
    +$  app-state  ~
    +$  move  [=bone =card]
    +$  card
      $%  [%poke =wire [=ship app=term] =action]
          [%wait =wire date=@da]
      ==
    +$  action
      $%  [%ping ~]
      ==
    --
::
=>  |%
    ::  +print-error: maybe +slog
    ::
    ++  print-error
      |=  [=tape error=(unit tang)]
      ^+  same
      ?~  error  same
      %-  (slog leaf+tape u.error)  same
    --
::
|_  [=bowl:gall state=app-state]
::
+|  %entry-points
::
::  +prep: (re)load, initializing on startup
::
++  prep
  |=  old=(unit app-state)
  ^-  [(list move) _app-core]
  ::  reload; no-op
  ::
  ?^  old
    [~ app-core(state u.old)]
  ::  first load; galaxies no-op; everyone else pings sponsor
  ::
  ?:  =(%czar (clan:title our.bowl))
    [~ app-core]
  send-ping
::  +coup-ping-send: handle ames ack
::
++  coup-ping-send
  |=  error=(unit tang)
  ^-  [(list move) _app-core]
  ::
  %-  (print-error "ping: coup" error)
  set-timer
::  +wake: handle timer firing
::
++  wake
  |=  [=wire error=(unit tang)]
  ^-  [(list move) _app-core]
  ::
  ?>  =(/ping-wait wire)
  %-  (print-error "ping: wake" error)
  send-ping
::  +poke-noun: handle request; no-op to ack, crash to nack
::
++  poke-noun
  |=  input=*
  ^-  [(list move) _app-core]
  ::
  ?>  ?=([%ping ~] input)
  [~ app-core]
::
+|  %helpers
::
++  app-core  .
::  +set-timer: send a move to behn to set a timer
::
++  set-timer
  ^-  [(list move) _app-core]
  ::
  :_  app-core
  [ost.bowl %wait /ping-wait `@da`(add ~s30 now.bowl)]~
::  +send-ping: scry our sponsor from jael and poke their %ping app
::
++  send-ping
  ^-  [(list move) _app-core]
  ::
  =/  our=@ta  (scot %p our.bowl)
  =/  now=@ta  (scot %da now.bowl)
  =/  =life    .^(life %k /[our]/life/[now]/[our])
  =/  =point   .^(point %k /[our]/deed/[now]/[our]/(scot %ud life))
  ::  TODO: handle missing sponsor case
  ::
  =/  sponsor=ship  (need sponsor.point)
  ::
  :_  app-core
  [ost.bowl %poke /ping-send [sponsor %ping] %ping ~]~
--
