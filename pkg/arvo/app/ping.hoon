/+  default-agent
=*  point  point:able:kale
::
=>  |%
    +$  card  card:agent:gall
    ::  +print-error: maybe +slog
    ::
    ++  print-error
      |=  [=tape error=(unit tang)]
      ^+  same
      ?~  error  same
      %-  (slog leaf+tape u.error)  same
    ::  +set-timer: send a card to behn to set a timer
    ::
    ++  set-timer
      |=  now=@da
      ^-  (list card)
      [%pass /ping-wait %arvo %b %wait `@da`(add ~s30 now)]~
    ::  +send-ping: scry our sponsor from jael and poke their %ping app
    ::
    ++  send-ping
      |=  [our=@p now=@da]
      ^-  (list card)
      ::
      =/  sponsor=ship  (sein:title our now our)
      ::
      ~>  %slog.0^leaf/"ping: {<our>} -> {<sponsor>}"
      [%pass /ping-send %agent [sponsor %ping] %poke %noun !>(~)]~
    --
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
::
::  +on-init: initializing on startup
::
++  on-init
  ^-  [(list card) _this]
  ::  first load; galaxies no-op; everyone else pings sponsor
  ::
  :_  this
  ?:  =(%czar (clan:title our.bowl))
    ~
  ~>  %slog.0^leaf/"ping: prep {<our.bowl>}"
  (send-ping our.bowl now.bowl)
::
++  on-save   on-save:def
++  on-load   on-load:def
::  +on-poke: positively acknowledge pokes
::
++  on-poke
  |=  [=mark =vase]
  `this
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
::  +on-agent: handle ames ack
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  [(list card) _this]
  ?>  ?=(%poke-ack -.sign)
  :_  this
  ::
  %-  (print-error "ping: ack" p.sign)
  (set-timer now.bowl)
::  +on-arvo: handle timer firing
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  [(list card) _this]
  ?>  ?=(%wake +<.sign-arvo)
  :_  this
  ~!  error.sign-arvo
  %-  (print-error "ping: wake" error.sign-arvo)
  (send-ping our.bowl now.bowl)
::
++  on-fail   on-fail:def
--
