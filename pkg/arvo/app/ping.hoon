::  To traverse NAT, we need the response to come back from someone
::  we've sent a message to.  We ping our sponsor so that they know
::  where we are.  However, we also need to ping our galaxy because if
::  the other ship tries to respond directly, it may be blocked by our
::  firewall or NAT.  Thus, the response must come from a ship we've
::  messaged direclty, and the only one we can guarantee is our galaxy.
::  Note this issue manifests itself even for bootstrapping a planet to
::  talk to its own star.
::
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
      |=  [now=@da n=@ud]
      ^-  (list card)
      [%pass /ping-wait/(scot %ud n) %arvo %b %wait `@da`(add ~s30 now)]~
    ::  +send-ping: scry our sponsor from jael and poke their %ping app
    ::
    ++  send-ping
      |=  [our=@p now=@da n=@ud]
      ^-  (list card)
      ::
      =/  sponsor=ship  (snag n (saxo:title our now our))
      ::
      ~>  %slog.0^leaf/"ping: {<our>} -> {<sponsor>}"
      [%pass /ping-send/(scot %ud n) %agent [sponsor %ping] %poke %noun !>(~)]~
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
  ~>  %slog.0^leaf/"ping: prep {<our.bowl>}"
  =/  saxo  (tail (saxo:title our.bowl now.bowl our.bowl))
  %-  zing
  %+  turn  (gulf 1 (lent saxo))
  |=  n=@ud
  (send-ping our.bowl now.bowl n)
::
++  on-save   on-save:def
++  on-load   ::  on-load:def
  |=  vase
  on-init
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
  ?>  ?=([%ping-send @ ~] wire)
  ?>  ?=(%poke-ack -.sign)
  :_  this
  ::
  %-  (print-error "ping: ack" p.sign)
  (set-timer now.bowl (slav %ud i.t.wire))
::  +on-arvo: handle timer firing
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  [(list card) _this]
  ?>  ?=([%ping-wait @ ~] wire)
  ?>  ?=(%wake +<.sign-arvo)
  :_  this
  ~!  error.sign-arvo
  %-  (print-error "ping: wake" error.sign-arvo)
  (send-ping our.bowl now.bowl (slav %ud i.t.wire))
::
++  on-fail   on-fail:def
--
