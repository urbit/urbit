::  Ping our sponsorship tree regularly for routing.
::
::  To traverse NAT, we need the response to come back from someone
::  we've sent a message to.  We ping our sponsor so that they know
::  where we are.  However, we also need to ping our galaxy because if
::  the other ship tries to respond directly, it may be blocked by our
::  firewall or NAT.  Thus, the response must come from a ship we've
::  messaged directly, and the only one we can guarantee is our galaxy.
::  Note this issue manifests itself even for bootstrapping a planet to
::  talk to its own star.
::
/+  default-agent, verb
=*  point  point:able:kale
::
|%
+$  card  card:agent:gall
+$  ship-state
  $%  [%idle ~]
      [%poking ~]
      [%waiting until=@da]
  ==
--
::
=|  state=[%0 ships=(map ship [=rift =ship-state])]
=>  |%
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
      |=  [now=@da =ship]
      ^-  (quip card _state)
      =/  s  (~(get by ships.state) ship)
      ?~  s
        `state
      ?.  ?=(%poking -.ship-state.u.s)
        %-  (slog leaf+"ping: strange state {<ship s>}" ~)
        `state
      ::  NAT timeouts are often pretty short for UDP entries.  5
      ::  minutes is a common value.  We use 30 seconds, which is fairly
      ::  aggressive, but should be safe.
      ::
      =/  until  (add ~s30 now)
      =.  ships.state
        (~(put by ships.state) ship u.s(ship-state [%waiting until]))
      :_  state
      =/  =wire  /ping-wait/(scot %p ship)/(scot %da until)
      [%pass wire %arvo %b %wait `@da`until]~
    ::  +send-ping: poke their %ping app
    ::
    ++  send-ping
      |=  [our=@p now=@da =ship]
      ^-  (quip card _state)
      ::
      ?:  =(our ship)
        `state
      =/  s  (~(get by ships.state) ship)
      ?~  s
        `state
      ?.  ?=(%idle -.ship-state.u.s)
        `state
      :_  state(ships (~(put by ships.state) ship u.s(ship-state [%poking ~])))
      [%pass /ping-send/(scot %p ship) %agent [ship %ping] %poke %noun !>(~)]~
    ::  +stop-ping-ship: stop listening to jael if not sponsor or old rift
    ::
    ++  stop-ping-ship
      |=  [our=@p now=@da =ship =old=rift =ship-state]
      ^-  (quip card _state)
      =+  .^(=new=rift %j /=rift/(scot %da now)/(scot %p ship))
      ::  if nothing's changed about us, don't cancel
      ::
      ?:  ?&  =(old-rift new-rift)
              (~(has in (silt (saxo:title our now our))) ship)
          ==
        `state
      ::  otherwise, kill jael subscription and timer
      ::
      :_  state(ships (~(del by ships.state) ship))
      [%pass /jael/(scot %p ship) %arvo %j %nuke (silt ship ~)]~
    ::  +start-ping-ship: start listening to jael updates if not already
    ::
    ::    While %public-keys is idempotent in most senses, it does
    ::    trigger a response, and this function is called on that
    ::    response, so we need a guard to avoid an infinite loop.
    ::
    ++  start-ping-ship
      |=  [our=@p now=@da =ship]
      ^-  (quip card _state)
      ::
      ?:  (~(has by ships.state) ship)
        (send-ping our now ship)
      ::
      ;<  new-state=_state  (rind card state)
        =+  .^(=rift %j /=rift/(scot %da now)/(scot %p ship))
        :_  state(ships (~(put by ships.state) ship rift %idle ~))
        [%pass /jael/(scot %p ship) %arvo %j %public-keys (silt ship ~)]~
      =.  state  new-state
      ::
      (send-ping our now ship)
    ::  +kick: idempotent operation to make clean start for all pings
    ::
    ++  kick
      |=  [our=@p now=@da]
      ^-  (quip card _state)
      ?:  =(%czar (clan:title our))
        `state
      ::
      =/  old-ships=(list [=ship =rift =ship-state])  ~(tap by ships.state)
      |-  ^-  (quip card _state)
      =*  loop  $
      ?^  old-ships
        ;<  new-state=_state  (rind card state)
          (stop-ping-ship our now i.old-ships)
        =.  state  new-state
        loop(old-ships t.old-ships)
      ::
      =/  new-ships  (saxo:title our now our)
      |-  ^-  (quip card _state)
      =*  loop  $
      ?^  new-ships
        ;<  new-state=_state  (rind card state)
          (start-ping-ship our now i.new-ships)
        =.  state  new-state
        loop(new-ships t.new-ships)
      ::
      `state
    ::  +rind: bind for the the writer monad on (quip effect state)
    ::
    ++  rind
      |*  [effect=mold state=*]
      |*  state-type=mold
      |=  $:  m-b=(quip effect state-type)
              fun=$-(state-type (quip effect state-type))
          ==
      ^-  (quip effect state-type)
      =^  effects-1=(list effect)  state  m-b
      =^  effects-2=(list effect)  state  (fun state)
      [(weld effects-1 effects-2) state]
    ::
    --
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
::
::  +on-init: initializing on startup
::
++  on-init
  ^-  [(list card) _this]
  =^  cards  state  (kick our.bowl now.bowl)
  [cards this]
::
++  on-save   !>(state)
++  on-load
  |=  old=vase
  =.  state  !<(_state old)
  (on-poke %noun !>(%kick))
::  +on-poke: positively acknowledge pokes
::
++  on-poke
  |=  [=mark =vase]
  ?:  =(q.vase %kick)
    =.  ships.state
      %-  ~(run by ships.state)
      |=  [=rift =ship-state]
      [999.999 ship-state]
    on-init
  `this
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ``noun+!>(state)
::  +on-agent: handle ames ack
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  [(list card) _this]
  ?>  ?=([%ping-send @ ~] wire)
  ?>  ?=(%poke-ack -.sign)
  ::
  %-  (print-error "ping: ack" p.sign)
  =^  cards  state
    (set-timer now.bowl (slav %p i.t.wire))
  [cards this]
::  +on-arvo: handle timer firing
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  [(list card) _this]
  ?+    wire  !!
      [%ping-wait @ @ ~]
    ?>  ?=(%wake +<.sign-arvo)
    =/  =ship      (slav %p i.t.wire)
    =/  until=@da  (slav %da i.t.t.wire)
    =/  s  (~(get by ships.state) ship)
    ?~  s
      `this
    ?.  =([%waiting until] ship-state.u.s)
      `this
    =.  ships.state  (~(put by ships.state) ship u.s(ship-state [%idle ~]))
    %-  (print-error "ping: wake" error.sign-arvo)
    =^  cards  state
      (send-ping our.bowl now.bowl ship)
    [cards this]
  ::
      [%jael @ ~]
    ::  whenever we get an update from Jael, kick
    ::
    ?>  ?=(%public-keys +<.sign-arvo)
    :_  this
    [%pass /delay %arvo %b %wait now.bowl]~
  ::  Delayed until next event so that ames can clear its state
  ::
      [%delay ~]
    ?>  ?=(%wake +<.sign-arvo)
    on-init
  ==
::
++  on-fail   on-fail:def
--
