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
/+  default-agent, verb, dbug
=*  point  point:kale
::
|%
::  How often to ping our sponsor when we might be behind a NAT.
::
::    NAT timeouts are often pretty short for UDP entries.  5 minutes is
::    a common value.  We use 25 seconds, same as Wireguard.
::
++  nat-timeout  ~s25
::
::  How often to check our IP when we know we're not behind a NAT.
::
++  ip-timeout   ~m5
::
::  Chosen because it's run by Cloudflare, and others I tried were
::  inconsistently slow.
::
++  ip-reflector  'https://icanhazip.com'
::
+$  card  card:agent:gall
+$  ship-state
  $%  [%idle ~]
      [%poking ~]
      [%http until=@da]
      [%waiting until=@da]
  ==
+$  state-1
  $:  %1
      ships=(set ship)
      nonce=@ud
      $=  plan
      $~  [%nat ~]
      $%  [%nat ~]
          [%pub ip=(unit @t)]
      ==
  ==
--
::
%-  agent:dbug
::
=|  state=state-1
=>  |%
    ::  Bind for the the writer monad on (quip effect state)
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
    ++  once
      |=  =cord
      =(cord (scot %uw nonce.state))
    ::
    ::  Subsystem to keep track of which ships to ping across breaches
    ::  and sponsorship changes
    ::
    ++  ships
      |%
      ++  rind  (^rind card state)
      ++  kick
        |=  [our=@p now=@da]
        ^-  (quip card _state)
        ::  ?:  =(%czar (clan:title our))
        ::    `state
        ::
        ::  NB: !! This includes our own ship, and for moons, this is
        ::  what has caused Jael to fetch our own rift from our parent.
        ::  This role may be taken by Ames's subscription to
        ::  %public-keys, but this must be tested before changing the
        ::  behavior here.
        ::
        =/  new-ships  (~(gas in *(set ship)) (saxo:title our now our))
        =/  removed  (~(dif in ships.state) new-ships)
        =/  added    (~(dif in new-ships) ships.state)
        ;<  new-state=_state  rind
          ?~  removed  `state
          [[%pass /jael %arvo %j %nuke removed]~ state]
        =.  state  new-state
        ::
        ;<  new-state=_state  rind
          ?~  added  `state
          [[%pass /jael %arvo %j %public-keys added]~ state]
        =.  state  new-state
        ::
        ::  Kick even if ships weren't added or removed
        ::
        (kick-pings our now new-ships)
      ::
      ::  Kick whenever we get a response.  We really care about
      ::  breaches and sponsorship changes.
      ::
      ::  Delay until next event in case of breach, so that ames can
      ::  clear its state.
      ::
      ++  take-jael
        |=  now=@da
        ^-  (quip card _state)
        [[%pass /jael/delay %arvo %b %wait now]~ state]
      ::
      ++  take-delay  kick
      --
    ::
    ::  Starts pinging a new set of `ships`.
    ::
    ++  kick-pings
      |=  [our=@p now=@da ships=(set ship)]
      ^-  (quip card _state)
      =:  nonce.state  +(nonce.state)
          ships.state  ships
        ==
      ::
      ?:  ?=(%nat -.plan.state)
        (kick:nat our)
      (kick:pub our now)
    ::
    ::  Subsystem for pinging our sponsors when we might be behind a NAT
    ::
    ::    Ping each ship every 25 seconds to keep the pinhole open.
    ::    This is expensive, but if you don't do it and you are behind a
    ::    NAT, you will stop receiving packets from other ships except
    ::    during the 30 seconds following each packet you send.
    ::
    ++  nat
      ?>  ?=(%nat -.plan.state)
      |%
      ++  rind  (^rind card state)
      ++  kick
        |=  our=@p
        ^-  (quip card _state)
        =/  ships  ~(tap in ships.state)
        |-  ^-  (quip card _state)
        ?~  ships  `state
        ?:  =(our i.ships)  $(ships t.ships)
        ;<  new-state=_state  rind  (send-ping i.ships)
        =.  state  new-state
        $(ships t.ships)
      ::
      ++  send-ping
        |=  =ship
        ^-  (quip card _state)
        :_  state
        =/  wire  /nat/(scot %uw nonce.state)/ping/(scot %p ship)
        [%pass wire %agent [ship %ping] %poke %noun !>(~)]~
      ::
      ++  take-ping
        |=  [now=@da =wire error=(unit tang)]
        ^-  (quip card _state)
        ?.  ?=([%nat @ %ping @ ~] wire)  `state
        ?.  (once i.t.wire)  `state
        =/  ship  (slav %p i.t.t.t.wire)
        %-  (slog ?~(error ~ ['ping: got nack' >ship< u.error]))
        :_  state
        =/  wire  /nat/(scot %uw nonce.state)/wait/(scot %p ship)
        [%pass wire %arvo %b %wait (add nat-timeout now)]~
      ::
      ++  take-wait
        |=  =wire
        ^-  (quip card _state)
        ?.  ?=([%nat @ %wait @ ~] wire)  `state
        ?.  (once i.t.wire)  `state
        =/  ship  (slav %p i.t.t.t.wire)
        (send-ping ship)
      --
    ::
    ::  Subsystem for pinging our sponsors when we know we're not behind a NAT
    ::
    ::    Check our IP address every minute, and only if it changes,
    ::    ping all our sponsors.
    ::
    ++  pub
      ?>  ?=(%pub -.plan.state)
      |%
      ++  rind  (^rind card state)
      ++  kick
        |=  [our=@p now=@da]
        ^-  (quip card _state)
        ;<  new-state=_state  rind  (send-pings our)
        =.  state  new-state
        ::
        ;<  new-state=_state  rind  check-ip
        =.  state  new-state
        ::
        (set-timer now)
      ::
      ++  send-pings
        |=  our=@p
        ^-  (quip card _state)
        :_  state
        %+  murn  ~(tap in ships.state)
        |=  =ship
        ?:  =(our ship)
          ~
        =/  wire  /pub/(scot %uw nonce.state)/ping/(scot %p ship)
        `u=[%pass wire %agent [ship %ping] %poke %noun !>(~)]
      ::
      ++  take-pings
        |=  [=wire error=(unit tang)]
        ^-  (quip card _state)
        ?.  ?=([%pub @ %ping @ ~] wire)  `state
        ?.  (once i.t.wire)  `state
        =/  ship  (slav %p i.t.t.t.wire)
        %-  (slog ?~(error ~ ['ping: got nack' >ship< u.error]))
        `state
      ::
      ++  check-ip
        ^-  (quip card _state)
        :_  state
        =/  wire  /pub/(scot %uw nonce.state)/ip
        =/  =request:http  [%'GET' ip-reflector ~ ~]
        [%pass wire %arvo %i %request request *outbound-config:iris]~
      ::
      ++  take-ip
        |=  [our=@p =wire resp=client-response:iris]
        ^-  (quip card _state)
        ?.  ?=([%pub @ %ip ~] wire)  `state
        ?.  (once i.t.wire)  `state
        ::
        ?.  ?=(%finished -.resp)  `state  :: will retry in a minute
        ?.  ?=(%200 status-code.response-header.resp)
          =*  s  status-code.response-header.resp
          %-  (slog leaf+"ping: ip check failed: {<s>}" ~)
          `state
        ::
        ?~  full-file.resp
          %-  (slog 'ping: ip check body empty' ~)
          `state
        ::
        =*  body  q.data.u.full-file.resp
        ?~  body
          %-  (slog 'ping: ip check body empty' ~)
          `state
        ::
        =/  ip  (end [3 (dec (met 3 body))] body)
        ?:  =(ip.plan.state `ip)  `state
        ::
        =.  ip.plan.state  `ip
        (send-pings our)
      ::
      ++  set-timer
        |=  now=@da
        ^-  (quip card _state)
        =/  =wire  /pub/(scot %uw nonce.state)/wait
        [[%pass wire %arvo %b %wait (add ip-timeout now)]~ state]
      ::
      ++  take-wait
        |=  [our=@p now=@da =wire]
        ^-  (quip card _state)
        ?.  ?=([%pub @ %wait ~] wire)  `state
        ?.  (once i.t.wire)  `state
        ;<  new-state=_state  rind  check-ip
        =.  state  new-state
        ::
        (set-timer now)
      --
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
  =.  plan.state  [%nat ~]
  =^  cards  state  (kick:ships our.bowl now.bowl)
  [cards this]
::
++  on-save   !>(state)
++  on-load
  |=  old-vase=vase
  |^
  =/  old  !<(state-any old-vase)
  =?  old  ?=(%0 -.old)  (state-0-to-1 old)
  ?>  ?=(%1 -.old)
  =.  state  old
  =^  cards  state  (kick:ships our.bowl now.bowl)
  [cards this]
  ::
  +$  state-any  $%(state-0 state-1)
  +$  state-0  [%0 ships=(map ship [=rift =ship-state])]
  ::
  ++  state-0-to-1
    |=  old=state-0
    ^-  state-1
    [%1 ~ 0 %nat ~]
  --
::  +on-poke: positively acknowledge pokes
::
++  on-poke
  |=  [=mark =vase]
  ?.  =(our src):bowl    :: don't crash, this is where pings are handled
    `this
  ::
  =^  cards  state
    ?:  =(q.vase %kick)  :: NB: ames calls this on %born
      (kick:ships our.bowl now.bowl)
    ?:  =(q.vase %nat)
      =.  plan.state  [%nat ~]
      (kick:ships our.bowl now.bowl)
    ?:  =(q.vase %no-nat)
      =.  plan.state  [%pub ~]
      (kick:ships our.bowl now.bowl)
    `state
  [cards this]
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
  =^  cards  state
    ?+    wire  `state
        [%nat *]
      ?.  ?=(%nat -.plan.state)  `state
      ?.  ?=(%poke-ack -.sign)   `state
      (take-ping:nat now.bowl wire p.sign)
    ::
        [%pub *]
      ?.  ?=(%pub -.plan.state)  `state
      ?.  ?=(%poke-ack -.sign)   `state
      (take-pings:pub wire p.sign)
    ==
  [cards this]
::  +on-arvo: handle timer firing
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  [(list card) _this]
  =^  cards  state
    ?+    wire  `state
        [%jael %delay ~]
      ?>  ?=(%wake +<.sign-arvo)
      ?^  error.sign-arvo
        %-  (slog 'ping: strange jael wake fail!' u.error.sign-arvo)
        `state
      (take-delay:ships our.bowl now.bowl)
    ::
        [%jael ~]
      ?>  ?=(%public-keys +<.sign-arvo)
      (take-jael:ships now.bowl)
    ::
        [%nat *]
      ?.  ?=(%nat -.plan.state)  `state
      ?>  ?=(%wake +<.sign-arvo)
      ?^  error.sign-arvo
        %-  (slog 'ping: strange nat wake fail!' u.error.sign-arvo)
        `state
      (take-wait:nat wire)
    ::
        [%pub @ %ip *]
      ?.  ?=(%pub -.plan.state)  `state
      ?>  ?=(%http-response +<.sign-arvo)
      (take-ip:pub our.bowl wire client-response.sign-arvo)
    ::
        [%pub @ %wait *]
      ?.  ?=(%pub -.plan.state)  `state
      ?>  ?=(%wake +<.sign-arvo)
      (take-wait:pub our.bowl now.bowl wire)
    ==
  [cards this]
::
++  on-fail   on-fail:def
--
