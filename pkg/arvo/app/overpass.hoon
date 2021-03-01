::  overpass: generating data to send to bridge
::
::    currently exposes the following endpoints over GET requests,
::    all prefixed with /~overpass:
::
::    /invite.json    array with 0 or 1 invite code, address & ship
::
/+  default-agent, verb, dbug, server
|%
+$  state-0  [%0 ~]
::
+$  eyre-id  @ta
+$  card     card:agent:gall
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    [%pass /eyre/connect %arvo %e %connect [~ /'~overpass'] dap.bowl]~
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?:  ?=([%http-response *] path)
      [~ this]
    (on-watch:def path)
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?.  ?=(%handle-http-request mark)
      (on-poke:def mark vase)
    :_  this
    =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
    ?.  authenticated.inbound-request
      %-  give-simple-payload:app:server
      =-  [eyre-id [[307 ['location' -]~] ~]]
      (crip "/~/login?redirect={(trip url.request.inbound-request)}")
    ?.  =('/~overpass/invite.json' url.request.inbound-request)
      %-  give-simple-payload:app:server
      [eyre-id [[404 ~] ~]]
    ?.  =(%'GET' method.request.inbound-request)
      %-  give-simple-payload:app:server
      [eyre-id [[405 ~] ~]]
    %+  fetch-invite:do  eyre-id
    'https://mainnet.infura.io/v3/2599df54929b47099bda360958d75aaf'  ::TODO
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?.  ?=([%fetch @ ~] wire)
      (on-agent:def wire sign)
    =*  eyre-id  i.t.wire
    :_  this
    ^-  (list card)
    ?-  -.sign
        %poke-ack
      ?~  p.sign  ~
      %-  (slog leaf+"{(trip dap.bowl)} couldn't start thread" u.p.sign)
      %-  give-simple-payload:app:server
      [eyre-id [[500 ~] ~]]
    ::
        %watch-ack
      ?~  p.sign  ~
      =/  =tank  leaf+"{(trip dap.bowl)} couldn't start listen to thread"
      %-  give-simple-payload:app:server
      [eyre-id [[500 ~] ~]]
    ::
        %kick
      ~
    ::
        %fact
      ?+  p.cage.sign  ~&([dap.bowl %unexpected-fact p.cage.sign] ~)
          %thread-fail
        =+  !<([=term =tang] q.cage.sign)
        %-  (slog leaf+"{(trip dap.bowl)} failed" leaf+<term> tang)
        %-  give-simple-payload:app:server
        [eyre-id [[500 ~] ~]]
      ::
          %thread-done
        (send-invite:do eyre-id q.cage.sign)
      ==
    ==
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  sign-arvo  (on-arvo:def wire sign-arvo)
        [%eyre %bound *]
      ?:  accepted.sign-arvo  [~ this]
      ~&  [dap.bowl %failed-to-bind path.binding.sign-arvo]
      [~ this]
    ==
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
++  fetch-invite
  |=  [=eyre-id node-url=@t]
  ^-  (list card)
  =/  ted=term
    %azimuth-delegated-sending-generate-invites
  =/  arg=vase
    !>([node-url 1 ~])
  |^  :~  (to-spider %watch /thread-result/[eyre-id])
          (to-spider %poke %spider-start !>([~ `eyre-id ted arg]))
      ==
  ++  to-spider
    |*  a=*
    [%pass /fetch/[eyre-id] %agent [our.bowl %spider] a]
  --
::
++  send-invite
  |=  [=eyre-id =vase]
  ^-  (list card)
  =;  =json
    %+  give-simple-payload:app:server
      eyre-id
    (json-response:gen:server json)
  =+  !<(invites=(list [=ship code=@q address=@ux]) vase)
  :-  %a
  %+  turn  (scag 1 invites)
  |=  [=ship code=@q address=@ux]
  ^-  json
  %-  pairs:enjs:format
  :~  'ship'^(ship:enjs:format ship)
      'code'^s+(crip (slag 2 (scow %q code)))
      'address'^s+(crip '0' 'x' ((x-co:co 40) address))
  ==
--
