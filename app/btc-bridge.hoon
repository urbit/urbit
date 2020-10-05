::  btc-bridge.hoon
::  Proxy for accessing BTC full node
::
/-  *btc-bridge, bnh=btc-node-hook
/+  dbug, default-agent
|%
+$  versioned-state
    $%  state-0
    ==
::
+$  state-0  [%0 =status]
::
+$  card  card:agent:gall
::
--
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
=<
|_  =bowl:gall
+*  this      .
    def   ~(. (default-agent this %|) bowl)
    hc    ~(. +> bowl)
::
++  on-init
  ^-  (quip card _this)
  ~&  >  '%btc-bridge initialized successfully'
  :-  ~
::  :-  ~[[%pass /btc-node-hook/[(scot %da now.bowl)] %agent [our.bowl %btc-node-hook] %watch /responses]]
  this(status [%client connected=%.n host=~])
++  on-save
  ^-  vase
  !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  >  '%btc-bridge recompiled successfully'
  `this(state !<(versioned-state old-state))
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ::  Only allow poke from our ship, unless we're a host
  ?>  ?|((team:title our.bowl src.bowl) ?=(%host -.status))
  =^  cards  state
    ?+  mark  (on-poke:def mark vase)
        %btc-bridge-command
      (handle-command:hc !<(command vase))
    ==
  [cards this]
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+  -.sign  (on-agent:def wire sign)
      %fact
    ?+  -.cage.sign  (on-agent:def wire sign)
        %btc-node-hook-response
      =/  resp=btc-node-hook-response:bnh
        !<(btc-node-hook-response:bnh +.cage.sign)
      ~&  >  resp
      `this
    ==
  ==
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
::  helper core
|_  =bowl:gall
++  handle-command
  |=  comm=command
  ^-  (quip card _state)
  ?-  -.comm
      %connect-as-host
      ::  TODO send a subscription to the node hook; update status in on-agent when ack'd
    `state
      %connect-as-client
      ::  TODO send a subscription to the btc-bridge host
      ::  update status in on-agent when ack'd by BTC-BRIDGE
    `state
      %allow-clients
    ?+  -.status  ~&(>>> 'Only a %host can add clients' `state)
        %host
      `state(clients.status (~(uni in clients.status) users.comm))
    ==
  ==
--
