::  btc-provider.hoon
::  Proxy that serves a BTC full node and ElectRS address indexer
::
::  Subscriptions: none
::  To Subscribers: /clients
::    current connection state
::    results/errors of RPC calls
::
/-  btc
/+  *btc-provider, dbug, default-agent
|%
+$  versioned-state
    $%  state-0
    ==
::
+$  state-0  [%0 =host-info whitelist=(set ship)]
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
  ~&  >  '%btc-provider initialized successfully'
  `this(host-info ['' connected=%.n clients=*(set ship)], whitelist *(set ship))
++  on-save
  ^-  vase
  !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  >  '%btc-provider recompiled successfully '
  `this(state !<(versioned-state old-state))
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  ?|((team:title our.bowl src.bowl) (is-client:hc src.bowl))
  =^  cards  state
    ?+  mark  (on-poke:def mark vase)
        %btc-provider-command
      ?>  (team:title our.bowl src.bowl)
      (handle-command:hc !<(command vase))
        %btc-provider-action
      (handle-action:hc !<(action vase))
    ==
  [cards this]
++  on-watch
  |=  pax=path
  ^-  (quip card _this)
  ?>  ?=([%clients *] pax)
  ?.  (is-whitelisted:hc src.bowl)
    ~&  >>>  "btc-provider: blocked client {<src.bowl>}"
    [~[[%give %kick ~ ~]] this]
  ~&  >  "btc-provider: added client {<src.bowl>}"
  :-  do-ping:hc
  this(clients.host-info (~(put in clients.host-info) src.bowl))
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ::  check for connectivity every 30 seconds
  ::
  ?:  ?=([%ping-timer *] wire)
    [do-ping:hc this]
  =^  cards  state
  ?+    +<.sign-arvo    (on-arvo:def wire sign-arvo)
      %http-response
      (handle-rpc-response:hc wire client-response.sign-arvo)
  ==
  [cards this]
::
++  on-fail   on-fail:def
--
::  helper core
|_  =bowl:gall
++  handle-command
  |=  comm=command
  ^-  (quip card _state)
  ?-  -.comm
      %set-credentials
    :-  do-ping
    state(host-info [api-url.comm connected=%.n clients=*(set ship)])
    ::
      %whitelist-clients
    `state(whitelist (~(uni in whitelist) clients.comm))
==
::  if not connected, only %ping action is allowed
::
++  handle-action
  |=  act=action
  ^-  (quip card _state)
  ?.  ?|(connected.host-info =(-.body.act %ping))
    ~&  >>>  "Not connected to RPC"
    [~[(send-update [%| %not-connected 500])] state]
  =/  ract=action:rpc
    ?-  -.body.act
        %address-info
      [%get-address-info address.body.act]
      ::
        %ping
      [%get-block-and-fee ~]
    ==
  [~[(req-card act ract)] state]
++  req-card
  |=  [act=action ract=action:rpc]
  =|  out=outbound-config:iris
  =/  req=request:http
    (gen-request host-info ract)
  [%pass (rpc-wire act) %arvo %i %request req out]
::  wire structure: /action-tas/req-id/now
::
++  rpc-wire
  |=  act=action  ^-  wire
  /[-.body.act]/[req-id.act]/[(scot %da now.bowl)]
::
++  get-req-id
  |=  =wire  ^-  req-id
  +<.wire
::  Handles HTTP responses from RPC servers. Parses for errors, then handles response. 
::  For actions that require collating multiple RPC calls, uses req-card to call out
::    to RPC again if more information is required.
::
++  handle-rpc-response
  |=  [=wire response=client-response:iris]
  ^-  (quip card _state)
  ?.  ?=(%finished -.response)  `state
  =*  status  status-code.response-header.response
  ::  handle error types: connection errors, RPC errors (in order)
  ::
  =^  conn-err  state
    (connection-error status)
  ?^  conn-err
    :_  state(connected.host-info %.n)
    ~[(send-status [%disconnected ~]) (send-update [%| u.conn-err])]
  =/  rpc-resp=response:rpc:jstd
    (get-rpc-response response)
  ?.  ?=([%result *] rpc-resp)
    [~[(send-update [%| [%rpc-error ~]])] state]
  ::  no error, switch on wire to handle RPC data
  ::
  =/  resp=response:rpc  (parse-response rpc-resp)
  ?+  wire  ~|("Unexpected HTTP response" !!)
      [%address-info @ *]
    ?>  ?=([%get-address-info *] resp)
    :_  state
    ~[(send-update [%& (get-req-id wire) %address-info +.resp])]
     ::
      [%ping @ *]
    ?>  ?=([%get-block-and-fee *] resp)
    :-  ~[(send-status [%connected blockcount.resp fee.resp])]
    state(connected.host-info %.y)
  ==
::
++  connection-error
  |=  status=@ud
  ^-  [(unit error) _state]
  ?+  status  [`[%http-error status] state]
      %200
    [~ state]
      %400
    [`[%bad-request status] state]
      %401
    [`[%no-auth status] state(connected.host-info %.n)]
      %502
    [`[%not-connected status] state(connected.host-info %.n)]
      %504
    [`[%not-connected status] state(connected.host-info %.n)]
  ==
::
++  send-status
  |=  =status  ^-  card
  [%give %fact ~[/clients] %btc-provider-status !>(status)]
++  send-update
  |=  =update
  ^-  card
  ~&  >>  "send-update: {<update>}"
  [%give %fact ~[/clients] %btc-provider-update !>(update)]
::
++  is-whitelisted
  |=  user=ship  ^-  ?
  ?|  (~(has in whitelist) user)
      =(our.bowl user)
  ==
::
++  is-client
  |=  user=ship  ^-  ?
  (~(has in clients.host-info) user)
::
++  start-ping-timer
  |=  interval=@dr  ^-  card
  [%pass /ping-timer %arvo %b %wait (add now.bowl interval)]
::
++  do-ping
  ^-  (list card)
  :~  :*  %pass  /ping/[(scot %da now.bowl)]  %agent
          [our.bowl %btc-provider]  %poke
          %btc-provider-action  !>([%blank-id %ping ~])
      ==
      (start-ping-timer ~s30)
  ==
--
