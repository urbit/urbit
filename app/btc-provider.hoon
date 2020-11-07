::  btc-provider.hoon
::  Proxy that serves a BTC full node and ElectRS address indexer
::
::  Subscriptions: none
::  To Subscribers:
::    current connection state
::    results/errors of RPC calls
::
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
  ::  ping every 30s to see whether connected to RPC
  ::
  :-  ~[(start-ping-timer:hc ~s30)]
  this(host-info [*credentials connected=%.n clients=*(set ship)], whitelist *(set ship))
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
  ::  Only allow clients/authorized to poke
  ::
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
  ?>  (is-whitelisted:hc src.bowl)
  ~&  >  "btc-provider: added client {<src.bowl>}"
  `this(clients.host-info (~(put in clients.host-info) src.bowl))
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ::  check for connectivity every 30 seconds
  ?:  ?=([%ping-timer *] wire)
    :_  this
    :~  :*  %pass  /ping/[(scot %da now.bowl)]  %agent
            [our.bowl %btc-provider]  %poke
            %btc-provider-action  !>([%ping ~])
        ==
        (start-ping-timer:hc ~s30)
    ==
  =^  cards  state
  ?+    +<.sign-arvo    (on-arvo:def wire sign-arvo)
      %http-response
      (handle-response:hc wire client-response.sign-arvo)
  ==
  [cards this]
::
++  on-fail   on-fail:def
--
::  helper core
|_  =bowl:gall
++  start-ping-timer
  |=  interval=@dr  ^-  card
  [%pass /ping-timer %arvo %b %wait (add now.bowl interval)]
++  handle-action
  |=  act=action
  |^  ^-  (quip card _state)
  ?:  ?=(%ping -.act)
    ~&  >>  "ping action"
    `state
  ?.  connected.host-info
    ~&  >>>  "Not connected to RPC"
    [~[(send-update [%| [%not-connected 500]])] state]
  =/  ract=action:rpc
    ?-  -.act
        %watch-address
      [%erpc %get-address-utxos address.act]
    ==
  [~[(req-card ract)] state]
  ++  req-card
    |=  ract=action:rpc
    =|  out=outbound-config:iris
    =/  req=request:http
      (gen-request host-info ract)
    [%pass /[-.act]/[-.ract]/[(scot %da now.bowl)] %arvo %i %request req out]
  --
++  handle-command
  |=  comm=command
  ^-  (quip card _state)
  ?-  -.comm
      %set-credentials
    `state(host-info [creds.comm connected=%.y clients=*(set ship)])
    ::
      %whitelist-clients
    `state(whitelist (~(uni in whitelist) clients.comm))
  ==
++  handle-response
  |=  [=wire response=client-response:iris]
  ^-  (quip card _state)
  ?.  ?=(%finished -.response)  `state
  =*  status  status-code.response-header.response
  ::  handle error types, in order:
  ::    - connection errors
  ::    - RPC errors
  ::
  =^  conn-err  state
    (connection-error status)
  ?^  conn-err
    ~&  >>>  conn-err
    [~[(send-update [%| u.conn-err])] state]
  =/  rpc-resp=response:rpc:jstd
    (get-rpc-response response)
  ?.  ?=([%result *] rpc-resp)
    [~[(send-update [%| [%rpc-error ~]])] state]
  ::  no error, so handle returned RPC data 
  ::
  ?+  wire  ~|("Unexpected HTTP response" !!)
      [%watch-address %erpc *]
    ~&  >>  +<.wire   ::  %brpc/%erpc 
    ~&  >  rpc-resp
    `state
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
      %502
    [`[%not-connected status] state(connected.host-info %.y)]
      %504
    [`[%not-connected status] state(connected.host-info %.n)]
  ==
::
++  send-update
  |=  =update  ^-  card
  [%give %fact [/clients]~ %btc-provider-update !>(update)]
::
++  is-whitelisted
  |=  user=ship  ^-  ?
  ?|  (~(has in whitelist) user)
      =(our.bowl user)
  ==
++  is-client
  |=  user=ship  ^-  ?
  (~(has in clients.host-info) user)
::
++  httr-to-rpc-response
  |=  hit=httr:eyre
  ^-  response:rpc:jstd
  ~|  hit
  =/  jon=json  (need (de-json:html q:(need r.hit)))
  ?.  =(%2 (div p.hit 100))
    (parse-rpc-error jon)
  =,  dejs-soft:format
  ^-  response:rpc:jstd
  =;  dere
    =+  res=((ar dere) jon)
    ?~  res  (need (dere jon))
    [%batch u.res]
  |=  jon=json
  ^-  (unit response:rpc:jstd)
  =/  res=[id=(unit @t) res=(unit json) err=(unit json)]
    %.  jon
    =,  dejs:format
    =-  (ou -)
    :~  ['id' (uf ~ (mu so))]
        ['result' (uf ~ (mu same))]
        ['error' (uf ~ (mu same))]
    ==
  ?:  ?=([^ * ~] res)
    `[%result [u.id.res ?~(res.res ~ u.res.res)]]
  ~|  jon
  `(parse-rpc-error jon)
::
++  get-rpc-response
  |=  response=client-response:iris
  ^-  response:rpc:jstd
  ?>  ?=(%finished -.response)
  %-  httr-to-rpc-response
    %+  to-httr:iris
      response-header.response
    full-file.response
::
++  parse-rpc-error
  |=  =json
  ^-  response:rpc:jstd
  :-  %error
  ?~  json  ['' '' '']
  %.  json
  =,  dejs:format
  =-  (ou -)
  :~  =-  ['id' (uf '' (cu - (mu so)))]
      |*(a=(unit) ?~(a '' u.a))
      :-  'error'
      =-  (uf ['' ''] -)
      =-  (cu |*(a=(unit) ?~(a ['' ''] u.a)) (mu (ou -)))
      :~  ['code' (uf '' no)]
          ['message' (uf '' so)]
  ==  ==
::
:: TODO: BELOW are deprecated. Rip out their functionality
++  btc-http-response
  |=  [status=@ud rpc-resp=response:rpc:jstd]
  ^-  (quip card _state)
  ?.  ?=([%result *] rpc-resp)
    ~&  [%error +.rpc-resp]
    [~ state]
::  ~&  >  (parse-response:btc-rpc:blib rpc-resp)
  [~ state]
::
++  electrum-http-response
  |=  [status=@ud rpc-resp=response:rpc:jstd]
  ^-  (quip card _state)
::  ~&  >>   (to-response (rpc-response [%erpc (parse-response:electrum-rpc:elib rpc-resp)]))
  `state
::
--
