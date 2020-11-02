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
  `this(host-info [*credentials connected=%.n clients=*(set ship)], whitelist *(set ship))
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
    ?:  ?=(%btc-provider-command mark)
    ?>  (team:title our.bowl src.bowl)
      (handle-command:hc !<(command vase))
    ?+  mark  (on-poke:def mark vase)
        %btc-provider-action
      ?.  connected.host-info
        ~|("Not connected to RPC endpoints" !!)
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
++  handle-action
  |=  act=action
  |^  ^-  (quip card _state)
  =/  ract=action:rpc
    ?-  -.act
      %watch-address
      [%erpc %get-address-utxos address.act]
    ==
  [~[(req-card act ract)] state]
  ++  req-card
    |=  [act=action ract=action:rpc]
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
  :: IMPORTANT:  whatever we make here gets sent out to subscribers at the end
  ^-  (quip card _state)
  ?.  ?=(%finished -.response)  `state
  =/  e=(unit error)  (check-connection status-code.response-header.response)
  ~&  >  "before"
  ?^  e
    :_  state(connected.host-info %.n)
    ~[(send-update [%| u.e])]
  ~&  >  "after"
  =/  rpc-resp=response:rpc:jstd
    (get-rpc-response response)
  ::  TODO: error handling goes here
  ?+  wire  ~|("Unexpected HTTP response" !!)
      [%get-address-info %erpc *]
    ~&  >>  +<.wire
    ~&  >  rpc-resp
    `state
  ==
++  check-connection
  |=  status=@ud
  ^-  (unit error)
  ?:  =(504 status)
    `[%not-connected ~]
  ~
++  send-update
  |=  =update  ^-  card
  [%give %fact [/clients]~ %btc-provider-update !>(update)]
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
    (parse-error jon)
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
  `(parse-error jon)
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
++  parse-error
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
