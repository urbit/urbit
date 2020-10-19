
::  btc-provider.hoon
::  Proxy that serves a BTC full node and ElectRS address indexer
::
/-  *btc-provider, bnh=btc-node-hook
/+  dbug, default-agent, base64, blib=btc-node-json, elib=electrum-rpc-http
|%
+$  versioned-state
    $%  state-0
    ==
::
+$  state-0  [%0 =status whitelist=(set ship)]
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
  `this(status [*credentials connected=%.n clients=*(set ship)], whitelist *(set ship))
++  on-save
  ^-  vase
  !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  >  '%btc-provider recompiled successfully'
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
      (handle-command:hc !<(command vase))
        %btc-provider-rpc-action
      (handle-rpc-action !<(rpc-action vase))
    ==
  [cards this]
++  on-watch
  |=  pax=path
  ^-  (quip card _this)
  ~&  >>  pax
  `this
::  ?>  (is-whitelisted:hc src.bowl)
::  ~&  >  "added client {<src.bowl>}"
::  :_  this(clients.status (~(put in clients.status) src.bowl))
::  ~[[%give %fact ~ [%btc-provider-update !>([%status connected.status])]]]
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  =*  response  client-response.sign-arvo
  =^  cards  state
  ?+    +<.sign-arvo    (on-arvo:def wire sign-arvo)
      %http-response
    ?.  ?=(%finished -.response)
      `state
    =/  [status=@ud rpc-resp=response:rpc:jstd]
      [status-code.response-header.response (get-rpc-response:hc response)]
    ?+    wire  (on-arvo:def wire sign-arvo)
        [%erpc *]
      (electrum-http-response:hc status rpc-resp)
        [%brpc *]
      (btc-http-response:hc status rpc-resp)
    ==
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
    `state(status [creds.comm connected=%.y clients=*(set ship)])
    ::
      %whitelist-clients
    `state(whitelist (~(uni in whitelist) clients.comm))
  ==
++  is-whitelisted
  |=  user=ship  ^-  ?
  ?|  (~(has in whitelist) user)
      =(our.bowl user)
  ==
++  is-client
  |=  user=ship  ^-  ?
  (~(has in clients.status) user)
++  btc-rpc-auth-header
  =*  user  rpc-user.bc.creds.status
  =*  pass  rpc-password.bc.creds.status
  :-  'Authorization'
  ;:  (cury cat 3)
  'Basic '
  %-  ~(en base64 | &)
  (as-octs:mimes:html :((cury cat 3) user ':' pass))
  ==
++  btc-gen-request
  |=  req=request:bitcoin-core:rpc
  ^-  request:http
  =*  endpoint  rpc-url.bc.creds.status
  =/  body=request:rpc:jstd
    (request-to-rpc:btc-rpc:blib req)
  =/  =header-list:http
    :~  ['Content-Type' 'application/json']
        btc-rpc-auth-header
    ==
  :*  %'POST'
      endpoint
      header-list
      =,  html
      %-  some
      %-  as-octt:mimes
      (en-json (request-to-json:rpc:jstd body))
  ==
::
++  electrum-gen-request
  |=  req=request:electrum:rpc
  %+  request-to-http:electrum-rpc:elib
  rpc-url.ec.creds.status  req
::
++  gen-request
  |=  ract=rpc-action
  ^-  request:http
  ?-  -.ract
      %erpc
    (electrum-gen-request +.ract)
      %brpc
    (btc-gen-request +.ract)
  ==
::
++  handle-rpc-action
  |=  ract=rpc-action
  ^-  (quip card _state)
  =/  out-wire=path
    /[-.ract]/[(scot %da now.bowl)]
  =/  out  *outbound-config:iris
  =/  req=request:http
    (gen-request ract)
  :_  state
  [%pass out-wire %arvo %i %request req out]~
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
++  btc-http-response
  |=  [status=@ud rpc-resp=response:rpc:jstd]
  ^-  (quip card _state)
  ?.  ?=([%result *] rpc-resp)
    ~&  [%error +.rpc-resp]
    [~ state]
  ~&  >  (parse-response:btc-rpc:blib rpc-resp)
  [~ state]
::
++  electrum-http-response
  |=  [status=@ud rpc-resp=response:rpc:jstd]
  ^-  (quip card _state)
  ~&  >  rpc-resp
  `state
::
--
