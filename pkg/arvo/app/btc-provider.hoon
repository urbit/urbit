::  btc-provider.hoon
::  Proxy that serves a BTC full node and ElectRS address indexer
::
::  Subscriptions: none
::  To Subscribers: /clients
::    current connection state
::    results/errors of RPC calls
::
::  Scrys
::  x/is-whitelisted/SHIP: bool, whether ship is whitelisted
::
/-  *bitcoin, json-rpc, *btc-provider
/+  dbug, default-agent, bl=btc, groupl=group, resource
|%
+$  versioned-state
    $%  state-0
    ==
::
+$  state-0  [%0 =host-info =whitelist]
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
  =|  wl=^whitelist
  :-  ~
  %_  this
      host-info
    ['' connected=%.n %main block=0 clients=*(set ship)]
      whitelist  wl(public %.n, kids %.n)
  ==
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  >  '%btc-provider recompiled successfully '
  `this(state !<(versioned-state old-state))
::
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
::
++  on-watch
  |=  pax=path
  ^-  (quip card _this)
  ::  checking provider permissions before trying to subscribe
  ::  terrible hack until we have cross-ship scries
  ::
  ?:  ?=([%permitted @ ~] pax)
    :_  this
    =/  jon=json
      %+  frond:enjs:format
        %'providerStatus'
      %-  pairs:enjs:format
      :~  provider+s+(scot %p our.bowl)
          permitted+b+(is-whitelisted:hc src.bowl)
      ==
    [%give %fact ~ %json !>(jon)]~
  ::
  ?>  ?=([%clients *] pax)
  ?.  (is-whitelisted:hc src.bowl)
    ~&  >>>  "btc-provider: blocked client {<src.bowl>}"
    [~[[%give %kick ~ ~]] this]
  ~&  >  "btc-provider: accepted client {<src.bowl>}"
  :-  do-ping:hc
  this(clients.host-info (~(put in clients.host-info) src.bowl))
::
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
++  on-peek
  |=  pax=path
  ^-  (unit (unit cage))
  ?+  pax  (on-peek:def pax)
      [%x %is-whitelisted @t ~]
    ``noun+!>((is-whitelisted:hc (ship (slav %p +>-.pax))))
    ::
      [%x %is-client @t ~]
    ``noun+!>((is-client (ship (slav %p +>-.pax))))
==
::
++  on-leave  on-leave:def
++  on-agent  on-agent:def
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
    state(host-info [api-url.comm connected=%.n network.comm block=0 clients=*(set ship)])
    ::
      %add-whitelist
    ?-  -.wt.comm
        %public
      `state(public.whitelist %.y)
      ::
        %kids
      `state(kids.whitelist %.y)
      ::
        %users
      `state(users.whitelist (~(uni in users.whitelist) users.wt.comm))
      ::
        %groups
      `state(groups.whitelist (~(uni in groups.whitelist) groups.wt.comm))
    ==
    :: 
      %remove-whitelist
    =.  state
      ?-  -.wt.comm
          %public
        state(public.whitelist %.n)
        ::
          %kids
        state(kids.whitelist %.n)
        ::
          %users
        state(users.whitelist (~(dif in users.whitelist) users.wt.comm))
        ::
          %groups
        state(groups.whitelist (~(dif in groups.whitelist) groups.wt.comm))
      ==
    clean-client-list
  ==
::  if not connected, only %ping action is allowed
::
++  handle-action
  |=  act=action
  ^-  (quip card _state)
  ?.  ?|(connected.host-info ?=(%ping -.act))
    ~&  >>>  "Not connected to RPC"
    [~[(send-update [%| %not-connected 500])] state]
  ::
  =/  ract=action:rpc-types
    ?-  -.act  ::  ~|("Invalid action" !!)
        %address-info
      [%get-address-info address.act]
      ::
        %tx-info
      [%get-tx-vals txid.act]
      ::
        %raw-tx
      [%get-raw-tx txid.act]
      ::
        %broadcast-tx
      [%broadcast-tx rawtx.act]
      ::
        %ping
      [%get-block-info ~]
    ==
  [~[(req-card act ract)] state]
::
++  req-card
  |=  [act=action ract=action:rpc-types]
  =|  out=outbound-config:iris
  =/  req=request:http
    (gen-request:bl host-info ract)
  [%pass (rpc-wire act) %arvo %i %request req out]
::  wire structure: /action-tas/now
::
++  rpc-wire
  |=  act=action  ^-  wire
  /[-.act]/[(scot %ux (cut 3 [0 20] eny.bowl))]
::
++  kick-client
  |=  client=ship
  ^-  (quip card _state)
  ~&  >>>  "dropping client {<client>}"
  :-  ~[[%give %kick ~[/clients] `client]]
  state(clients.host-info (~(dif in clients.host-info) (silt ~[client])))
::
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
  ::
  %+  handle-rpc-result  wire
  %-  parse-result:rpc:bl
  (get-rpc-response:bl response)
::
++  connection-error
  |=  status=@ud
  ^-  [(unit error) _state]
  ?+  status  [`[%rpc-error ~] state]
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
++  handle-rpc-result
  |=  [=wire r=result:rpc-types]
  ^-  (quip card _state)
  ?+  -.wire  ~|("Unexpected HTTP response" !!)
      %address-info
    ?>  ?=([%get-address-info *] r)
    :_  state
    ~[(send-update [%.y %address-info +.r])]
    ::
      %tx-info
    ?>  ?=([%get-tx-vals *] r)
    :_  state
    ~[(send-update [%.y %tx-info +.r])]
    ::
      %raw-tx
    ?>  ?=([%get-raw-tx *] r)
    :_  state
    ~[(send-update [%.y %raw-tx +.r])] 
    ::
      %broadcast-tx
    ?>  ?=([%broadcast-tx *] r)
    :_  state
    ~[(send-update [%.y %broadcast-tx +.r])]
    ::
      %ping
    ?>  ?=([%get-block-info *] r)
    :_  state(connected.host-info %.y, block.host-info block.r)
    ?:  =(block.host-info block.r)
      ~[(send-status [%connected network.host-info block.r fee.r])]
    ~[(send-status [%new-block network.host-info block.r fee.r blockhash.r blockfilter.r])]
  ==
::
++  send-status
  |=  =status  ^-  card
  %-  ?:  ?=(%new-block -.status)
        ~&(>> "%new-block: {<block.status>}" same)
      same
  [%give %fact ~[/clients] %btc-provider-status !>(status)]
::
++  send-update
  |=  =update
  ^-  card
  =+  c=[%give %fact ~[/clients] %btc-provider-update !>(update)]
  ?:  ?=(%.y -.update)
::    ~&  >>  "prov. update: {<p.update>}"
    c
  ~&   >>  "prov. err: {<p.update>}"
  c
::
++  is-whitelisted
  |=  user=ship  ^-  ?
  |^
  ?|  public.whitelist
      =(our.bowl user)
      ?&(kids.whitelist is-kid)
      (~(has in users.whitelist) user)
      in-group
  ==
  ++  is-kid
    =(our.bowl (sein:title our.bowl now.bowl user))
  ++  in-group
    =/  gs  ~(tap in groups.whitelist)
    |-
    ?~  gs  %.n
    ?:  (~(is-member groupl bowl) user i.gs)
      %.y
    $(gs t.gs)
    ::  .^((unit group:g) %gx ;:(weld /=group-store=/groups p /noun))
  --
::  +clean-client-list: remove clients who are no longer whitelisted
::   called after a whitelist change
::
++  clean-client-list
  ^-  (quip card _state)
  =/  to-kick=(set ship)
    %-  silt
    %+  murn  ~(tap in clients.host-info)
    |=  c=ship  ^-  (unit ship)
    ?:((is-whitelisted c) ~ `c)
  :_  state(clients.host-info (~(dif in clients.host-info) to-kick))
  %+  turn  ~(tap in to-kick)
  |=(c=ship [%give %kick ~[/clients] `c]) 
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
  =/  act=action  [%ping ~]
  :~  :*  %pass  /ping/[(scot %da now.bowl)]  %agent
          [our.bowl %btc-provider]  %poke
          %btc-provider-action  !>(act)
      ==
      (start-ping-timer ~s30)
  ==
--
