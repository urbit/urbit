::  btc-wallet-hook.hoon
::
::  Subscribes to:
::    btc-provider:
::      - connection status
::      - RPC call results/errors
::
::    btc-wallet-store
::      - requests for address info
::      - updates to existing address info
::
::  Sends updates to:
::    none
::
/-  *btc, *btc-wallet-hook, bws=btc-wallet-store, bp=btc-provider
/+  shoe, dbug, default-agent, bwsl=btc-wallet-store
|%
+$  versioned-state
    $%  state-0
    ==
::  provdider: maybe ship if provider is set
::
+$  state-0
  $:  %0
      provider=(unit [host=ship connected=?])
      pend=back
  ==
::
+$  card  card:shoe
+$  command
  $?  %add-xpub
  ==
::
--
=|  state-0
=*  state  -
%-  agent:dbug
^-  agent:gall
%-  (agent:shoe command)
^-  (shoe:shoe command)
=<
|_  =bowl:gall
+*  this      .
    des   ~(. (default:shoe this command) bowl)
    def   ~(. (default-agent this %|) bowl)
    hc    ~(. +> bowl)
::
++  command-parser  command-parser:des
++  tab-list  tab-list:des
++  can-connect  can-connect:des
++  on-command  on-command:des
++  on-connect  on-connect:des
++  on-disconnect  on-disconnect:des
::
++  on-init
  ^-  (quip card _this)
  ~&  >  '%btc-wallet-hook initialized'
  :_  this
  :~  [%pass /r/[(scot %da now.bowl)] %agent [our.bowl %btc-wallet-store] %watch /requests]
      [%pass /u/[(scot %da now.bowl)] %agent [our.bowl %btc-wallet-store] %watch /updates]
  ==
++  on-save
  ^-  vase
  !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  >  '%btc-wallet-hook recompiled'
  `this(state !<(versioned-state old-state))
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state
  ?+  mark  (on-poke:def mark vase)
      %btc-wallet-hook-action
    (handle-action:hc !<(action vase))
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
      %kick
    ?~  provider  `this
    ?:  ?&  ?=(%set-provider -.wire)
            =(host.u.provider src.bowl)
        ==
      `this(provider ~)
    `this
    ::
      %fact
    =^  cards  state
      ?+  p.cage.sign  `state
          %btc-provider-status
        (handle-provider-status:hc !<(status:bp q.cage.sign))
        ::
          %btc-provider-update
        (handle-provider-update:hc !<(update:bp q.cage.sign))
        ::
          %btc-wallet-store-request
        (handle-request:hc !<(request:bws q.cage.sign))
      ==
    [cards this]
  ==
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
|_  =bowl:gall
++  handle-command
  |=  comm=command
  ^-  (quip card _state)
  ~&  >  comm
  `state
::
++  handle-action
  |=  act=action
  ^-  (quip card _state)
  ?-  -.act
      %set-provider
    =*  sub-card
      [%pass /set-provider %agent [provider.act %btc-provider] %watch /clients]
    :_  state(provider [~ provider.act %.n])
    ?~  provider  ~[sub-card]
    :~  [%pass /set-provider %agent [host.u.provider %btc-provider] %leave ~]
        sub-card
    ==
    ::
      %force-retry
    [(retry pend) state]
  ==
::  if status is %connected, retry all pending address lookups
::
++  handle-provider-status
  |=  s=status:bp
  ^-  (quip card _state)
  ?~  provider  `state
  ?.  =(host.u.provider src.bowl)  `state
  ?-  s
      %connected
    ::  only retry if previously disconnected
    :-  ?:(connected.u.provider ~ (retry pend))
    state(provider `[host.u.provider %.y])
      %disconnected
    `state(provider `[host.u.provider %.n])
  ==
::
++  handle-provider-update
  |=  =update:bp
  |^  ^-  (quip card _state)
  ?.  ?=(%& -.update)  `state
  ?-  -.body.p.update
      %address-info
    =/  ureq  (~(get by pend) req-id.p.update)
    ?~  ureq  `state
    :_  state(pend (~(del by pend) req-id.p.update))
    :~  %-  poke-store
        :*  %watch-address  xpub.u.ureq  chyg.u.ureq  idx.u.ureq
            utxos.body.p.update  used.body.p.update
        ==
    ==
  ==
  ++  poke-store
    |=  act=action:bws  ^-  card
    :*  %pass  /[(scot %da now.bowl)]  %agent
        [our.bowl %btc-wallet-store]  %poke
        %btc-wallet-store-action  !>(act)
    ==
  --
::
++  handle-request
  |=  req=request:bws
  ^-  (quip card _state)
  ?-  -.req
      %scan-address
    =/  ri=req-id:bp  (mk-req-id (hash-xpub:bwsl +>.req))
    :_  state(pend (~(put by pend) ri req))
    ?~  provider  ~
    ?:  provider-connected
      ~[(get-address-info ri host.u.provider a.req)]
    ~&  >  "provider not connected"
    ~
  ==
::
++  retry
  |=  =back
  ^-  (list card)
  ?~  provider  ~|("provider not set" !!)
  %+  turn  ~(tap by back)
  |=  [ri=req-id:bp req=request:bws]
  (get-address-info ri host.u.provider a.req)
::
++  get-address-info
  |=  [ri=req-id:bp host=ship a=address]  ^-  card
  :*  %pass  /[(scot %da now.bowl)]  %agent  [host %btc-provider]
      %poke  %btc-provider-action  !>([ri %address-info a])
  ==
++  mk-req-id
  |=  hash=@ux  ^-  req-id:bp
  (scot %ux hash)
++  provider-connected
  ^-  ?
  ?~  provider  %.n
  connected.u.provider
--
