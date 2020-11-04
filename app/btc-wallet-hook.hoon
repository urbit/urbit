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
/-  *btc, *btc-wallet-hook, bws=btc-wallet-store
/+  shoe, dbug, default-agent, lib-bws=btc-wallet-store
|%
+$  versioned-state
    $%  state-0
    ==
::  provdider: maybe ship if provider is set
::
+$  state-0
  $:  %0
      provider=(unit [host=ship works=?])
      pend=back
      fail=back
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
      %watch-ack
    ?:  ?=(%set-provider -.wire)
      `this(provider.state `[src.bowl %.y])
    `this
      %fact
    =^  cards  state
      ?+  p.cage.sign  `state
          %btc-provider-response
        `state
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
::  ==
++  handle-action
  |=  act=action
  ^-  (quip card _state)
  ?-  -.act
      %set-provider
    =*  sub-card
      [%pass /set-prov %agent [provider.act %btc-provider] %watch /clients]
    :_  state
    ?~  provider  ~[sub-card]
    :~  [%pass /leave-prov %agent [host.u.provider %btc-provider] %leave ~]
        sub-card
    ==
  ==
++  handle-request
  |=  req=request:bws
  ^-  (quip card _state)
  ?-  -.req
      %scan-address
    ?~  provider
      ~|("provider not set" !!)
    =/  ri=req-id  (mk-req-id +>.req)
    :-  ~[(get-address-info host.u.provider a.req)]
    state(pend (~(put by pend) ri +>.req))
  ==
::
++  get-address-info
  |=  [host=ship a=address]  ^-  card
  :*  %pass  /[(scot %da now.bowl)]  %agent  [host %btc-provider]
      %poke  %btc-provider-action  !>([%get-address-info a])
  ==
::
++  mk-req-id
  |=  [=xpub =chyg:bws =idx:bws]  ^-  req-id
  =/  chygidx=@  (cat 3 ?:(=(%0 chyg) '0' '1') idx)
  =/  dat=@  (cat 3 xpub chygidx)

  %-  ripemd-160:ripemd:crypto
  [(met 3 dat) dat]
--
