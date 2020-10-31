::  btc-wallet-store.hoon
::  Manages wallet pubkeys
::
::  Subscriptions: none
::  To Subscribers:
::    watched address updates
::
/-  *btc-wallet-store
/+  dbug, default-agent, *btc-wallet-store, btc, bip32
|%
+$  versioned-state
    $%  state-0
    ==
::
+$  state-0
  $:  %0
      walts=(map xpub:btc walt)
      max-gap=@
  ==
::
+$  card  card:agent:gall
::
--
=|  state-0
=*  state  -
%-  agent:dbug
^-  agent:gall
=<
|_  =bowl:gall
+*  this      .
    def   ~(. (default-agent this %|) bowl)
    hc    ~(. +> bowl)
::
++  on-init
  ^-  (quip card _this)
  ~&  >  '%btc-wallet-store initialized'
  `this(state [%0 *(map tape walt) max-gap=20])
++  on-save
  ^-  vase
  !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  >  '%btc-wallet-store recompiled'
  `this(state !<(versioned-state old-state))
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
  ?+  mark  (on-poke:def mark vase)
      %btc-wallet-store-action
    (handle-action:hc !<(action vase))
  ==
  [cards this]
::
++  on-watch
  |=  pax=path
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  ?>  ?=([%wallets *] pax)
  `this
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
::
|_  =bowl:gall
++  handle-action
  |=  act=action
  ^-  (quip card _state)
  ?-  -.act
      %add-wallet
    `state
::    (add-wallet +.act)
    ::
      %update-address
    `state
    ::  (update-address +.act)
  ==
--
