::  btc-wallet-store.hoon
::  Manages wallet pubkeys
::
::  Subscriptions: none
::  To Subscribers:
::    watched address updates
::
/-  *btc-wallet-store
/+  dbug, default-agent, bip32, btc
|%
+$  versioned-state
    $%  state-0
    ==
::
+$  state-0  [%0 wallets=(map tape walt)]
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
  `this
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
++  on-watch  on-watch:def
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
    (add-wallet xpub.act)
  ==
++  add-wallet
  |=  xpub=tape
  ^-  (quip card _state)
  ?:  (~(has by wallets.state) xpub)
    ~&  >>>  "xpub already imported"
    `state
  =/  wallet=walt
    :*  (from-extended:bip32 xpub)
        (xpub-type:btc xpub)
        [%.n 0 *wach]
        [%.n 0 *wach]
    ==
  `state(wallets (~(put by wallets.state) xpub wallet))
--
