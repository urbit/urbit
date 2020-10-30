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
+$  state-0
  $:  %0
      walts=(map tape walt)
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
    (add-wallet +.act)
    ::
      %update-address
    (update-address +.act)
  ==
++  update-address
  |=  [a=address us=(set utxo)]
  ^-  (quip card _state)
  =/  xpubs=(list tape)
    %~  tap  in
    ~(key by walts.state)
  |-  ?~  xpubs  `state
  =/  w=walt  (~(got by walts.state) i.xpubs)
  ?:  (~(has by wach.w) a)
    %:  send-address-update
        i.xpubs
        (update-wallet w a us)
        a
        us
    ==
  $(xpubs t.xpubs)
::
++  update-wallet
  |=  [w=walt a=address us=(set utxo)]
  ^-  walt
  =/  curr-addi=addi
    (~(got by wach.w) a)
  w(wach (~(put by wach.w) a curr-addi(used %.y, utxos us)))
::
++  send-address-update
  |=  [xpub=tape =walt a=address us=(set utxo)]
  ^-  (quip card _state)
  :_  state(walts (~(put by walts.state) xpub walt))
  ~[[%give %fact ~[/wallets] %btc-wallet-store-update !>([%address a us])]]
::
++  add-wallet
  |=  [xpub=tape scan-to=(unit scon) max-gap=(unit @)]
  ^-  (quip card _state)
  ?:  (~(has by walts.state) xpub)
    ~&  >>>  "xpub already imported"
    `state
  =/  wallet=walt
    :*  (from-extended:bip32 xpub)
        (xpub-type:btc xpub)
        *wach
        [0 0]
        %.n
        (fall scan-to *scon)
        (fall max-gap max-gap.state)
    ==
  `state(walts (~(put by walts.state) xpub wallet))
--
