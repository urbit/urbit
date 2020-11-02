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
::  walts: all wallets, keyed by their xpubs
::  scans: batch info for wallets being scanned
::  batch-size: how many addresses to send out at once for checking
::
+$  state-0
  $:  %0
      walts=(map xpub:btc _walt)
      =scans
      batch-size=@
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
  `this(state [%0 *(map xpub:btc _walt) *^scans default-max-gap])
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
    ::
      %scan
    `state
    ::
      %watch-address
      ::  [%watch-address =xpub =chyg =idx utxos=(set utxo) used=?]
    (watch-address +.act)
    ::
      %update-address
    `state
  ==
::
++  scan-action
  |=  =xpub  ^-  card
  :*  %pass   /[(scot %da now.bowl)]
      %agent  [our.bowl %btc-wallet-store]  %poke
      %btc-wallet-store-action  !>([%scan xpub])
  ==
::
++  do-scan  2
::  watch the address passed
::  update wallet if it's used
::  if this idx was the last in todo.scans, 
::
++  watch-address
  |=  [=xpub:btc =chyg =idx utxos=(set utxo) used=?]
  ^-  (quip card _state)
  ?.  (~(has by scans) [xpub chyg])  `state
  =/  w=_walt   (~(got by walts) xpub)
  =/  s=scan-batch  (~(got by scans) [xpub chyg])
  =?  w  used
    %+  ~(watch-address w chyg)
      (~(mk-address w chyg) idx)
    [chyg idx utxos]
  ::  if todo is empty, either scan a new batch or finish
  ::
  :-  ?:(=(0 ~(wyt in todo.s)) ~[(scan-action xpub)] ~)
  %=  state
      walts  (~(put by walts) xpub w)
      scans  %+  ~(put by scans)
              [xpub chyg]
             s(todo (~(del in todo.s) idx))
  ==
--
