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
    =/  w=_walt  (from-xpub:walt +.act)
    :-  ~[(force-scan xpub.act)]
    state(walts (~(put by walts) xpub.act w))
    ::
      %run-scan
    (run-scan +.act)
    ::
      %watch-address
    (watch-address +.act)
    ::
      %update-address
    `state
  ==
::
++  force-scan
  |=  =xpub  ^-  card
  :*  %pass   /[(scot %da now.bowl)]
      %agent  [our.bowl %btc-wallet-store]  %poke
      %btc-wallet-store-action  !>([%run-scan xpub])
  ==
::
++  req-scan
  |=  [b=batch =xpub =chyg]
  ^-  (list card)
  %+  turn  ~(tap in todo.b)
  |=  =idx
  :*  %give  %fact  ~[/requests]
      %btc-wallet-store-request
      !>([%scan-address xpub chyg idx])
  ==
::
++  scan-status
  |=  [=xpub =chyg]
  ^-  [empty=? done=?]
  =/  b=batch  (~(got by scans) [xpub chyg])
  :-  =(0 ~(wyt in todo.b))
  (~(scan-done (~(got by walts) xpub) chyg) endpoint.b)
::
++  insert-batches
  |=  [=xpub b0=batch b1=batch]
  ^-  ^scans
  =.  scans  (~(put by scans) [xpub %0] b0)
  (~(put by scans) [xpub %1] b1)
::
++  init-batches
  |=  =xpub
  ^-  (quip card _state)
  =/  w=_walt  (~(got by walts) xpub)
  =/  b=batch
    [(sy (gulf 0 (dec max-gap.st.w))) (dec max-gap.st.w)]
  :-  (weld (req-scan b xpub %0) (req-scan b xpub %1))
  state(scans (insert-batches xpub b b))
::  if the batch is done but the wallet isn't done scanning, returns new address requests and updated batch
::
++  bump-batch
  |=  [=xpub =chyg]
  ^-  (quip card batch)
  =/  b=batch  (~(got by scans) xpub chyg)
  =/  s  (scan-status xpub chyg)
  ?.  ?&(empty.s ?!(done.s))
    `b
  =/  w=_walt  (~(got by walts) xpub)
  =/  newb=batch
    :*  (sy (gulf +(endpoint.b) (add endpoint.b max-gap.st.w)))
        (add endpoint.b max-gap.st.w)
    ==
  :-  (req-scan newb xpub chyg)
  newb
::  delete the xpub from scans and set wallet to scanned
::
++  end-scan
  |=  [=xpub]
  ^-  _state
  =/  w=_walt  (~(got by walts) xpub)
  =.  scans  (~(del by scans) [xpub %0])
  =.  scans  (~(del by scans) [xpub %1])
  state(walts (~(put by walts) xpub w(scanned.st %.y)))
::  initiate a scan if one hasn't started
::  check status of scan if one is running
::
++  run-scan
  |=  =xpub
  ^-  (quip card _state)
  ?.  (~(has by scans) [xpub %0])
    (init-batches xpub)
  =/  s0  (scan-status xpub %0)
  =/  s1  (scan-status xpub %1)
  ?:  ?&(empty.s0 done.s0 empty.s1 done.s1)
    `(end-scan xpub)
  =/  [cards0=(list card) batch0=batch]
    (bump-batch xpub %0)
  =/  [cards1=(list card) batch1=batch]
    (bump-batch xpub %1)
  :-  (weld cards0 cards1)
  state(scans (insert-batches xpub batch0 batch1))
::  watch the address passed, update wallet if it's used
::  if this idx was the last in todo.scans, check whether scan is done
::
++  watch-address
  |=  [=xpub:btc =chyg =idx utxos=(set utxo) used=?]
  ^-  (quip card _state)
  ?.  (~(has by scans) [xpub chyg])  `state
  =/  w=_walt   (~(got by walts) xpub)
  =/  b=batch  (~(got by scans) [xpub chyg])
  =?  w  used
    %+  ~(watch-address w chyg)
      (~(mk-address w chyg) idx)
    [chyg idx utxos]
  ::  if todo is empty, force checking of scan *after* this update
  ::
  :-  ?:  empty:(scan-status xpub chyg)  ~[(force-scan xpub)]  ~
  %=  state
      walts  (~(put by walts) xpub w)
      scans  %+  ~(put by scans)
              [xpub chyg]
             b(todo (~(del in todo.b) idx))
  ==
--
