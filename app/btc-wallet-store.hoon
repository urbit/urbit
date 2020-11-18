::  btc-wallet-store.hoon
::  Manages wallet pubkeys
::
::  Subscribes to: none
::
::  Sends updates on:
::    - /requests: to request data about addresses
::    - /updates: new data about one of our addresses
::
::  Scrys
::  x/scanned: (list xpub) of all scanned wallets
::  x/balance/xpub: balance (in sats) of wallet
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
::  last-block: most recent block seen by the store
::
+$  state-0
  $:  %0
      walts=(map xpub:btc walt)
      =scans
      batch-size=@ud
      last-block=@ud
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
  `this(state [%0 *(map xpub:btc walt) *^scans max-gap:defaults 0])
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
  ?+  pax  (on-watch:def pax)
      [%requests *]
    :_  this
    %-  zing
    %~  val  by
    %-  ~(urn by scans)
      |*  [k=[=xpub:btc =chyg] b=batch]
      ^-  (list card)
      (req-scan ~ b xpub.k chyg.k)
    ::
      [%updates *]
    `this
  ==
++  on-peek
  |=  pax=path
  ^-  (unit (unit cage))
  ?+  pax  (on-peek:def pax)
      [%x %scanned ~]
    ``noun+!>(scanned-wallets)
    ::
      [%x %balance @ ~]
    ``noun+!>((balance:hc (xpub:btc +>-.pax)))
  ==
++  on-leave  on-leave:def
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
    =/  w=walt  (from-xpub +.act)
    =.  walts  (~(put by walts) xpub.act w)
    (init-batches xpub.act (dec max-gap.w))
    ::
      %address-info
    (update-address +.act)
    ::
      %generate-address
    =/  uw=(unit walt)  (~(get by walts) xpub.act)
    ?~  uw
      ~|("btc-wallet-store, %generate-address: non-existent wallet" !!)
    =/  [a=address:btc w=walt]
      ~(gen-address wad u.uw chyg.act)
    :_  state(walts (~(put by walts) xpub.act w))
    ~[[%give %fact ~[/updates] %btc-wallet-store-update !>([%generate-address a])]]
  ==
::  wallet scan algorithm:
::  Initiate a batch for each chyg, with max-gap idxs in it
::  Send that to /requests subscribers to call out to providers and get the info
::  Whenever a %watch-address result comes back
::    - remove that idx from todo.batch
::    - do run-scan to check whether that chyg is done
::    - if it isn't, refill it with idxs to scan
::
++  req-scan
  |=  [pax=(list path) b=batch =xpub =chyg]
  ^-  (list card)
  =/  w=walt  (~(got by walts) xpub)
  %+  turn  ~(tap in todo.b)
  |=  =idx
  :*  %give  %fact  pax
      %btc-wallet-store-request
      !>([%scan-address (~(mk-address wad w chyg) idx) xpub chyg idx])
  ==
::
++  scan-status
  |=  [=xpub =chyg]
  ^-  [empty=? done=?]
  =/  b=batch  (~(got by scans) [xpub chyg])
  =/  empty=?  =(0 ~(wyt in todo.b))
  :-  empty
  ?&(empty ?!(has-used.b))
::
++  insert-batches
  |=  [=xpub b0=batch b1=batch]
  ^-  ^scans
  =.  scans  (~(put by scans) [xpub %0] b0)
  (~(put by scans) [xpub %1] b1)
::
++  init-batches
  |=  [=xpub endpoint=idx]
  ^-  (quip card _state)
  =/  b=batch
    [(sy (gulf 0 endpoint)) endpoint %.n]
  :-  (weld (req-scan ~[/requests] b xpub %0) (req-scan ~[/requests] b xpub %1))
  state(scans (insert-batches xpub b b))
::  if the batch is done but the wallet isn't done scanning,
::  returns new address requests and updated batch
::
++  bump-batch
  |=  [=xpub =chyg]
  ^-  (quip card batch)
  =/  b=batch  (~(got by scans) xpub chyg)
  =/  s  (scan-status xpub chyg)
  ?.  ?&(empty.s ?!(done.s))
    `b
  =/  w=walt  (~(got by walts) xpub)
  =/  newb=batch
    :*  (sy (gulf +(endpoint.b) (add endpoint.b max-gap.w)))
        (add endpoint.b max-gap.w)
        %.n
    ==
  :-  (req-scan ~[/requests] newb xpub chyg)
  newb
::
++  iter-scan
  |=  [b=batch =xpub =chyg to-delete=idx]
  ^-  ^scans
  %+  ~(put by scans)  [xpub chyg]
  b(todo (~(del in todo.b) to-delete))
::  delete the xpub from scans and set wallet to scanned
::
++  end-scan
  |=  [=xpub]
  ^-  (quip card _state)
  =/  w=walt  (~(got by walts) xpub)
  =.  scans  (~(del by scans) [xpub %0])
  =.  scans  (~(del by scans) [xpub %1])
  :-  ~[[%give %fact ~[/updates] %btc-wallet-store-update !>([%scan-done xpub])]]
  state(walts (~(put by walts) xpub w(scanned %.y)))
::  initiate a scan if one hasn't started
::  check status of scan if one is running
::
++  run-scan
  |=  =xpub
  ^-  (quip card _state)
  =/  s0  (scan-status xpub %0)
  =/  s1  (scan-status xpub %1)
  ?:  ?&(empty.s0 done.s0 empty.s1 done.s1)
    (end-scan xpub)
  =/  [cards0=(list card) batch0=batch]
    (bump-batch xpub %0)
  =/  [cards1=(list card) batch1=batch]
    (bump-batch xpub %1)
  :-  (weld cards0 cards1)
  state(scans (insert-batches xpub batch0 batch1))
::  watch the address passed, update wallet if it's used
::  if this idx was the last in todo.scans, do run-scan to see whether scan is done
::  updates wallet-store state to have last-block
::
++  update-address
  |=  [=xpub:btc =chyg =idx utxos=(set utxo) used=? last-block=@ud]
  ^-  (quip card _state)
  =?  state  (gth last-block last-block.state)
    state(last-block last-block)
  =/  w=(unit walt)  (~(get by walts) xpub)
  ?~  w  `state
  =?  walts  used
    %+  ~(put by walts)  xpub
    %+  ~(update-address wad u.w chyg)
      (~(mk-address wad u.w chyg) idx)
    [chyg idx utxos]
  ::  if the wallet is being scanned, update the scan batch
  ::
  ?.  (~(has by scans) [xpub chyg])  `state
  =/  b=(unit batch)  (~(get by scans) [xpub chyg])
  ?~  b  `state
  =.  scans
    (iter-scan u.b(has-used ?|(used has-used.u.b)) xpub chyg idx)
  ?:  empty:(scan-status xpub chyg)
    (run-scan xpub)
  `state
::
++  scanned-wallets
  ^-  (list xpub)
  %+  murn  ~(tap by walts)
  |=  [=xpub:btc w=walt]
  ^-  (unit xpub:btc)
  ?:  scanned.w  `xpub  ~
::
++  balance
  |=  =xpub:btc
  ^-  sats:btc
  =/  w  (~(get by walts) xpub)
  ?~  w  ~|("btc-wallet-store: non-existent xpub" !!)
  =/  values=(list sats:btc)
    %+  turn  ~(val by wach.u.w)
    |=  =addi  ^-  sats:btc
    %+  roll
      %+  turn  ~(tap by utxos.addi)
      |=(=utxo:btc value.utxo)
    add
  (roll values add)
--
