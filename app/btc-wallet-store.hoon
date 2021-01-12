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
++  req-pax  /requests
+$  versioned-state
    $%  state-0
    ==
::  walts: all wallets, keyed by their xpubs
::  scans: batch info for wallets being scanned
::  gena:  generated addresses that haven't had activity yet
::  batch-size: how many addresses to send out at once for checking
::  last-block: most recent block seen by the store
::
+$  state-0
  $:  %0
      walts=(map xpub:btc walt)
      =scans
      batch-size=@ud
      last-block=@ud
      =history
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
  `this(state [%0 *(map xpub:btc walt) *^scans max-gap:defaults 0 *^history])
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
    ::  TODO
    :: if blank address we're watching gets a value
    :: "blank" = unused
    :: send a %tx-info request--txinfo handles history stuff
    ::
      %address-info
    (update-address +.act)
    ::
      %tx-info
    (handle-tx-info +.act)
    ::
      %generate-address
    (generate-address +.act)
    ::  %generate-txbu
    ::  - get txbu and change amount
    ::  - if txbu is blank, fail
    ::  - if change is blank, send txbu as update
    ::  - if change:
    ::    - generate new change address
    ::    - add that address+change value to the txbu
    ::    - send txbu update
    ::    - send a request for info on the address (watch it)
    ::    - DON'T send an address update for the address, since it's change
    ::
      %generate-txbu
    =+  uw=(~(get by walts) xpub.act)
    ?~  uw
      ~|("btc-wallet-store: non-existent xpub" !!)
    ?.  scanned.u.uw
    ~|("btc-wallet-store: wallet not scanned yet" !!)
    =/  [tb=(unit txbu) chng=(unit sats)]
      %~  with-change  sut
      [u.uw eny.bowl last-block payee.act feyb.act txos.act]
    ?~  tb  ~&(>>> "btc-wallet-store: insufficient balance" `state)
    ::  if no change, just return txbu
    ::
    ?~  chng
      [~[(send-update [%generate-txbu xpub.act u.tb])] state]
    =/  [addr=address:btc =idx w=walt]
      ~(nixt-address wad u.uw %1)
    =/  new-txbu=txbu
      (~(add-output txb u.tb) addr u.chng `(~(hdkey wad w %1) idx))
    :_  state(walts (~(put by walts) xpub.act w))
    :~  (send-update [%generate-txbu xpub.act new-txbu])
        %+  send-req  ~[req-pax]
          :*  %address-info  last-block
              addr  xpub.act  %1  idx
          ==
    ==
    ::
      %add-history-entry
    :_  state(history (~(put by history) txid.hest.act hest.act))
    ~[(send-req ~[req-pax] [%tx-info last-block txid.hest.act])]
    ::
      %del-history-entry
    :_  state(history (~(del by history) txid.act))
    ~[(send-req ~[req-pax] [%tx-info last-block txid.act])]
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
  =/  req=request
    :*  %address-info  last-block=1
        (~(mk-address wad w chyg) idx)
        xpub  chyg  idx
    ==
  (send-req pax req)
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
  :-  %+  weld
        (req-scan ~[req-pax] b xpub %0)
      (req-scan ~[req-pax] b xpub %1)
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
  :-  (req-scan ~[req-pax] newb xpub chyg)
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
::  +update-address: watch the address passed;
::  - update wallet with the address
::  - if address is unused, send %address-info request
::  - if address doesn't have enough confs, send %address-info request
::  - if this idx was the last in todo.scans, do run-scan to see whether scan is done
::  - updates wallet-store state to have last-block
::
++  update-address
  |=  [=xpub:btc =chyg =idx utxos=(set utxo) used=? last-block=@ud]
  |^  ^-  (quip card _state)
  =?  state  (gth last-block last-block.state)
    state(last-block last-block)
  =/  w=(unit walt)  (~(get by walts) xpub)
  ?~  w  `state
  =.  walts
    %+  ~(put by walts)  xpub
    %+  ~(update-address wad u.w chyg)
      (~(mk-address wad u.w chyg) idx)
    [used chyg idx utxos]
  ::  if the wallet is being scanned, update the scan batch
  ::  if not, just get more-info for the address if still being scanned
  ::
  =+  b=(~(get by scans) [xpub chyg])
  ?~  b  [(more-info u.w) state]
  =.  scans
    (iter-scan u.b(has-used ?|(used has-used.u.b)) xpub chyg idx)
  ?:  empty:(scan-status xpub chyg)
    =^  cards  state  (run-scan xpub)
    [(weld (more-info u.w) cards) state]
  [(more-info u.w) state]
  ::
  ++  more-info
    |=  w=walt
    ^-  (list card)
    ?:  (is-done w)  ~
    :~
      %+  send-req  ~[req-pax]
      :*  %address-info  last-block
          (~(mk-address wad w chyg) idx)
          xpub  chyg  idx
      ==
    ==
  ::
  ++  is-done
    |=  w=walt
    ?&  used
        %+  levy  (turn ~(tap in utxos) (cury num-confs last-block))
          |=(nc=@ud (gte nc confs:w))
    ==
  --
::
::  -if txid not in history but has one of our wallet addresses
::   - add it to history and request info on the addresses+tx
::  - if txid not "included" in blockchain AND was in history
::   - ("included" = in mempool or blockchain)
::   - delete from history
::   - send txinfo request again
::  - check whether this txid is in history
::   - if yes, update its confs and received
::   - request info on all its addresses
::   - request info on the tx again if not enough confs
::
++  handle-tx-info
  |=  [ti=info:tx:btc block=@ud]
  |^
  =.  state  state(last-block block)
  =+  h=(~(get by history) txid.ti)
  =/  rs=(list request)  (address-reqs ti)
  =/  cards=(list card)  (turn rs to-card)
  ::  when addresses in wallets, but tx not in history
  ::
  ?~  h
    ?~  rs  `state
    :-  [(send-req ~[req-pax] [%tx-info block txid.ti]) cards]
    state(history (~(put by history) txid.ti (mk-hest rs)))
  ?.  included.ti
    :_  state(history (~(del by history) txid.ti))
    ~[(send-req ~[req-pax] [%tx-info block txid.ti])]
  =+  w=(~(get by walts) xpub.u.h)
  ?~  w  `state
  =.  history
    %+  ~(put by history)  txid.ti
    u.h(confs confs.ti, recvd recvd.ti)
  :_  state
  ?:  (gte confs.ti confs.u.w)  cards
  [(send-req ~[req-pax] [%tx-info block txid.ti]) cards]
  ::
  ++  address-reqs
    |=  ti=info:tx:btc
    ^-  (list request)
    =|  rs=(list request)
    =/  ws=(list walt)  ~(val by walts)
    |-  ?~  ws  rs
    %=  $
        ws  t.ws
        rs
      %-  zing
      :~  rs
          (murn inputs.ti (cury to-req i.ws))
          (murn outputs.ti (cury to-req i.ws))
      ==
    ==
  ::
  ++  to-req
    |=  [w=walt v=val:tx:btc]
    ^-  (unit request)
    =+  addi=(~(get by wach.w) address.v)
    ?~  addi  ~
    `[%address-info last-block address.v xpub.w chyg.u.addi idx.u.addi]
  ::
  ++  to-card
    |=  r=request  ^-  card
    (send-req ~[req-pax] r)
  ::
  ++  mk-hest
      |=  rs=(lest request)
      ^-  hest
      =/  as=(set address:btc)
      %-  sy
        %+  turn  rs
        |=(r=request ?>(?=(%address-info -.r) a.r))
      :*  ?>(?=(%address-info -.i.rs) xpub.i.rs)
          txid.ti
          confs.ti
          recvd.ti
          (turn inputs.ti |=(v=val:tx:btc (our-ship as v)))
          (turn outputs.ti |=(v=val:tx:btc (our-ship as v)))
      ==
    ++  our-ship
      |=  [as=(set address:btc) v=val:tx:btc]
      ^-  [=val:tx s=(unit ship)]
      [v ?:((~(has in as) address.v) `our.bowl ~)]
    --
::  +generate-address: generate and return address
::    sends a request for info on the new address
::
++  generate-address
  |=  [=xpub =chyg =pmet]
  ^-  (quip card _state)
  =+  uw=(~(get by walts) xpub)
  ?~  uw
    ~|("btc-wallet-store: non-existent xpub" !!)
  ?.  scanned.u.uw
    ~|("btc-wallet-store: wallet not scanned yet" !!)
  =/  [addr=address:btc =idx w=walt]
    ~(gen-address wad u.uw chyg)
  :_  state(walts (~(put by walts) xpub w))
  :~  (send-update [%generate-address xpub addr pmet])
      %+  send-req  ~[req-pax]
      :*  %address-info  last-block
          addr  xpub  chyg  idx
      ==
  ==
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
::
++  send-req
  |=  [pax=(list path) req=request]  ^-  card
::  ~&  >>   "send-req: {<chyg.req>}, {<idx.req>}"
  :*  %give  %fact  pax
      %btc-wallet-store-request  !>(req)
  ==
::
++  send-update
  |=  upd=update  ^-  card
  [%give %fact ~[/updates] %btc-wallet-store-update !>(upd)]
--
