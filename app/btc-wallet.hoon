::  btc-wallet
::
::  Scrys
::  x/scanned: (list xpub) of all scanned wallets
::  x/balance/xpub: balance (in sats) of wallet
/-  *btc-wallet
/+  dbug, default-agent, *btc-wallet, bp=btc-provider, *bitcoin, bip32
|%
++  defaults
  |%
  ++  params
    :*  batch-size=20
        fam-limit=10
        piym-limit=3
    ==
  ++  confs  6
  ++  fee  100
  --
::
+$  versioned-state
    $%  state-0
    ==
::  batch-size: how many addresses to send out at once for checking
::  last-block: most recent block seen by the store
::
+$  state-0
  $:  %0
      prov=(unit provider)
      walts=(map xpub walt)
      =btc-state
      =history
      curr-xpub=(unit xpub)
      =scans
      =params
      feybs=(map ship sats)
      =piym
      =poym
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
  ~&  >  '%btc-wallet initialized'
  :-  ~
  %_  this
      state
    :*  %0
        ~
        *(map xpub walt)
        *^btc-state
        *^history
        ~
        *^scans
        params:defaults
        *(map ship sats)
        *^piym
        *^poym
    ==
  ==
++  on-save
  ^-  vase
  !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  >  '%btc-wallet recompiled'
  `this(state !<(versioned-state old-state))
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state
  ?+  mark  (on-poke:def mark vase)
      %btc-wallet-command
    ?>  =(our.bowl src.bowl)
    (handle-command:hc !<(command vase))
    ::
      %btc-wallet-action
    ?<  =(our.bowl src.bowl)
    (handle-action:hc !<(action vase))
    ::
      %btc-wallet-internal
    ?>  =(our.bowl src.bowl)
    (handle-internal:hc !<(internal vase))
  ==
  [cards this]
++  on-peek
  |=  pax=path
  ^-  (unit (unit cage))
  ?+  pax  (on-peek:def pax)
      [%x %scanned ~]
    ``noun+!>(scanned-wallets)
  ::
      [%x %balance @ ~]
    ``noun+!>((balance:hc (xpub +>-.pax)))
  ==
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+  -.sign  (on-agent:def wire sign)
      %kick
    ~&  >>>  "kicked from prov {<src.bowl>}"
    ?~  prov  `this
    ?:  ?&  ?=(%set-provider -.wire)
            =(host.u.prov src.bowl)
        ==
      `this(prov ~)
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
      ==
  [cards this]
  ==
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
|_  =bowl:gall
++  handle-command
  |=  comm=command
  ^-  (quip card _state)
  ?-  -.comm
      %set-provider
    =*  sub-card
    :*  %pass  /set-provider/[(scot %p provider.comm)]
          %agent  [provider.comm %btc-provider]  %watch  /clients
      ==
    :_  state(prov [~ provider.comm %.n])
    ?~  prov  ~[sub-card]
    :~  [%pass /set-provider/[(scot %p host.u.prov)] %agent [host.u.prov %btc-provider] %leave ~]
        sub-card
    ==
    ::
      %set-current-wallet
    (set-curr-xpub xpub.comm)
    ::
      %add-wallet
    ?~  (~(has by walts) xpub.comm)
    ((slog ~[leaf+"xpub already in wallet"]) `state)
    =/  w=walt  (from-xpub +.comm)
    =.  walts  (~(put by walts) xpub.comm w)
    =^  c1  state  (init-batches xpub.comm (dec max-gap.w))
    =^  c2  state  (set-curr-xpub xpub.comm)
    [(weld c1 c2) state]
    ::
      %delete-wallet
    =*  cw  curr-xpub.state
    =?  cw  ?&(?=(^ cw) =(u.cw xpub.comm))
      ~
    =.  scans  (~(del by scans) [xpub.comm %0])
    =.  scans  (~(del by scans) [xpub.comm %1])
    `state(walts (~(del by walts) xpub.comm))
    ::
    ::  overwrites any payment being built in poym
    ::
      %req-pay-address
    ~|  "Can't pay ourselves; no comets; can't do while tx is being signed"
    ?<  =(src.bowl payee.comm)
    ?<  ?=(%pawn (clan:title payee.comm))
    ?<  is-broadcasting
    :_  state(poym ~, feybs (~(put by feybs) payee.comm feyb.comm))
     ~[(poke-peer payee.comm [%gen-pay-address value.comm])]
    ::
      %broadcast-tx
    ?~  prov  ~|("Provider not connected" !!)
    =+  signed=(to-hexb txhex.comm)
    =/  tx-match=?
      ?~  poym  %.n
      =((get-id:txu (decode:txu signed)) ~(get-txid txb u.poym))
    :-  ?.  tx-match
          ((slog leaf+"txid didn't match txid in wallet") ~)
        ~[(poke-provider [%broadcast-tx signed])]
    ?.  tx-match  state
      ?~  poym  state
    state(signed-tx.u.poym `signed)
  ==
::
++  handle-action
  |=  act=action
  ^-  (quip card _state)
  ?-  -.act
    ::  comets can't pay (could spam address requests)
    ::  reuses payment address for ship if ship in piym already
    ::
      %gen-pay-address
    ~|  "no comets"
    ?<  ?=(%pawn (clan:title src.bowl))
    ?~  curr-xpub  ~|("btc-wallet: no curr-xpub set" !!)
    |^
    =^  cards  state  reuse-address
    ?^  cards  [cards state]             ::  if cards returned, means we already have an address
    =+  f=(fam our.bowl now.bowl src.bowl)
    =+  n=(~(gut by num-fam.piym) f 0)
    ?:  (gte n fam-limit.params)
      ~|("More than {<fam-limit.params>} addresses for moons + planet" !!)

    =.  state  state(num-fam.piym (~(put by num-fam.piym) f +(n)))
    =^  a=address  state
      (generate-address u.curr-xpub %0)
    :-  ~[(poke-peer src.bowl [%recv-pay-address a value.act])]
    state(ps.piym (~(put by ps.piym) src.bowl [~ u.curr-xpub a src.bowl value.act]))
    ::
    ++  generate-address
      |=  [=xpub =chyg]
      =/  uw=(unit walt)  (~(get by walts) xpub)
      ?:  ?|(?=(~ uw) ?!(scanned.u.uw))
        ~|("no wallet with xpub or wallet not scanned yet" !!)
      =/  [addr=address =idx w=walt]
        ~(gen-address wad u.uw chyg)
      [addr state(walts (~(put by walts) xpub w))]
    ::
    ++  reuse-address
      ^-  (quip card _state)
      =*  payer  src.bowl
      =+  p=(~(get by ps.piym) payer)
      ?~  p  `state
      ?^  pend.u.p  ~|("%gen-address: {<payer>} already has pending payment to us" !!)
      =+  newp=u.p(value value.act)
      :_  state(ps.piym (~(put by ps.piym) payer newp))
      ~[(poke-peer payer [%recv-pay-address address.newp value.act])]
    --
    ::
      %recv-pay-address
    ?:  =(src.bowl our.bowl)  ~|("Can't pay ourselves" !!)
    ?:  is-broadcasting  ~|("Broadcasting a transaction" !!)
    ?~  curr-xpub  ~|("btc-wallet-hook: no curr-xpub set" !!)
    =+  feyb=(~(gut by feybs) src.bowl ?~(fee.btc-state fee:defaults u.fee.btc-state))
    |^
    =^  tb=(unit txbu)  state
      (generate-txbu u.curr-xpub `src.bowl feyb ~[[address.act value.act ~]])
    :_  state(poym tb)
    ?~  tb  ~
    %+  turn  txis.u.tb
    |=(=txi (poke-provider [%raw-tx txid.utxo.txi]))
    ::
    ++  generate-txbu
      |=  [=xpub payee=(unit ship) feyb=sats txos=(list txo)]
      ^-  [(unit txbu) _state]
      =/  uw  (~(get by walts) xpub)
      ?:  ?|(?=(~ uw) ?!(scanned.u.uw))
        ~|("no wallet with xpub or wallet not scanned yet" !!)
      =/  [tb=(unit txbu) chng=(unit sats)]
        %~  with-change  sut
        [u.uw eny.bowl block.btc-state payee feyb txos]
      ?~  tb  ((slog ~[leaf+"insufficient balance or not enough confirmed balance"]) [tb state])
      ::  if no change, return txbu; else add change output to txbu
      ::
      ?~  chng  [tb state]
      =/  [addr=address =idx w=walt]
        ~(nixt-address wad u.uw %1)
      :-  `(~(add-output txb u.tb) addr u.chng `(~(hdkey wad w %1) idx))
      state(walts (~(put by walts) xpub w))
    --
    ::
    ::  %expect-payment
    ::  - check that payment is in piym
    ::  - replace pend.payment with incoming txid (lock)
    ::  - add txid to pend.piym
    ::  - request tx-info from provider
    ::
      %expect-payment
    |^
    =+  pay=(~(get by ps.piym) src.bowl)
    ~|  "%expect-payment: matching payment not in piym"
    ?~  pay  !!
    ?>  (piym-matches u.pay)
    :_  (update-pend-piym txid.act u.pay(pend `txid.act))
    ?~  prov  ~
    ~[(poke-provider [%tx-info txid.act])]
    ::
    ++  piym-matches
      |=  p=payment
      ?&  =(payer.p src.bowl)
          =(value.p value.act)
      ==
    --
  ==
::
++  handle-internal
  |=  intr=internal
  ^-  (quip card _state)
  ?-    -.intr
        %add-poym-raw-txi
    ?>  =(src.bowl our.bowl)
    ?~  poym  `state
    =.  txis.u.poym
      (update-poym-txis txis.u.poym +.intr)
    :_  state
    =+  pb=~(to-psbt txb u.poym)
    ?~  pb  ~
    =+  vb=~(vbytes txb u.poym)
    =+  fee=~(fee txb u.poym)
    ~&  >>  "{<vb>} vbytes, {<(div fee vb)>} sats/byte, {<fee>} sats fee"
    %-  (slog [%leaf "PSBT: {<u.pb>}"]~)
    ~
    ::  delete an incoming/outgoing payment when we see it included in a tx
    ::
      %close-pym
    ?>  =(src.bowl our.bowl)
    |^
    =^  cards  state
      ?.  included.ti.intr
        `state
      ?:  (~(has by pend.piym) txid.ti.intr)
        (piym-to-history ti.intr)
      ?:  (poym-has-txid txid.ti.intr)
        (poym-to-history ti.intr)
      `state
    :-  cards
    (handle-tx-info ti.intr)
    ::
    ++  poym-has-txid
      |=  txid=hexb
      ^-  ?
      ?~  poym  %.n
      ?~  signed-tx.u.poym  %.n
      =(txid (get-id:txu (decode:txu u.signed-tx.u.poym)))
    ::   - checks whether poym has a signed tx
    ::   - checks whether the txid matches that signed tx, if not, skip
    ::   - clears poym
    ::   - returns card that adds hest to history
    ::
    ++  poym-to-history
      |=  ti=info:tx
      ^-  (quip card _state)
      |^
      ?~  poym  `state
      ?~  signed-tx.u.poym  `state
      ?.  (poym-has-txid txid.ti)
        `state
      =+  vout=(get-vout txos.u.poym)
      ?~  vout  ~|("poym-to-history: poym should always have an output" !!)
      :-  ~
      %=  state
          poym  ~
          history
        (add-history-entry ti xpub.u.poym our.bowl payee.u.poym u.vout)
      ==
      ::
      ++  get-vout
        |=  txos=(list txo)
        ^-  (unit @ud)
        =|  idx=@ud
        |-  ?~  txos  ~
        ?~  hk.i.txos  `idx
        $(idx +(idx), txos t.txos)
      --
    ::   - checks whether txid in pend.piym
    ::   - checks whether ti has a matching value output to piym
    ::   - if no match found, just deletes pend.piym with this tx
    ::     stops peer from spamming txids
    ::   - returns card that adds hest to history
    ::
    ++  piym-to-history
      |=  ti=info:tx
      |^  ^-  (quip card _state)
      =+  pay=(~(get by pend.piym) txid.ti)
      ?~  pay  `state
      ::  if no matching output in piym, delete from pend.piym to stop DDOS of txids
      ::
      =+  vout=(get-vout value.u.pay)
      ?~  vout
        `(del-pend-piym txid.ti)
      =.  state  (del-all-piym txid.ti payer.u.pay)
      `state(history (add-history-entry [ti xpub.u.pay payer.u.pay `our.bowl u.vout]))
      ::
      ++  get-vout
        |=  value=sats
        ^-  (unit @ud)
        =|  idx=@ud
        =+  os=outputs.ti
        |-  ?~  os  ~
        ?:  =(value.i.os value)
          `idx
        $(os t.os, idx +(idx))
      ::
      ::
      ++  del-pend-piym
        |=  txid=hexb
        ^-  _state
        state(pend.piym (~(del by pend.piym) txid.ti))
      ::
      ++  del-all-piym
        |=  [txid=hexb payer=ship]
        ^-  _state
        =+  nf=(~(gut by num-fam.piym) payer 1)
        %=  state
            pend.piym  (~(del by pend.piym) txid)
            ps.piym    (~(del by ps.piym) payer)
            num-fam.piym  (~(put by num-fam.piym) payer (dec nf))
        ==
      --
    ::
    ++  add-history-entry
      |=  [ti=info:tx =xpub payer=ship payee=(unit ship) vout=@ud]
      ^-  ^history
      =/  =hest
        :*  xpub
            txid.ti
            confs.ti
            recvd.ti
            (turn inputs.ti |=(i=val:tx [i `payer]))
            %+  turn  outputs.ti
              |=  o=val:tx
              ?:  =(pos.o vout)   ::  check whether this is the output that went to payee
              [o payee]
              [o `payer]
         ==
      (~(put by history) txid.hest hest)
    --
    ::
      %fail-broadcast-tx
    ?>  =(src.bowl our.bowl)
    ~&  >>>  "%fail-broadcast-tx"
    `state(poym ~)
    ::
      %succeed-broadcast-tx
    ?>  =(src.bowl our.bowl)
    ~&  >  "%succeed-broadcast-tx"
    :_  state
    ?~  prov  ~
    :-  (poke-provider [%tx-info txid.intr])
    ?~  poym          ~
    ?~  payee.u.poym  ~
    :_  ~
    %-  poke-peer
    :*  u.payee.u.poym
        %expect-payment
        txid.intr
        value:(snag 0 txos.u.poym)
    ==
  ==
::
::  +handle-provider-status: handle connectivity updates from provider
::    - retry pend.piym on any %connected event, since we're checking mempool
::    - if status is %connected, retry all pending address lookups
::      - only retry all if previously disconnected
::    - if block is updated, retry all address reqs
::    - if provider's network doesn't match network in our state, leave
::
++  handle-provider-status
  |=  s=status:bp
  ^-  (quip card _state)
  |^
  ?~  prov  `state
  ?.  =(host.u.prov src.bowl)  `state
  ?-  -.s
      %new-block
    (on-connected u.prov network.s block.s fee.s `blockhash.s `blockfilter.s)
    ::
      %connected
    (on-connected u.prov network.s block.s fee.s ~ ~)
    ::
      %disconnected
    `state(prov `u.prov(connected %.n))
  ==
  ::
  ++  on-connected
    |=  $:  p=provider
            =network
            block=@ud
            fee=(unit sats)
            blockhash=(unit hexb)
            blockfilter=(unit hexb)
        ==
    ^-  (quip card _state)
    :_  %_  state
            prov  `p(connected %.y)
            btc-state  [block fee now.bowl]
        ==
    ?:  ?|(?!(connected.p) (lth block.btc-state block))
      ;:  weld
          (retry-pend-piym network)
          (retry-addrs network)
          (retry-txs network)
          (retry-scans network)
      ==
    (retry-pend-piym network)
  ::
  ++  retry-scans
    |=  =network
    ^-  (list card)
    %-  zing
    %+  murn  ~(tap by scans)
    |=  [[=xpub =chyg] =batch]
    ?.  =(network network:(~(got by walts) xpub))  ~
    `-:(req-scan batch xpub chyg)
  ::  +retry-addrs: get info on addresses with unconfirmed UTXOs
  ::
  ++  retry-addrs
    |=  =network
    ^-  (list card)
    %-  zing
    %+  murn  ~(val by walts)
    |=  w=walt
    ?.  =(network network.w)  ~
    ^-  (unit (list card))
    :-  ~
    %+  murn  ~(tap by wach.w)
    |=  [a=address ad=addi]
    ?:  %+  levy  ~(tap in utxos.ad)
        |=(u=utxo (gth height.u (sub block.btc-state confs.w)))
      ~
    `(poke-provider [%address-info a])
  ::  +retry-txs: get info on txs without enough confirmations
  ::
  ++  retry-txs
    |=  =network
    ^-  (list card)
    %+  murn  ~(tap by history)
    |=  [=txid =hest]
    =/  w  (~(get by walts) xpub.hest)
    ?~  w  ~
    ?.  =(network network.u.w)  ~
    ?:  (gte confs.hest confs.u.w)  ~
    `(poke-provider [%tx-info txid])
  ::
  ++  retry-poym
    |=  =network
    ^-  (list card)
    ?~  poym  ~
    =/  w  (~(get by walts) xpub.u.poym)
    ?~  w  ~
    ?.  =(network network.u.w)  ~
    %+  weld
      ?~  signed-tx.u.poym  ~
      ~[(poke-provider [%broadcast-tx u.signed-tx.u.poym])]
    %+  turn  txis.u.poym
    |=  =txi
    (poke-provider [%raw-tx ~(get-txid txb u.poym)])
  ::  +retry-pend-piym: check whether txids in pend-piym are in mempool
  ::
  ++  retry-pend-piym
    |=  =network
    ^-  (list card)
    %+  murn  ~(tap by pend.piym)
    |=  [=txid p=payment]
    =/  w  (~(get by walts) xpub.p)
    ?~  w  ~
    ?.  =(network network.u.w)  ~
    `(poke-provider [%tx-info txid])
  --
::
++  handle-provider-update
  |=  upd=update:bp
  ^-  (quip card _state)
  ?~  prov  `state
  ?.  =(host.u.prov src.bowl)  `state
  ?.  ?=(%.y -.upd)  `state
  ?-  -.p.upd
      %address-info
    ::  located in the helper in Scan Logic to keep all of that unified
    ::
    (handle-address-info address.p.upd utxos.p.upd used.p.upd)
    ::
      %tx-info
    :-  ~[(poke-internal [%close-pym info.p.upd])]
    (handle-tx-info info.p.upd)
    ::
      %raw-tx
    :_  state
    ~[(poke-internal [%add-poym-raw-txi +.p.upd])]
    ::
      %broadcast-tx
    ?~  poym  `state
    ?.  =(~(get-txid txb u.poym) txid.p.upd)
      `state
    :_  state
    ?:  ?|(broadcast.p.upd included.p.upd)
      ~[(poke-internal [%succeed-broadcast-tx txid.p.upd])]
    ~[(poke-internal [%fail-broadcast-tx txid.p.upd])]
  ==
::
++  handle-tx-info
  |=  ti=info:tx
  ^-  _state
  |^
  =/  h  (~(get by history) txid.ti)
  =/  our-addrs=(set address)             ::  all our addresses in inputs/outputs of tx
    %-  silt
    %+  skim
      %+  turn  (weld inputs.ti outputs.ti)
      |=(=val:tx address.val)
    is-our-address
  ?:  =(0 ~(wyt in our-addrs))  state
  =/  =xpub
    xpub.w:(need (address-coords (snag 0 ~(tap in our-addrs)) ~(val by walts)))
  ?~  h                           ::  addresses in wallets, but tx not in history
    =.  history
      %+  ~(put by history)  txid.ti
      (mk-hest xpub our-addrs)
    state
  ?.  included.ti                 ::  tx in history, but not in mempool/blocks
    state(history (~(del by history) txid.ti))
  %_  state
      history
    %+  ~(put by history)  txid.ti
    u.h(confs confs.ti, recvd recvd.ti)
  ==
  ::
  ++  mk-hest
    :: has tx-info
      |=  [=xpub our-addrs=(set address)]
      ^-  hest
      :*  xpub
          txid.ti
          confs.ti
          recvd.ti
          (turn inputs.ti |=(v=val:tx (is-our-ship our-addrs v)))
          (turn outputs.ti |=(v=val:tx (is-our-ship our-addrs v)))
      ==
  ::
  ++  is-our-ship
    |=  [as=(set address) v=val:tx]
    ^-  [=val:tx s=(unit ship)]
    [v ?:((~(has in as) address.v) `our.bowl ~)]
  ::
  ++  is-our-address
    |=(a=address ?=(^ (address-coords a ~(val by walts))))
  --
++  set-curr-xpub
  |=  =xpub
  ^-  (quip card _state)
  ?~  (find ~[xpub] scanned-wallets)  `state
  `state(curr-xpub `xpub)
::
::
::  Scan Logic
::
::  Algorithm
::  Initiate a batch for each chyg, with max-gap idxs in it
::  Watch all of the addresses made from idxs
::  Request info on all addresses from provider
::  When an %address-info comes back:
::    - remove that idx from todo.batch
::    - run check-scan to check whether that chyg is done
::    - if it isn't, refill it with max-gap idxs to scan
::
::  +handle-address-info: updates scans and wallet with address info
::
++  handle-address-info
  |=  [=address utxos=(set utxo) used=?]
  ^-  (quip card _state)
  =/  ac  (address-coords address ~(val by walts))
  ?~  ac  `state
  =/  [w=walt =chyg =idx]  u.ac
  =.  walts
    %+  ~(put by walts)  xpub.w
    %+  ~(update-address wad w chyg)
      address
   [used chyg idx utxos]
  ::  if the wallet+chyg is being scanned, update the scan batch
  ::
  =/  b  (~(get by scans) [xpub.w chyg])
  ?~  b  `state
  =.  scans
   (del-scanned u.b(has-used ?|(used has-used.u.b)) xpub.w chyg idx)
  ?:  empty:(scan-status xpub.w chyg)
    (check-scan xpub.w)
  `state
::  +req-scan
::   - adds addresses in batch to wallet's watch map as un-used addresses
::   - returns provider %address-info request cards
::
++  req-scan
  |=  [b=batch =xpub =chyg]
  ^-  (quip card _state)
  =/  w=walt  (~(got by walts) xpub)
  =/  as=(list [address [? ^chyg idx (set utxo)]])
    %+  turn  ~(tap in todo.b)
    |=(=idx [(~(mk-address wad w chyg) idx) [%.n chyg idx *(set utxo)]])
  =.  w
    |-  ?~  as  w
    $(as t.as, w (~(update-address wad w chyg) -.i.as +.i.as))
  :-  (turn as |=([a=address *] (poke-provider [%address-info a])))
  %=  state
      scans
    (~(put by scans) [xpub chyg] b)
      walts
      (~(put by walts) xpub w)
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
++  init-batches
  |=  [=xpub endpoint=idx]
  ^-  (quip card _state)
  =/  b=batch
    [(silt (gulf 0 endpoint)) endpoint %.n]
  =^  cards0  state  (req-scan b xpub %0)
  =^  cards1  state  (req-scan b xpub %1)
  [(weld cards0 cards1) state]
::  +bump-batch
::  if the batch is done but the wallet isn't done scanning,
::  returns new address requests and updated batch
::
++  bump-batch
  |=  [=xpub =chyg]
  ^-  (quip card _state)
  =/  b=batch  (~(got by scans) xpub chyg)
  =/  s  (scan-status xpub chyg)
  ?.  ?&(empty.s ?!(done.s))
    `state
  =/  w=walt  (~(got by walts) xpub)
  =/  newb=batch
    :*  (silt (gulf +(endpoint.b) (add endpoint.b max-gap.w)))
        (add endpoint.b max-gap.w)
        %.n
    ==
  (req-scan newb xpub chyg)
::  +del-scanned: delete scanned idxs
::
++  del-scanned
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
  %-  (slog ~[leaf+"Scanned xpub {<xpub>}"])
  =.  state  state(walts (~(put by walts) xpub w(scanned %.y)))
  (set-curr-xpub xpub)
::  +check-scan: initiate a scan if one hasn't started
::               check status of scan if one is running
::
++  check-scan
  |=  =xpub
  ^-  (quip card _state)
  =/  s0  (scan-status xpub %0)
  =/  s1  (scan-status xpub %1)
  ?:  ?&(empty.s0 done.s0 empty.s1 done.s1)
    (end-scan xpub)
  =^  cards0=(list card)  state
    (bump-batch xpub %0)
  =^  cards1=(list card)  state
    (bump-batch xpub %1)
  [(weld cards0 cards1) state]
::
::

::  +update-pend.piym
::   - set pend.payment to txid (lock)
::   - add txid to pend.piym
::
++  update-pend-piym
  |=  [txid=hexb p=payment]
  ^-  _state
  ?~  pend.p  ~|("update-pend-piym: no pending payment" !!)
  %=  state
      ps.piym  (~(put by ps.piym) payer.p p)
      pend.piym  (~(put by pend.piym) txid p)
  ==
::
::  +update-poym-txis:
::    update outgoing payment with a rawtx, if the txid is in poym's txis
::
++  update-poym-txis
  |=  [txis=(list txi) txid=hexb rt=hexb]
  ^-  (list txi)
  =|  i=@
  |-  ?:  (gte i (lent txis))  txis
  =/  ith=txi  (snag i txis)
  =?  txis  =(txid txid.utxo.ith)
   (snap txis i `txi`ith(ur `rt))
  $(i +(i))
::
::
::
++  poke-provider
  |=  [act=action:bp]
  ^-  card
  ?~  prov  ~|("provider not set" !!)
  :*  %pass  /[(scot %da now.bowl)]
      %agent  [host.u.prov %btc-provider]
      %poke   %btc-provider-action  !>([act])
  ==
::
++  poke-peer
  |=  [target=ship act=action]
  ^-  card
  :*  %pass  /[(scot %da now.bowl)]  %agent
      [target %btc-wallet]  %poke
      %btc-wallet-action  !>(act)
  ==
++  poke-internal
  |=  [intr=internal]
  ^-  card
  :*  %pass  /[(scot %da now.bowl)]  %agent
      [our.bowl %btc-wallet]  %poke
      %btc-wallet-internal  !>(intr)
  ==
::
++  is-broadcasting
  ^-  ?
  ?~  poym  %.n
  ?=(^ signed-tx.u.poym)
::
::  Scry Helpers
::
++  scanned-wallets
  ^-  (list xpub)
  %+  murn  ~(tap by walts)
  |=  [=xpub w=walt]
  ^-  (unit ^xpub)
  ?:  scanned.w  `xpub  ~
::
++  balance
  |=  =xpub
  ^-  (unit sats)
  =/  w  (~(get by walts) xpub)
  ?~  w  ~
  =/  values=(list sats)
    %+  turn  ~(val by wach.u.w)
    |=  =addi  ^-  sats
    %+  roll
      %+  turn  ~(tap by utxos.addi)
     |=(=utxo value.utxo)
    add
  `(roll values add)
  ::
--
