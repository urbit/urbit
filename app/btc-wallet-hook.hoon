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
::    /updates
::
/-  *btc-wallet-hook, bws=btc-wallet-store
/+  dbug, default-agent, bp=btc-provider, bwsl=btc-wallet-store, *btc
|%
++  defaults
  |%
  ++  fam-limit   10
  ++  piym-limit  3
  --
+$  versioned-state
    $%  state-0
    ==
::  provider: maybe ship if provider is set
::  fam-limit: how many addresses a ship and its moons can request in piym
::  piym-limit: how many entries a given ship can have in pend-piym
::    A ship can only broadcast X payments to us until we see one of them in the mempool
::  feybs: fee/byte in sats used for a given ship payee
::
+$  state-0
  $:  %0
      provider=(unit [host=ship connected=?])
      =btc-state
      def-wallet=(unit xpub)
      fam-limit=@ud
      piym-limit=@ud
      feybs=(map ship sats)
      =reqs
      =piym
      =poym
      =pend-piym
  ==
::
+$  card  card:agent:gall
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
  ~&  >  '%btc-wallet-hook initialized'
  :_  this(fam-limit.state fam-limit:defaults)
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
++  on-watch
  |=  pax=path
  ^-  (quip card _this)
  ?+  pax  (on-watch:def pax)
      [%sign-me ~]
      `this
  ==
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+  -.sign  (on-agent:def wire sign)
      %kick
    ~&  >>>  "kicked from provider {<src.bowl>}"
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
        (handle-wallet-store-request:hc !<(request:bws q.cage.sign))
        ::
          %btc-wallet-store-update
        (handle-wallet-store-update:hc wire !<(update:bws q.cage.sign))
      ==
    [cards this]
  ==
++  on-arvo  on-arvo:def
++  on-fail   on-fail:def
--
|_  =bowl:gall
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
      %set-default-wallet
    =/  xs=(list xpub)  scry-scanned
    ?.  (gth (lent xs) 0)  `state
    `state(def-wallet `(snag 0 xs))
    ::
    ::  %req-pay-address
    ::  overwrites any payment being built currently
    ::  can't pay if there's an outstanding payment being broadcast
    ::  can't pay yourself; comets can't pay (could spam requests)
    ::  forwards poke to payee if payee isn't us
    ::  deletes poym since we'll be making a new outgoing payment
    ::  lets us set fee per byte and recall it once we get a payment address back
    ::  wire is /payer/value/timestamp
    ::
      %req-pay-address
    ?:  broadcasting  ~|("Broadcasting a transaction" !!)
    ~|  "Can't pay ourselves; no comets; can't do while tx is being signed"
    ?<  =(src.bowl payee.act)
    ?<  ?=(%pawn (clan:title payee.act))
    ?<  broadcasting
    =+  feyb=?~(feyb.act fee.btc-state u.feyb.act)
    =>  .(poym ~, feybs (~(put by feybs) payee.act feyb))
    :_  state
    ~[(poke-wallet-hook payee.act [%gen-pay-address value.act])]
    ::  can't pay yourself; comets can't pay (could spam requests)
    ::  must have default wallet set
    ::  reuses payment address for ship if exists in piym
    ::
      %gen-pay-address
    ~|  "Can't pay ourselves; no comets"
    ?<  =(src.bowl our.bowl)
    ?<  ?=(%pawn (clan:title src.bowl))
    =^  cards  state
      (reuse-address src.bowl value.act)
    ?^  cards  [cards state]
    ::  if no reuseable address, call store to generate
    ::
    =+  f=(fam src.bowl)
    =+  n=(~(gut by num-fam.piym) f 0)
    ?~  def-wallet  ~|("btc-wallet-hook: no def-wallet set" !!)
    ?:  (gte n fam-limit)
      ~|("More than {<fam-limit>} addresses for moons + planet" !!)
    :_  state(num-fam.piym (~(put by num-fam.piym) f +(n)))
    :~  %-  poke-wallet-store
        [%generate-address u.def-wallet %0 `[src.bowl value.act]]
    ==
    ::
      %ret-pay-address
    ?:  =(src.bowl our.bowl)  ~|("Can't pay ourselves" !!)
    ?:  broadcasting  ~|("Broadcasting a transaction" !!)
    ?~  def-wallet  ~|("btc-wallet-hook: no def(ault)-wallet set" !!)
    =+  feyb=(~(gut by feybs) src.bowl fee.btc-state)
    ?>  =(payer.act our.bowl)
    :_  state
    :~  %-  poke-wallet-store
        [%generate-txbu u.def-wallet `src.bowl feyb ~[[address.act value.act ~]]]
    ==
    ::  %broadcast-tx
    ::   - poym txid must match incoming txid
    ::   - update sitx in poym
    ::   - send to provider
    ::
      %broadcast-tx
    =/  tx-match=?
      ?~  poym  %.n
      =((get-id:txu (decode:txu signed.act)) ~(get-txid txb:bwsl u.poym))
    :_  ?.  tx-match  state
        ?~  poym  state
        state(sitx.u.poym `signed.act)
    ?.  tx-match
      ~[(send-update [%broadcast-tx-mismatch-poym signed.act])]
    ?~  provider  ~|("Provider not connected" !!)
    ~[(poke-provider host.u.provider ~ [%broadcast-tx signed.act])]
    ::
    ::  %expect-payment
    ::  - check that src.bowl isn't past piym-limit in pend-piym
    ::  - check that payment is in piym
    ::  - add payment to pend-piym
    ::  - send tx-info to provider (poke)
    ::
      %expect-payment
    ~|  "Too many pending %expect-payment sent, or payer+value not found in incoming payments"
    =+  num-pend=(~(gut by num.pend-piym) payer.act 0)
    ?>  (gte piym-limit num-pend)
    =+  pay=(~(get by ps.piym) payer.act)
    ?~  pay  !!
    ?>  ?&  =(payer.u.pay payer.act)
            =(value.u.pay value.act)
        ==
    :-  ?~  provider  ~
        ~[(get-tx-info host.u.provider txid.act)]
    %=  state
        ps.pend-piym  (~(put by ps.pend-piym) txid.act [u.pay vout-n.act])
        num.pend-piym  (~(put by num.pend-piym) payer.act +(num-pend))
    ==
    ::
      %clear-poym
    `state(poym ~)
    ::
      %force-retry
    [(retry-reqs block.btc-state) state]
  ==
::  +handle-provider-status: handle connectivity updates from provider
::    - retry pend-piym on any %connected event, since we're checking mempool
::    - if status is %connected, retry all pending address lookups
::      - only retry all if previously disconnected
::    - if block is updated, retry all address reqs
::
++  handle-provider-status
  |=  s=status:bp
  ^-  (quip card _state)
  ?~  provider  `state
  ?.  =(host.u.provider src.bowl)  `state
  ?-  -.s
      %connected
    :_  %=  state
            provider  `[host.u.provider %.y]
            btc-state  [block.s fee.s now.bowl]
        ==
    ?:  ?!(connected.u.provider)
      %-  zing
      :~  (retry-reqs block.s)
          retry-txbu
          retry-pend-piym
      ==
    ?.  (lth block.btc-state block.s)
      retry-pend-piym
    ~&  >  "got new block, retrying {<(lent (retry-reqs block.s))>} reqs "
    (weld retry-pend-piym (retry-reqs block.s))
    ::
      %disconnected
    `state(provider `[host.u.provider %.n])
  ==
::
++  handle-provider-update
  |=  upd=update:bp
  ^-  (quip card _state)
  ?.  ?=(%.y -.upd)  `state
  ?-  -.body.p.upd
      %address-info
    =/  req=(unit request:bws)
      (~(get by reqs) req-id.p.upd)
    ?~  req  `state
    ?>  ?=([%address-info *] u.req)
    :_  state(reqs (~(del by reqs) req-id.p.upd))
    :~  %-  poke-wallet-store
        :*  %address-info  xpub.u.req  chyg.u.req  idx.u.req
            utxos.body.p.upd  used.body.p.upd  block.body.p.upd
        ==
    ==
    ::
    ::  %txinfo
    ::  - call piyom-to-history to add entry to store history if necessary
    ::  - forward tx to wallet-store
    ::  - check whether payment in pend-piym matches this tx's output values
    ::
      %tx-info
    =^  cards  state
      (pioym-to-history +.body.p.upd)
    :_  state(pend-piym (del-txid txid.ti))
    [(poke-wallet-store [%tx-info ti]) cards]
    ::
    ::  %raw-tx
    ::  - if the req-id is for current poym, add txid/rawtx to the poym
    ::  - if the raw tx matches one of poym's inputs, add it
    ::
      %raw-tx
    =*  rt  body.p.upd
    ?~  poym  `state
    =.  txis.u.poym  (update-poym-txis txis.u.poym txid.rt rawtx.rt)
    :_  state
    =+  pb=~(to-psbt txb:bwsl u.poym)
    ?~  pb  ~
    ~&  >  "PSBT ready:"
    ~&  >>  u.pb
    ~[(send-update [%sign-tx u.poym])]
    ::
    ::  %broadcast-tx
    ::   If no provider, return; Try again on-reconnect
    ::   If tx is included or broadcast
    ::    - send %expect-payment to peer
    ::    - send a %txinfo request
    ::   If tx it is not included and not broadcast
    ::    - clear poym
    ::    - send a %broadcast-tx-spent-utxos update/error
    ::
      %broadcast-tx
    =*  res  body.p.upd
    ?~  provider  `state
    ?~  poym  `state
    ?.  =(~(get-txid txb:bwsl u.poym) txid.res)
      `state
    =/  success=?  ?|(broadcast.res included.res)
    :_  ?:(success state state(poym ~))
    ?.  success
      ~[(send-update [%broadcast-tx-spent-utxos txid.res])] 
    %+  weld  ~[(poke-provider host.u.provider ~ )]
    ?~  payee.u.poym  ~
    :_  ~
    %-  poke-wallet-hook
    :*  u.payee.u.poym
        %expect-payment
        txid.res
        our.bowl
        value:(snag 0 txos.u.poym)
        ::  first output of poym is to payee, by convention
        0
    ==
  ==
::
++  handle-wallet-store-request
  |=  req=request:bws
  ^-  (quip card _state)
  ?~  provider  `state
  =/  should-send=?
    ?&  provider-connected
        (lth last-block.req block.btc-state)
    ==
  ?-  -.req
      %address-info
    =+  ri=(gen-req-id:bp eny.bowl)
    :_  state(reqs (~(put by reqs) ri req))
    ?:  should-send
      ~[(get-address-info ri host.u.provider a.req)]
    ~
    ::
      %tx-info
    :: TODO: push the request out
    :: put it in reqs
    :: check whether last-block has passed
    `state
  ==
::
++  handle-wallet-store-update
  |=  [=wire upd=update:bws]
  ^-  (quip card _state)
  ?-  -.upd
    ::  %generate-address
    ::   if no peta (payer/value), just prints address
    ::
      %generate-address
    ?~  peta.upd  ~&(> "wallet-hook: %generate-address: {<address.upd>}" `state)
    =/  [payer=ship value=sats]  u.peta.upd
    :_  state(ps.piym (~(put by ps.piym) payer [xpub.upd address.upd payer value]))
    ~[(poke-wallet-hook payer [%ret-pay-address address.upd payer value])]
    ::  %generate-txbu
    ::   - replace current txbu (otherwise can have UTXO overlap)
    ::   - get rawtxs for each input
    ::
      %generate-txbu
    :_  state(poym `txbu.upd)
    ?~  provider  ~&(>>> "provider not set" ~)
    %+  turn  txis.txbu.upd
    |=(=txi:bws (get-raw-tx host.u.provider txid.utxo.txi))
    ::
      %saw-piym
    `state
    ::
      %scan-done
    ~&  >  "scanned wallet: {<xpub.upd>}"
    ?~  def-wallet
      `state(def-wallet `xpub.upd)
    `state
  ==
::  +reuse-address: if piym already has address for payer,
::    replace address and return to payer
::
++  reuse-address
  |=  [payer=ship value=sats]
  ^-  (quip card _state)
  =+  p=(~(get by ps.piym) payer)
  ?~  p  `state
  =+  newp=u.p(value value)
  :_  state(ps.piym (~(put by ps.piym) payer newp))
  :~  %+  poke-wallet-hook  payer
          [%ret-pay-address address.newp payer value]
  ==
::  +del-txid: delete txid from pend-piym
::
++  del-txid
  |=  =txid
  ^-  ^pend-piym
  =+  p=(~(get by ps.pend-piym) txid)
  =.  ps.pend-piym  (~(del by ps.pend-piym) txid)
  ?~  p  pend-piym
  =*  payer  payer.pay.u.p
  =+  n=(~(get by num.pend-piym) payer)
  ?~  n  pend-piym
  ?:  =(0 u.n)  pend-piym
  pend-piym(num (~(put by num.pend-piym) payer (dec u.n)))
::
::  +pioym-to-history: tx info checks against pend-piym/poym
::   %add-history-entry to wallet-store if matches
::   If in pend-piym/poym, remove
::
++  pioym-to-history
  |=  ti=info:tx
  |^  ^-  (quip card _state)
  ::  calcuate in-pend-piym, in-poym
  ::   in-pend-piym also should compute vout as a unit
  ::  unless one of those, return
  ::  =?  state  in-pend-piym
  ::  =?  state  in-poym
  ::
  ::  compute [xpub payer] (depending on which it was)
  ?~  p  ~
  ?.  ?&  (gth (lent outputs.info) vout-n.u.p)
          =(value.pay.u.p value:(snag vout-n.u.p outputs.info))
      ==
    ~
  :-  ~
  :-  xpub.pay.u.p
  :*  txid.info
      confs.info
      recvd.info
      %+  turn  inputs.info
        |=(i=val:tx [i `payer.pay.u.p])
      :: TODO: make below into a helper fn
      %+  turn  outputs.info
        |=  o=val:tx
        ?:  =(pos.o vout-n.u.p)
          [o `our.bowl]
        [o `payer.pay.u.p]
  ==
  ++  output-w-payee
    |=  o=val:tx
    ^-  [val:tx (unit ship)]
  --
  :: TODO: add the below poke here
::  ~[(poke-wallet-store [%add-history-entry xpub.u.mh hest.u.mh])]
::
::  +fam: planet parent if s is a moon 
::
++  fam
  |=  s=ship
  ^-  ship
  ?.  =(%earl (clan:title s))  s
  (sein:title our.bowl now.bowl s)
::  +update-poym-txis:
::    update outgoing payment with a rawtx, if the txid is in poym's txis
::
++  update-poym-txis
  |=  [txis=(list txi:bws) =txid rt=rawtx]
  ^-  (list txi:bws)
  =|  i=@
  |-  ?:  (gte i (lent txis))  txis
  =/  ith=txi:bws  (snag i txis)
  =?  txis  =(txid txid.utxo.ith)
   (snap txis i `txi:bws`ith(ur `rt))
  $(i +(i))
::  +retry-reqs: get-address-info for any reqs with old last-block
::
++  retry-reqs
  |=  [latest-block=@ud]
  ^-  (list card)
  ?~  provider  ~|("provider not set" !!)
  %+  murn  ~(tap by reqs)
  |=  [ri=req-id:bp req=request:bws]
  ?:  (gte last-block.req latest-block)  ~
  ?-  -.req
      %address-info
    `(get-address-info ri host.u.provider a.req)
    ::
      %tx-info
    `(get-tx-info host.u.provider txid.req)
  ==
::
++  retry-txbu
  ^-  (list card)
  ?~  poym  ~
  ?~  provider  ~|("provider not set" !!)
  %+  turn  txis.u.poym
  |=  =txi:bws
  (get-raw-tx host.u.provider txid.utxo.txi)
::  +retry-pend-piym: check whether txids in pend-piym are in mempool
::
++  retry-pend-piym
  ^-  (list card)
  ?~  provider  ~|("provider not set" !!)
  %+  turn  ~(tap in ~(key by ps.pend-piym))
  |=(=txid (get-tx-info host.u.provider txid))
::
++  get-address-info
  |=  [ri=req-id:bp host=ship a=address]
  ^-  card
  (poke-provider host `ri [%address-info a])
::
++  get-raw-tx
  |=  [host=ship =txid]
  ^-  card
  (poke-provider host ~ [%raw-tx txid])
::
++  get-tx-info
  |=  [host=ship =txid]
  ^-  card
  (poke-provider host ~ [%tx-info txid])
::
++  poke-provider
  |=  [host=ship uri=(unit req-id:bp) actb=action-body:bp]
  ^-  card
  =+  ri=?^(uri u.uri (gen-req-id:bp eny.bowl))
  :*  %pass  /[(scot %da now.bowl)]  %agent  [host %btc-provider]
      %poke  %btc-provider-action  !>([ri actb])
  ==
::
++  broadcasting
  ^-  ?
  ?~  poym  %.n
  ?~  sitx.u.poym  %.n
  %.y
::
++  provider-connected
  ^-  ?
  ?~  provider  %.n
  connected.u.provider
::
++  poke-wallet-hook
  |=  [target=ship act=action]
  ^-  card
  :*  %pass  /[(scot %da now.bowl)]  %agent
      [target %btc-wallet-hook]  %poke
      %btc-wallet-hook-action  !>(act)
  ==
::
++  send-update
  |=  =update
  ^-  card
  [%give %fact ~[/updates] %btc-wallet-hook-update !>(update)]
::
++  poke-wallet-store
  |=  act=action:bws
  ^-  card
  :*  %pass  /[(scot %da now.bowl)]
      %agent  [our.bowl %btc-wallet-store]  %poke
      %btc-wallet-store-action  !>(act)
  ==
::
++  scry-scanned
  .^  (list xpub)
    %gx
    (scot %p our.bowl)
    %btc-wallet-store
    (scot %da now.bowl)
    %scanned
    %noun
  ==
--
