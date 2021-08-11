::  btc-wallet
::
::  Scrys
::  x/scanned: (list xpub) of all scanned wallets
::  x/balance/xpub: balance (in sats) of wallet
/-  *btc-wallet, bp=btc-provider, settings
/+  dbug, default-agent, bl=btc, bc=bitcoin, bcu=bitcoin-utils, bip32
~%  %btc-wallet-top  ..part  ~
|%
+$  card  card:agent:gall
::
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
        state-1
        state-2
    ==
::
+$  state-0
  $:  %0
      prov=(unit provider)
      walts=(map xpub:bc walt-0)
      =btc-state
      =history
      curr-xpub=(unit xpub:bc)
      =scans
      =params
      feybs=(map ship sats)
      =piym
      =poym
      ahistorical-txs=(set txid)
  ==
::
+$  base-state
  $:  prov=(unit provider)
      walts=(map xpub:bc walt)
      =btc-state
      =history
      curr-xpub=(unit xpub:bc)
      =scans
      =params
      feybs=(map ship sats)
      =piym
      =poym
      ahistorical-txs=(set txid)
  ==
::
+$  state-1  [%1 base-state]
+$  state-2  [%2 base-state]
--
=|  state-2
=*  state  -
%-  agent:dbug
^-  agent:gall
=<
~%  %btc-wallet-agent  ..retry-filtered-addrs  ~
|_  =bowl:gall
+*  this      .
    def   ~(. (default-agent this %|) bowl)
    hc    ~(. +> bowl)
::
++  on-init
^-  (quip card _this)
  ~&  >  '%btc-wallet initialized'
  ::
  =/  warning=event:settings   [%put-entry %btc-wallet %warning %b %.y]
  =/  currency=event:settings  [%put-entry %btc-wallet %currency %s 'USD']
  =/  cards=(list card)
    :~  (poke-our:hc %settings-store %settings-event !>(warning))
        (poke-our:hc %settings-store %settings-event !>(currency))
    ==
  ::
  :-  cards
  %_  this
      state
    :*  %2
        ~
        *(map xpub:bc walt)
        *^btc-state
        *^history
        ~
        *^scans
        params:defaults
        *(map ship sats)
        *^piym
        *^poym
        ~
    ==
  ==
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  >  '%btc-wallet recompiled'
  =/  ver  !<(versioned-state old-state)
  =|  cards=(list card)
  |-
  ?-  -.ver
      %2
    [cards this(state ver)]
  ::
      %1
    =?  cards  ?=(^ prov.ver)
      :_  cards
      =/  =dock  [host.u.prov.ver %btc-provider]
      =/  wir=wire  /set-provider/(scot %p host.u.prov.ver)
      =/  priv-wire=^wire  (welp wir [%priv ~])
      [%pass priv-wire %agent dock %watch /clients/(scot %p our.bowl)]
    $(-.ver %2)
  ::
      %0
    =/  new-walts=(map xpub:bc walt)
      %-  ~(run by walts.ver)
      |=  old-walt=walt-0
      ^-  walt
      old-walt(wilt +6:wilt.old-walt)
    $(ver [%1 +.ver(walts new-walts)])
  ==
::
++  on-poke
  ~/  %on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  =^  cards  state
    ?+  mark  (on-poke:def mark vase)
        %btc-wallet-command
      ?>  =(our.bowl src.bowl)
      (handle-command !<(command vase))
      ::
        %btc-wallet-action
      ?<  =(our.bowl src.bowl)
      (handle-action !<(action vase))
      ::
        %btc-wallet-internal
      ?>  =(our.bowl src.bowl)
      (handle-internal !<(internal vase))
    ==
  [cards this]
  ::
  ++  handle-command
    |=  comm=command
    ^-  (quip card _state)
    ?>  (team:title our.bowl src.bowl)
    ?-  -.comm
        %set-provider
      |^
      ?~  provider.comm
        ?~  prov  `state
        :_  state(prov ~)
        %-  zing
        :~  (leave-provider host.u.prov)
            (give-update:hc %change-provider ~)^~
        ==
      :_  state(prov `[u.provider.comm %.n])
      ?~  prov
        (watch-provider:hc u.provider.comm)
      %-  zing
      :~  (leave-provider host.u.prov)
          (watch-provider:hc u.provider.comm)
          (give-update:hc %change-provider `[u.provider.comm %.n])^~
      ==
      ::
      ++  leave-provider
        |=  who=@p
        ^-  (list card)
        =/  wir=wire  /set-provider/(scot %p who)
        =/  priv-wir=wire  (welp wir %priv^~)
        :+  [%pass wir %agent who^%btc-provider %leave ~]
          [%pass priv-wir %agent who^%btc-provider %leave ~]
        ~
      --
    ::
        %check-provider
      =/  pax  /permitted/(scot %p provider.comm)
      :_  state
      [%pass pax %agent [provider.comm %btc-provider] %watch pax]~
    ::
        %check-payee
      =/  pax  /check-payee/(scot %p payee.comm)
      :_  state
      [%pass pax %agent [payee.comm %btc-wallet] %watch pax]~
    ::
        %set-current-wallet
      (set-curr-xpub:hc xpub.comm)
    ::
        %add-wallet
      |^
      ?~  (~(has by walts) xpub.comm)
        ((slog ~[leaf+"xpub already in wallet"]) `state)
      =/  w=walt  (from-xpub:bl +.comm)
      =.  walts  (~(put by walts) xpub.comm w)
      =^  c1  state  (init-batches xpub.comm (dec max-gap.w))
      =^  c2  state  (set-curr-xpub:hc xpub.comm)
      [(weld c1 c2) state]
      ::
      ++  init-batches
        |=  [=xpub:bc endpoint=idx]
        ^-  (quip card _state)
        =/  b=batch
          [(silt (gulf 0 endpoint)) endpoint %.n]
        =^  cards0  state  (req-scan:hc b xpub %0)
        =^  cards1  state  (req-scan:hc b xpub %1)
        :_  state
        [(scan-progress:hc xpub) (weld cards0 cards1)]
      --
    ::
        %delete-wallet
      =*  cw  curr-xpub.state
      =?  cw  ?&(?=(^ cw) =(u.cw xpub.comm))
        ~
      =.  scans    (~(del by scans) [xpub.comm %0])
      =:    scans  (~(del by scans) [xpub.comm %1])
            walts  (~(del by walts) xpub.comm)
            history
          %-  ~(rep by history)
          |=  [[=txid =hest] out=_history]
          ?:  =(xpub.hest xpub.comm)
            (~(del by out) txid)
          out
        ==
      [[give-initial:hc]~ state]
    ::
        %init-payment-external
      ?:  is-broadcasting:hc
        %-  (slog ~[leaf+"broadcasting a transaction"])
        [[(give-update:hc %error %tx-being-signed)]~ state]
      ?~  curr-xpub
        ~|("btc-wallet: no curr-xpub set" !!)
      ?:  (is-dust:hc value.comm address.comm)
        %-  (slog ~[leaf+"sending dust"])
        [[(give-update:hc %error %no-dust)]~ state]
      ::
      ~|  "no wallet with xpub"
      =/  wal  (~(got by walts) u.curr-xpub)
      ~|  "wallet not scanned yet"
      ?>  scanned.wal
      =/  [tb=(unit txbu) chng=(unit sats)]
        %~  with-change  sut:bl
        :*  wal  eny.bowl
            block.btc-state  ~
            feyb.comm  ~[[address.comm value.comm ~]]
        ==
      ?~  tb
        %-  %-  slog
          ~[leaf+"insufficient balance or not enough confirmed balance"]
        [[(give-update:hc %error %insufficient-balance)]~ state]
      =^  tb=(unit txbu)  state
        ?~  chng  `state
        =/  [addr=address =idx w=walt]
          ~(nixt-address wad:bl wal %1)
        :_  state(walts (~(put by walts) u.curr-xpub w))
        `(~(add-output txb:bl u.tb) addr u.chng `(~(hdkey wad:bl w %1) idx))
      =/  po=^poym
        ?~(tb [~ ~] [tb note.comm])
      :_  state(poym po)
      ?~  tb  ~
      %+  turn  txis.u.tb
      |=(=txi (poke-provider:hc %raw-tx txid.utxo.txi))
    ::
    ::  overwrites any payment being built in poym
    ::
        %init-payment
      ?:  =(src.bowl payee.comm)
        %-  (slog ~[leaf+"can't pay ourselves"])
        [[(give-update:hc %error %cant-pay-ourselves)]~ state]
      ?:  ?=(%pawn (clan:title payee.comm))
        %-  (slog ~[leaf+"no comets"])
        [[(give-update:hc %error %no-comets)]~ state]
      ?:  is-broadcasting:hc
        %-  (slog ~[leaf+"broadcasting a transaction"])
        [[(give-update:hc %error %tx-being-signed)]~ state]
      =:  poym   `note.comm
          feybs  (~(put by feybs) payee.comm feyb.comm)
        ==
      :_  state
       ~[(poke-peer:hc payee.comm [%gen-pay-address value.comm note.comm])]
    ::
        %broadcast-tx
      ?~  prov  ~|("Provider not connected" !!)
      =+  signed=(from-cord:hxb:bcu txhex.comm)
      =/  tx-match=?
        ?~  txbu.poym  %.n
        =((get-id:txu:bc (decode:txu:bc signed)) ~(get-txid txb:bl u.txbu.poym))
      :-  ?.  tx-match
            %-  (slog leaf+"txid didn't match txid in wallet")
            [(give-update:hc %error %broadcast-fail)]~
          ~[(poke-provider:hc [%broadcast-tx signed])]
      ?.  tx-match  state
      ?~  txbu.poym  state
      state(signed-tx.u.txbu.poym `signed)
    ::
        %gen-new-address
      ?~  curr-xpub
        ~|("btc-wallet: no curr-xpub set" !!)
      ~|  "no wallet with xpub"
      =/  wal  (~(got by walts) u.curr-xpub)
      ~|  "wallet not scanned yet"
      ?>  scanned.wal
      =/  [addr=address =idx w=walt]
        ~(gen-address wad:bl wal %0)
      :_  state(walts (~(put by walts) u.curr-xpub w))
      [(give-update:hc %new-address addr)]~
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
      ?~  curr-xpub
        ~|("btc-wallet: no curr-xpub set" !!)
      |^
      =^  cards  state  reuse-address
      ?^  cards
        ::  if cards returned, means we already have an address
        [cards state]
      =+  f=(fam:bl our.bowl now.bowl src.bowl)
      =+  n=(~(gut by num-fam.piym) f 0)
      ?:  (gte n fam-limit.params)
        ~|("More than {<fam-limit.params>} addresses for moons + planet" !!)
      =.  num-fam.piym  (~(put by num-fam.piym) f +(n))
      =^  a=address  state
        (generate-address u.curr-xpub %0)
      :-  ~[(poke-peer:hc src.bowl [%give-pay-address a value.act])]
      %_  state
          ps.piym
        %+  ~(put by ps.piym)  src.bowl
        `[u.curr-xpub a src.bowl value.act note.act]
      ==
      ::
      ++  generate-address
        |=  [=xpub:bc =chyg]
        ~|  "no wallet with xpub"
        =/  wal=walt  (~(got by walts) xpub)
        ~|  "wallet not scanned yet"
        ?>  scanned.wal
        =/  [addr=address =idx w=walt]
          ~(gen-address wad:bl wal chyg)
        [addr state(walts (~(put by walts) xpub w))]
      ::
      ++  reuse-address
        ^-  (quip card _state)
        =*  payer  src.bowl
        =+  p=(~(get by ps.piym) payer)
        ?~  p  `state
        ?^  pend.u.p
          ~|("%gen-address: {<payer>} already has pending payment to us" !!)
        =+  newp=u.p(value value.act)
        :_  state(ps.piym (~(put by ps.piym) payer newp))
        ~[(poke-peer:hc payer [%give-pay-address address.newp value.act])]
      --
      ::
        %give-pay-address
      ~|  "Can't pay ourselves"
      ?<  =(src.bowl our.bowl)
      ~|  "Broadcasting a transaction"
      ?<  is-broadcasting:hc
      ?~  curr-xpub
        ~|("btc-wallet-hook: no curr-xpub set" !!)
      ?:  (is-dust:hc value.act address.act)
        %-  (slog ~[leaf+"sending dust"])
        [[(give-update:hc %error %no-dust)]~ state]
      ::
      =/  feyb
        %+  ~(gut by feybs)
          src.bowl
        ?~(fee.btc-state fee:defaults u.fee.btc-state)
      |^
      =^  tb=(unit txbu)  state
        (generate-txbu u.curr-xpub `src.bowl feyb ~[[address.act value.act ~]])
      =/  po=^poym
        ?~(tb [~ ~] [tb note.poym])
      :_  state(poym po)
      ?~  tb  [(give-update:hc %error %insufficient-balance)]~
      %+  turn  txis.u.tb
      |=(=txi (poke-provider:hc [%raw-tx txid.utxo.txi]))
      ::
      ++  generate-txbu
        |=  [=xpub:bc payee=(unit ship) feyb=sats txos=(list txo)]
        ^-  [(unit txbu) _state]
        ~|  "no wallet with xpub"
        =/  wal  (~(got by walts) xpub)
        ~|  "wallet not scanned yet"
        ?>  scanned.wal
        =/  [tb=(unit txbu) chng=(unit sats)]
          %~  with-change  sut:bl
          [wal eny.bowl block.btc-state payee feyb txos]
        ?~  tb
          %-  %-  slog
            ~[leaf+"insufficient balance or not enough confirmed balance"]
          [tb state]
        ::  if no change, return txbu; else add change output to txbu
        ::
        ?~  chng  [tb state]
        =/  [addr=address =idx w=walt]
          ~(nixt-address wad:bl wal %1)
        :_  state(walts (~(put by walts) xpub w))
        `(~(add-output txb:bl u.tb) addr u.chng `(~(hdkey wad:bl w %1) idx))
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
      ~|  "%expect-payment: matching payment not in piym"
      =/  pay  (~(got by ps.piym) src.bowl)
      ?>  (piym-matches pay)
      :_  (update-pend-piym txid.act pay(pend `txid.act))
      ?~  prov  ~
      ~[(poke-provider:hc [%tx-info txid.act])]
      ::
      ++  piym-matches
        |=  p=payment
        ?&  =(payer.p src.bowl)
            =(value.p value.act)
        ==
      ::
      ++  update-pend-piym
        |=  [txid=hexb p=payment]
        ^-  _state
        ?~  pend.p  ~|("update-pend-piym: no pending payment" !!)
        %=  state
            ps.piym  (~(put by ps.piym) payer.p p)
            pend.piym  (~(put by pend.piym) txid p)
        ==
      --
    ==
  ::
  ++  handle-internal
    |=  intr=internal
    ^-  (quip card _state)
    ?-    -.intr
        %add-poym-raw-txi
      |^
      ?>  =(src.bowl our.bowl)
      ?~  txbu.poym  `state
      =.  txis.u.txbu.poym
        (update-poym-txis txis.u.txbu.poym +.intr)
      :_  state
      =+  pb=~(to-psbt txb:bl u.txbu.poym)
      ?~  pb  ~
      =+  vb=~(vbytes txb:bl u.txbu.poym)
      =+  fee=~(fee txb:bl u.txbu.poym)
      ~&  >>  "{<vb>} vbytes, {<(div fee vb)>} sats/byte, {<fee>} sats fee"
      %-  (slog [%leaf "PSBT: {<u.pb>}"]~)
      [(give-update:hc [%psbt u.pb fee])]~
      ::    update outgoing payment with a rawtx, if the txid is in poym's txis
      ::
      ++  update-poym-txis
        |=  [txis=(list txi) txid=hexb rawtx=hexb]
        ^-  (list txi)
        =|  i=@
        |-  ?:  (gte i (lent txis))  txis
        =/  ith=txi  (snag i txis)
        =?  txis  =(txid txid.utxo.ith)
         (snap txis i `txi`ith(rawtx `rawtx))
        $(i +(i))
      --
    ::
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
      =^  cards2  state
        (handle-tx-info:hc ti.intr)
      :_  state
      (weld cards cards2)
      ::
      ++  poym-has-txid
        |=  txid=hexb
        ^-  ?
        ?~  txbu.poym  %.n
        ?~  signed-tx.u.txbu.poym  %.n
        =(txid (get-id:txu:bc (decode:txu:bc u.signed-tx.u.txbu.poym)))
      ::   - checks whether poym has a signed tx
      ::   - checks whether the txid matches that signed tx, if not, skip
      ::   - clears poym
      ::   - returns card that adds hest to history
      ::
      ++  poym-to-history
        |=  ti=info:tx
        ^-  (quip card _state)
        |^
        ?~  txbu.poym  `state
        ?~  signed-tx.u.txbu.poym  `state
        ?.  (poym-has-txid txid.ti)
          `state
        =+  vout=(get-vout txos.u.txbu.poym)
        ?~  vout
          ~|("poym-to-history: poym should always have an output" !!)
        =/  new-hest=hest
          %:  mk-hest
              ti        xpub.u.txbu.poym
              our.bowl  payee.u.txbu.poym
              u.vout    note.poym
          ==
        :-  [(give-update:hc %new-tx new-hest)]~
        %=  state
          poym     [~ ~]
          history  (~(put by history) txid.ti new-hest)
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
        ::  if no matching output in piym,
        ::  delete from pend.piym to stop DDOS of txids
        ::
        =+  vout=(get-vout value.u.pay)
        ?~  vout
          `(del-pend-piym txid.ti)
        =/  new-hest
          (mk-hest ti xpub.u.pay payer.u.pay `our.bowl u.vout note.u.pay)
        =.  state  (del-all-piym txid.ti payer.u.pay)
        :-  [(give-update:hc %new-tx new-hest)]~
        %_  state
          history  (~(put by history) txid.ti new-hest)
        ==
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
              pend.piym     (~(del by pend.piym) txid)
              ps.piym       (~(del by ps.piym) payer)
              num-fam.piym  (~(put by num-fam.piym) payer (dec nf))
          ==
        --
      ::
      ++  mk-hest
        |=  $:  ti=info:tx
                =xpub:bc
                payer=ship
                payee=(unit ship)
                vout=@ud
                note=(unit @t)
            ==
        ^-  hest
        :*  xpub
            txid.ti
            confs.ti
            recvd.ti
            (turn inputs.ti |=(i=val:tx [i `payer]))
            %+  turn  outputs.ti
              |=  o=val:tx
              ?:  =(pos.o vout)
                ::  check whether this is the output that went to payee
                [o payee]
              [o `payer]
            note
         ==
      --
    ::
        %fail-broadcast-tx
      ?>  =(src.bowl our.bowl)
      ~&  >>>  "%fail-broadcast-tx"
      :_  state(poym [~ ~])
      [(give-update:hc %error %broadcast-fail)]~
    ::
        %succeed-broadcast-tx
      ?>  =(src.bowl our.bowl)
      ~&  >  "%succeed-broadcast-tx"
      :_  state
      :-  (give-update:hc %broadcast-success ~)
      ?~  prov  ~
      :-  (poke-provider:hc [%tx-info txid.intr])
      ?~  txbu.poym  ~
      ?~  payee.u.txbu.poym  ~
      :_  ~
      %-  poke-peer:hc
      :*  u.payee.u.txbu.poym
          %expect-payment
          txid.intr
          value:(snag 0 txos.u.txbu.poym)
      ==
    ==
  --
::
++  on-agent
  ~/  %on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?+    -.sign  (on-agent:def wire sign)
      %watch-ack
    ?~  p.sign  `this
    %-  (slog leaf+"connection rejected by provider ({<src.bowl>})" u.p.sign)
    `this
  ::
      %kick
    ?~  prov  `this
    ?.  ?&  ?=(%set-provider -.wire)
            =(host.u.prov src.bowl)
        ==
      `this
    :_  this(prov [~ src.bowl %.n])
    %-  zing
    :~  (watch-provider:hc src.bowl)
        (give-update:hc %change-provider `[src.bowl %.n])^~
    ==
  ::
      %fact
    =^  cards  state
      ?+  p.cage.sign  `state
          %btc-provider-status
        (handle-provider-status !<(status:bp q.cage.sign))
      ::
          %btc-provider-update
        (handle-provider-update !<(update:bp q.cage.sign))
      ::
          %json
        ?+  wire  `state
            [%check-payee @ ~]
          =/  who  (slav %p i.t.wire)
          :_  state
          :~  [%give %fact ~[/all] cage.sign]
              [%pass wire %agent [who %btc-wallet] %leave ~]
          ==
        ::
            [%permitted @ ~]
          =/  who  (slav %p i.t.wire)
          :_  state
          :~  [%give %fact ~[/all] cage.sign]
              [%pass wire %agent [who %btc-provider] %leave ~]
          ==
        ==
      ==
    [cards this]
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
    =^  cards  state
      ?~  prov  `state
      ?.  =(host.u.prov src.bowl)  `state
      ?-  -.s
          %new-block
        %:  on-connected
          u.prov        network.s
          block.s       fee.s
          `blockhash.s  `blockfilter.s
        ==
      ::
          %connected
        (on-connected u.prov network.s block.s fee.s ~ ~)
      ::
          %disconnected
        `state(prov `u.prov(connected %.n))
      ==
    :_  state
    :+  (give-update:hc %btc-state btc-state)
      (give-update:hc %change-provider prov)
    cards
  ::
  ++  on-connected
    |=  $:  p=provider
            net=network
            block=@ud
            fee=(unit sats)
            blockhash=(unit hexb)
            blockfilter=(unit hexb)
        ==
    ^-  (quip card _state)
    |^
    ::  request block-info for missing blocks
    ::  if blockhash or blockfilter are ~ request block-info for current block
    ::
    =|  blocks=(list @ud)
    =/  gap  (sub block block.btc-state)
    =?  blocks  (gth gap 1)
      (gulf +(block.btc-state) (dec block))
    =?  blocks  ?&((gth gap 0) ?|(?=(~ blockhash) ?=(~ blockfilter)))
      (snoc blocks block)
    =?  blocks  (gth gap 50)  ~
    :_  %_  state
          prov       `p(connected %.y)
          btc-state  [block fee now.bowl]
        ==
    %-  zing
    :~  retry-ahistorical-txs
        (retry-pend-piym net)
        (retry-block-info blocks)
      ::
        ?.  ?|(!connected.p (gth gap 0))
          ~
        %-  zing
        :~  (retry-poym net)
            (retry-txs net)
            (retry-scans net)
        ==
      ::
        ?.  ?&(?=(^ blockhash) ?=(^ blockfilter) (gth gap 0))
          ~
        (retry-filtered-addrs:hc net u.blockhash u.blockfilter)
      ::
        ?.  (gth gap 50)
          ~
        (retry-addrs net)
    ==
    ::
    ++  retry-ahistorical-txs
      ^-  (list card)
      %+  turn  ~(tap in ahistorical-txs)
      |=  =txid
      (poke-provider:hc [%tx-info txid])
    ::
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
      `(poke-provider:hc [%tx-info txid])
    ::
    ++  retry-block-info
      |=  blocks=(list @ud)
      %+  turn  blocks
      |=  block=@ud
      (poke-provider:hc %block-info `block)
    ::
    ++  retry-poym
      |=  =network
      ^-  (list card)
      ?~  txbu.poym  ~
      =/  w  (~(get by walts) xpub.u.txbu.poym)
      ?~  w  ~
      ?.  =(network network.u.w)  ~
      %+  weld
        ?~  signed-tx.u.txbu.poym  ~
        ~[(poke-provider:hc [%broadcast-tx u.signed-tx.u.txbu.poym])]
      %+  turn  txis.u.txbu.poym
      |=  =txi
      (poke-provider:hc [%raw-tx ~(get-txid txb:bl u.txbu.poym)])
    ::
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
      `(poke-provider:hc [%tx-info txid])
    ::
    ++  retry-scans
      |=  =network
      ^-  (list card)
      %-  zing
      %+  murn  ~(tap by scans)
      |=  [[=xpub:bc =chyg] =batch]
      =/  w  (~(get by walts) xpub)
      ?~  w  ~
      ?.  =(network network.u.w)     ~
      `-:(req-scan:hc batch xpub chyg)
    ::
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
      %+  turn  ~(tap by wach.w)
      |=  [a=address *]
      (poke-provider:hc [%address-info a])
    --
  ::
  ++  handle-provider-update
    |=  upd=update:bp
    ^-  (quip card _state)
    |^
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
      ::  TODO: why do we get a nest-fail when using =^ ?
      =/  [cards=(list card) sty=state-2]
        (handle-tx-info:hc info.p.upd)
      :_  sty
      :_  cards
      (poke-internal:hc [%close-pym info.p.upd])
    ::
        %raw-tx
      :_  state
      ~[(poke-internal:hc [%add-poym-raw-txi +.p.upd])]
    ::
        %broadcast-tx
      :_  state
      ?~  txbu.poym  ~
      ?.  =(~(get-txid txb:bl u.txbu.poym) txid.p.upd)
        ~
      ?:  ?|(broadcast.p.upd included.p.upd)
        ~[(poke-internal:hc [%succeed-broadcast-tx txid.p.upd])]
      :~  (poke-internal:hc [%fail-broadcast-tx txid.p.upd])
          (give-update:hc %cancel-tx txid.p.upd)
      ==
    ::
        %block-info
      :_  state
      %^  retry-filtered-addrs:hc
          network.p.upd
        blockhash.p.upd
      blockfilter.p.upd
    ==
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
      =/  ac  (address-coords:bl address ~(val by walts))
      ?~  ac
        `state
      =/  [w=walt =chyg =idx]  u.ac
      =.  walts
        %+  ~(put by walts)  xpub.w
        %+  ~(update-address wad:bl w chyg)
          address
       [used chyg idx utxos]
      ::  if transactions haven't made it into history, request transaction info
      ::
      =^  cards=(list card)  ahistorical-txs
        %+  roll  ~(tap in utxos)
        |=  [u=utxo cad=(list card) ah=(set txid)]
        ^-  [(list card) (set txid)]
        ?:  (~(has by history) txid.u)
          [cad ah]
        :-  [(poke-provider:hc [%tx-info txid.u]) cad]
        (~(put by ah) txid.u)
      ::  if the wallet+chyg is being scanned, update the scan batch
      ::
      =/  b  (~(get by scans) [xpub.w chyg])
      ?~  b
        [cards state]
      =.  scans
       (del-scanned u.b(has-used ?|(used has-used.u.b)) xpub.w chyg idx)
      ?:  empty:(scan-status xpub.w chyg)
        =^  scan-cards=(list card)  state
          (check-scan xpub.w)
        [(weld scan-cards cards) state]
      ::
      [cards state]
    ::
    ::  +del-scanned: delete scanned idxs
    ::
    ++  del-scanned
      |=  [b=batch =xpub:bc =chyg to-delete=idx]
      ^-  ^scans
      %+  ~(put by scans)  [xpub chyg]
      b(todo (~(del in todo.b) to-delete))
    ::
    ++  scan-status
      |=  [=xpub:bc =chyg]
      ^-  [empty=? done=?]
      =/  b=batch  (~(got by scans) [xpub chyg])
      =/  empty=?  =(0 ~(wyt in todo.b))
      :-  empty
      ?&(empty ?!(has-used.b))
    ::  +check-scan: initiate a scan if one hasn't started
    ::               check status of scan if one is running
    ::
    ++  check-scan
      |=  =xpub:bc
      ^-  (quip card _state)
      =/  s0  (scan-status xpub %0)
      =/  s1  (scan-status xpub %1)
      ?:  ?&(empty.s0 done.s0 empty.s1 done.s1)
        (end-scan xpub)
      =^  cards0=(list card)  state
        (bump-batch xpub %0)
      =^  cards1=(list card)  state
        (bump-batch xpub %1)
      :_  state
      [(scan-progress:hc xpub) (weld cards0 cards1)]
    ::
    ::  delete the xpub from scans and set wallet to scanned
    ::
    ++  end-scan
      |=  [=xpub:bc]
      ^-  (quip card _state)
      =/  w=walt  (~(got by walts) xpub)
      =.  scans  (~(del by scans) [xpub %0])
      =:  scans  (~(del by scans) [xpub %1])
          walts  (~(put by walts) xpub w(scanned %.y))
        ==
      %-  (slog ~[leaf+"Scanned xpub {<xpub>}"])
      =^  cards  state
        (set-curr-xpub:hc xpub)
      [[(give-update:hc [%scan-progress ~ ~]) cards] state]
    ::
    ::  +bump-batch
    ::  if the batch is done but the wallet isn't done scanning,
    ::  returns new address requests and updated batch
    ::
    ++  bump-batch
      |=  [=xpub:bc =chyg]
      ^-  (quip card _state)
      =/  b=batch  (~(got by scans) xpub chyg)
      =/  s  (scan-status xpub chyg)
      ?.  ?&(empty.s ?!(done.s))
        `state
      =/  w=walt  (~(got by walts) xpub)
      =/  newb=batch
        :+  (silt (gulf +(endpoint.b) (add endpoint.b max-gap.w)))
          (add endpoint.b max-gap.w)
        %.n
      (req-scan:hc newb xpub chyg)
    --
  --
::
++  on-peek
  ~/  %on-peek
  |=  pax=path
  ^-  (unit (unit cage))
  ?+  pax  (on-peek:def pax)
      [%x %configured ~]
    =/  provider=json
      ?~  prov  ~
      [%s (scot %p host.u.prov)]
    =/  result=json
      %-  pairs:enjs:format
      :~  [%provider provider]
          [%'hasWallet' b+?=(^ walts)]
      ==
    ``json+!>(result)
  ::
      [%x %scanned ~]
    ``noun+!>(scanned-wallets:hc)
  ::
      [%x %balance @ ~]
    ``noun+!>((balance:hc (xpub:bc +>-.pax)))
  ==
::
++  on-watch
  ~/  %on-watch
  |=  =path
  ^-  (quip card _this)
  ?+  path  (on-watch:def path)
      [%check-payee @ ~]
    =/  who  (slav %p i.t.path)
    ?>  =(who our.bowl)
    =/  response=json
      %+  frond:enjs:format  'checkPayee'
      %-  pairs:enjs:format
      :~  ['hasWallet' b+?=(^ curr-xpub)]
          ['payee' (ship:enjs:format our.bowl)]
      ==
    :_  this
    [%give %fact ~ %json !>(response)]~
  ::
      [%all ~]
    ?>  (team:title our.bowl src.bowl)
    :_  this
    [give-initial:hc]~
  ==
::
++  on-leave  on-leave:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
~%  %btc-wallet-helper  ..card  ~
|_  =bowl:gall
::
++  retry-filtered-addrs
  ~/  %retry-filtered-addrs
  |=  [=network blockhash=hexb blockfilter=hexb]
  ^-  (list card)
  %-  zing
  %+  murn  ~(val by walts)
  |=  w=walt
  ^-  (unit (list card))
  ?.  =(network network.w)  ~
  :-  ~
  %+  turn
    %~  tap  in
    %^  all-match:bip-b158:bc
        blockfilter
      blockhash
    %+  turn  ~(tap by wach.w)
    |=  [a=address *]
    [a (to-script-pubkey:adr:bc a)]
  |=  [a=address spk=hexb]
  ^-  card
  (poke-provider [%address-info a])
::
++  handle-tx-info
  ~/  %handle-tx-info
  |=  ti=info:tx
  ^-  (quip card _state)
  |^
  =/  h  (~(get by history) txid.ti)
  =.  ahistorical-txs  (~(del in ahistorical-txs) txid.ti)
  =/  our-inputs=(set address)
    %-  silt
    %+  skim
      %+  turn  inputs.ti
      |=(=val:tx address.val)
    is-our-address
  =/  our-outputs=(set address)
    %-  silt
    %+  skim
      %+  turn  outputs.ti
      |=(=val:tx address.val)
    is-our-address
  ::  all our addresses in inputs/outputs of tx
  ::
  =/  our-addrs=(set address)
    (~(uni in our-inputs) our-outputs)
  ::
  =/  addr-info-cards=(list card)
    %+  turn  ~(tap in our-addrs)
    |=  a=address
    ^-  card
    (poke-provider [%address-info a])
  ?:  =(0 ~(wyt in our-addrs))  `state
  =/  =xpub
    xpub.w:(need (address-coords:bl (snag 0 ~(tap in our-addrs)) ~(val by walts)))
  ::  addresses in wallets, but tx not in history
  ::
  ?~  h
    =/  new-hest=hest  (mk-hest xpub our-inputs our-outputs)
    =.  history  (~(put by history) txid.ti new-hest)
    :_  state
    :+  (give-update %balance current-balance)
      (give-update %new-tx new-hest)
    addr-info-cards
  ::  tx in history, but not in mempool/blocks
  ::
  ?.  included.ti
    :_  state(history (~(del by history) txid.ti))
    :+  (give-update %balance current-balance)
      (give-update %cancel-tx txid.ti)
    addr-info-cards
  =/  new-hest  u.h(confs confs.ti, recvd recvd.ti)
  =.  history  (~(put by history) txid.ti new-hest)
  :_  state
  :+  (give-update %balance current-balance)
    (give-update %new-tx new-hest)
  addr-info-cards
  ::
  ++  mk-hest
    :: has tx-info
    |=  [=xpub:bc our-inputs=(set address) our-outputs=(set address)]
    ^-  hest
    :*  xpub
        txid.ti
        confs.ti
        recvd.ti
        (turn inputs.ti |=(v=val:tx (is-our-ship our-inputs v)))
        (turn outputs.ti |=(v=val:tx (is-our-ship our-outputs v)))
        ~
    ==
  ::
  ++  is-our-ship
    |=  [as=(set address) v=val:tx]
    ^-  [=val:tx s=(unit ship)]
    [v ?:((~(has in as) address.v) `our.bowl ~)]
  ::
  ++  is-our-address
    |=(a=address ?=(^ (address-coords:bl a ~(val by walts))))
  --
::
++  set-curr-xpub
  |=  =xpub
  ^-  (quip card _state)
  ?~  (find ~[xpub] scanned-wallets)  `state
  =.  curr-xpub  `xpub
  :_  state
  [give-initial]~
::
::  +req-scan
::   - adds addresses in batch to wallet's watch map as un-used addresses
::   - returns provider %address-info request cards
::
++  req-scan
  ~/  %req-scan
  |=  [b=batch =xpub:bc =chyg]
  ^-  (quip card _state)
  =/  w=walt  (~(got by walts) xpub)
  =/  as=(list [address [? ^chyg idx (set utxo)]])
    %+  turn  ~(tap in todo.b)
    |=(=idx [(~(mk-address wad:bl w chyg) idx) [%.n chyg idx *(set utxo)]])
  =.  w
    |-  ?~  as  w
    $(as t.as, w (~(update-address wad:bl w chyg) -.i.as +.i.as))
  :-  (turn as |=([a=address *] (poke-provider [%address-info a])))
  %_    state
      scans
    (~(put by scans) [xpub chyg] b)
  ::
      walts
    (~(put by walts) xpub w)
  ==
::
++  poke-provider
  |=  act=action:bp
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
::
++  poke-internal
  |=  intr=internal
  ^-  card
  :*  %pass  /[(scot %da now.bowl)]  %agent
      [our.bowl %btc-wallet]  %poke
      %btc-wallet-internal  !>(intr)
  ==
::
++  poke-our
  |=  [app=term =cage]
  ^-  card
  [%pass / %agent [our.bowl app] %poke cage]
::
++  give-update
  |=  upd=update
  ^-  card
  [%give %fact ~[/all] %btc-wallet-update !>(upd)]
::
++  scan-progress
  |=  [=xpub:bc]
  |^  ^-  card
  %-  give-update
  :+  %scan-progress
  (to-idx (~(gut by scans.state) [xpub %0] *batch))
  (to-idx (~(gut by scans.state) [xpub %1] *batch))
  ++  to-idx
    |=  b=batch
    ^-  (unit idx:bc)
    =/  s=(list idx:bc)
      (sort ~(tap in todo.b) lth)
    ?~  s  ~  `i.s
  --
::
++  watch-provider
  |=  who=@p
  ^-  (list card)
  =/  =dock  [who %btc-provider]
  =/  wir=wire  /set-provider/(scot %p who)
  =/  priv-wire=^wire  (welp wir [%priv ~])
  :+  [%pass wir %agent dock %watch /clients]
    [%pass priv-wire %agent dock %watch /clients/(scot %p our.bowl)]
  ~
::
++  give-initial
  ^-  card
  =^  a=(unit address)  state
    ?~  curr-xpub  `state
    =/  uw=(unit walt)  (~(get by walts) u.curr-xpub)
    ?:  ?|(?=(~ uw) ?!(scanned.u.uw))
      ~|("no wallet with xpub or wallet not scanned yet" !!)
    =/  [addr=address =idx w=walt]
      ~(gen-address wad:bl u.uw %0)
    [`addr state(walts (~(put by walts) u.curr-xpub w))]
  %-  give-update
  ^-  update
  :*  %initial
      prov
      curr-xpub
      current-balance
      current-history
      btc-state
      a
  ==
::
++  current-balance
  ^-  (unit [sats sats])
  ?~  curr-xpub  ~
  (balance u.curr-xpub)
::
++  current-history
  ^-  ^history
  ?~  curr-xpub  ~
  %-  ~(gas by *^history)
  %+  skim  ~(tap by history)
  |=  [txid =hest]
  =(u.curr-xpub xpub.hest)
::
++  balance
  ~/  %balance
  |=  =xpub:bc
  ^-  (unit [sats sats])
  =/  w  (~(get by walts) xpub)
  ?~  w  ~
  =/  values=(list [confirmed=sats unconfirmed=sats])
    %+  turn  ~(val by wach.u.w)
    |=  =addi  ^-  [sats sats]
    %+  roll
      %+  turn  ~(tap by utxos.addi)
      |=  =utxo
      ^-  [sats sats]
      ?:  (~(spendable sut:bl [u.w eny.bowl block.btc-state ~ 0 ~]) utxo)
        [value.utxo 0]
      [0 value.utxo]
    |=  [[a=sats b=sats] out=[p=sats q=sats]]
    [(add a p.out) (add b q.out)]
  :-  ~
  %+  roll  values
  |=  [[a=sats b=sats] out=[p=sats q=sats]]
  [(add a p.out) (add b q.out)]
::
++  is-dust
  |=  [=sats =address]
  ^-  ?
  %+  lth  sats
  (mul 3 (input-weight:bc (get-bipt:adr:bc address)))
::
++  is-broadcasting
  ^-  ?
  ?~  txbu.poym  %.n
  ?=(^ signed-tx.u.txbu.poym)
::
++  scanned-wallets
  ^-  (list xpub:bc)
  %+  murn  ~(tap by walts)
  |=  [=xpub:bc w=walt]
  ^-  (unit xpub:bc)
  ?:(scanned.w `xpub ~)
::
++  gall-scry
  |*  [=mold app=@tas =path]
  .^(mold %gx (weld /(scot %p our.bowl)/[app]/(scot %da now.bowl) path))
--
