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
::    /sign-me
::
/-  *btc, *btc-wallet-hook, bws=btc-wallet-store
/+  dbug, default-agent, bwsl=btc-wallet-store, bp=btc-provider
|%
++  defaults
  |%
  ++  moon-limit  10
  --
+$  versioned-state
    $%  state-0
    ==
::  provider: maybe ship if provider is set
::  moon-limit: how many addresses a ship and its moons can request in piym
::  piym/poym-watch: listen to btc-wallet-store for address updates; update payment info
::
+$  state-0
  $:  %0
      provider=(unit [host=ship connected=?])
      =btc-state
      def-wallet=(unit xpub)
      moon-limit=@ud
      =pend-addr
      =piym
      =poym
      =piym-watch
      =poym-watch
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
  :_  this(moon-limit.state moon-limit:defaults)
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
::  TODO: handle /sign-me path
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+  -.sign  (on-agent:def wire sign)
      %kick
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
++  on-arvo   on-arvo:def
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
      %req-pay-address
    :: TODO: add whitelisting here instead of comet block
    ::  can't pay yourself; comets can't pay (could spam requests)
    ::  forwards poke to payee if payee isn't us
    ::  wire is /payer/value/timestamp
    ::
    ?<  =(src.bowl payee.act)
    ?<  ?=(%pawn (clan:title src.bowl))
    :_  state
    ?.  =(payee.act our.bowl)
      ~[(poke-wallet-hook payee.act act)]
    ?~  def-wallet  ~|("btc-wallet-hook: no def-wallet set" !!)
    ~[(poke-wallet-store [%generate-address u.def-wallet %0 `[src.bowl value.act]])]
    ::
      %ret-pay-address
    ?~  def-wallet  ~|("btc-wallet-hook: no def-wallet set" !!)
    ?>  =(payer.act our.bowl)
    :_  state
    :~  %-  poke-wallet-store
        [%generate-txbu u.def-wallet `src.bowl fee.btc-state ~[[address.act value.act]]]
    ==
    ::
      %force-retry
    [(retry pend-addr) state]
  ==
::  if status is %connected, retry all pending address lookups
::  only retry if previously disconnected
::
++  handle-provider-status
  |=  s=status:bp
  ^-  (quip card _state)
  ?~  provider  `state
  ?.  =(host.u.provider src.bowl)  `state
  ?-  -.s
      %connected
    :-  ?:(connected.u.provider ~ (retry pend-addr))
    %=  state
        provider  `[host.u.provider %.y]
        btc-state  [blockcount.s fee.s now.bowl]
    ==
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
    =/  ureq  (~(get by pend-addr) req-id.p.upd)
    ?~  ureq  `state
    :_  state(pend-addr (~(del by pend-addr) req-id.p.upd))
    :~  %-  poke-wallet-store
        :*  %address-info  xpub.u.ureq  chyg.u.ureq  idx.u.ureq
            utxos.body.p.upd  used.body.p.upd  blockcount.body.p.upd
        ==
    ==
    ::
      %raw-tx
    =.  state  (update-poym +.body.p.upd)
    :_  state
    ?.  poym-ready  ~
    ~[(send-tx poym)]
  ==
::
++  handle-wallet-store-request
  |=  req=request:bws
  ^-  (quip card _state)
  ?-  -.req
      %scan-address
    =/  ri=req-id:bp  (gen-req-id:bp eny.bowl)
    :_  state(pend-addr (~(put by pend-addr) ri req))
    ?~  provider  ~
    ?:  provider-connected
      ~[(get-address-info ri host.u.provider a.req)]
    ~&  >  "provider not connected"
    ~
  ==
::
++  handle-wallet-store-update
  |=  [=wire upd=update:bws]
  ^-  (quip card _state)
  ?-  -.upd
      %generate-address
    ::  if no meta (payer/value), just prints address
    ::
    ?~  meta.upd  ~&(> address.upd `state)
    =/  [payer=ship value=sats]  u.meta.upd
    :-  ~[(poke-wallet-hook payer [%ret-pay-address address.upd payer value])]
    (update-piym address.upd u.meta.upd)
    ::
      %generate-txbu
    ::  txbus can potentially use the same UTXO inputs, so if another payment
    ::   was in process of fetching raw-txs for a txbu, replace it
    ::
    :_  state(poym [payee.upd txbu.upd])
    ?~  provider  ~&(>>> "provider not set" ~)
    %+  turn  txis.txbu.upd
    |=(=txi:bws (get-raw-tx host.u.provider txid.utxo.txi))
    ::
      %scan-done
    ?~  def-wallet
      `state(def-wallet `xpub.upd)
    `state
  ==
::  update piym with a payment
::  moons are stored with their sponsor
::  if ship already has a payment for the payer ship, replace
::
++  update-piym
  |=  p=payment
  |^  ^-  _state
  =/  fam=ship
    ?:  =(%earl (clan:title payer.p))
      (sein:title our.bowl now.bowl payer.p)
    payer.p
  =/  ups=(unit (list payment))
    (~(get by piym) fam)
  ?~  ups  (insert fam ~[p])
  ~|  "btc-wallet-hook: too many address requests from moons"
  ?>  (lte (lent u.ups) moon-limit.state)
  =/  i=(unit @)
    (find ~[payer.p] (turn u.ups |=([* py=ship *] py)))
  ?~  i  (insert fam [p u.ups])
  (insert fam (snap u.ups u.i p))
  ++  insert
    |=  [fam=ship ps=(list payment)]
    state(piym (~(put by piym) fam ps))
  --
::
++  update-poym
  |=  [=txid rt=rawtx]
  ^-  _state
  =*  txis  txis.txbu.poym
  =|  i=@
  |-
  ?:  (gte i (lent txis))  state
  =/  ith=txi:bws  (snag i txis)
  =?  txis  =(txid txid.utxo.ith)
   (snap txis i `txi:bws`ith(ur `rt))
  $(i +(i))
::  poym-ready: do we have all rawtx for inputs?
::
++  poym-ready
  ^-  ?
  %+  levy  txis.txbu.poym
  |=(t=txi:bws ?=(^ ur.t))
::
++  retry
  |=  p=^pend-addr
  ^-  (list card)
  ?~  provider  ~|("provider not set" !!)
  %+  turn  ~(tap by p)
  |=  [ri=req-id:bp req=request:bws]
  (get-address-info ri host.u.provider a.req)
::
++  get-address-info
  |=  [ri=req-id:bp host=ship a=address]
  ^-  card
  :*  %pass  /[(scot %da now.bowl)]  %agent  [host %btc-provider]
      %poke  %btc-provider-action  !>([ri %address-info a])
  ==
::
++  get-raw-tx
  |=  [host=ship =txid]
  ^-  card
  =/  ri=req-id:bp  (gen-req-id:bp eny.bowl)
  :*  %pass  /[(scot %da now.bowl)]  %agent  [host %btc-provider]
      %poke  %btc-provider-action  !>([ri %raw-tx txid])
  ==
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
++  send-tx
  |=  p=^poym
  ^-  card
  [%give %fact ~[/sign-me] %btc-wallet-hook-request !>([%sign-tx p])]
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
