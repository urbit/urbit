/-  btc-wallet, btc-provider, bitcoin
/+  bl=bitcoin
|%
++  dejs
  =,  dejs:format
  |%
  ++  command
    |=  jon=json
    ^-  command:btc-wallet
    %.  jon
    %-  of
    :~  set-provider+(mu ship)
        check-provider+ship
        check-payee+ship
        set-current-wallet+so
        add-wallet+add-wallet
        delete-wallet+so
        init-payment-external+init-payment-external
        init-payment+init-payment
        broadcast-tx+so
        gen-new-address+|=(json ~)
    ==
  ::
  ++  ship  (su ;~(pfix sig fed:ag))
  ::
  ++  add-wallet
    %-  ot
    :~  xpub+so
        fprint+(at [ni ni ~])
        scan-to+(mu (at [ni ni ~]))
        max-gap+(mu ni)
        confs+(mu ni)
    ==
  ::
  ++  init-payment-external
    %-  ot
    :~  address+address
        value+ni
        feyb+ni
        note+(mu so)
    ==
  ::
  ++  init-payment
    %-  ot
    :~  payee+ship
        value+ni
        feyb+ni
        note+(mu so)
    ==
  ::
  ++  address
    |=  jon=json
    ?>  ?=([%s @t] jon)
    ^-  address:bitcoin
    (from-cord:adr:bl +.jon)
  --
::
++  enjs
  =,  enjs:format
  |%
  ++  status
    |=  sta=status:btc-provider
    ^-  json
    %+  frond  -.sta
    ?-  -.sta
      %connected    (connected sta)
      %new-block    (new-block sta)
      %disconnected  ~
    ==
  ::
  ++  connected
    |=  sta=status:btc-provider
    ?>  ?=(%connected -.sta)
    %-  pairs
    :~  network+s+network.sta
        block+(numb block.sta)
        fee+?~(fee.sta ~ (numb u.fee.sta))
    ==
  ::
  ++  new-block
    |=  sta=status:btc-provider
    ?>  ?=(%new-block -.sta)
    %-  pairs
    :~  network+s+network.sta
        block+(numb block.sta)
        fee+?~(fee.sta ~ (numb u.fee.sta))
        blockhash+(hexb blockhash.sta)
        blockfilter+(hexb blockfilter.sta)
    ==
  ::
  ++  hexb
    |=  h=hexb:bitcoin
    ^-  json
    %-  pairs
    :~  wid+(numb:enjs wid.h)
        dat+s+(scot %ux dat.h)
    ==
  ::
  ++  update
    |=  upd=update:btc-wallet
    ^-  json
    %+  frond  -.upd
    ?-  -.upd
      %initial             (initial upd)
      %change-provider     (change-provider upd)
      %change-wallet       (change-wallet upd)
      %psbt                (psbt upd)
      %btc-state           (btc-state btc-state.upd)
      %new-tx              (hest hest.upd)
      %cancel-tx           (hexb txid.upd)
      %new-address         (address address.upd)
      %balance             (balance balance.upd)
      %scan-progress       (scan-progress main.upd change.upd)
      %error               s+error.upd
      %broadcast-success   ~
    ==
  ::
  ++  initial
    |=  upd=update:btc-wallet
    ?>  ?=(%initial -.upd)
    ^-  json
    %-  pairs
    :~  provider+(provider provider.upd)
        wallet+?~(wallet.upd ~ [%s u.wallet.upd])
        balance+(balance balance.upd)
        history+(history history.upd)
        btc-state+(btc-state btc-state.upd)
        address+?~(address.upd ~ (address u.address.upd))
    ==
  ::
  ++  change-provider
    |=  upd=update:btc-wallet
    ?>  ?=(%change-provider -.upd)
    ^-  json
    (provider provider.upd)
  ::
  ++  change-wallet
    |=  upd=update:btc-wallet
    ?>  ?=(%change-wallet -.upd)
    ^-  json
    %-  pairs
    :~  wallet+?~(wallet.upd ~ [%s u.wallet.upd])
        balance+(balance balance.upd)
        history+(history history.upd)
    ==
  ::
  ++  psbt
    |=  upd=update:btc-wallet
    ?>  ?=(%psbt -.upd)
    ^-  json
    %-  pairs
    :~  pb+s+pb.upd
        fee+(numb fee.upd)
    ==
  ::
  ++  balance
    |=  b=(unit [p=@ q=@])
    ^-  json
    ?~  b  ~
    %-  pairs
    :~  confirmed+(numb p.u.b)
        unconfirmed+(numb q.u.b)
    ==
  ::
  ++  scan-progress
    |=  [main=(unit idx:bitcoin) change=(unit idx:bitcoin)]
    |^  ^-  json
    %-  pairs
    :~  main+(from-unit main)
        change+(from-unit change)
    ==
    ++  from-unit
      |=  i=(unit idx:bitcoin)
      ?~  i  ~
      (numb u.i)
    --
  ::
  ++  btc-state
    |=  bs=btc-state:btc-wallet
    ^-  json
    %-  pairs
    :~  block+(numb block.bs)
        fee+?~(fee.bs ~ (numb u.fee.bs))
        date+(sect t.bs)
    ==
  ::
  ++  provider
    |=  p=(unit provider:btc-wallet)
    ^-  json
    ?~  p  ~
    %-  pairs
    :~  host+(ship host.u.p)
        connected+b+connected.u.p
    ==
  ::
  ++  history
    |=  hy=history:btc-wallet
    ^-  json
    :-   %o
    ^-  (map @t json)
    %-  ~(rep by hy)
    |=  [[=txid:btc-wallet h=hest:btc-wallet] out=(map @t json)]
    ^-  (map @t json)
    (~(put by out) (scot %ux dat.txid) (hest h))
  ::
  ++  hest
    |=  h=hest:btc-wallet
    ^-  json
    %-  pairs
    :~  xpub+s+xpub.h
        txid+(hexb txid.h)
        confs+(numb confs.h)
        recvd+?~(recvd.h ~ (sect u.recvd.h))
        inputs+(vals inputs.h)
        outputs+(vals outputs.h)
        note+?~(note.h ~ [%s u.note.h])
    ==
  ::
  ++  vals
    |=  vl=(list [=val:tx:bitcoin s=(unit @p)])
    ^-  json
    :-  %a
    %+  turn  vl
    |=  [v=val:tx:bitcoin s=(unit @p)]
    %-  pairs
    :~  val+(val v)
        ship+?~(s ~ (ship u.s))
    ==
  ::
  ++  val
    |=  v=val:tx:bitcoin
    ^-  json
    %-  pairs
    :~  txid+(hexb txid.v)
        pos+(numb pos.v)
        address+(address address.v)
        value+(numb value.v)
    ==
  ::
  ++  address
    |=  a=address:bitcoin
    ^-  json
    ?-  -.a
      %base58  [%s (rsh [3 2] (scot %uc +.a))]
      %bech32  [%s +.a]
    ==
  --
--
