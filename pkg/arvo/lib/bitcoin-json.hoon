/-  btc-wallet, btc-provider, bitcoin
|%
++  dejs
  =,  dejs:format
  |%
  ++  command
    |=  jon=json
    ^-  command:btc-wallet
    %.  jon
    %-  of
    :~  set-provider+ship
        check-provider+ship
        set-current-wallet+so
        add-wallet+add-wallet
        delete-wallet+so
        init-payment+init-payment
        broadcast-tx+so
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
  ++  init-payment
    %-  ot
    :~  payee+ship
        value+ni
        feyb+ni
    ==
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
  --
--
