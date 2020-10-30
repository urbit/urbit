# btc-wallet-* Scratch Code

## xpub
```
=xpub1 "zpub6r8dKyWJ31XF6n69KKeEwLjVC5ruqAbiJ4QCqLsrV36Mvx9WEjUaiPNPGFLHNCCqgCdy6iZC8ZgHsm6a1AUTVBMVbKGemNcWFcwBGSjJKbD"
=xpub2 "xpub6D7yaZieZEeG617UcKXDhbsDeso6bmxSAiGWkvkASoiwcjaRtrH5HeNRnDT25s7zmxYzj6MtFe32dVqcf9YcBKKgn9THHjwn2uSjkvobK4e"
=bl -build-file %/lib/btc-wallet-store/hoon
```

### add
```
:btc-wallet-store|action [%add-wallet xpub ~ ~]
```

### get address at indices
```
=walt1 (from-xpub:walt:bl xpub1 ~ ~)
=walt2 (from-xpub:walt:bl xpub2 ~ ~)
(get-address:walt1 %0 0)
(get-address:walt2 %0 0)
```


## scratch code, refactor
++  update-address
  |=  [a=address:btc us=(set utxo)]
  ^-  (quip card _state)
  =/  xpubs=(list tape)
    %~  tap  in
    ~(key by walts.state)
  |-  ?~  xpubs  `state
  =/  w=walt  (~(got by walts.state) i.xpubs)
  ?:  (~(has by wach.w) a)
    %:  send-address-update
        i.xpubs
        (update-wallet w a us)
        a
        us
    ==
  $(xpubs t.xpubs)
::
++  update-wallet
  |=  [w=walt a=address:btc us=(set utxo)]
  ^-  walt
  =/  curr-addi=addi
    (~(got by wach.w) a)
  w(wach (~(put by wach.w) a curr-addi(used %.y, utxos us)))
::
++  send-address-update
  |=  [xpub=tape =walt a=address:btc us=(set utxo)]
  ^-  (quip card _state)
  :_  state(walts (~(put by walts.state) xpub walt))
  ~[[%give %fact ~[/wallets] %btc-wallet-store-update !>([%address a us])]]
::
++  add-wallet
  |=  [xpub=tape scan-to=(unit scon) max-gap=(unit @)]
  ^-  (quip card _state)
  ?:  (~(has by walts.state) xpub)
    ~&  >>>  "xpub already imported"
    `state
  =/  wallet=walt
    :*  (from-extended:bip32 xpub)
        (xpub-type:btc xpub)
        *wach
        [0 0]
        %.n
        (fall scan-to *scon)
        (fall max-gap max-gap.state)
    ==
  `state(walts (~(put by walts.state) xpub wallet))
