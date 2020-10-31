::
::
/-  *btc-wallet-store
/+  bip32, btc
=,  secp:crypto
=+  ecc=secp256k1
|%
++  default-max-gap  20
::  xpub
::  wilt
::  bipt: BIP44/49/84
::  next: next index to generate address for in non-change/change accounts respectively
::  scanned: whether the wallet's addresses have been checked for prior activity
::           if unscanned, 'next' values won't be valid
::  scan-to
::  max-gap
::
++  walt
  |_  [=wilt =bipt =wach =nixt scanned=? scan-to=scon max-gap=@]
  +*  this  .
  ::
  ++  from-xpub
    |=  [=xpub:btc scan-to=(unit scon) max-gap=(unit @)]
    ^-  _this
    %=  this
        wilt  (from-extended:bip32 (trip xpub))
        bipt  (xpub-type:btc xpub)
        wach  *^wach
        nixt  [0 0]
        scanned  %.n
        scan-to  (fall scan-to *scon)
        max-gap  (fall max-gap default-max-gap)
    ==
  ::  generate an address; add it to wach (i.e. treat it as used and out in the wild)
  ::
  ++  gen-address
    |=  =chyg
    ^-  (pair address:btc _this)
    =/  addr=address:btc
      (mk-address chyg (get-nixt chyg))
    :-  addr
    %=  this
        wach  (~(put by wach) addr [chyg (get-nixt chyg) *(set utxo:btc)])
        nixt  (bump-nixt chyg)
    ==
  ::
  ++  mk-address
    |=  [=chyg =idx]
    ^-  address:btc
    =/  pubkey=@ux
      %-  compress-point:ecc
      pub:(derive-public:(derive-public:wilt (@ chyg)) idx)
    ?:  ?=(%bip84 bipt)
      (need (encode-pubkey:bech32:btc %main pubkey))
    ~|("legacy addresses not supported yet" !!)
  ::  insert a new address; update "nixt" free address if this one was it
  ::
  ++  insert-address
    |=  [a=address:btc =addi]
    ^-  _this
    =?  nixt  (is-nixt addi)
      (bump-nixt chyg.addi)
    this(wach (~(put by wach) a addi))
  ::  update an address if it's in our wach (we're watching it already)
  ::
  ++  update-address
    |=  [a=address:btc utxos=(set utxo:btc)]
    ^-  _this
    =/  adi=(unit addi)
      (~(get by wach) a)
    ?~  adi  this
    this(wach (~(put by wach) a u.adi(utxos utxos)))
  ::
  ++  is-nixt
  |=  =addi  ^-  ?
  ?:  ?=(%0 chyg.addi)
    =(idx.addi p.nixt)
  =(idx.addi q.nixt)
  ::  Returns the next unused index in the (non-)change account
  ::
  ++  bump-nixt
    |=  =chyg  ^-  ^nixt
    =/  new-idx=idx  (add 1 (get-nixt chyg))
    |-  ?>  (lte new-idx max-index)
    ?.  (~(has by wach) (mk-address chyg new-idx))
        (set-nixt chyg new-idx)
    $(new-idx +(new-idx))
  ++  get-nixt
    |=  =chyg  ?:(?=(%0 chyg) p.nixt q.nixt)
  ++  set-nixt
    |=  [=chyg idx=@]  ^-  ^nixt
    ?:(?=(%0 chyg) [idx q.nixt] [p.nixt idx])
  --
--
