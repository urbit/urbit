::
::
/-  *btc-wallet-store
/+  bip32, btc
=,  secp:crypto
=+  ecc=secp256k1
|%
++  defaults
  |%
  ++  max-gap  20
  ++  confs    6
  --
::
++  hash-xpub
  |=  [=xpub:btc =chyg =idx]
  ^-  @ux
  =/  chygidx=@  (cat 3 ?:(=(%0 chyg) '0' '1') idx)
  =/  dat=@  (cat 3 xpub chygidx)
  %-  ripemd-160:ripemd:crypto
  [(met 3 dat) dat]
::
++  from-xpub
  |=  [=xpub:btc scan-to=(unit scon) max-gap=(unit @ud) confs=(unit @ud)]
  ^-  walt
  :*  (from-extended:bip32 (trip xpub))
      (xpub-type:btc xpub)
      *wach
      [0 0]
      %.n
      (fall scan-to *scon)
      (fall max-gap max-gap:defaults)
      (fall confs confs:defaults)
  ==
::  wad: door for processing walts (wallets)
::        parameterized on a walt and it's chyg account
::
++  wad
  |_  [w=walt =chyg]
  ++  mk-address
    |=  =idx
    ^-  address:btc
    =/  pubkey=@ux
      %-  compress-point:ecc
      pub:(derive-public:(derive-public:wilt.w (@ chyg)) idx)
    ?:  ?=(%bip84 bipt.w)
      (need (encode-pubkey:bech32:btc %main pubkey))
    ~|("legacy addresses not supported yet " !!)
  ::  generates and watches the next available address
  ::
  ++  gen-address
    ^-  (pair address:btc walt)
    =/  addr  (mk-address nixt-idx)
    :-  addr
    (watch-address addr [chyg nixt-idx *(set utxo:btc)])
  ::  insert a new address; update "nixt" free address if this one was it
  ::
  ++  watch-address
    |=  [a=address:btc =addi]
    ^-  walt
    ?>  =(chyg chyg.addi)
    ?>  =(a (mk-address idx.addi))
    =?  nixt.w  (is-nixt addi)
      new:bump-nixt
    w(wach (~(put by wach.w) a addi))
  ::  update an address if it's in wach map
  ::
  ++  update-address
    |=  [a=address:btc utxos=(set utxo:btc)]
    ^-  walt
    =/  adi=(unit addi)
      (~(get by wach.w) a)
    ?~  adi  w
    w(wach (~(put by wach.w) a u.adi(utxos utxos)))
  ::
  ++  is-nixt
    |=  =addi  ^-  ?
    ?:  ?=(%0 chyg.addi)
      =(idx.addi p.nixt.w)
    =(idx.addi q.nixt.w)
  ++  nixt-idx
    ?:(?=(%0 chyg) p.nixt.w q.nixt.w)
  ::  Returns: the prior idx in the account
  ::           nixt with account idx bumped
  ::  Increments idx until an unwatched address is found
  ::  Crashes if max-index is passed
  ::
  ++  bump-nixt
    |^  ^-  [old=idx new=nixt]
    :-  nixt-idx
    =/  new-idx=idx  +(nixt-idx)
    |-  ?>  (lte new-idx max-index)
    ?.  (~(has by wach.w) (mk-address new-idx))
        (set-nixt new-idx)
    $(new-idx +(new-idx))
    ::
    ++  set-nixt
      |=  idx=@  ^-  nixt
      ?:(?=(%0 chyg) [idx q.nixt.w] [p.nixt.w idx])
    --
  --
--
