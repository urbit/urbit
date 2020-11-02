::
::
/-  *btc-wallet-store
/+  bip32, btc
=,  secp:crypto
=+  ecc=secp256k1
|%
++  default-max-gap  20
::  walt: door parameterized on chyg (the change account to use for current operation)
::        to access state, use VARNAME.st.WALTNAME, e.g. nixt.st.walt1
::
::  wach: map of watched addresses
::  scanned: whether the wallet's addresses have been checked for prior activity
::  scan-to
::  max-gap
::
::
++  walt
  =|  st=[=wilt =bipt =wach =nixt scanned=? scan-to=scon max-gap=@]
  |_  =chyg
  +*  this  .
  ::
  ++  from-xpub
    |=  [=xpub:btc scan-to=(unit scon) max-gap=(unit @)]
    ^-  _this
    %=  this
        wilt.st  (from-extended:bip32 (trip xpub))
        bipt.st  (xpub-type:btc xpub)
        wach.st  *wach
        nixt.st  [0 0]
        scanned.st  %.n
        scan-to.st  (fall scan-to *scon)
        max-gap.st  (fall max-gap default-max-gap)
    ==
  ::  generates the next available address
  ::  adds it to wach (i.e. treat it as used and out in the wild)
  ::
  ++  gen-address
    ^-  (pair address:btc _this)
    =^  curr-idx  nixt.st
      bump-nixt
    =/  addr  (mk-address curr-idx)
    :-  addr
    %=  this
        wach.st  %+  ~(put by wach.st)
                      addr
                    [chyg curr-idx *(set utxo:btc)]
    ==
  ::
  ++  mk-address
    |=  =idx
    ^-  address:btc
    =/  pubkey=@ux
      %-  compress-point:ecc
      pub:(derive-public:(derive-public:wilt.st (@ chyg)) idx)
    ?:  ?=(%bip84 bipt.st)
      (need (encode-pubkey:bech32:btc %main pubkey))
    ~|("legacy addresses not supported yet" !!)
  ::  insert a new address; update "nixt" free address if this one was it
  ::
  ++  insert-address
    |=  [a=address:btc =addi]
    ^-  _this
    ?>  =(chyg chyg.addi)
    ?>  =(a (mk-address idx.addi))
    =?  nixt.st  (is-nixt addi)
      new:bump-nixt
    this(wach.st (~(put by wach.st) a addi))
  ::  update an address if it's in our wach map
  ::
  ++  update-address
    |=  [a=address:btc utxos=(set utxo:btc)]
    ^-  _this
    =/  adi=(unit addi)
      (~(get by wach.st) a)
    ?~  adi  this
    this(wach.st (~(put by wach.st) a u.adi(utxos utxos)))
  ::  returns true if there are max-gap blank spots up to last-checked
  ::
  ++  scan-done
    |=  last-checked=idx
    ^-  ?
    (gth (add max-gap.st nixt-idx) last-checked)
  ++  is-nixt
    |=  =addi  ^-  ?
    ?:  ?=(%0 chyg.addi)
      =(idx.addi p.nixt.st)
    =(idx.addi q.nixt.st)
  ++  nixt-idx
    ?:(?=(%0 chyg) p.nixt.st q.nixt.st)
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
    ?.  (~(has by wach.st) (mk-address new-idx))
        (set-nixt new-idx)
    $(new-idx +(new-idx))
    ::
    ++  set-nixt
      |=  idx=@  ^-  nixt
      ?:(?=(%0 chyg) [idx q.nixt.st] [p.nixt.st idx])
    --
  --
--
