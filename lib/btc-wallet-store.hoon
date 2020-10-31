/-  *btc-wallet-store
/+  bip32, btc
=,  secp:crypto
=+  ecc=secp256k1
|%
++  default-max-gap  20
::  xpub
::  wilt
::  bipt: BIP44/49/84
::  wach
::  next: next index to generate address for in non-change/change accounts respectively
::  scanned: whether the wallet's addresses have been checked for prior activity
::           if unscanned, 'next' values won't be valid
::  scan-to
::  max-gap
::
++  is-next
  |=  [adi=addi next=idxs]  ^-  ?
  ?|  =(idx.addi p.next)
      =(idx.addi q.next)
  ==
::
++  walt
  |_  [=wilt =bipt =wach next=idxs scanned=? scan-to=scon max-gap=@u]
  +*  this  .
  ::
  ++  from-xpub
    |=  [=xpub:btc scan-to=(unit scon) max-gap=(unit @u)]
    ^-  _this
    %=  this
        wilt  (from-extended:bip32 (trip xpub))
        bipt  (xpub-type:btc xpub)
        wach  *^wach
        next  [[%0 0] [%1 0]]
        scanned  %.n
        scan-to  (fall scan-to *scon)
        max-gap  (fall max-gap default-max-gap)
    ==
  ++  get-address
    |=  [=chyg idx=@]
    ^-  address:btc
    =/  pubkey=@ux
      %-  compress-point:ecc
      pub:(derive-public:(derive-public:wilt (@u chyg)) idx)
    ?:  ?=(%bip84 bipt)
      (need (encode-pubkey:bech32:btc %main pubkey))
    ~|("legacy addresses not supported yet" !!)
  ::
  :: should take index as a parameter so that we can write that to addi and update used
  ++  insert-address  %dummy
  ++  update-address
    |=  [a=address:btc used=? utxos=(set utxo:btc)]
    ^-  _this
    =/  adi=(unit addi)
      (~(get by wach) a)
    ?~  adi  this
    =.  wach
      (~(put by wach) a u.adi(used used, utxos utxos))
    ?.  ?&(used (is-next u.adi next))  this
    ::  if used AND this is next index, we need to do update next
    this
  ::
  ++  next-unused-idx
    |=  c=chyg
    ^-  idx
    =/  indices=(list idx)
      %+  turn  ~(val by wach)
      |=(a=addi idx.a)
    *idx
    ::  check next for this one--problem is that we can't index in
  --
--
