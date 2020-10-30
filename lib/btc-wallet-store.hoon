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
++  walt
  |_  [=wilt =bipt =wach next=idxs scanned=? scan-to=scon max-gap=@u]
  ++  from-xpub
    |=  [xpub=tape scan-to=(unit scon) max-gap=(unit @u)]
    %=  +>
        wilt  (from-extended:bip32 xpub)
        bipt  (xpub-type:btc xpub)
        wach  *^wach
        next  [0 0]
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
    ~&  >  pubkey
    ?:  ?=(%bip84 bipt)
      (need (encode-pubkey:bech32:btc %main pubkey))
::    [%legacy (@uc pubkey)]
    ~|("legacy addresses not supported yet" !!)
  --
--
