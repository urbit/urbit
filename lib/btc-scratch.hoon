::  btc-scratch.hoon
/+  bip32, btc
=+  ecc=secp256k1:secp:crypto
=,  bech32:btc
|%
+$  bech32-address  $%([%bech32 tape])
+$  address  ?(@uc bech32-address)
++  bip84
  |_  [network=network:btc xpub=tape]
  ++  address
    |=  [change=@ index=@]
    ^-  bech32-address
    ?>  =("zpub" (tape (scag 4 xpub)))            ::  only for bip84
    =/  pubkey=@ux
      %-  compress-point:ecc
      pub:(derive-public:(derive-public:(from-extended:bip32 xpub) change) index)
    [%bech32 (need (encode-pubkey network pubkey))]
  --
--
