::  DEPRECATED: use lib/bip32.hoon
|%
+$  pubkey       (list @ux)
+$  chaincode    (list @ux)
+$  parsed-xpub  [cc=chaincode pubk=pubkey]
+$  il-ir        [il=(list @ux) ir=(list @ux)]
::  b[ytes]rip: 0x6261 -> ~[98 97]
::
++  big-endian-brip
  |=  a=@ux
  ^-  (list @ux)
  (flop (rip 3 a))
::  b[ytes]rap: ~[98 97] -> 0x6261
::
++  big-endian-brap
  |=  bytes=(list @ux)
  ^-  @ux
  (swp 3 (rap 3 bytes))
::
++  pubkey-to-point
  |=  =pubkey
  ^-  pont:secp:crypto
  ~&  >>  "compressed pubkey length: {<(lent pubkey)>}"
  %-  decompress-point:secp256k1:secp:crypto
    (big-endian-brap pubkey)
::
++  is-point
  |=  =pubkey  ^-  ?
  -:(mule |.((pubkey-to-point pubkey)))
::
++  parse-xpub
  |=  xpub=tape
  ^-  (unit parsed-xpub)
  =/  as-atom=@ux
    (de-base58:mimes:html xpub)
  =/  bytes=(list @ux)
    (big-endian-brip as-atom)
  ~&  >>  "parse-xpub, depth: {<(snag 4 bytes)>}"
  =/  pp=parsed-xpub
    [(swag [13 32] bytes) (swag [45 33] bytes)]
  ?:  (is-point pubk.pp)
    `pp
  ~
::
++  compute-i
  |=  [=parsed-xpub index=@ud]
  ^-  il-ir
  ~|  'Public key cannot use a hardened index'
  ?>  (lth index (bex 31))
  ::  "append" index to pubkey as 4 bytes
  =/  data=@
    %+  add
      (lsh 3 4 (big-endian-brap pubk.parsed-xpub))
    index
  =/  chaincode=@
    (big-endian-brap cc.parsed-xpub)
  =/  i=(list @ux)
    (big-endian-brip (hmac-sha512:hmac:crypto chaincode data))
  =/  il=(list @ux)  (swag [0 32] i)
  =/  ir=(list @ux)  (swag [32 32] i)
  [il ir]
++  child-from-xpub
  |=  [xpub=tape index=@ud]
  =,  secp256k1:secp:crypto
  =/  upx=(unit parsed-xpub)
    (parse-xpub xpub)
  ?~  upx  ~
  =/  px=parsed-xpub  u.upx
  =/  is  (compute-i px index)
  (compress-point (jc-add (priv-to-pub (big-endian-brap il.is)) (pubkey-to-point pubk.px)))
--

::  `@ux`(compress-point:secp256k1:secp:crypto (jc-add:secp256k1:secp:crypto (pubkey-to-point:btca pubk.u.px) x))
