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
  ~&  (lent pubkey)
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
  =/  is=(unit il-ir)
    %+  bind
      (parse-xpub xpub)
    |=(px=parsed-xpub (compute-i px index))
  is
--
