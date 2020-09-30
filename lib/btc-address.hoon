|%
::  b[ytes]rip: 0x6261 -> ~[98 97]
::
+$  pubkey     (list @ux)
+$  chaincode  (list @ux)
+$  parsed-xpub  [cc=chaincode pubk=pubkey]
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
  |=  pp=parsed-xpub
  %.y
--
