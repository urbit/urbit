|%
::  bytes rip: 0x6261 -> ~[98 97]
::
+$  pubkey     (list @ux)
+$  chaincode  (list @ux)
++  big-endian-brip
  |=  a=@ux
  ^-  (list @ux)
  (flop (rip 3 a))
::  bytes rap: ~[98 97] -> 0x6261
::
++  big-endian-brap
  |=  bytes=(list @ux)
  ^-  @ux
  (swp 3 (rap 3 bytes))
::
++  parse-zpub
  |=  zpub=tape
  ^-  [cc=chaincode pubk=pubkey]
  =/  as-atom=@ux
    (de-base58:mimes:html zpub)
  =/  bytes=(list @ux)
    (big-endian-brip as-atom)
  [(swag [13 32] bytes) (swag [45 33] bytes)]
::
++  pubkey-to-point
  |=  pubkey-bytes=pubkey
  ^-  pont:secp:crypto
  =/  as-atom=@
    (rap 3 (flop (swag [45 33] pubkey-bytes)))
  (decompress-point:secp256k1:secp:crypto as-atom)
::
++  is-point
  |=  pubkey=pubkey  ^-  ?
  -:(mule |.((pubkey-to-point pubkey)))
--
