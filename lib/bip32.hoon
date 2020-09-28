|%
++  pubkey-to-point
  |=  pubkey-bytes=(list @ux)
  ^-  pont:secp:crypto
  =/  as-atom=@
    (rap 3 (flop (swag [45 33] pubkey-bytes)))
  (decompress-point:secp256k1:secp:crypto as-atom)
++  is-point
  |=  pubkey=(list @ux)  ^-  ?
  %.y
--
