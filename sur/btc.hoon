|%
+$  network  ?(%main %testnet)
+$  legacy-address  $%([%legacy @uc])
+$  bech32-address  $%([%bech32 cord])
+$  address  ?(legacy-address bech32-address)
+$  sats  @ud
+$  hash256  [wid=%32 dat=@ux]
+$  hash160  [wid=%20 dat=@ux]
+$  hash  ?(hash256 hash160)
+$  buffer  (list @ux)
++  address-info
  $:  =address
  ==
++  tx
  |%
  +$  unsigned
    $:  version=@
        locktime=@
        inputs=(list input)
        outputs=(list output)
    ==
  +$  input
    $:  tx-hash=byts
        tx-index=@
        witness-ver=@
        sequence=byts
        script-pubkey=byts
        redeem-script=(unit byts)
        pubkey=(unit byts)
        value=sats
    ==
  +$  output
    $:  =address
        value=sats
    ==
  --
++  ops
  |%
  ++  op-dup  118
  ++  op-equalverify  136
  ++  op-hash160      169
  ++  op-checksig     172
  --
--
