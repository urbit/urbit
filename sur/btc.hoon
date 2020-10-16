|%
+$  network  ?(%main %testnet)
+$  legacy-address  $%([%legacy @uc])
+$  bech32-address  $%([%bech32 cord])
+$  address  ?(legacy-address bech32-address)
++  tx
  |%
  +$  buffer  (list @ux)
  +$  unsigned
    $:  (list input)
        (list output)
    ==
  +$  input
    $:  tx-hash=byts
        tx-index=@
        witness-ver=@
        script-pubkey=byts
        redeem-script=(unit byts)
        pubkey=byts
    ==
  +$  output
    $:  =address
        value=@
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
