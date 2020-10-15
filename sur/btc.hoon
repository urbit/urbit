|%
+$  network  ?(%main %testnet)
+$  legacy-address  $%([%legacy @uc])
+$  bech32-address  $%([%bech32 cord])
+$  address  ?(legacy-address bech32-address)
++  ops
  |%
  ++  op-dup          118
  ++  op-equalverify  136
  ++  op-hash160      169
  ++  op-checksig     172
  --
--
