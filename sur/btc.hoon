|%
+$  network  ?(%main %testnet)
+$  xpub  @ta
+$  legacy-address  $%([%legacy @uc])
+$  bech32-address  $%([%bech32 cord])
+$  address  ?(legacy-address bech32-address)
+$  bipt  $?(%bip44 %bip49 %bip84)
+$  sats  @ud
+$  vbytes  @ud
+$  btc-byts  [wid=@ dat=@ux]
+$  hash256  [wid=%32 dat=@ux]
+$  hash160  [wid=%20 dat=@ux]
+$  hash  ?(hash256 hash160)
+$  txid  hash256
+$  rawtx  btc-byts
+$  buffer  (list @ux)
+$  utxo  [pos=@ =txid height=@ value=sats recvd=(unit @da)]
++  address-info
  $:  =address
      confirmed-value=sats
      unconfirmed-value=sats
      utxos=(set utxo)
  ==
++  tx
  |%
  +$  val
    $:  =txid
        pos=@ud
        =address
        value=sats
    ==
  +$  info
    $:  =txid
        confs=@ud
        recvd=(unit @da)
        inputs=(list val)
        outputs=(list val)
    ==
  +$  unsigned
    $:  version=@
        locktime=@
        inputs=(list input)
        outputs=(list val)
    ==
  +$  input
    $:  =txid
        tx-index=@
        witness-ver=@
        sequence=byts
        script-pubkey=byts
        redeem-script=(unit byts)
        pubkey=(unit byts)
        value=sats
    ==
  --
++  psbt
  |%
  +$  keyval  [key=btc-byts val=btc-byts]
  +$  map  (list keyval)
  --
++  ops
  |%
  ++  op-dup  118
  ++  op-equalverify  136
  ++  op-hash160      169
  ++  op-checksig     172
  --
--
