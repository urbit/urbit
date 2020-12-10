::  chyg: whether account is (non-)change. 0 or 1
::  bytc: "btc-byts" with dat cast to @ux
|%
+$  network  ?(%main %testnet)
+$  xpub  @ta
+$  legacy-address  $%([%legacy @uc])
+$  bech32-address  $%([%bech32 cord])
+$  address  ?(legacy-address bech32-address)
+$  fprint  [%4 @ux]
+$  bipt  $?(%44 %49 %84)
+$  chyg  $?(%0 %1)
+$  idx   @ud
+$  hdkey  [=fprint pubkey=bytc =bipt =chyg =idx]
+$  sats  @ud
+$  vbytes  @ud
+$  bytc  [wid=@ dat=@ux]
+$  hash256  [wid=%32 dat=@ux]
+$  hash160  [wid=%20 dat=@ux]
+$  hash  ?(hash256 hash160)
+$  txid  hash256
+$  rawtx  bytc
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
  +$  in  [=utxo =rawtx =hdkey]
  +$  out  [=address hk=(unit hdkey)]
  +$  target  $?(%input %output)
  +$  keyval  [key=bytc val=bytc]
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
