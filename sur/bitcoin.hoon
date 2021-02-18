::  sur/btc.hoon
::  Utilities for working with BTC data types and transactions
::
::  chyg: whether account is (non-)change. 0 or 1
::  bytc: "btc-byts" with dat cast to @ux
|%
+$  network  ?(%main %testnet)
+$  hexb  [wid=@ dat=@ux]                :: hex byts
+$  bits  [wid=@ dat=@ub]
+$  xpub  @ta
+$  address
  $%  [%base58 @uc]
      [%bech32 cord]
  ==
+$  fprint  hexb
+$  bipt  $?(%44 %49 %84)
+$  chyg  $?(%0 %1)
+$  idx   @ud
+$  hdkey  [=fprint pubkey=hexb =network =bipt =chyg =idx]
+$  sats  @ud
+$  vbytes  @ud
+$  txid  hexb
+$  utxo  [pos=@ =txid height=@ value=sats recvd=(unit @da)]
++  address-info
  $:  =address
      confirmed-value=sats
      unconfirmed-value=sats
      utxos=(set utxo)
  ==
++  tx
  |%
  +$  data
    $:  is=(list input)
        os=(list output)
        locktime=@ud
        nversion=@ud
        segwit=(unit @ud)
    ==
  +$  val
    $:  =txid
        pos=@ud
        =address
        value=sats
    ==
  ::  included: whether tx is in the mempool or blockchain
  ::
  +$  info
    $:  included=?
        =txid
        confs=@ud
        recvd=(unit @da)
        inputs=(list val)
        outputs=(list val)
    ==
  +$  input
    $:  =txid
        pos=@ud
        sequence=hexb
        script-sig=(unit hexb)
        pubkey=(unit hexb)
        value=sats
    ==
  +$  output
    $:  script-pubkey=hexb
        value=sats
    ==
  --
++  psbt
  |%
  +$  base64  cord
  +$  in  [=utxo rawtx=hexb =hdkey]
  +$  out  [=address hk=(unit hdkey)]
  +$  target  $?(%input %output)
  +$  keyval  [key=hexb val=hexb]
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
