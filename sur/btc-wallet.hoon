/-  *btc, bp=btc-provider
/+  bip32
|%
+$  params  [batch-size=@ud fam-limit=@ud piym-limit=@ud]
+$  provider  [host=ship connected=?]
+$  block  @ud
+$  btc-state  [=block fee=(unit sats) t=@da]
+$  payment  [pend=(unit txid) =xpub =address payer=ship value=sats]
+$  piym  [ps=(map ship payment) pend=(map txid payment) num-fam=(map ship @ud)]
+$  poym  (unit txbu)
::
+$  command
  $%  [%set-provider provider=ship =network]
      [%set-current-wallet =xpub]
      [%add-wallet =xpub =fprint scan-to=(unit scon) max-gap=(unit @ud) confs=(unit @ud)]
      [%delete-wallet =xpub]
      [%req-pay-address payee=ship value=sats feyb=sats]
      [%broadcast-tx txhex=cord]
  ==
+$  action
      ::  local-only actions
      ::
  $%  [%close-pym ti=info:tx]
      [%add-poym-raw-txi =txid rawtx=hexb]
      [%fail-broadcast-tx =txid]
      [%succeed-broadcast-tx =txid]
      ::  peer actions
      ::
      [%gen-pay-address value=sats]
      [%recv-pay-address =address value=sats]
      [%expect-payment =txid value=sats]
  ==
::
::
::  Wallet Types
::
::  nixt: next indices to generate addresses from (non-change/change)
::  addi: HD path along with UTXOs
::  wach: map for watched addresses.
::        Membership implies the address is known by outside parties or had prior activity
::  scon: indices to initially scan to in (non-)change accounts
::        defaults to 2^32-1 (i.e. all the addresses, ~4B)
::  wilt: copulates with thousands of indices to form addresses
::
++  max-index  (dec (pow 2 32))
+$  nixt  (pair idx idx)
+$  addi  [used=? =chyg =idx utxos=(set utxo)]
+$  wach  (map address addi)
+$  scon  $~([max-index max-index] (pair idx idx))
+$  wilt  _bip32
::
::  walt: wallet datastructure
::  scanned: whether the wallet's addresses have been checked for prior activity
::  scan-to
::  max-gap: maximum number of consec blank addresses before wallet stops scanning
::  confs:   confirmations required (after this is hit for an address, wallet stops refreshing it)
::
+$  walt
  $:  =xpub
      =network
      =fprint
      =wilt
      =bipt
      =wach
      =nixt
      scanned=?
      scan-to=scon
      max-gap=@ud
      confs=@ud
  ==
::  batch: indexes to scan for a given chyg
::  scans: all scans underway (batches)
::
+$  batch  [todo=(set idx) endpoint=idx has-used=?]
+$  scans  (map [xpub chyg] batch)
::
::  insel: a selected utxo for input to a transaction
::  pmet: optional payment metadata
::  feyb: fee per byte in sats
::  txi/txo:  input/output for a transaction being built
::   - txo has an hdkey if it's a change account
::   - by convention, first output of txo is to the payee, if one is present
::  txbu: tx builder -- all information needed to make a transaction for signing
::
+$  insel  [=utxo =chyg =idx]
+$  feyb  sats
+$  txi  [=utxo ur=(unit hexb) =hdkey]
+$  txo  [=address value=sats hk=(unit hdkey)]
+$  txbu
  $:  =xpub
      payee=(unit ship)
      =vbytes
      txis=(list txi)
      txos=(list txo)
      signed-tx=(unit hexb)
  ==
::  hest: an entry in the history log
::
+$  hest
  $:  =xpub
      =txid
      confs=@ud
      recvd=(unit @da)
      inputs=(list [=val:tx s=(unit ship)])
      outputs=(list [=val:tx s=(unit ship)])
  ==
+$  history  (map txid hest)
--
