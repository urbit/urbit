::  wallets are compatible with BIPs 44, 49, and 84
::  m / purpose' / coin_type' / account' / change / address_index
::  change can be 0 or 1
::
/-  *btc, bp=btc-provider
/+  bip32
|%
++  max-index  (dec (pow 2 32))
::  idx:  an address_index
::  nixt: next indices to generate addresses from (non-change/change)
::  addi: HD path along with UTXOs
::    - used: whether the address has been used
::  wach: map for watched addresses.
::        Membership implies the address is known by outside parties or had prior activity
::  scon: indices to initially scan to in (non-)change accounts
::        defaults to 2^32-1 (i.e. all the addresses, ~4B)
::  wilt: stores xpub; copulates with thousands of indices to form addresses
::
+$  nixt  (pair idx idx)
+$  addi  [used=? =chyg =idx utxos=(set utxo)]
+$  wach  (map address addi)
+$  scon  $~([max-index max-index] (pair idx idx))
+$  wilt  _bip32
::  walt: wallet datastructure
::  scanned: whether the wallet's addresses have been checked for prior activity
::  scan-to
::  max-gap: maximum number of consec blank addresses before wallet stops scanning
::  confs:   confirmations required (after this is hit for an address, wallet stops refreshing it)
::
+$  walt
  $:  =xpub
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
::  input: utxo for a transaction
::  feyb: fee per byte in sats
::  txi/txo:  input/output for a transaction being built
::   -txo has an hdkey if it's a change account
::  txbu: tx builder -- all information needed to make a transaction for signing
::  peta: optional payment metadata
::
+$  input  [=utxo =chyg =idx]
+$  peta  (unit [payer=ship value=sats])
+$  feyb  sats
+$  txi  [=utxo ur=(unit rawtx) =hdkey]
+$  txo  [=address value=sats hk=(unit hdkey)]
+$  txbu
  $:  =req-id:bp
      txinfo=(unit [=txid =rawtx])
      =xpub
      payee=(unit ship)
      =vbytes
      txis=(list txi)
      txos=(list txo)
  ==
::  hest: an entry in the history log
::
+$  hest
  $:  =txid
      confs=@ud
      recvd=(unit @da)
      inputs=(list [=val:tx s=(unit ship)])
      outputs=(list [=val:tx s=(unit ship)])
  ==
+$  history  (map xpub (map txid hest))
::  state/watch variables:
::  scanning addresses and monitoring generated addresses
::  batch: indexes to scan for a given chyg
::  scans: all scans underway (batches)
::  piym-watch: any address we've been told has an incoming payment promised
::
+$  batch  [todo=(set idx) endpoint=idx has-used=?]
+$  scans  (map [xpub chyg] batch)
::
::  %add-wallet: add wallet to state and initiate a scan
::  %address-info: give new data about an address.
::    - used:  address has been seen on the BTC blockchain?
::    - block: the most recent block at the time of this information being retrieved
::  TODO: document
::
+$  action
  $%  [%add-wallet =xpub scan-to=(unit scon) max-gap=(unit @ud) confs=(unit @ud)]
      [%address-info =xpub =chyg =idx utxos=(set utxo) used=? block=@ud]
      [%tx-info =info:tx]
      [%generate-address =xpub =chyg =peta]
      [%generate-txbu =xpub payee=(unit ship) feyb=sats txos=(list txo)]
      [%add-history-entry =xpub =hest]
  ==
::
+$  update
  $%  [%generate-address =xpub =address =peta]
      [%generate-txbu =xpub =txbu]
      [%saw-piym s=ship =txid]
      [%scan-done =xpub]
  ==
::  last-block: most recent block this address was checked
::
+$  request
  $%  [%address-info last-block=@ud a=address =xpub =chyg =idx]
      [%tx-info last-block=@ud =txid]
  ==
--
