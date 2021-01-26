/-  *btc
|%
+$  host-info
  $:  api-url=@t
      connected=?
      block=@ud
      clients=(set ship)
  ==
+$  command
  $%  [%set-credentials api-url=@t]
      [%whitelist-clients clients=(set ship)]
  ==
+$  action
  $%  [%address-info =address]
      [%tx-info =txid]
      [%raw-tx =txid]
      [%broadcast-tx =rawtx]
      [%ping ~]
  ==
+$  result
  $%  [%address-info =address utxos=(set utxo) used=? block=@ud]
      [%tx-info =info:tx]
      [%raw-tx =txid =rawtx]
      [%broadcast-tx =txid broadcast=? included=?]
  ==
+$  error
  $%  [%not-connected status=@ud]
      [%bad-request status=@ud]
      [%no-auth status=@ud]
      [%rpc-error ~]
  ==
+$  update  (each result error)
+$  status
  $%  [%connected block=@ud fee=sats]
      [%new-block block=@ud fee=sats blockhash=bytc blockfilter=bytc]
      [%disconnected ~]
  ==
::
++  rpc-types
  |%
  +$  action
    $%  [%get-address-info =address]
        [%get-tx-vals =txid]
        [%get-raw-tx =txid]
        [%broadcast-tx =rawtx]
        [%get-block-count ~]
        [%get-block-info ~]
    ==
  ::
  +$  result
    $%  [%get-address-info =address utxos=(set utxo) used=? block=@ud]
        [%get-tx-vals =info:tx]
        [%get-raw-tx =txid =rawtx]
        [%create-raw-tx =rawtx]
        [%broadcast-tx =txid broadcast=? included=?]
        [%get-block-count block=@ud]
        [%get-block-info block=@ud fee=sats blockhash=bytc blockfilter=bytc]

    ==
  --
--
::
