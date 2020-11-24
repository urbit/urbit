/-  *btc
|%
+$  host-info  [api-url=@t connected=? clients=(set ship)]
+$  req-id  @t
+$  command
  $%  [%set-credentials api-url=@t]
      [%whitelist-clients clients=(set ship)]
  ==
+$  action  [=req-id body=action-body]
+$  action-body
  $%  [%address-info =address]
      [%raw-tx =txid]
      [%ping ~]
  ==
+$  result  [=req-id body=result-body]
+$  result-body
  $%  [%address-info utxos=(set utxo) used=? blockcount=@ud]
      [%raw-tx =txid =rawtx]
  ==
+$  error
  $%  [%not-connected status=@ud]
      [%bad-request status=@ud]
      [%no-auth status=@ud]
      [%http-error status=@ud]
      [%rpc-error ~]
  ==
+$  update  (each result error)
+$  status
  $%  [%connected blockcount=@ud fee=@ud]
      [%disconnected ~]
  ==
::
++  rpc
  |%
  +$  action
    $%  [%get-address-info =address]
        [%get-raw-tx =txid]
        [%get-block-count ~]
        [%get-block-and-fee ~]
    ==
  ::
  +$  response
    $%  [%get-address-info utxos=(set utxo) used=? blockcount=@ud]
        [%get-raw-tx =txid =rawtx]
        [%get-block-count blockcount=@ud]
        [%get-block-and-fee blockcount=@ud fee=sats]

    ==
  --
--
::
