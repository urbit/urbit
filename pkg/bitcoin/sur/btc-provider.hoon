/-  *bitcoin, resource
|%
+$  host-info
  $:  api-url=@t
      connected=?
      =network
      block=@ud
      clients=(set ship)
  ==
+$  whitelist
  $:  public=?
      kids=?
      users=(set ship)
      groups=(set resource:resource)
  ==
::
+$  whitelist-target
  $%  [%public ~]
      [%kids ~]
      [%users users=(set ship)]
      [%groups groups=(set resource:resource)]
  ==
+$  command
  $%  [%set-credentials api-url=@t =network]
      [%add-whitelist wt=whitelist-target]
      [%remove-whitelist wt=whitelist-target]
      [%set-interval inte=@dr]
  ==
+$  action
  $%  [%address-info =address]
      [%tx-info txid=hexb]
      [%raw-tx txid=hexb]
      [%broadcast-tx rawtx=hexb]
      [%ping ~]
      [%block-info block=(unit @ud)]
      [%histogram ~]
      [%block-headers start=@ud count=@ud cp=(unit @ud)]
      [%tx-from-pos height=@ud pos=@ud merkle=?]
      [%fee block=@ud]
      [%psbt psbt=@t]
  ==
::
+$  result
  $%  [%address-info =address utxos=(set utxo) used=? block=@ud]
      [%tx-info =info:tx]
      [%raw-tx txid=hexb rawtx=hexb]
      [%broadcast-tx txid=hexb broadcast=? included=?]
      [%block-info =network block=@ud fee=(unit sats) blockhash=hexb blockfilter=hexb]
      [%histogram hist=(list (list @ud))]
      [%block-headers count=@ud hex=hexb max=@ud root=(unit hexb) branch=(list hexb)]
      [%tx-from-pos tx-hash=hexb merkle=(list hexb)]
      [%fee fee=@rd]
      [%psbt psbt=@t]
  ==
+$  error
  $%  [%not-connected status=@ud]
      [%bad-request status=@ud]
      [%no-auth status=@ud]
      [%rpc-error ~]
  ==
+$  update  (each result error)
+$  status
  $%  [%connected =network block=@ud fee=(unit sats)]
      [%new-block =network block=@ud fee=(unit sats) blockhash=hexb blockfilter=hexb]
      [%disconnected ~]
  ==
::
++  rpc-types
  |%
  +$  action
    $%  [%get-address-info =address]
        [%get-tx-vals txid=hexb]
        [%get-raw-tx txid=hexb]
        [%broadcast-tx rawtx=hexb]
        [%get-block-count ~]
        [%get-block-info block=(unit @ud)]
        [%get-histogram ~]
        [%get-block-headers start=@ud count=@ud cp=(unit @ud)]
        [%get-tx-from-pos height=@ud pos=@ud merkle=?]
        [%get-fee block=@ud]
        [%update-psbt psbt=@t]
    ==
  ::
  +$  result
    $%  [%get-address-info =address utxos=(set utxo) used=? block=@ud]
        [%get-tx-vals =info:tx]
        [%get-raw-tx txid=hexb rawtx=hexb]
        [%create-raw-tx rawtx=hexb]
        [%broadcast-tx txid=hexb broadcast=? included=?]
        [%get-block-count block=@ud]
        [%get-block-info block=@ud fee=(unit sats) blockhash=hexb blockfilter=hexb]
        [%get-histogram hist=(list (list @ud))]
        [%get-block-headers count=@ud hex=hexb max=@ud root=(unit hexb) branch=(list hexb)]
        [%get-tx-from-pos tx-hash=hexb merkle=(list hexb)]
        [%get-fee fee=@rd]
        [%update-psbt psbt=@t]
    ==
  --
--
::
