/+  naive, ethereum
::
|%
+$  keccak  @ux
::
+$  tx-status
  $:  status=?(%unknown %pending %sending %confirmed %failed)
      pointer=(unit l1-tx-pointer)
  ==
::
+$  l1-tx-pointer
  $:  =address:ethereum
      nonce=@ud
  ==
::
::TODO  cache sender address?
+$  pend-tx  [force=? =raw-tx:naive]
::
+$  part-tx
  $%  [%raw raw=octs]
      [%don =tx:naive]
      [%ful raw=octs =tx:naive]  ::TODO  redundant?
  ==
::
+$  rpc-send-roll
  $:  endpoint=@t
      contract=address:ethereum
      chain-id=@
      pk=@
    ::
      nonce=@ud
      next-gas-price=@ud
      txs=(list raw-tx:naive)
  ==
--