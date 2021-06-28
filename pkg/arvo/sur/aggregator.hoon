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
+$  l2-tx
  $?  %transfer-point
      %spawn
      %configure-keys
      %escape
      %cancel-escape
      %adopt
      %reject
      %detach
      %set-management-proxy
      %set-spawn-proxy
      %set-transfer-proxy
  ==
::
::  TODO:  add submission time?
::
+$  roller-tx  [status=tx-status hash=keccak type=l2-tx]
::
+$  pend-tx    [force=? =address:naive =raw-tx:naive]
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