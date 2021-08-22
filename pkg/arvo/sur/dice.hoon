::  dice: structures for L2 Rollers
::
/+  naive, ethereum
::
|%
+$  owners  (jug address:naive ship)
::
+$  roller-config
  $:  next-batch=time
      frequency=@dr
      refresh-time=@dr
      contract=@ux
      chain-id=@
  ==
::
+$  keccak  @ux
::
+$  status
  ?(%unknown %pending %sending %confirmed %failed)
::
+$  tx-status
  $:  =status
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
+$  update
  $%  [%point =address:ethereum =ship =point:naive]
      [%tx =address:ethereum =roller-tx]
  ==
::
::  TODO:  add submission time?
::
+$  roller-tx  [=ship =status hash=keccak type=l2-tx]
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
