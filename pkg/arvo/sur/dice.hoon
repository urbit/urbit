::  dice: structures for L2 rollers
::
/+  naive, ethereum
::
|%
+$  owner     [=proxy:naive =address:naive]
+$  owners    (jug owner ship)
+$  sponsors  (map ship [residents=(set ship) requests=(set ship)])
+$  net       ?(%mainnet %ropsten %local %default)
::
+$  config
  $%  [%frequency frequency=@dr]
      [%setkey pk=@]
      [%endpoint endpoint=@t =net]
      [%resend-time time=@dr]
      [%update-rate rate=@dr]
      [%slice slice=@dr]
      [%quota quota=@ud]
  ==
::
+$  indices
  $:  nas=^state:naive
      own=owners
      spo=sponsors
  ==
::
+$  azimuth-config
  $:  refresh-rate=@dr
  ==
::
+$  roller-config
  $:  next-batch=time
      frequency=@dr
      resend-time=@dr
      update-rate=@dr
      contract=@ux
      chain-id=@
      slice=@dr
      quota=@ud
  ==
::
+$  keccak  @ux
::
+$  status
  ?(%unknown %pending %sending %confirmed %failed %cancelled)
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
  $%  [%point =ship =point:naive new=owner old=(unit owner)]
      [%tx =address:ethereum =roll-tx]
  ==
::
+$  hist-tx  [p=time q=roll-tx]
+$  roll-tx  [=ship =status hash=keccak type=l2-tx]
+$  pend-tx  [force=? =address:naive =time =raw-tx:naive]
+$  send-tx  [next-gas-price=@ud sent=? txs=(list raw-tx:naive)]
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
