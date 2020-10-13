/-  *btc, bnh=btc-node-hook
|%
+$  btc-credentials  [rpc-url=@t rpc-user=@t rpc-password=@t]
+$  electrum-credentials  [rpc-url=@t]
+$  credentials  [bc=btc-credentials ec=electrum-credentials]
+$  status
  $%  [%host connected=? clients=(set ship)]
      [%client connected=? host=(unit ship)]
  ==
+$  action
  $%  [%check-status ~]
      [%get-block-count ~]
      [%balance =address]
      [%transactions =address]
  ==
::
+$  command
  $%  [%become-host =credentials]
      [%connect-as-client host=ship]
      [%allow-clients users=(set ship)]
  ==
+$  rpc-action
  $%  [%erpc request:electrum:rpc]
      [%brpc request:bitcoin-core:rpc]
  ==
++  rpc
  |%
  ++  electrum
    |%
    +$  request
      $%  [%get-address-balance =address]
      ==
    --
  ++  bitcoin-core
    |%
    +$  request  btc-node-hook-action:bnh
    --
  --
--

