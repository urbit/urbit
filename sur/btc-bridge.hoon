/-  *btc, bnh=btc-node-hook

|%
+$  credentials  [rpc-url=@t rpc-user=@t rpc-password=@t]
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
  $%  [%erpc action:electrum:rpc]
      [%brpc action:bitcoin-core:rpc]
  ==
++  rpc
  |%
  ++  electrum
    |%
    +$  action
      $%  [%timluc ~]
      ==
    --
  ++  bitcoin-core
    |%
    +$  action  btc-node-hook-action:bnh
    --
  --
--
