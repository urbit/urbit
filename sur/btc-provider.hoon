/-  *btc, bnh=btc-node-hook
|%
+$  btc-credentials  [rpc-url=@t rpc-user=@t rpc-password=@t]
+$  electrum-credentials  [rpc-url=@t]
+$  credentials  [bc=btc-credentials ec=electrum-credentials]
+$  status  [creds=credentials connected=? clients=(set ship)]
+$  action
  $%  [%get-balance addresses=(set address)]
      [%get-transactions addresses=(set address)]
  ==
+$  response
  $%  [%block-count count=@]
      [%balance (set [=address balance=sats])]
  ==
+$  update                              ::  sub updates from /clients path (connection etc.)
  $%  [%status connected=?]
  ==
::
+$  command
  $%  [%set-credentials creds=credentials]
      [%whitelist-clients clients=(set ship)]
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

