/-  *btc, bnh=btc-node-hook
|%
+$  btc-credentials  [rpc-url=@t rpc-user=@t rpc-password=@t]
+$  electrum-credentials  [rpc-url=@t]
+$  credentials  [bc=btc-credentials ec=electrum-credentials]
+$  status  [creds=credentials connected=? clients=(set ship)]
+$  action
  $%  [%get-address-info =address]
  ==
+$  response
  $%  [%block-count count=@]
      [%get-address-info info=address-info]
      [%get-balance (set [=address balance=sats])]
      [%get-transactions ~]
  ==
+$  update                              :: sub updates from /clients path (connection etc.)
  $%  [%status connected=?]
  ==
::
+$  command
  $%  [%set-credentials creds=credentials]
      [%whitelist-clients clients=(set ship)]
  ==
++  rpc
  |%
  +$  action
    $%  [%erpc request:electrum]
        [%brpc request:bitcoin-core]
    ==
  +$  response
    $%  [%erpc response:electrum]
        [%brpc response:bitcoin-core]
    ==
  ++  electrum
    |%
    +$  request
      $%  [%get-address-balance =address]
          [%get-address-utxos =address]
      ==
    +$  response
      $%  [%get-address-utxos utxos=(set utxo)]
      ==
    --
  ++  bitcoin-core
    |%
    +$  request  btc-node-hook-action:bnh
    +$  response  btc-node-hook-response:bnh
    --
  --
--

