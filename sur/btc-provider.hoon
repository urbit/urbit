/-  *btc, brpc=btc-node-hook, erpc=electrum-rpc
|%
+$  btc-credentials  [rpc-url=@t rpc-user=@t rpc-password=@t]
+$  electrum-credentials  [rpc-url=@t]
+$  credentials  [bc=btc-credentials ec=electrum-credentials]
+$  host-info  [creds=credentials connected=? clients=(set ship)]
+$  action
  $%  [%get-address-info =address]
  ==
+$  result
  $%  [%get-address-info a=address utxos=(set utxo) used=?]
  ==
+$  error
  $%  [%not-connected ~]
  ==
+$  update  (each result error)             ::  sub updates to /clients path (connection etc.)
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
    +$  request  request:erpc
    +$  response  response:erpc
    --
  ++  bitcoin-core
    |%
    +$  request  btc-node-hook-action:brpc
    +$  response  btc-node-hook-response:brpc
    --
  --

--

