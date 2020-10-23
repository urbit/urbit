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
  $%  [%get-address-info info=address-info]
  ==
+$  error
  $%  [%not-connected ~]
  ==
+$  update                              ::  sub updates from /clients path (connection etc.)
  $%  [%result =result]
      [%error =error]
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

