/-  *btc
|%
+$  method
  $?  %get-address-utxos
      %get-address-history
  ==
+$  request  [=method =address]
+$  response
  $%  [%get-address-utxos utxos=(set utxo)]
      [%get-address-history txs=(set [tx=hash256 height=@])]
  ==
--
