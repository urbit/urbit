/-  *btc
|%
+$  tx  @
+$  credentials  [rpc-url=@t rpc-user=@t rpc-password=@t]
+$  status
  $:  [%host connected=? clients=(set ship)]
      [%client connected=? host=ship]
  ==
+$  action
  $%  [%connect-as-host =credentials]
      [%connect-as-client host=ship]
      [%check-status ~]
      [%balance =address]
      [%transactions =address]
      [%broadcast-tx =tx]
  ==
--
