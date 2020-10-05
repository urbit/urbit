/-  *btc
|%
+$  tx  @
+$  credentials  [rpc-url=@t rpc-user=@t rpc-password=@t]
+$  status
  $:  [%host connected=? clients=(set ship)]
      [%client connected=? host=ship]
  ==
+$  action
  $%  [%check-status ~]
      [%balance =address]
      [%transactions =address]
  ==
::
+$  command
  $%  [%connect-as-host =credentials]
      [%connect-as-client host=ship]
      [%add-clients clients=(set ship)]
      [%broadcast-tx =tx]
  ==
--
