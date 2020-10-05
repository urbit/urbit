/-  *btc
|%
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
      [%allow-clients clients=(set ship)]
  ==
--
