|%
+$  address  ?(@uc [%bech32 @t])
+$  tx  @
+$  credentials  [rpc-url=@t rpc-user=@t rpc-password=@t]
+$  bio  $:  mode=?(%remote %local)
             =credentials            ::  for connecting to btc-provider
             clients=(set ship)      ::  allowed clients for this node
         ==
+$  action
  $%  [%connect =credentials]
      [%status ~]
      [%balance =address]
      [%transactions =address]
      [%broadcast-tx =tx]
  ==
--
