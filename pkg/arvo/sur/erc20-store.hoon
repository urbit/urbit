|%
+$  poke
  $%  [%send-erc20 =txn]
      [%add-erc20 =contract-id =address:ethereum]
      [%set-key key-path=path]
  ==
+$  txn  [=contract-id to=address:ethereum amount=@ud]
+$  contract-id  @t
--
 
