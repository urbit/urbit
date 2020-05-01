|%
+$  poke
  $%  [$send-erc20 txn=pending-txn]
      [$add-erc20 =contract-id =address:ethereum]
      [$set-key key-path=path]
  ==
+$  gift
  $%  [$initial =balances]
  ==
+$  pending-txn  [=contract-id to=address:ethereum amount=@ud]
+$  txn-log  (list [from=@ux to=@ux amount=@ud])
+$  contract-id  @t
+$  balances  (map contract-id [=address:ethereum balance=@ud =txn-log])
--
