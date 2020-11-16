/-  *btc, bws=btc-wallet-store, bp=btc-provider
|%
::  req-id: hash of [xpub chyg idx]
::  back: lookup table of req-id -> requests from wallet-store
::  piym: incoming payments
::  poym: outgoing payments
::  piym-watch/poym-watch:
::   let us link an address back to its incoming/outgoing payment
::
+$  btc-state  [blockcount=@ud fee=sats t=@da]
+$  back  (map req-id:bp request:bws)
+$  payment  [=address payer=ship amount=sats]
+$  key  [=bipt =chyg:bws =idx:bws]
+$  txin  [=utxo raw-tx=byts =key]
::
+$  piym  (jar ship payment)
+$  poym  (jar ship txin)
+$  piym-watch  (map address ship)
+$  poym-watch  (map address ship)
::
+$  action
  $%  [%set-provider provider=ship]
      [%pay payee=ship amount=sats]
      [%force-retry ~]
  ==
--
