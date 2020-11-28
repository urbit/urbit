/-  *btc, bws=btc-wallet-store, bp=btc-provider
|%
::  btc-state: state from the provider; t is last update time
::  req-id: hash of [xpub chyg idx]
::  reqs: lookup of req-id -> requests from wallet-store+blockcount
::    blockcount included so that we only request address info when
::    there's a newer block, in the case of addresses we are cooking
::
::  payment: a payment expected from another ship
::    - address: address generated for this payment
::  piym: incoming payments. Stores all ship moons under their planet.
::  piym-watch/poym-watch:
::   let us link an address back to its incoming/outgoing payment
::   checked when address updates come from btc-wallet-store
::
+$  btc-state  [blockcount=@ud fee=sats t=@da]
+$  reqs (map req-id:bp [blockcount=@ud req=request:bws])
::
+$  payment  [=address payer=ship value=sats]
::
+$  piym  (jar ship payment)
+$  piym-watch  (map address ship)
+$  poym-watch  (map address ship)
::  req-pay-address: request a payment address from another ship
::  gen-pay-address: generate a payment address from our ship to another
::
+$  action
  $%  [%set-provider provider=ship]
      [%set-default-wallet ~]
      [%req-pay-address payee=ship value=sats feyb=(unit sats)]
      [%gen-pay-address value=sats]
      [%ret-pay-address =address payer=ship value=sats]
      [%clear-poym ~]
      [%force-retry ~]
  ==
+$  request
  $%  [%sign-tx txbu:bws]
  ==
--
