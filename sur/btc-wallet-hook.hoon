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
::  poym: outgoing payments. One at a time: new replaces old
::
+$  btc-state  [block=@ud fee=sats t=@da]
+$  reqs  (map req-id:bp req=request:bws)
::
+$  payment  [=address payer=ship value=sats]
+$  piym  (jar ship payment)
+$  poym  (unit txbu:bws)
+$  piym-lock  (map ship txid)
::  req-pay-address: request a payment address from another ship
::  gen-pay-address: generate a payment address from our ship to another
::  ret-pay-address: give an address to a payer who requested it
::  broadcast-tx: broadcast a signed-psbt, associate with poym
::
+$  action
  $%  [%set-provider provider=ship]
      [%set-default-wallet ~]
      [%req-pay-address payee=ship value=sats feyb=(unit sats)]
      [%gen-pay-address value=sats]
      [%ret-pay-address =address payer=ship value=sats]
      [%broadcast-tx =req-id:bp signed-psbt=cord]
      [%clear-poym ~]
      [%force-retry ~]
  ==
+$  request
  $%  [%sign-tx txbu:bws]
  ==
--
