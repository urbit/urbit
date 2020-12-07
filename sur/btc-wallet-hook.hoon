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
::    - num-fam: total payments (addresses) outstanding for ship and its moons
::  pend-piym: incoming payment txs that peer says they have broadcast
::  poym: outgoing payments. One at a time: new replaces old
::
+$  btc-state  [block=@ud fee=sats t=@da]
+$  reqs  (map req-id:bp req=request:bws)
::
+$  payment  [=xpub =address payer=ship value=sats]
+$  piym  [ps=(map ship payment) num-fam=(map ship @ud)]
+$  pend-piym  [ps=(map txid payment) num=(map ship @ud)]
+$  poym  (unit txbu:bws)
::  req-pay-address: request a payment address from another ship
::   - target of action is local ship
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
      [%broadcast-tx signed-psbt=cord]
      [%clear-poym ~]
      [%force-retry ~]
  ==
+$  request
  $%  [%sign-tx txbu:bws]
  ==
--
