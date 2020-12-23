/-  *btc, bws=btc-wallet-store, bp=btc-provider
|%
::  btc-state: state from the provider; t is last update time
::  reqs: last block checked for an address/tx request to provider.
::   Used to determine whether to retry request
::
::  payment: a payment expected from another ship
::    - address: address generated for this payment
::  piym: incoming payments. Stores all ship moons under their planet.
::    - num-fam: total payments (addresses) outstanding for ship and its moons
::  pend-piym: incoming payment txs that peer says they have broadcast
::  poym: outgoing payments. One at a time: new replaces old
::
+$  block  @ud
+$  btc-state  [=block fee=sats t=@da]
+$  reqs  (map $?(address txid) request:bws)
::
+$  payment  [pend=(unit txid) =xpub =address payer=ship value=sats]
+$  piym  [ps=(map ship payment) num-fam=(map ship @ud)]
+$  pend-piym  (map txid payment)
+$  poym  (unit txbu:bws)
::  req-pay-address: request a payment address from another ship
::   - target of action is local ship
::  gen-pay-address: generate a payment address from our ship to another
::  ret-pay-address: give an address to a payer who requested it
::  broadcast-tx: broadcast a signed-psbt, must be current poym
::  expect-payment: tell another ship that we're paying a previously requested address
::    - vout-n is the index of the output that has value
::
+$  action
  $%  settings
      local
      peer
  ==
+$  settings
  $%  [%set-provider provider=ship]
      [%set-default-wallet ~]
      [%clear-poym ~]
      [%force-retry ~]
  ==
+$  local
  $%  [%req-pay-address payee=ship value=sats feyb=(unit sats)]
      [%broadcast-tx signed=rawtx]
      [%add-piym =xpub =address payer=ship value=sats]
      [%add-poym =txbu:bws]
      [%add-poym-txi =txid =rawtx]
      [%close-pym ti=info:tx]
      [%fail-broadcast-tx =txid]
      [%succeed-broadcast-tx =txid]
  ==
+$  peer
  $%  [%gen-pay-address value=sats]
      [%ret-pay-address =address payer=ship value=sats]
      [%expect-payment =txid value=sats]
  ==
::
::
+$  update
  $%  request
      error
  ==
::
+$  request
  $%  [%sign-tx txbu:bws]
  ==
::
+$  error
  $%  [%broadcast-tx-mismatch-poym signed=rawtx]
      [%broadcast-tx-spent-utxos =txid]
  ==
--
