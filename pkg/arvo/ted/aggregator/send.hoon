::  aggregator/send: send rollup tx
::
/-  rpc=json-rpc
/+  naive, ethereum, ethio, strandio
::
=/  gas-limit=@ud  30.000  ::TODO  verify, maybe scale with roll size
::
|=  args=vase
=+  !<  $:  endpoint=@t
            contract=address:ethereum
            chain-id=@
            pk=@
          ::
            nonce=@ud
            next-gas-price=@ud
            txs=(list raw-tx:naive)
        ==
    args
=/  m  (strand:strandio ,vase)
|^
^-  form:m
=*  not-sent  (pure:m !>(next-gas-price))
::
=/  =address:ethereum
  (address-from-pub:key:ethereum pk)
;<  expected-nonce=@ud  bind:m
  (get-next-nonce:ethio endpoint address)
::  if chain expects a different nonce, don't send this transaction
::
?.  =(nonce expected-nonce)
  not-sent
::  if a gas-price of 0 was specified, fetch the recommended one
::
;<  use-gas-price=@ud  bind:m
  ?:  =(0 next-gas-price)  fetch-gas-price
  (pure:(strand:strandio @ud) next-gas-price)
::  if we cannot pay for the transaction, don't bother sending it out
::
=/  max-cost=@ud  (mul gas-limit use-gas-price)
;<  balance=@ud  bind:m
  ::TODO  implement %eth-get-balance in /lib/ethio and /lib/ethereum
  !!
?:  (gth max-cost balance)
  ~&  [%insufficient-aggregator-balance address]
  not-sent
::
=/  tx=@ux
  =;  tx=transaction:rpc:ethereum
    (sign-transaction:key:ethereum tx pk)
  :*  nonce
      use-gas-price
      gas-limit
      contract
      0
      roll  ::TODO  tx data
      chain-id
  ==
::
::NOTE  this fails the thread if sending fails, which in the app gives us
::      the "retry with same gas price" behavior we want
;<  jon=json  bind:m
  %+  request-rpc:ethio  endpoint
  [~ %eth-send-raw-transaction tx]
::TODO  check that tx-hash in jon is non-zero?
::TODO  enforce max here, or in app?
::  add five gwei to gas price of next attempt
::
(pure:m !>((add use-gas-price 5.000.000.000)))
::
::TODO  should be distilled further, partially added to strandio?
++  fetch-gas-price
  =/  m  (strand:strandio @ud)  ::NOTE  return in wei
  ^-  form:m
  =/  =request:http
    :*  method=%'GET'
        url='https://api.etherscan.io/api?module=gastracker&action=gasoracle'
        header-list=~
        ~
    ==
  ;<  ~  bind:m
    (send-request:strandio request)
  ;<  rep=(unit client-response:iris)  bind:m
    take-maybe-response:strandio
  =*  fallback
    ~&  %fallback-gas-price
    (pure:m 40.000.000.000)  ::TODO  maybe even lower, considering we increment?
  ?.  ?&  ?=([~ %finished *] rep)
          ?=(^ full-file.u.rep)
      ==
    fallback
  ?~  jon=(de-json:html q.data.u.full-file.u.rep)
    fallback
  =;  res=(unit @ud)
    ?~  res  fallback
    %-  pure:m
    (mul 1.000.000.000 u.res)  ::NOTE  gwei to wei
  %.  u.jon
  =,  dejs-soft:format
  (ot 'result'^(ot 'FastGasPrice'^ni) ~)
--
