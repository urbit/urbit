::  aggregator/send: send rollup tx
::
/-  rpc=json-rpc, *aggregator
/+  naive, ethereum, ethio, strandio
::
=/  gas-limit=@ud  30.000  ::TODO  verify, maybe scale with roll size
=/  batch-function=octs
  %-  as-octs:mimes:html
  %+  rsh  [3 28]
  (keccak-256:keccak:crypto (as-octs:mimes:html 'batch(bytes)'))
::
|=  args=vase
=+  !<(rpc-send-roll args)
=/  m  (strand:strandio ,vase)
|^
^-  form:m
=*  not-sent  (pure:m !>(%.y^next-gas-price))
::
=/  =address:ethereum  (address-from-prv:key:ethereum pk)
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
::TODO  implement %eth-get-balance in /lib/ethio and /lib/ethereum
:: ;<  balance=@ud  bind:m
::   !!
:: ?:  (gth max-cost balance)
::   ~&  [%insufficient-aggregator-balance address]
::   not-sent
::
=/  tx-data=@ux
  %+  can:naive  3
  :_  [batch-function ~]
  =;  =cord
    =/  parsed=(unit (pair @ud @ux))
      (de:base16:mimes:html cord)
    ?~(parsed !! u.parsed)
  %-  crip
  %-  encode-args:abi:ethereum
  :_  ~
  :-  %bytes
  %+  cad:naive  3
  %+  roll  txs
  |=  [=raw-tx:naive out=(list octs)]
  [raw.raw-tx 65^sig.raw-tx out]
=/  tx=@ux
  =;  tx=transaction:rpc:ethereum
    (sign-transaction:key:ethereum tx pk)
  :*  nonce
      use-gas-price
      gas-limit
      contract
      0
      tx-data
      chain-id
  ==
::
::NOTE  this fails the thread if sending fails, which in the app gives us
::      the "retry with same gas price" behavior we want
;<  =response:rpc  bind:m  (send-batch endpoint tx)
%-  pure:m
!>  ^-  (each @ud @t)
?+  -.response  %.n^'unexpected rpc response'
  %error   %.n^message.res
  :: TODO:
  ::  check that tx-hash in +.response is non-zero?
  ::  log tx-hash to getTransactionReceipt(tx-hash)?
  ::  enforce max here, or in app?
  ::  add five gwei to gas price of next attempt
  ::
  %result  %.y^(add use-gas-price 5.000.000.000)
==
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
  (ot 'result'^(ot 'FastGasPrice'^ni ~) ~)
::
++  send-batch
  |=  [endpoint=@ta batch=@ux]
  =/  m  (strand:strandio ,response:rpc)
  ^-  form:m
  =/  req=[(unit @t) request:rpc:ethereum]
    [`'sendRawTransaction' %eth-send-raw-transaction batch]
  ;<  res=(list response:rpc)  bind:m
    (request-batch-rpc-loose:ethio endpoint [req]~)
  ?:  ?=([* ~] res)
    (pure:m i.res)
  %+  strand-fail:strandio
    %unexpected-multiple-results
  [>(lent res)< ~]
::
--
