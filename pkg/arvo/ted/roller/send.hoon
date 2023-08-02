::  roller/send: send rollup tx
::
/-  rpc=json-rpc, *dice
/+  naive, ethereum, ethio, strandio
::
::
|=  args=vase
=+  !<(rpc-send-roll args)
=/  m  (strand:strandio ,vase)
|^
^-  form:m
::
=/  =address:ethereum  (address-from-prv:key:ethereum pk)
;<  expected-nonce=@ud  bind:m
  (get-next-nonce:ethio endpoint address)
::  Infura enforces a max calldata size (32, 64, 128 Kb?) so we calculate how
::  many txs are included in a batch of that size, and only send those
::
=/  max-calldata=@ud  128.000
=/  [n-txs=@ud batch-data=octs]
  =|  n-txs=@ud
  =|  size=@ud
  =|  out=(list octs)
  |-  ^-  [@ud octs]
  ?~  txs
    [n-txs (cad:naive 3 (flop out))]
  =*  raw-tx  i.txs
  =.  size    :(add 65 p.raw.raw-tx size)
  ?:  (gth size max-calldata)
    [n-txs (cad:naive 3 (flop out))]
  %_  $
    n-txs  +(n-txs)
    txs    t.txs
    out    [raw.raw-tx 65^sig.raw-tx out]
  ==
::  if the batch is malformed, emit error to kick it out of sending
::
?~  (parse-roll:naive q.batch-data)
  (pure:m !>(%.n^[%not-sent %batch-parse-error]))
::  if chain expects a different nonce, don't send this transaction
::
?.  =(nonce expected-nonce)
  %-  pure:m
  !>  ^-  [%.n @tas @t]
  :+  %.n
    %not-sent
  ?:  (lth expected-nonce nonce)
    ::  if ahead, use the same next-gas-price when resending
    ::
    %ahead-nonce
  ::  if behind, start out-of-sync flow if batch was not sent before
  ::
  %behind-nonce
::  if a gas-price of 0 was specified, fetch the recommended one
::
;<  use-gas-price=@ud  bind:m
  ?:  =(0 next-gas-price)  fetch-gas-price
  (pure:(strand:strandio @ud) next-gas-price)
::
::  each l2 signature is 65 bytes + XX bytes for the raw data
::  from the ethereum yellow paper:
::  gasLimit = G_transaction + G_txdatanonzero × dataByteLength
::  where
::      G_transaction = 21000 gas (base fee)
::    + G_txdatanonzero = 16 gas (previously 68; see EIP-2028)
::    * dataByteLength = (65 + raw) * (lent txs) bytes
::
::  1.000 gas are added to the base fee as extra, for emitting the log
::
=/  gas-limit=@ud  (add 22.000 (mul 16 p.batch-data))
=/  max-cost=@ud   (mul gas-limit use-gas-price)
;<  balance=@ud  bind:m
  (get-balance:ethio endpoint address)
?:  (gth max-cost balance)
  ::  if we cannot pay for the transaction, don't bother sending it out
  ::
  (pure:m !>(%.n^[%not-sent %insufficient-roller-balance]))
::
::NOTE  this fails the thread if sending fails, which in the app gives us
::      the "retry with same gas price" behavior we want
::
;<  =response:rpc  bind:m
  %+  send-batch  endpoint
  =;  tx=transaction:rpc:ethereum
    (sign-transaction:key:ethereum tx pk)
  :*  nonce
      use-gas-price
      gas-limit
      contract
      0
      q.batch-data
      chain-id
  ==
::  log batch tx-hash to getTransactionReceipt(tx-hash)
::
~?  &(?=(%result -.response) ?=(%s -.res.response))
  ^-  [nonce=@ud batch-hash=@t gas=@ud sent-txs=@ud bytes=@ud]
  :*  nonce
      (so:dejs:format res.response)
      use-gas-price
      n-txs
      p.batch-data
  ==
%-  pure:m
!>  ^-  (each [@ud @ud] [term @t])
::  TODO: capture if the tx fails (e.g. Runtime Error: revert)
::  check that tx-hash in +.response is non-zero?
::
?+  -.response  %.n^[%error 'unexpected rpc response']
  %error   %.n^[%error message.response]
  ::  add five gwei to gas price of next attempt
  ::
  %result  %.y^[n-txs (add use-gas-price 5.000.000.000)]
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
    ~&  >>  %fallback-gas-price
    (pure:m fallback-gas-price)
  ?.  ?&  ?=([~ %finished *] rep)
          ?=(^ full-file.u.rep)
          ::  get suggested price only for mainnet txs
          ::
          =(chain-id 1)
      ==
    fallback
  ?~  jon=(de:json:html q.data.u.full-file.u.rep)
    fallback
  =;  res=(unit @ud)
    ?~  res  fallback
    %-  pure:m
    (mul 1.000.000.000 u.res)  ::NOTE  gwei to wei
  %.  u.jon
  =,  dejs-soft:format
  (ot 'result'^(ot 'FastGasPrice'^(su dem) ~) ~)
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
