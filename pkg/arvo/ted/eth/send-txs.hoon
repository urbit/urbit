::  eth/send-txs: query ethereum for contract data
::
::    produces hex string result, for use with +decode-results:rpc:ethereum
::
/-  rpc=json-rpc
/+  ethio, strandio
::
=>
  |%
  ++  tape-to-ux
    |=  t=tape
    (scan t ;~(pfix (jest '0x') hex))
  --
::
|=  args=vase
=+  !<([url=@t step-size=@ud txs=(list @ux)] args)
=/  m  (strand:strandio ,vase)
^-  form:m
|-
=*  submit-loop  $
~&  ['remaining txs:' (lent txs)]
?:  =(~ txs)  (pure:m !>(~))
::  send a step-size batch of transactions
::
;<  responses=(list response:rpc)  bind:m
  %+  request-batch-rpc-loose:ethio  url
  %+  turn  (scag step-size txs)
  |=  tx=@ux
  :-  `(scot %ux (end [3 10] tx))
  [%eth-send-raw-transaction tx]
::  parse tx hashes out of responses, bailing on submission failure
::
=/  pending=(each (set @ux) [term tang])
  =|  pending=(list @ux)
  |-
  ?~  responses  &+(sy pending)
  =/  res=response:rpc  i.responses  ::NOTE  =* breaks typechecks
  ?+  -.res  |+[%unexpected-non-result >res< ~]
      %result
    %_  $
      responses  t.responses
      pending    [(tape-to-ux (sa:dejs:format res.res)) pending]
    ==
  ::
      %error
    ?:  ?|  =('known transaction' (end [3 17] message.res))
            =('Known transaction' (end [3 17] message.res))
            =('Transaction with the same ' (end [3 26] message.res))
        ==
      ~&  [%sent-a-known-transaction--skipping id.res]
      $(responses t.responses)
    ?:  ?|  =('nonce too low' message.res)
            =('Nonce too low' message.res)
        ==
      ::NOTE  this assumes it's an "oops re-sent txs" case
      ~&  [%nonce-too-low--skipping id.res]
      $(responses t.responses)
    |+[%transaction-send-failed >+.res< ~]
  ==
?:  ?=(%| -.pending)
  (strand-fail:strandio p.pending)
::  wait for the transactions to get confirmed
::
|-
=*  confirm-loop  $
?:  =(~ p.pending)
  ::  all confirmed, continue to next step-size transactions
  ::
  submit-loop(txs (slag step-size txs))
~&  [~(wyt in p.pending) 'txs awaiting confirmation']
::  get receipts
::
;<  responses=(list response:rpc)  bind:m
  %+  request-batch-rpc-loose:ethio  url
  %+  turn  ~(tap in p.pending)
  |=  txh=@ux
  :-  `(crip '0' 'x' ((x-co:co 64) txh))
  [%eth-get-transaction-receipt txh]
::  find transactions that haven't been confirmed yet, bailing on failure
::
=/  unconfirmed=(each (set @ux) [term tang])
  =|  done=(list @ux)
  |-
  ?~  responses  &+(~(dif in p.pending) (sy done))
  =/  res=response:rpc  i.responses  ::NOTE  =* breaks typechecks
  ?.  ?=(?(%error %result) -.res)
    |+[%unexpected-non-result >res< ~]
  =/  txh=@ux  (tape-to-ux (trip id.res))
  ?-  -.res
      %error
    ~&  :-  'receipt fetch error'
        [code.res message.res]
    $(responses t.responses)
  ::
      %result
    ?.  ?=([%o *] res.res)
      $(responses t.responses)
    =/  status=@
      %-  tape-to-ux
      %-  sa:dejs:format
      (~(got by p.res.res) 'status')
    ?.  =(1 status)
      |+[%transaction-failed >txh< ~]
    $(responses t.responses, done [txh done])
  ==
?:  ?=(%| -.unconfirmed)
  (strand-fail:strandio p.unconfirmed)
::  some transactions still pending, wait for a bit, then check again
::
;<  ~  bind:m  (sleep:strandio ~s30)
confirm-loop(pending unconfirmed)
