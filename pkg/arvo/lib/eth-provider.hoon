::  ethio: Asynchronous Ethereum input/output functions.
::
/-  spider, rpc=json-rpc, eth-provider
/+  ethereum, strandio
=,  ethereum-types
=,  jael
::
|%
+$  id-response  [id=(unit @t) res=response:rpc:ethereum]
::  +request-rpc: send rpc request, with retry
::
++  request-rpc
  |=  [id=(unit @t) req=request:rpc:ethereum]
  =/  m  (strand:strandio ,[id=(unit @t) res=response:rpc:ethereum])
  ^-  form:m
  ;<  res=(list [id=(unit @t) res=response:rpc:ethereum])  bind:m
    (request-batch-rpc-strict [id req]~)
  ?:  ?=([* ~] res)
    (pure:m i.res)
  %+  strand-fail:strandio
    %unexpected-multiple-results
  [>(lent res)< ~]
::  +request-batch-rpc-strict: send rpc requests, with retry
::
::    sends a batch request. produces results for all requests in the batch,
::    but only if all of them are successful.
::
++  request-batch-rpc-strict
  |=  [reqs=(list [id=(unit @t) req=request:rpc:ethereum])]
  |^  %+  (retry:strandio (list id-response))
        `10
      attempt-request
  ::
  ++  attempt-request
    =/  m  (strand:strandio ,(unit (list id-response)))
    ^-  form:m
    ;<  responses=(list id-response)  bind:m
      (request-batch-rpc-loose reqs)
    =/  err  (skim responses |=(r=id-response =(+<.r %error)))
    ?~  err
      (pure:m `responses)
    (pure:m ~)
  --
::  +request-batch-rpc-loose: send rpc requests, with retry
::
::    sends a batch request. produces results for all requests in the batch,
::    including the ones that are unsuccessful.
::
++  request-batch-rpc-loose
  |=  [reqs=(list [id=(unit @t) req=request:rpc:ethereum])]
  |^  %+  (retry:strandio (list id-response))
        `10
      attempt-request
  ::
  +$  result   response:rpc
  ++  take-fact
  |=  =wire
  =/  m  (strand:strandio ,cage)
  ^-  form:m
  |=  tin=strand-input:strand:strandio
  ~&  'take-fact strand'
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %sign [%wait @ ~] %behn %wake *]
    `[%fail [%timeout ~]]
      [~ %agent * %fact *]
    ?.  =(watch+wire wire.u.in.tin)
      `[%skip ~]
    `[%done cage.sign.u.in.tin]
  ==
  ::
  ++  attempt-request
    =/  m  (strand:strandio ,(unit (list id-response)))
    ^-  form:m
    ;<  =bowl:spider  bind:m  get-bowl:strandio
    ~&  'after take-wake'
    ;<    provider-mode=provider-mode:eth-provider
      bind:m
    (scry:strandio provider-mode:eth-provider /gx/eth-provider/get-provider-mode/noun)
    ?:  =(%client -.provider-mode)
      ?>  ?=(%client -.provider-mode)
      =/  client  `client:eth-provider`+.provider-mode
      =/  rid  (crip (weld (weld (trip (scot %p our.bowl)) (trip tid.bowl)) (trip (scot %ux eny.bowl))))
      ;<  ~  bind:m
        (watch:strandio [%responses rid ~] [provider.client %eth-provider] [%responses rid ~])
      ;<  ~  bind:m
        %+  poke:strandio
          [provider.client %eth-provider] 
        [%provider-action !>([%provide rid reqs])]
      ;<  ~  bind:m  (send-wait:strandio (add now.bowl ~s10))
      ;<  =cage  bind:m  (take-fact [%responses rid ~])
      ;<  ~  bind:m  (take-wake:strandio ~)
      ~&  'after take-fact'
      ::  convert `(list [id response:rpc:ethereum])`q.cage to (list
      ::  response:rpc
      :: (pure:m [~ !<(ethout:eth-provider q.cage)])
      (pure:m [~ ~[[[~ '123'] [%error %.y]]]])
    =/  url  ''
    =?  url  ?=(%local -.provider-mode)  url.local.provider-mode
    =?  url  ?=(%provider -.provider-mode)  url.provider.provider-mode
    =/  =request:http
      :*  method=%'POST'
          url=url
          header-list=['Content-Type'^'application/json' ~]
        ::
          ^=  body
          %-  some  %-  as-octt:mimes:html
          %-  en-json:html
          a+(turn reqs request-to-json:rpc:ethereum)
      ==
    ;<  ~  bind:m
      (send-request:strandio request)
    ;<  rep=(unit client-response:iris)  bind:m
      take-maybe-response:strandio
    ?~  rep
      (pure:m ~)
    (parse-responses [reqs u.rep])
  ::
  ++  parse-responses
    |=  [requests=(list [id=(unit @t) req=request:rpc:ethereum]) =client-response:iris]
    =/  m  (strand:strandio ,(unit (list id-response)))
    ^-  form:m
    ?>  ?=(%finished -.client-response)
    ?~  full-file.client-response
      (pure:m ~)
    =/  body=@t  q.data.u.full-file.client-response
    =/  jon=(unit json)  (de-json:html body)
    ?~  jon
      (pure:m ~)
    =/  json-array  ((ar:dejs:format |=(=json json)) u.jon)
    =/  typed-json-array  
      %+  turn 
        (gulf 0 (sub (lent json-array) 1))
      |=  n=@ud
      =/  req  (snag n requests)
      [+<.req (snag n json-array)]
      :: [%type (snag 0 json-array)]
    =/  parsed-array=(list [id=(unit @t) response:rpc:ethereum])
      (turn typed-json-array parse-response:rpc:ethereum)
    :: ~&  [%parsed-array parsed-array]
    ?~  parsed-array
      (strand-fail:strandio %rpc-result-incomplete-batch >u.jon< ~)
    (pure:m [~ parsed-array])
  ::
  --
::
::  +read-contract: calls a read function on a contract, produces result hex
::
::++  read-contract
::  |=  [req=proto-read-request:rpc:ethereum]
::  =/  m  (strand:strandio ,@t)
::  ;<  res=(list [id=@t res=@t])  bind:m
::    (batch-read-contract-strict [req]~)
::  ?:  ?=([* ~] res)
::    (pure:m res.i.res)
::  %+  strand-fail:strandio
::    %unexpected-multiple-results
::  [>(lent res)< ~]
::::  +batch-read-contract-strict: calls read functions on contracts
::::
::::    sends a batch request. produces results for all requests in the batch,
::::    but only if all of them are successful.
::::
::++  batch-read-contract-strict
::  |=  [reqs=(list proto-read-request:rpc:ethereum)]
::  |^  =/  m  (strand:strandio ,results)
::      ^-  form:m
::      ;<  res=(list [id=@t =json])  bind:m
::        %-  request-batch-rpc-strict  
::        (turn reqs proto-to-rpc)
::      =+  ^-  [=results =failures]
::        (roll res response-to-result)
::      ?~  failures  (pure:m results)
::      (strand-fail:strandio %batch-read-failed-for >failures< ~)
::  ::
::  +$  results   (list [id=@t res=@t])
::  +$  failures  (list [id=@t =json])
::  ::
::  ++  proto-to-rpc
::    |=  proto-read-request:rpc:ethereum
::    ^-  [(unit @t) request:rpc:ethereum]
::    :-  id
::    :+  %eth-call
::      ^-  call:rpc:ethereum
::      [~ to ~ ~ ~ `tape`(encode-call:rpc:ethereum function arguments)]
::    [%label %latest]
::  ::
::  ++  response-to-result
::    |=  [[id=@t =json] =results =failures]
::    ^+  [results failures]
::    ?:  ?=(%s -.json)
::      [[id^p.json results] failures]
::    [results [id^json failures]]
::  --
::::
::::
::++  get-latest-block
::  |=  b=?
::  =/  m  (strand:strandio ,block)
::  ^-  form:m
::  ;<  =json  bind:m
::    (request-rpc `'block number' %eth-block-number ~)
::  (get-block-by-number (parse-eth-block-number:rpc:ethereum json))
::
++  get-block-by-number
  |=  [=number:block]
  =/  m  (strand:strandio ,block)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  |^
  %+  (retry:strandio ,block)  `10
  =/  m  (strand:strandio ,(unit block))
  ^-  form:m
  ;<  res=[id=(unit @t) response:rpc:ethereum]  bind:m
    %-  request-rpc  
    :-  `'block by number'
    [%eth-get-block-by-number number |]
  ~&  [%res +<.res]
  ?:  =(%block +<.res)
    (pure:m [~ [id=[hash=0xd4e5.6740.f876.aef8.c010.b86a.40d5.f567.45a1.18d0.906a.34e6.9aec.8c0d.b1cb.8fa3 number=0] parent-hash=0x0]])
  (strand-fail:strandio %unexpected-response-type ~)
  :: (pure:m (parse-block json))
  ::
  ::
  :: ++  parse-hex  |=(=json `(unit @)`(some (parse-hex-result:rpc:ethereum json)))
  --
::
::++  get-tx-by-hash
::  |=  [tx-hash=@ux]
::  =/  m  (strand:strandio transaction-result:rpc:ethereum)
::  ^-  form:m
::  ;<  =json  bind:m
::    %-  request-rpc  
::    :*  `'tx by hash'
::        %eth-get-transaction-by-hash
::        tx-hash
::    ==
::  %-  pure:m
::  (parse-transaction-result:rpc:ethereum json)
::::
::++  get-logs-by-hash
::  |=  [=hash:block contracts=(list address) =topics]
::  =/  m  (strand:strandio (list event-log:rpc:ethereum))
::  ^-  form:m
::  ;<  =json  bind:m
::    %-  request-rpc  
::    :*  `'logs by hash'
::        %eth-get-logs-by-hash
::        hash
::        contracts
::        topics
::    ==
::  %-  pure:m
::  (parse-event-logs:rpc:ethereum json)
::::
::++  get-logs-by-range
::  |=  $:  
::          contracts=(list address)
::          =topics
::          =from=number:block
::          =to=number:block
::      ==
::  =/  m  (strand:strandio (list event-log:rpc:ethereum))
::  ^-  form:m
::  ;<  =json  bind:m
::    %-  request-rpc  
::    :*  `'logs by range'
::        %eth-get-logs
::        `number+from-number
::        `number+to-number
::        contracts
::        topics
::    ==
::  %-  pure:m
::  (parse-event-logs:rpc:ethereum json)
::::
::++  get-next-nonce
::  |=  [=address]
::  =/  m  (strand:strandio ,@ud)
::  ^-  form:m
::  ;<  =json  bind:m
::    %+  request-rpc  `'nonce'
::    [%eth-get-transaction-count address [%label %latest]]
::  %-  pure:m
::  (parse-eth-get-transaction-count:rpc:ethereum json)
::::
::++  get-balance
::  |=  [=address]
::  =/  m  (strand:strandio ,@ud)
::  ^-  form:m
::  ;<  =json  bind:m
::    %+  request-rpc  `'balance'
::    [%eth-get-balance address [%label %latest]]
::  %-  pure:m
::  (parse-eth-get-balance:rpc:ethereum json)
--
