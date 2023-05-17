::  ethio: Asynchronous Ethereum input/output functions.
::
/-  spider, rpc=json-rpc, eth-provider
/+  ethereum, strandio
=,  ethereum-types
=,  jael
::
|%
::  +request-rpc: send rpc request, with retry
::
++  request-rpc
  |=  [id=(unit @t) req=request:rpc:ethereum]
  =/  m  (strand:strandio ,json)
  ^-  form:m
  ;<  res=(list [id=@t =json])  bind:m
    (request-batch-rpc-strict [id req]~)
  ?:  ?=([* ~] res)
    (pure:m json.i.res)
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
  |^  %+  (retry:strandio results)
        `10
      attempt-request
  ::
  +$  results  (list [id=@t =json])
  ::
  ++  attempt-request
    =/  m  (strand:strandio ,(unit results))
    ^-  form:m
    ;<  responses=(list response:rpc)  bind:m
      (request-batch-rpc-loose reqs)
    =-  ?~  err
          (pure:m `res)
        (pure:m ~)
    %+  roll  responses
    |=  $:  rpc=response:rpc
            [res=results err=(list [id=@t code=@t message=@t])]
        ==
    ?:  ?=(%error -.rpc)
      [res [+.rpc err]]
    ?.  ?=(%result -.rpc)
      [res [['' 'ethio-rpc-fail' (crip <rpc>)] err]]
    [[+.rpc res] err]
  --
::  +request-batch-rpc-loose: send rpc requests, with retry
::
::    sends a batch request. produces results for all requests in the batch,
::    including the ones that are unsuccessful.
::
++  request-batch-rpc-loose
  |=  [reqs=(list [id=(unit @t) req=request:rpc:ethereum])]
  |^  %+  (retry:strandio results)
        `10
      attempt-request
  ::
  +$  result   response:rpc
  +$  results  (list response:rpc)
  ::
  ++  attempt-request
    =/  m  (strand:strandio ,(unit results))
    ^-  form:m
    ;<    provider-mode=provider-mode:eth-provider
      bind:m
    (scry:strandio provider-mode:eth-provider /gx/eth-provider/get-provider-mode/noun)
    ?:  =(%client -.provider-mode)
      ?>  ?=(%client -.provider-mode)
      =/  client  `client:eth-provider`+.provider-mode
      ;<  =bowl:spider  bind:m  get-bowl:strandio
      =/  rid  (crip (weld (weld (trip (scot %p our.bowl)) (trip tid.bowl)) (trip (scot %ux eny.bowl))))
      ;<  ~  bind:m
        (watch:strandio [%responses rid ~] [provider.client %eth-provider] [%responses rid ~])
      ;<  ~  bind:m
        %+  poke:strandio
          [provider.client %eth-provider] 
        [%provider-action !>([%provide rid reqs])]
      ;<  =cage  bind:m  (take-fact:strandio [%responses rid ~])
      (pure:m [~ !<(ethout:eth-provider q.cage)])
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
    (parse-responses u.rep)
  ::
  ++  parse-responses
    |=  =client-response:iris
    =/  m  (strand:strandio ,(unit results))
    ^-  form:m
    ?>  ?=(%finished -.client-response)
    ?~  full-file.client-response
      (pure:m ~)
    =/  body=@t  q.data.u.full-file.client-response
    =/  jon=(unit json)  (de-json:html body)
    ?~  jon
      (pure:m ~)
    =/  array=(unit (list response:rpc))
      ((ar:dejs-soft:format parse-one-response) u.jon)
    ?~  array
      (strand-fail:strandio %rpc-result-incomplete-batch >u.jon< ~)
    (pure:m array)
  ::
  ++  parse-one-response
    |=  =json
    ^-  (unit response:rpc)
    ?.  &(?=([%o *] json) (~(has by p.json) 'error'))
      =/  res=(unit [@t ^json])
        %.  json
        =,  dejs-soft:format
        (ot id+so result+some ~)
      ?~  res  ~
      `[%result u.res]
    ~|  parse-one-response=json
    =/  error=(unit [id=@t ^json code=@ta mssg=@t])
      %.  json
      =,  dejs-soft:format
      ::  A 'result' member is present in the error
      ::  response when using ganache, even though
      ::  that goes against the JSON-RPC spec
      ::
      (ot id+so result+some error+(ot code+no message+so ~) ~)
    ?~  error  ~
    =*  err  u.error
    `[%error id.err code.err mssg.err]
  --
::
::  +read-contract: calls a read function on a contract, produces result hex
::
++  read-contract
  |=  [req=proto-read-request:rpc:ethereum]
  =/  m  (strand:strandio ,@t)
  ;<  res=(list [id=@t res=@t])  bind:m
    (batch-read-contract-strict [req]~)
  ?:  ?=([* ~] res)
    (pure:m res.i.res)
  %+  strand-fail:strandio
    %unexpected-multiple-results
  [>(lent res)< ~]
::  +batch-read-contract-strict: calls read functions on contracts
::
::    sends a batch request. produces results for all requests in the batch,
::    but only if all of them are successful.
::
++  batch-read-contract-strict
  |=  [reqs=(list proto-read-request:rpc:ethereum)]
  |^  =/  m  (strand:strandio ,results)
      ^-  form:m
      ;<  res=(list [id=@t =json])  bind:m
        %-  request-batch-rpc-strict  
        (turn reqs proto-to-rpc)
      =+  ^-  [=results =failures]
        (roll res response-to-result)
      ?~  failures  (pure:m results)
      (strand-fail:strandio %batch-read-failed-for >failures< ~)
  ::
  +$  results   (list [id=@t res=@t])
  +$  failures  (list [id=@t =json])
  ::
  ++  proto-to-rpc
    |=  proto-read-request:rpc:ethereum
    ^-  [(unit @t) request:rpc:ethereum]
    :-  id
    :+  %eth-call
      ^-  call:rpc:ethereum
      [~ to ~ ~ ~ `tape`(encode-call:rpc:ethereum function arguments)]
    [%label %latest]
  ::
  ++  response-to-result
    |=  [[id=@t =json] =results =failures]
    ^+  [results failures]
    ?:  ?=(%s -.json)
      [[id^p.json results] failures]
    [results [id^json failures]]
  --
::
::
++  get-latest-block
  |=  b=?
  =/  m  (strand:strandio ,block)
  ^-  form:m
  ;<  =json  bind:m
    (request-rpc `'block number' %eth-block-number ~)
  (get-block-by-number (parse-eth-block-number:rpc:ethereum json))
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
  ;<  =json  bind:m
    %-  request-rpc  
    :-  `'block by number'
    [%eth-get-block-by-number number |]
  (pure:m (parse-block json))
  ::
  ++  parse-block
    |=  =json
    ^-  (unit block)
    =<  ?~(. ~ `[[&1 &2] |2]:u)
    ^-  (unit [@ @ @])
    ~|  json
    %.  json
    =,  dejs-soft:format
    %-  ot
    :~  hash+parse-hex
        number+parse-hex
        'parentHash'^parse-hex
    ==
  ::
  ++  parse-hex  |=(=json `(unit @)`(some (parse-hex-result:rpc:ethereum json)))
  --
::
++  get-tx-by-hash
  |=  [tx-hash=@ux]
  =/  m  (strand:strandio transaction-result:rpc:ethereum)
  ^-  form:m
  ;<  =json  bind:m
    %-  request-rpc  
    :*  `'tx by hash'
        %eth-get-transaction-by-hash
        tx-hash
    ==
  %-  pure:m
  (parse-transaction-result:rpc:ethereum json)
::
++  get-logs-by-hash
  |=  [=hash:block contracts=(list address) =topics]
  =/  m  (strand:strandio (list event-log:rpc:ethereum))
  ^-  form:m
  ;<  =json  bind:m
    %-  request-rpc  
    :*  `'logs by hash'
        %eth-get-logs-by-hash
        hash
        contracts
        topics
    ==
  %-  pure:m
  (parse-event-logs:rpc:ethereum json)
::
++  get-logs-by-range
  |=  $:  
          contracts=(list address)
          =topics
          =from=number:block
          =to=number:block
      ==
  =/  m  (strand:strandio (list event-log:rpc:ethereum))
  ^-  form:m
  ;<  =json  bind:m
    %-  request-rpc  
    :*  `'logs by range'
        %eth-get-logs
        `number+from-number
        `number+to-number
        contracts
        topics
    ==
  %-  pure:m
  (parse-event-logs:rpc:ethereum json)
::
++  get-next-nonce
  |=  [=address]
  =/  m  (strand:strandio ,@ud)
  ^-  form:m
  ;<  =json  bind:m
    %+  request-rpc  `'nonce'
    [%eth-get-transaction-count address [%label %latest]]
  %-  pure:m
  (parse-eth-get-transaction-count:rpc:ethereum json)
::
++  get-balance
  |=  [=address]
  =/  m  (strand:strandio ,@ud)
  ^-  form:m
  ;<  =json  bind:m
    %+  request-rpc  `'balance'
    [%eth-get-balance address [%label %latest]]
  %-  pure:m
  (parse-eth-get-balance:rpc:ethereum json)
--
