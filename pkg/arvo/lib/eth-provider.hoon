::  ethio: Asynchronous Ethereum input/output functions.
::
/-  spider, rpc=json-rpc, eth-provider
/+  ethereum, strandio
=,  ethereum-types
=,  jael
::
|%
+$  id-response  [id=(unit @t) res=response:rpc:ethereum]
+$  id-dirty-response  [id=(unit @t) res=dirty-response:rpc:ethereum]
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
    ;<  responses=(list id-dirty-response)  bind:m
      (request-batch-rpc-loose reqs)
    =/  err  (skim responses |=(r=id-dirty-response =(+<.r %error)))
    ~&  [%err err]
    ?~  err
      =/  clean-responses  
        %+  turn  
          responses  
        |=  r=id-dirty-response 
          ?.  ?=([%error *] +.r)  
            `id-response`r
          !!
      ~&  [%clean-responses clean-responses]
      (pure:m `clean-responses)
    (pure:m ~)
  --
::  +request-batch-rpc-loose: send rpc requests, with retry
::
::    sends a batch request. produces results for all requests in the batch,
::    including the ones that are unsuccessful.
::
++  request-batch-rpc-loose
  |=  [reqs=(list [id=(unit @t) req=request:rpc:ethereum])]
  |^  %+  (retry:strandio (list id-dirty-response))
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
    =/  m  (strand:strandio ,(unit (list id-dirty-response)))
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
      :: ;<  ~  bind:m  (take-wake:strandio ~)
      ~&  'after take-fact'
      ::  convert `(list [id response:rpc:ethereum])`q.cage to (list
      ::  response:rpc
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
    (parse-responses [reqs u.rep])
  ::
  ++  parse-responses
    |=  [requests=(list [id=(unit @t) req=request:rpc:ethereum]) =client-response:iris]
    =/  m  (strand:strandio ,(unit (list id-dirty-response)))
    ^-  form:m
    ?>  ?=(%finished -.client-response)
    ?~  full-file.client-response
      (pure:m ~)
    =/  body=@t  q.data.u.full-file.client-response
    =/  jon=(unit json)  (de-json:html body)
    ?~  jon
      (pure:m ~)
    ~&  [%jon jon]
    ::  TODO handle json if the whole batch fails 
    ::  (when we get one error json rpc instead 
    ::  of a batch of json-rpc responses)
    =/  json-array  ((ar:dejs:format |=(=json json)) u.jon)
    =/  typed-json-array  
      %+  turn 
        (gulf 0 (sub (lent json-array) 1))
      |=  n=@ud
      =/  req  (snag n requests)
      [+<.req (snag n json-array)]
      :: [%type (snag 0 json-array)]
    =/  parsed-array=(list id-dirty-response)
      (turn typed-json-array parse-response:rpc:ethereum)
    :: ~&  [%parsed-array parsed-array]
    ?~  parsed-array
      (strand-fail:strandio %rpc-result-incomplete-batch >u.jon< ~)
    (pure:m [~ parsed-array])
  ::
  --
::
::  +read-contract: calls a read function on a contract, produces result hex

++  read-contract
  |=  [req=proto-read-request:rpc:ethereum]
  =/  m  (strand:strandio ,@t)
  ;<  res=(list id-response)  bind:m
    (batch-read-contract-strict [req]~)
  ?:  ?=([[* [%data @t]] ~] res)
  :: ?:  ?=([* ~] res)
    ~&  [%read-contract res]
    :: TODO
    (pure:m '123')
    :: (pure:m +>.res.i.res)
  %+  strand-fail:strandio
    %unexpected-results
  [>(lent res)< ~]
::  +batch-read-contract-strict: calls read functions on contracts
::
::    sends a batch request. produces results for all requests in the batch,
::    but only if all of them are successful.

++  batch-read-contract-strict
  |=  [reqs=(list proto-read-request:rpc:ethereum)]
  |^  =/  m  (strand:strandio ,(list id-response))
      ^-  form:m
      ;<  result=(list id-dirty-response)  bind:m
        %-  request-batch-rpc-strict  
        (turn reqs proto-to-rpc)
      =+  ^-  [results=(list id-response) failures=(list id-dirty-response)]
        (roll result response-to-result)
      ?~  failures  (pure:m results)
      (strand-fail:strandio %batch-read-failed-for >failures< ~)
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
    |=  [idr=id-dirty-response results=(list id-response) failures=(list id-dirty-response)]
    ^+  [results failures]
    ?.  ?=([%error *] +.idr)  
      [[`id-response`idr results] failures]
    [results [`id-dirty-response`idr failures]]
  --
::
::
++  get-latest-block
  |=  b=?
  =/  m  (strand:strandio ,block)
  ^-  form:m
  ;<  result=id-response  bind:m
    (request-rpc `'block number' %eth-block-number ~)
  ?:  ?=([%atom @ux] +.result)
    (get-block-by-number `@udblocknumber`+>.result)
  (strand-fail:strandio %unexpected-response-type ~)

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
  ?:  ?=([%block block] +.res)
    (pure:m [~ +>.res])
  (strand-fail:strandio %unexpected-response-type ~)
  --
::
++  get-tx-by-hash
  |=  [tx-hash=@ux]
  =/  m  (strand:strandio transaction-result:rpc:ethereum)
  ^-  form:m
  ;<  result=id-response  bind:m
    %-  request-rpc  
    :*  `'tx by hash'
        %eth-get-transaction-by-hash
        tx-hash
    ==
  ?:  ?=([%transaction-result transaction-result:rpc:ethereum] +.result)
    (pure:m +>.result)
  (strand-fail:strandio %unexpected-response-type ~)
::::
++  get-logs-by-hash
  |=  [=hash:block contracts=(list address) =topics]
  =/  m  (strand:strandio (list event-log:rpc:ethereum))
  ^-  form:m
  ;<  result=id-response  bind:m
    %-  request-rpc  
    :*  `'logs by hash'
        %eth-get-logs-by-hash
        hash
        contracts
        topics
    ==
  ?:  ?=([%logs *] +.result)
    (pure:m +>.result)
  (strand-fail:strandio %unexpected-response-type ~)
::::
++  get-logs-by-range
  |=  $:  
          contracts=(list address)
          =topics
          =from=number:block
          =to=number:block
      ==
  =/  m  (strand:strandio (list event-log:rpc:ethereum))
  ^-  form:m
  ;<  result=id-response  bind:m
    %-  request-rpc  
    :*  `'logs by range'
        %eth-get-logs
        `number+from-number
        `number+to-number
        contracts
        topics
    ==
  ?:  ?=([%logs *] +.result)
    (pure:m +>.result)
  (strand-fail:strandio %unexpected-response-type ~)
::::
++  get-next-nonce
  |=  [=address]
  =/  m  (strand:strandio ,@ud)
  ^-  form:m
  ;<  result=id-response  bind:m
    %+  request-rpc  `'nonce'
    [%eth-get-transaction-count address [%label %latest]]
  ?:  ?=([%atom @ud] +.result)
    (pure:m `@ud`+>.result)
  (strand-fail:strandio %unexpected-response-type ~)
::::
++  get-balance
  |=  [=address]
  =/  m  (strand:strandio ,@ud)
  ^-  form:m
  ;<  result=id-response  bind:m
    %+  request-rpc  `'balance'
    [%eth-get-balance address [%label %latest]]
  ?:  ?=([%atom @ud] +.result)
    (pure:m `@ud`+>.result)
  (strand-fail:strandio %unexpected-response-type ~)
--
