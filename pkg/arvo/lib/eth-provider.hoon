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
  |^  %+  (retry:strandio (list id-response:eth-provider))
        `10
      attempt-request
  ::
  ++  attempt-request
    =/  m  (strand:strandio ,(unit (list id-response:eth-provider)))
    ^-  form:m
    ;<  responses=ethout:eth-provider  bind:m
      (request-batch-rpc-loose reqs)
    ?~  -.responses
      =/  err  (skim +.responses |=(r=id-dirty-response:eth-provider =(+<.r %error)))
      ?~  err
        =/  clean-responses  
          %+  turn  
            +.responses  
          |=  r=id-dirty-response:eth-provider 
            ?.  ?=([%error *] +.r)  
              `id-response:eth-provider`r
            !!
        (pure:m `clean-responses)
      (strand-fail:strandio %json-rpc-error >+.responses< ~)
    (strand-fail:strandio %json-rpc-batch-error >-.responses< ~)
  --
::  +request-batch-rpc-loose: send rpc requests, with retry
::
::    sends a batch request. produces results for all requests in the batch,
::    including the ones that are unsuccessful.
::
++  request-batch-rpc-loose
  |=  [reqs=(list [id=(unit @t) req=request:rpc:ethereum])]
  |^  %+  (retry:strandio ethout:eth-provider)
        `10
      attempt-request
  ::
  ++  watch
    |=  [=wire =dock =path]
    =/  m  (strand:strandio ,~)
    ^-  form:m
    =/  =card:agent:gall  [%pass watch+wire %agent dock %watch path]
    ;<  ~  bind:m  (send-raw-card:strandio card)
    (pure:m ~)
  ::
  ++  take-fact
  |=  =wire
  =/  m  (strand:strandio ,cage)
  ^-  form:m
  |=  tin=strand-input:strand:strandio
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
  ++  take-watch-ack
    |=  =wire
    =/  m  (strand:strandio ,~)
    ^-  form:m
    |=  tin=strand-input:strand:strandio
    ?+  in.tin  `[%skip ~]
        ~  `[%wait ~]
        [~ %sign [%wait @ ~] %behn %wake *]
      `[%fail [%timeout ~]]
        [~ %agent * %watch-ack *]
      ?.  =(watch+wire wire.u.in.tin)
        `[%skip ~]
      ?~  p.sign.u.in.tin
        `[%done ~]
      `[%fail %watch-ack-fail u.p.sign.u.in.tin]
    ==
  ::
  ++  attempt-request
    =/  m  (strand:strandio ,(unit ethout:eth-provider))
    ^-  form:m
    ;<  =bowl:spider  bind:m  get-bowl:strandio
    ;<    provider-mode=provider-mode:eth-provider
      bind:m
    (scry:strandio provider-mode:eth-provider /gx/eth-provider/get-provider-mode/noun)
    ?:  =(%client -.provider-mode)
      ?>  ?=(%client -.provider-mode)
      =/  client  `client:eth-provider`+.provider-mode
      =/  rid  (crip (weld (weld (trip (scot %p our.bowl)) (trip tid.bowl)) (trip (scot %ux eny.bowl))))
      ;<  ~  bind:m  (send-wait:strandio (add now.bowl ~s10))
      ;<  ~  bind:m
        (watch [%responses rid ~] [provider.client %eth-provider] [%responses rid ~])
      ;<  ~  bind:m
        (take-watch-ack [%responses rid ~])
      =/  =card:agent:gall  
        :*  %pass   /poke 
            %agent  [provider.client %eth-provider] 
            %poke   [%provider-action !>([%provide rid reqs])]
        ==
      ;<  ~  bind:m  (send-raw-card:strandio card)
      ;<  =cage  bind:m  (take-fact [%responses rid ~])
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
    =/  m  (strand:strandio ,(unit ethout:eth-provider))
    ^-  form:m
    ?>  ?=(%finished -.client-response)
    ?~  full-file.client-response
      (pure:m ~)
    =/  body=@t  q.data.u.full-file.client-response
    =/  jon=(unit json)  (de-json:html body)
    ?~  jon
      (pure:m ~)
    ?:  &(?=([%o *] u.jon) |((~(has by p.u.jon) 'error') %.n))
      =/  error=[id=@t code=@t message=@t]
        %.  u.jon
        =,  dejs:format
        (ot id+so error+(ot code+no message+so ~) ~)
      (pure:m [~ [[~ [code.error message.error]] ~]])
    =/  json-array  ((ar:dejs:format |=(=json json)) u.jon)
    =/  typed-json-array
      %+  turn 
        (gulf 0 (sub (lent json-array) 1))
      |=  n=@ud
      =/  req  (snag n requests)
      [+<.req (snag n json-array)]
      :: [%type (snag 0 json-array)]
    =/  parsed-array=(list id-dirty-response:eth-provider)
      (turn typed-json-array parse-response:rpc:ethereum)
    ?~  parsed-array
      (strand-fail:strandio %rpc-result-incomplete-batch >u.jon< ~)
    (pure:m [~ [~ parsed-array]])
  ::
  --
::
::  +read-contract: calls a read function on a contract, produces result hex
++  read-contract
  |=  req=proto-read-request:rpc:ethereum
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

++  batch-read-contract-strict
  |=  [reqs=(list proto-read-request:rpc:ethereum)]
  |^  =/  m  (strand:strandio ,(list [id=@t res=@t]))
      ^-  form:m
      ;<  result=(list id-response:eth-provider)  bind:m
        %-  request-batch-rpc-strict  
        (turn reqs proto-to-rpc)
      =+  ^-  [=results =failures]
        (roll result response-to-result)
      ?~  failures  (pure:m results)
      (strand-fail:strandio %batch-read-failed-for >failures< ~)
  ::
  +$  results   (list [id=@t res=@t])
  +$  failures  (list id-response:eth-provider)
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
    |=  [idr=id-response:eth-provider =results =failures]
    ^+  [results failures]
    ?:  &(?=(^ id.idr) ?=(%eth-call -.res.idr))
      [[[u.id.idr data.res.idr] results] failures]
    [results [`id-response:eth-provider`idr failures]]
  --
::
++  get-latest-block
  |=  b=?
  =/  m  (strand:strandio ,block:rpc:ethereum)
  ^-  form:m
  ;<  result=id-response:eth-provider  bind:m
    (request-rpc `'block number' %eth-block-number ~)
  ?:  ?=([%eth-block-number @ux] +.result)
    (get-block-by-number `@udblocknumber`+>.result)
  (strand-fail:strandio %unexpected-response-type ~)

++  get-block-by-number
  |=  [=number:block:rpc:ethereum]
  =/  m  (strand:strandio ,block:rpc:ethereum)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  |^
  %+  (retry:strandio ,block:rpc:ethereum)  `10
  =/  m  (strand:strandio ,(unit block:rpc:ethereum))
  ^-  form:m
  ;<  res=[id=(unit @t) response:rpc:ethereum]  bind:m
    %-  request-rpc  
    :-  `'block by number'
    [%eth-get-block-by-number number |]
  ?:  ?=([%eth-get-block-by-number (unit block:rpc:ethereum)] +.res)
    (pure:m +>.res)
  (strand-fail:strandio %unexpected-response-type ~)
  --
::
++  get-tx-by-hash
  |=  [tx-hash=@ux]
  =/  m  (strand:strandio (unit transaction-result:rpc:ethereum))
  ^-  form:m
  ;<  result=id-response:eth-provider  bind:m
    %-  request-rpc  
    :*  `'tx by hash'
        %eth-get-transaction-by-hash
        tx-hash
    ==
  ?:  ?=([%eth-get-transaction-by-hash (unit transaction-result:rpc:ethereum)] +.result)
    (pure:m +>.result)
  (strand-fail:strandio %unexpected-response-type ~)
::::
++  get-logs-by-hash
  |=  [=hash:block:rpc:ethereum contracts=(list address) =topics]
  =/  m  (strand:strandio (list event-log:rpc:ethereum))
  ^-  form:m
  ;<  result=id-response:eth-provider  bind:m
    %-  request-rpc  
    :*  `'logs by hash'
        %eth-get-logs-by-hash
        hash
        contracts
        topics
    ==
  ?:  ?=([%eth-get-logs-by-hash *] +.result)
    (pure:m +>.result)
  (strand-fail:strandio %unexpected-response-type ~)
::::
++  get-logs-by-range
  |=  $:  
          contracts=(list address)
          =topics
          =from=number:block:rpc:ethereum
          =to=number:block:rpc:ethereum
      ==
  =/  m  (strand:strandio (list event-log:rpc:ethereum))
  ^-  form:m
  ;<  result=id-response:eth-provider  bind:m
    %-  request-rpc  
    :*  `'logs by range'
        %eth-get-logs
        `number+from-number
        `number+to-number
        contracts
        topics
    ==
  ?:  ?=([%eth-get-logs *] +.result)
    (pure:m +>.result)
  (strand-fail:strandio %unexpected-response-type ~)
::::
++  get-next-nonce
  |=  [=address]
  =/  m  (strand:strandio ,@ud)
  ^-  form:m
  ;<  result=id-response:eth-provider  bind:m
    %+  request-rpc  `'nonce'
    [%eth-get-transaction-count address [%label %latest]]
  ?:  ?=([%eth-get-transaction-count @ud] +.result)
    (pure:m `@ud`+>.result)
  (strand-fail:strandio %unexpected-response-type ~)
::::
++  get-balance
  |=  [=address]
  =/  m  (strand:strandio ,@ud)
  ^-  form:m
  ;<  result=id-response:eth-provider  bind:m
    %+  request-rpc  `'balance'
    [%eth-get-balance address [%label %latest]]
  ?:  ?=([%eth-get-balance @ud] +.result)
    (pure:m `@ud`+>.result)
  (strand-fail:strandio %unexpected-response-type ~)
--
