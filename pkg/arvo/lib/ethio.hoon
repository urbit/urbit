::  ethio: Asynchronous Ethereum input/output functions.
::.
/+  strandio
=,  ethereum-types
=,  able:jael
::
=>  |%
    +$  topics  (list ?(@ux (list @ux)))
    --
|%
::  +request-rpc: send rpc request, with retry
::
++  request-rpc
  |=  [url=@ta id=(unit @t) req=request:rpc:ethereum]
  =/  m  (strand:strandio ,json)
  ^-  form:m
  ;<  res=(list [id=@t =json])  bind:m
    (request-batch-rpc-strict url [id req]~)
  ?:  ?=([* ~] res)
    (pure:m json.i.res)
  ~|  [%ethio %unexpected-results (lent res)]
  !!
::  +request-batch-rpc-strict: send rpc request, with retry
::
::    sends a batch requests. produces results for all requests in the batch,
::    but only if all of them are successful.
::
++  request-batch-rpc-strict
  |=  [url=@ta reqs=(list [id=(unit @t) req=request:rpc:ethereum])]
  |^  %+  (retry:strandio results)
        `10
      attempt-request
  ::
  +$  result   [id=@t =json]
  +$  results  (list result)
  ::
  ++  attempt-request
    =/  m  (strand:strandio ,(unit results))
    ^-  form:m
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
    =/  array=(unit (list response:rpc:jstd))
      ((ar:dejs-soft:format parse-one-response) u.jon)
    ?~  array
      ~&  %incomplete-batch
      (strand-fail:strandio %rpc-result-incomplete-batch >u.jon< ~)
    =-  ?~  err
          (pure:m `res)
        ~&  [%error-results err]
        (pure:m ~)
    %+  roll  u.array
    |=  $:  rpc=response:rpc:jstd
            [res=results err=(list [id=@t code=@t message=@t])]
        ==
    ?:  ?=(%error -.rpc)
      [res [+.rpc err]]
    ?.  ?=(%result -.rpc)
      [res [['' 'ethio-rpc-fail' (crip <rpc>)] err]]
    [[+.rpc res] err]
  ::
  ++  parse-one-response
    |=  =json
    ^-  (unit response:rpc:jstd)
    =/  res=(unit [@t ^json])
      %.  json
      =,  dejs-soft:format
      (ot id+so result+some ~)
    ?^  res  `[%result u.res]
    ~|  parse-one-response=json
    :+  ~  %error  %-  need
    %.  json
    =,  dejs-soft:format
    (ot id+so error+(ot code+no message+so ~) ~)
  --
::  +read-contract: calls a read function on a contract, produces result hex
::
++  read-contract
  |=  [url=@t proto-read-request:rpc:ethereum]
  =/  m  (strand:strandio ,@t)
  ;<  =json  bind:m
    %^  request-rpc  url  id
    :+  %eth-call
      ^-  call:rpc:ethereum
      [~ to ~ ~ ~ `tape`(encode-call:rpc:ethereum function arguments)]
    [%label %latest]
  ?.  ?=(%s -.json)  (strand-fail:strandio %request-rpc-fail >json< ~)
  (pure:m p.json)
::
++  get-latest-block
  |=  url=@ta
  =/  m  (strand:strandio ,block)
  ^-  form:m
  ;<  =json  bind:m
    (request-rpc url `'block number' %eth-block-number ~)
  (get-block-by-number url (parse-eth-block-number:rpc:ethereum json))
::
++  get-block-by-number
  |=  [url=@ta =number:block]
  =/  m  (strand:strandio ,block)
  ^-  form:m
  |^
  %+  (retry:strandio ,block)  `10
  =/  m  (strand:strandio ,(unit block))
  ^-  form:m
  ;<  =json  bind:m
    %+  request-rpc  url
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
++  get-logs-by-hash
  |=  [url=@ta =hash:block contracts=(list address) =topics]
  =/  m  (strand:strandio (list event-log:rpc:ethereum))
  ^-  form:m
  ;<  =json  bind:m
    %+  request-rpc  url
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
  |=  $:  url=@ta
          contracts=(list address)
          =topics
          =from=number:block
          =to=number:block
      ==
  =/  m  (strand:strandio (list event-log:rpc:ethereum))
  ^-  form:m
  ;<  =json  bind:m
    %+  request-rpc  url
    :*  `'logs by range'
        %eth-get-logs
        `number+from-number
        `number+to-number
        contracts
        topics
    ==
  %-  pure:m
  (parse-event-logs:rpc:ethereum json)
--
