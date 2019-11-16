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
  |^  %+  (retry:strandio json)  `10
      =/  m  (strand:strandio ,(unit json))
      ^-  form:m
      =/  =request:http
        :*  method=%'POST'
            url=url
            header-list=['Content-Type'^'application/json' ~]
            ^=  body
            %-  some  %-  as-octt:mimes:html
            %-  en-json:html
            (request-to-json:rpc:ethereum id req)
        ==
      ;<  ~  bind:m  (send-request:strandio request)
      ;<  rep=(unit client-response:iris)  bind:m
        take-maybe-response:strandio
      ?~  rep
        (pure:m ~)
      (parse-response u.rep)
  ::
  ++  parse-response
    |=  =client-response:iris
    =/  m  (strand:strandio ,(unit json))
    ^-  form:m
    ?>  ?=(%finished -.client-response)
    ?~  full-file.client-response
      (pure:m ~)
    =/  body=@t  q.data.u.full-file.client-response
    =/  jon=(unit json)  (de-json:html body)
    ?~  jon
      (pure:m ~)
    =,  dejs-soft:format
    =/  array=(unit (list response:rpc:jstd))
      ((ar parse-one-response) u.jon)
    ?~  array
      =/  res=(unit response:rpc:jstd)  (parse-one-response u.jon)
      ?~  res
        (strand-fail:strandio %request-rpc-parse-error >id< ~)
      ?:  ?=(%error -.u.res)
        (strand-fail:strandio %request-rpc-error >id< >+.res< ~)
      ?.  ?=(%result -.u.res)
        (strand-fail:strandio %request-rpc-fail >u.res< ~)
      (pure:m `res.u.res)
    (strand-fail:strandio %request-rpc-batch >%not-implemented< ~)
    ::  (pure:m `[%batch u.array])
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
  ;<  =json  bind:m
    %+  request-rpc  url
    :-  `'block by number'
    [%eth-get-block-by-number number |]
  =/  =block  (parse-block json)
  ?.  =(number number.id.block)
    (strand-fail:strandio %reorg-detected >number< >block< ~)
  (pure:m block)
  ::
  ++  parse-block
    |=  =json
    ^-  block
    =<  [[&1 &2] |2]
    ^-  [@ @ @]
    ~|  json
    %.  json
    =,  dejs:format
    %-  ot
    :~  hash+parse-hex-result:rpc:ethereum
        number+parse-hex-result:rpc:ethereum
        'parentHash'^parse-hex-result:rpc:ethereum
    ==
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
