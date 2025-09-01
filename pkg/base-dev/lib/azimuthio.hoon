/-  rpc=json-rpc
/+  ethereum, azimuth, strandio
=,  strand=strand:strandio
=,  jael
|%
++  tract  azimuth:contracts:azimuth
++  fetch-point
  |=  [url=@ta who=ship]
  =/  m  (strand ,point:azimuth)
  ^-  form:m
  =/  =request:rpc:ethereum
    :+  %eth-call
      =-  [from=~ to=tract gas=~ price=~ value=~ data=-]
      (encode-call:rpc:ethereum 'points(uint32)' [%uint `@`who]~)
    [%label %latest]
  ;<  jon=json  bind:m  (request-rpc url `'point' request)
  =/  res=cord  (so:dejs:format jon)
  =/  =point:eth-noun:azimuth
    (decode-results:abi:ethereum res point:eth-type:azimuth)
  ::
  =/  =request:rpc:ethereum
    :+  %eth-call
      =-  [from=~ to=tract gas=~ price=~ value=~ data=-]
      (encode-call:rpc:ethereum 'rights(uint32)' [%uint `@`who]~)
    [%label %latest]
  ;<  jon=json  bind:m  (request-rpc url `'deed' request)
  =/  res=cord  (so:dejs:format jon)
  =/  =deed:eth-noun:azimuth
    (decode-results:abi:ethereum res deed:eth-type:azimuth)
  ::
  (pure:m (point-from-eth:azimuth who point deed))
::
++  request-rpc
  |=  [url=@ta id=(unit @t) req=request:rpc:ethereum]
  =/  m  (strand ,json)
  ^-  form:m
  %+  (retry json)  `10
  =/  m  (strand ,(unit json))
  ^-  form:m
  |^
  =/  =request:http
    :*  method=%'POST'
        url=url
        header-list=['Content-Type'^'application/json' ~]
        ^=  body
        %-  some  %-  as-octs:mimes:html
        %-  en:json:html
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
    =/  m  (strand ,(unit json))
    ^-  form:m
    ?>  ?=(%finished -.client-response)
    ?~  full-file.client-response
      (pure:m ~)
    =/  body=@t  q.data.u.full-file.client-response
    =/  jon=(unit json)  (de:json:html body)
    ?~  jon
      (pure:m ~)
    =,  dejs-soft:format
    =/  array=(unit (list response:rpc))
      ((ar parse-one-response) u.jon)
    ?~  array
      =/  res=(unit response:rpc)  (parse-one-response u.jon)
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
    ^-  (unit response:rpc)
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
::
++  retry
  |*  result=mold
  |=  [crash-after=(unit @ud) computation=_*form:(strand (unit result))]
  =/  m  (strand ,result)
  =|  try=@ud
  |-  ^-  form:m
  =*  loop  $
  ?:  =(crash-after `try)
    (strand-fail:strandio %retry-too-many ~)
  ;<  ~                  bind:m  (backoff:strandio try ~m1)
  ;<  res=(unit result)  bind:m  computation
  ?^  res
    (pure:m u.res)
  loop(try +(try))
::
++  get-latest-block
  |=  url=@ta
  =/  m  (strand ,block)
  ^-  form:m
  ;<  =json  bind:m  (request-rpc url `'block number' %eth-block-number ~)
  (get-block-by-number url (parse-eth-block-number:rpc:ethereum json))
::
++  get-block-by-number
  |=  [url=@ta =number:block]
  =/  m  (strand ,block)
  ^-  form:m
  |^
  ;<  =json  bind:m
    (request-rpc url `'block by number' %eth-get-block-by-number number |)
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
--
