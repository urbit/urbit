::  ethio: Asynchronous Ethereum input/output functions.
::.
/+  stdio
::
|*  [out-poke-data=mold out-peer-data=mold]
=>  |%
    ++  stdio  (^stdio out-poke-data out-peer-data)
    --
|%
::  +request-rpc: send rpc request, with retry
::
++  request-rpc
  |=  [url=@ta id=(unit @t) req=request:rpc:ethereum]
  =/  m  (async:stdio ,json)
  ^-  form:m
  |^  %+  (retry json)  `10
      =/  m  (async:stdio ,(unit json))
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
      ;<  ~  bind:m  (send-request:stdio request)
      ;<  rep=(unit client-response:iris)  bind:m
        take-maybe-response:stdio
      ?~  rep
        (pure:m ~)
      (parse-response u.rep)
  ::
  ++  retry
    |*  result=mold
    |=  [crash-after=(unit @ud) computation=_*form:(async:stdio (unit result))]
    =/  m  (async:stdio ,result)
    =|  try=@ud
    |^  |-  ^-  form:m
        =*  loop  $
        ?:  =(crash-after `try)
          (async-fail:stdio %retry-too-many ~)
        ;<  ~                  bind:m  (backoff try ~m1)
        ;<  res=(unit result)  bind:m  computation
        ?^  res
          (pure:m u.res)
        loop(try +(try))
    ::
    ++  backoff
      |=  [try=@ud limit=@dr]
      =/  m  (async:stdio ,~)
      ^-  form:m
      ;<  eny=@uvJ  bind:m  get-entropy:stdio
      ;<  now=@da   bind:m  get-time:stdio
      %-  wait:stdio
      %+  add  now
      %+  min  limit
      ?:  =(0 try)  ~s0
      %+  add
        (mul ~s1 (bex (dec try)))
      (mul ~s0..0001 (~(rad og eny) 1.000))
    --
  ::
  ++  parse-response
    |=  =client-response:iris
    =/  m  (async:stdio ,(unit json))
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
        (async-fail:stdio %request-rpc-parse-error >id< ~)
      ?:  ?=(%error -.u.res)
        (async-fail:stdio %request-rpc-error >id< >+.res< ~)
      ?.  ?=(%result -.u.res)
        (async-fail:stdio %request-rpc-fail >u.res< ~)
      (pure:m `res.u.res)
    (async-fail:stdio %request-rpc-batch >%not-implemented< ~)
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
--