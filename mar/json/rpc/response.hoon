::
/-  json-rpc
=,  json-rpc
::
!:
|_  res=raw-response
::
++  grab                                                ::  convert from
  |%
  ++  noun  raw-response                                    ::  from noun
  ++  httr                                              ::  from httr
    |=  hit/httr:eyre
    ^-  raw-response
    ~|  hit
    ?:  ?=($2 (div p.hit 100))
      =,  html
      (json (need (de-json q:(need r.hit))))
    fail+hit
  ++  json                                              ::  from json
    =,  dejs-soft:format
    |=  a=json
    ^-  raw-response
    =;  resp
      ?-    -.resp
          %error   resp
          %fail    resp
          %batch   resp
          %result
        ~|  resp
        [%result id.resp (crip (en-json:html res.resp))]
      ==
    ^-  response
    =;  dere
      =+  res=((ar dere) a)
      ?~  res  (need (dere a))
      [%batch u.res]
    |=  a=json
    ^-  (unit response)
    =/  res=(unit [@t json])
      ::TODO  breaks when no id present
      ((ot id+so result+some ~) a)
    ?^  res  `[%result u.res]
    ~|  a
    :+  ~  %error  %-  need
    ((ot id+so error+(ot code+no message+so ~) ~) a)
  --
--
