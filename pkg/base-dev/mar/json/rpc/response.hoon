::
/-  *json-rpc
::
|_  res=response
::
++  grad  %noun
++  grow
  |%
  ++  noun  res
  --
++  grab                                                ::  convert from
  |%
  ++  noun  response                                    ::  from noun
  ++  httr                                              ::  from httr
    |=  hit=httr:eyre
    ^-  response
    ~|  hit
    ?:  ?=(%2 (div p.hit 100))
      %-  json
      ?~  r.hit
        a+~
      (need (de:json:html q:u.r.hit))
    fail+hit
  ++  json                                              ::  from json
    =,  dejs-soft:format
    |=  a=json
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
