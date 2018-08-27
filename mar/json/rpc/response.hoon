::
/-  json-rpc
/+  httr-to-json
=,  json-rpc
::
|_  res=response
::
++  grab                                                ::  convert from
  |%
  ++  noun  response                                    ::  from noun
  ++  httr  (cork httr-to-json json)                    ::  from httr
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
      ((ot id+so result+some ~) a)
    ?^  res  `[%result u.res]
    ~|  a
    :+  ~  %error  %-  need
    ((ot id+so error+(ot code+no message+so ~) ~) a)
  --
--
