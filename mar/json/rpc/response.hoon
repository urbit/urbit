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
  ++  httr  (cork httr-to-json json)
  ++  json                                              ::  from json
    =,  dejs-soft:format
    |=  a=json
    ^-  response
    =/  res=(unit @t)
      ((ot result+so ~) a)
    ?^  res  result+u.res
    :-  %error  %-  need
    ((ot code+so message+so ~) a)
  --
--
