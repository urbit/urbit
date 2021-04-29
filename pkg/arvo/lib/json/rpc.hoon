::  json-rpc: protocol utilities
::
/-  *json-rpc
|%
++  request-to-hiss
  |=  [url=purl:eyre req=request]
  ^-  hiss:eyre
  :-  url
  :+  %post
    %-  ~(gas in *math:eyre)
    ~['Content-Type'^['application/json']~]
  %-  some
  %-  as-octt:mimes:html
  (en-json:html (request-to-json req))
::
++  request-to-json
  |=  request
  ^-  json
  %-  pairs:enjs:format
  :~  jsonrpc+s+'0.2'
      id+s+id
      method+s+method
    ::
      :-  %params
      ^-  json
      ?-  -.params
        %list     [%a +.params]
        %object   [%o (~(gas by *(map @t json)) +.params)]
      ==
  ==
::
++  response-to-json
  |=  =response
  ^-  json
  ::  TODO: consider all cases
  ::
  ?+  -.response  ~|([%unsupported-rpc-response response] !!)
      %result
    :-  %o
    %-  molt
    ^-  (list [@t json])
    ~[['id' s+id.response] ['res' res.response]]
  ::
      %error
    :-  %o
    %-  molt
    ^-  (list [@t json])
    :~  ['id' s+id.response] 
        ['code' s+code.response]
        ['message' s+message.response]
    ==
  ==
::
++  validate-request
  |=  [body=(unit octs) parse-method=$-(@t term)]
  ^-  (unit request)
  ?~  body  ~
  ?~  jon=(de-json:html q.u.body)  ~
  ::  ignores non-object responses
  ::
  :: ?.  ?=([%o *] json)  ~|([%format-not-valid json] !!)
  ?.  ?=([%o *] u.jon)  ~
  %-  some
  %.  u.jon
  =,  dejs:format
  %-  ot
  :~  ['id' no]
      ['jsonrpc' so]
      ['method' (cu parse-method so)]
    ::
      :-  'params'
      |=  =json
      ^-  request-params
      ?:  =(%a -.json)
        [%list ((ar same) json)]
      ?.  =(%o -.json)
        !!
      [%object ~(tap by ((om same) json))]
  ==
--
