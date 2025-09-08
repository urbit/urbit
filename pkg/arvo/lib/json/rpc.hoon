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
  %-  as-octs:mimes:html
  (en:json:html (request-to-json req))
::
++  request-to-json
  |=  request
  ^-  json
  %-  pairs:enjs:format
  :~  jsonrpc+s+'2.0'
      id+s+id
      method+s+method
    ::
      :-  %params
      ^-  json
      ?-  -.params
        %list    [%a +.params]
        ::  FIXME: support either %map or %object (also in /sur/json/rpc)
        ::
        %map     [%o +.params]
        %object  [%o (~(gas by *(map @t json)) +.params)]
  ==  ==
::
++  response-to-json
  |=  =response
  ^-  json
  ::  TODO: consider all cases
  ::
  ?+  -.response  ~|([%unsupported-rpc-response response] !!)
    %batch  a+(turn bas.response response-to-json)
    ::
      %result
    %-  pairs:enjs:format
    ::  FIXME: return 'id' as string, number or NULL
    ::
    :~  ['jsonrpc' s+'2.0']
        ['id' s+id.response]
        ['result' res.response]
    ==
  ::
      %error
    =,  enjs:format
    %-  pairs
    :~  ['jsonrpc' s+'2.0']
        ['id' ?~(id.response ~ s+id.response)]
      ::
        :-  'error'
        %-  pairs
        :~  ['code' n+code.response]
            ['message' s+message.response]
    ==  ==
  ==
::
++  validate-request
  |=  body=(unit octs)
  ^-  (unit batch-request)
  ?~  body  ~
  ?~  jon=(de:json:html q.u.body)  ~
  =,  dejs-soft:format
  =;  reparser
    ?:  ?=([%a *] u.jon)
      (bind ((ar reparser) u.jon) (lead %a))
    (bind (reparser u.jon) (lead %o))
  %-  ot
  :~  ::  FIXME: parse 'id' as string, number or NULL
      ::
      ['id' so]
      ['jsonrpc' (su (jest '2.0'))]
      ['method' so]
    ::
      :-  'params'
      |=  =json
      ^-  (unit request-params)
      ?+  -.json  ~
        %a  `[%list ((ar:dejs:format same) json)]
        %o  `[%map ((om:dejs:format same) json)]
  ==  ==
::
++  error
  |_  id=@t
  ::  https://www.jsonrpc.org/specification#error_object
  ::
  ++  parse      [%error id '-32700' 'Failed to parse']
  ++  request    [%error id '-32600' 'Invalid Request']
  ++  method     [%error id '-32601' 'Method not found']
  ++  params     [%error id '-32602' 'Invalid params']
  ++  internal   [%error id '-32603' 'Internal error']
  ++  not-found  [%error id '-32000' 'Resource not found']
  ++  todo       [%error id '-32001' 'Method not implemented']
  --
--
