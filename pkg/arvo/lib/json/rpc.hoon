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
--
