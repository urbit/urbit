/-  sur=btc-provider, *btc
/+  blib=btc-node-json, elib=electrum-rpc, base64
^?
=<  [sur .]
=,  sur
|%
++  btc-rpc-auth-header
  |=  =host-info
  =*  user  rpc-user.bc.creds.host-info
  =*  pass  rpc-password.bc.creds.host-info
  :-  'Authorization'
  ;:  (cury cat 3)
      'Basic '
      %-  ~(en base64 | &)
      (as-octs:mimes:html :((cury cat 3) user ':' pass))
  ==
++  btc-gen-request
  |=  [=host-info req=request:bitcoin-core:rpc]
  ^-  request:http
  =*  endpoint  rpc-url.bc.creds.host-info
  =/  body=request:rpc:jstd
    (request-to-rpc:btc-rpc:blib req)
  =/  =header-list:http
    :~  ['Content-Type' 'application/json']
        (btc-rpc-auth-header host-info)
    ==
  :*  %'POST'
      endpoint
      header-list
      =,  html
      %-  some
      %-  as-octt:mimes
      (en-json (request-to-json:rpc:jstd body))
   ==
::
++  electrum-gen-request
  |=  [=host-info req=request:electrum:rpc]
  %+  request-to-http:electrum-rpc:elib
  rpc-url.ec.creds.host-info  req
::
++  gen-request
|=  [=host-info ract=action:rpc]
  ^-  request:http
  ?-  -.ract
      %erpc
    (electrum-gen-request host-info +.ract)
      %brpc
    (btc-gen-request host-info +.ract)
  ==
::
++  to-response
  |=  response:rpc
  ^-  response
  *response
  ::[%get-address-info *address-info]
--
