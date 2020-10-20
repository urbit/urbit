/-  sur=btc-provider, *btc
/+  blib=btc-node-json, elib=electrum-rpc, base64
^?
=<  [sur .]
=,  sur
|%
++  btc-rpc-auth-header
  |=  =status
  =*  user  rpc-user.bc.creds.status
  =*  pass  rpc-password.bc.creds.status
  :-  'Authorization'
  ;:  (cury cat 3)
      'Basic '
      %-  ~(en base64 | &)
      (as-octs:mimes:html :((cury cat 3) user ':' pass))
  ==
++  btc-gen-request
  |=  [=status req=request:bitcoin-core:rpc]
  ^-  request:http
  =*  endpoint  rpc-url.bc.creds.status
  =/  body=request:rpc:jstd
    (request-to-rpc:btc-rpc:blib req)
  =/  =header-list:http
    :~  ['Content-Type' 'application/json']
        (btc-rpc-auth-header status)
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
  |=  [=status req=request:electrum:rpc]
  %+  request-to-http:electrum-rpc:elib
  rpc-url.ec.creds.status  req
::
++  gen-request
  |=  [=status ract=action:rpc]
  ^-  request:http
  ?-  -.ract
      %erpc
    (electrum-gen-request status +.ract)
      %brpc
    (btc-gen-request status +.ract)
  ==
::
++  to-response
  |=  response:rpc
  ^-  response
  *response
  ::[%get-address-info *address-info]
--
