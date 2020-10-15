
/-  *btc-bridge, *btc
/+  lib=btc-node-json
|%
++  address-to-cord
  |=  =address  ^-  cord
  ?:  ?=([%legacy *] address)
    (scot %uc +.address)
  +.address
++  http-request
  |=  url=@t
  ^-  request:http
  [%'GET' url ~ ~]
::
++  electrum-rpc
  |%
  ++  request-to-http
    |=  [endpoint=@t req=request:electrum:rpc]
    ^-  request:http
    ?-  -.req
        %get-address-balance
      %-  http-request
      %^  cat  3
        (cat 3 endpoint '/addresses/balance/')
      (address-to-cord address.req)
    ==
  --
--
