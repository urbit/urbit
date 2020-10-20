
/-  *btc-provider, *btc
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
  ++  parse-response
    |=  rpc-resp=@t
    %.y
  ++  request-to-http
    |=  [endpoint=@t req=request:electrum:rpc]
    ^-  request:http
    %-  http-request
    ?-  -.req
        %get-address-balance
      %^  cat  3
        (cat 3 endpoint '/addresses/balance/')
      (address-to-cord address.req)
      ::
        %get-address-utxos
      %^  cat  3
        (cat 3 endpoint '/addresses/listunspent/')
      (address-to-cord address.req)
    ==
  --
--
