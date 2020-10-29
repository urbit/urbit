/-  *btc-provider, *btc
|%
++  address-to-cord
  |=  =address  ^-  cord
  ?:  ?=([%legacy *] address)
    (scot %uc +.address)
  +.address
++  to-hex
  |=  h=@t
  ^-  @ux
  ?:  =('' h)  0x0
  ::  Add leading 00
  ::
  =+  (lsh 3 2 h)
  ::  Group by 4-size block
  ::
  =+  (rsh 3 2 -)
  ::  Parse hex to atom
  ::
  `@ux`(rash - hex)
++  to-hash256
  |=  h=@t
  (hash256 [32 (to-hex h)])
++  http-request
  |=  url=@t
  ^-  request:http
  [%'GET' url ~ ~]
::
++  electrum-rpc
  |%
  ++  parse-response
    |=  res=response:rpc:jstd
    |^  ^-  response:electrum:rpc
    ~|  -.res
    ::  only deals with successful requests
    ::  ignores (%error, %fails and %batch)
    ::
    ?>  ?=(%result -.res)
    ?+  id.res  ~|([%unsupported-response id.res] !!)
        %get-address-utxos
      [id.res ((as:dejs:format utxo) res.res)]
    ==
    ++  utxo
      %-  ot:dejs:format
      :~  ['tx_pos' ni:dejs:format]
          ['tx_hash' (cu:dejs:format to-hash256 so:dejs:format)]
          [%height ni:dejs:format]
          [%value ni:dejs:format]
      ==
    --
  ::
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
        (cat 3 endpoint '/addresses/utxos/')
      (address-to-cord address.req)
    ==
  --
--
