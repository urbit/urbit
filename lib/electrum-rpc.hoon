/-  *btc-provider, *btc
|%
++  address-to-cord
  |=  =address  ^-  cord
  ?:  ?=([%legacy *] address)
    (scot %uc +.address)
  +.address
::
++  address-from-cord
  |=  addrc=@t  ^-  address
  ?.  ?|  =("bc1" (scag 3 (trip addrc)))
          =("tb1" (scag 3 (trip addrc)))
      ==
    ~|("legacy addresses not yet supported" !!)
  [%bech32 addrc]
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
      ::
        %get-address-history
      [id.res ((as:dejs:format history) res.res)]
    ==
    ++  utxo
      %-  ot:dejs:format
      :~  ['tx_pos' ni:dejs:format]
          ['tx_hash' (cu:dejs:format to-hash256 so:dejs:format)]
          [%height ni:dejs:format]
          [%value ni:dejs:format]
      ==
    ++  history
      %-  ot:dejs:format
      :~  ['tx_hash' (cu:dejs:format to-hash256 so:dejs:format)]
          [%height ni:dejs:format]
      ==
    --
  ::
  ++  request-to-http
    |=  [endpoint=@t req=request:electrum:rpc]
    |^  ^-  request:http
    %-  http-request
    ?-  method.req
        %get-address-utxos
      (mk-url '/addresses/utxos/' address.req)
      ::
        %get-address-history
      (mk-url '/addresses/history/' address.req)
    ==
    ++  mk-url
      |=  [base=@t addr=address]
      %^  cat  3
        (cat 3 endpoint base)
      (address-to-cord addr)
    --
  --
--
