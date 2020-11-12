/-  sur=btc-provider, *btc
/+  base64
^?
=<  [sur .]
=,  sur
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
::
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
::
++  to-hash256
  |=  h=@t
  (hash256 [32 (to-hex h)])
::
++  http-request
  |=  url=@t
  ^-  request:http
  [%'GET' url ~ ~]
::
++  gen-request
 |=  [=host-info ract=action:rpc]
  ^-  request:http
  %+  action-to-http:rpc
  api-url.creds.host-info
::
++  to-response
  |=  result:rpc
  ^-  result
  *result
++  rpc
  |%
  ++  parse-response
    |=  res=response:rpc:jstd
    |^  ^-  response:rpc
    ~|  -.res
    ::  only deals with successful requests
    ::  ignores (%error, %fails and %batch)
    ::
    ?>  ?=(%result -.res)
    ?+  id.res  ~|([%unsupported-response id.res] !!)
        %get-address-info
      [id.res (address-info res.res)]
      ::
        %get-block-count
      [id.res (ni:dejs:format res.res)]
    ==
    ++  address-info
    :: TODO: top element is blockcount+utxos
    ::  use: (as:dejs:format utxo)
    ++  utxo
      %-  ot:dejs:format
      :~  ['tx_pos' ni:dejs:format]
          ['tx_hash' (cu:dejs:format to-hash256 so:dejs:format)]
          [%height ni:dejs:format]
          [%value ni:dejs:format]
          [%blockcount ni:dejs:format]
      ==
    ++  history
      %-  ot:dejs:format
      :~  ['tx_hash' (cu:dejs:format to-hash256 so:dejs:format)]
          [%height ni:dejs:format]
          [%blockcount ni:dejs:format]
      ==
    --
  ::
  ++  action-to-http
    |=  [endpoint=@t ract=action:rpc]
    |^  ^-  request:http
    %-  http-request
    ?-  -.ract
        %get-address-utxos
      (mk-url '/addresses/utxos/' `address.ract)
      ::
        %get-address-history
      (mk-url '/addresses/history/' `address.ract)
        %get-block-count
      (mk-url '/blockcount' ~)
    ==
    ++  mk-url
      |=  [base=@t uaddr=(unit address)]
      =/  addr=@t
        ?~  uaddr  ''  (address-to-cord u.uaddr)
      %^  cat  3
        (cat 3 endpoint base)
      (address-to-cord addr)
    --
  --
--
