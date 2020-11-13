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
++  get-request
  |=  url=@t
  ^-  request:http
  [%'GET' url ~ ~]
::
++  gen-request
 |=  [=host-info ract=action:rpc]
  ^-  request:http
  %+  rpc-action-to-http
  api-url.host-info  ract
::
++  to-response
  |=  result:rpc
  ^-  result
  *result
++  parse-response
  |=  res=response:rpc:jstd
  |^  ^-  response:rpc
  ~|  -.res
  ::  ignores RPC responses of %error, %fails and %batch
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
    %-  ot:dejs:format
    :~  [%utxos (as:dejs:format utxo)]
        [%used bo:dejs:format]
        [%blockcount ni:dejs:format]
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
++  rpc-action-to-http
  |=  [endpoint=@t ract=action:rpc]
  |^  ^-  request:http
  %-  get-request
  ?-  -.ract
      %get-address-info
    (mk-url '/addresses/info/' `address.ract)
    ::
      %get-block-count
    (mk-url '/getblockcount' ~)
  ==
  ++  mk-url
    |=  [base=@t uaddr=(unit address)]
    =/  addr=@t
      ?~  uaddr  ''  (address-to-cord u.uaddr)
    %^  cat  3
    (cat 3 endpoint base)  addr
  --
--

