/-  sur=btc-provider, json-rpc
/+  *btc
^?
=<  [sur .]
=,  sur
|%
++  gen-req-id
  |=  eny=@uvJ  ^-  req-id
  %+  scot  %ux
  (ripemd-160:ripemd:crypto [(met 3 eny) eny])
::
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
++  bytc-to-cord
  |=  =bytc  ^-  cord
  (en:base16:mimes:html bytc)
::
++  txid-to-cord
  |=  =txid  ^-  cord
  (en:base16:mimes:html txid)
::  +from-epoch: time since Jan 1, 1970 in seconds.
::
++  from-epoch
  |=  secs=@ud
  ^-  (unit @da)
  ?:  =(0 secs)  ~
  [~ (add ~1970.1.1 `@dr`(mul secs ~s1))]

::
++  to-hex
  |=  h=@t
  ^-  @ux
  ?:  =('' h)  0x0
  ::  Add leading 00
  ::
  =+  (lsh [3 2] h)
  ::  Group by 4-size block
  ::
  =+  (rsh [3 2] -)
  ::  Parse hex to atom
  ::
  `@ux`(rash - hex)
::
++  to-hash256
  |=  h=@t
  (hash256 [32 (to-hex h)])
::
++  to-bytc
  |=  h=@t
  ^-  bytc
  :-  (div (lent (trip h)) 2)
  (to-hex h)
::
++  get-request
  |=  url=@t
  ^-  request:http
  [%'GET' url ~ ~]
::
++  post-request
  |=  [url=@t body=json]
  ^-  request:http
  :*  %'POST'
      url
      ~[['Content-Type' 'application/json']]
      =,  html
      %-  some
        %-  as-octt:mimes
        (en-json body)
  ==
::
++  gen-request
 |=  [=host-info ract=action:rpc-types]
  ^-  request:http
  %+  rpc-action-to-http
  api-url.host-info  ract
::
++  action-to-json
  |=  ract=action:rpc-types
  |^  ^-  json
  =,  enjs:format
  ?+  -.ract  ~|("Unsupported action for POST" !!)
      %create-raw-tx
    %-  pairs
    :~  [%inputs [%a (turn inputs.ract input)]]
        [%outputs [%a (turn outputs.ract output)]]
    ==
  ==
  ::
  ++  input
    |=  [=txid pos=@ud]
    ^-  json
    =,  enjs:format
    %-  pairs
    :~  [%txid [%s (txid-to-cord txid)]]
        [%vout (numb pos)]
    ==
  ++  output
    |=  [=address value=sats]
    =,  enjs:format
    ^-  json
    %-  frond
    [(address-to-cord address) (numb value)]
  --
::
++  rpc
  |%
  ++  parse-response
    |=  res=response:json-rpc
    ^-  response:rpc-types
    ?+  -.res  [%unhandled-response ~]
        %error
      [%error (parse-error res)]
        %result
      [%result (parse-result res)]
    ==
  ::
  ++  parse-error
    |=  res=response:json-rpc
    ^-  error:rpc-types
    ?>  ?=(%error -.res)
    ?:  =('-25' code.res)
      %tx-inputs-missing-or-spent
    ?:  =('28' code.res)
      %blocks-not-ready
    %connection-error
  ::
  ++  parse-result
    |=  res=response:json-rpc
    |^  ^-  result:rpc-types
    ~|  -.res
    ?>  ?=(%result -.res)
    ?+  id.res  ~|([%unsupported-result id.res] !!)
        %get-address-info
      [id.res (address-info res.res)]
      ::
        %get-tx-vals
      [id.res (tx-vals res.res)]
      ::
        %get-raw-tx
      [id.res (raw-tx res.res)]
      ::
        %create-raw-tx
      [%get-raw-tx (raw-tx res.res)]
      ::
        %broadcast-tx
      [%broadcast-tx ((cu:dejs:format to-hash256 so:dejs:format) res.res)]
      ::
        %get-block-count
      [id.res (ni:dejs:format res.res)]
      ::
        %get-block-and-fee
      [id.res (block-and-fee res.res)]
    ==
    ++  address-info
      %-  ot:dejs:format
      :~  [%utxos (as:dejs:format utxo)]
          [%used bo:dejs:format]
          [%block ni:dejs:format]
      ==
    ++  utxo
      %-  ot:dejs:format
      :~  ['tx_pos' ni:dejs:format]
          ['tx_hash' (cu:dejs:format to-hash256 so:dejs:format)]
          [%height ni:dejs:format]
          [%value ni:dejs:format]
          [%recvd (cu:dejs:format from-epoch ni:dejs:format)]
      ==
    ++  tx-vals
      %-  ot:dejs:format
      :~  [%txid (cu:dejs:format to-hash256 so:dejs:format)]
          [%confs ni:dejs:format]
          [%recvd (cu:dejs:format from-epoch ni:dejs:format)]
          [%inputs (ar:dejs:format tx-val)]
          [%outputs (ar:dejs:format tx-val)]
      ==
    ++  tx-val
      %-  ot:dejs:format
      :~  [%txid (cu:dejs:format to-hash256 so:dejs:format)]
          [%pos ni:dejs:format]
          [%address (cu:dejs:format address-from-cord so:dejs:format)]
          [%value ni:dejs:format]
      ==
    ++  raw-tx
      %-  ot:dejs:format
      :~  [%txid (cu:dejs:format to-hash256 so:dejs:format)]
          [%rawtx (cu:dejs:format to-bytc so:dejs:format)]
      ==
    ++  block-and-fee
      %-  ot:dejs:format
      :~  [%block ni:dejs:format]
          [%fee ni:dejs:format]
      ==
    --
  --
::
++  rpc-action-to-http
  |=  [endpoint=@t ract=action:rpc-types]
  |^  ^-  request:http
  ?-  -.ract
      %get-address-info
    %-  get-request
    %+  mk-url  '/addresses/info/'
    (address-to-cord address.ract)
    ::
      %get-tx-vals
    %-  get-request
    %+  mk-url  '/gettxvals/'
    (txid-to-cord txid.ract)
    ::
      %get-raw-tx
    %-  get-request
    %+  mk-url  '/getrawtx/'
    (txid-to-cord txid.ract)
    ::
      %create-raw-tx
    %+  post-request
      (mk-url '/createrawtx' '')
    (action-to-json ract)
    ::
      %broadcast-tx
    %-  get-request
    %+  mk-url  '/broadcasttx/'
    (bytc-to-cord rawtx.ract)
    ::
      %get-block-count
    %-  get-request
    (mk-url '/getblockcount' '')
    ::
      %get-block-and-fee
    %-  get-request
    (mk-url '/getblockandfee' '')
  ==
  ++  mk-url
    |=  [base=@t params=@t]
    %^  cat  3
    (cat 3 endpoint base)  params
  --
::  RPC/HTTP Utilities
::
++  httr-to-rpc-response
  |=  hit=httr:eyre
  ^-  response:json-rpc
  ~|  hit
  =/  jon=json  (need (de-json:html q:(need r.hit)))
  ?.  =(%2 (div p.hit 100))
    (parse-rpc-error jon)
  =,  dejs-soft:format
  ^-  response:json-rpc
  =;  dere
    =+  res=((ar dere) jon)
    ?~  res  (need (dere jon))
    [%batch u.res]
  |=  jon=json
  ^-  (unit response:json-rpc)
  =/  res=[id=(unit @t) res=(unit json) err=(unit json)]
    %.  jon
    =,  dejs:format
    =-  (ou -)
    :~  ['id' (uf ~ (mu so))]
        ['result' (uf ~ (mu same))]
        ['error' (uf ~ (mu same))]
    ==
  ?:  ?=([^ * ~] res)
    `[%result [u.id.res ?~(res.res ~ u.res.res)]]
  ~|  jon
  `(parse-rpc-error jon)
::
++  get-rpc-response
  |=  response=client-response:iris
  ^-  response:json-rpc
  ?>  ?=(%finished -.response)
  %-  httr-to-rpc-response
    %+  to-httr:iris
      response-header.response
    full-file.response
::
++  parse-rpc-error
  |=  =json
  ^-  response:json-rpc
  :-  %error
  ?~  json  ['' '' '']
  %.  json
  =,  dejs:format
  =-  (ou -)
  :~  =-  ['id' (uf '' (cu - (mu so)))]
      |*(a=(unit) ?~(a '' u.a))
      :-  'error'
      =-  (uf ['' ''] -)
      =-  (cu |*(a=(unit) ?~(a ['' ''] u.a)) (mu (ou -)))
      :~  ['code' (uf '' no)]
          ['message' (uf '' so)]
  ==  ==
--
