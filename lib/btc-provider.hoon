/-  sur=btc-provider, json-rpc
/+  *bitcoin
^?
=<  [sur .]
=,  sur
|%
++  address-to-cord
  |=  =address  ^-  cord
  ?:  ?=([%base58 *] address)
    (scot %uc +.address)
  +.address
::
++  address-from-cord
  |=  addrc=@t
  ^-  address
  ?.  ?|  =("bc1" (scag 3 (trip addrc)))
          =("tb1" (scag 3 (trip addrc)))
      ==
    ~|("base58 addresses not yet supported" !!)
  [%bech32 addrc]
::
++  hexb-to-cord
  |=  =hexb  ^-  cord
  (en:base16:mimes:html hexb)
::  +from-epoch: time since Jan 1, 1970 in seconds.
::
++  from-epoch
  |=  secs=@ud
  ^-  (unit @da)
  ?:  =(0 secs)  ~
  [~ (add ~1970.1.1 `@dr`(mul secs ~s1))]
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
++  rpc
  =,  dejs:format
  |%
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
        %broadcast-tx
      [%broadcast-tx (broadcast-tx res.res)]
      ::
        %get-block-count
      [id.res (ni res.res)]
      ::
        %get-block-info
      [id.res (block-info res.res)]
    ==
    ++  address-info
      %-  ot
      :~  [%address (cu address-from-cord so)]
          [%utxos (as utxo)]
          [%used bo]
          [%block ni]
      ==
    ++  utxo
    %-  ot
      :~  ['tx_pos' ni]
          ['tx_hash' (cu to-hexb so)]
          [%height ni]
          [%value ni]
          [%recvd (cu from-epoch ni)]
      ==
    ++  tx-vals
      %-  ot
      :~  [%included bo]
          [%txid (cu to-hexb so)]
          [%confs ni]
          [%recvd (cu from-epoch ni)]
          [%inputs (ar tx-val)]
          [%outputs (ar tx-val)]
      ==
    ++  tx-val
      %-  ot
      :~  [%txid (cu to-hexb so)]
          [%pos ni]
          [%address (cu address-from-cord so)]
          [%value ni]
      ==
    ++  raw-tx
      %-  ot
      :~  [%txid (cu to-hexb so)]
          [%rawtx (cu to-hexb so)]
      ==
    ++  broadcast-tx
      %-  ot
      :~  [%txid (cu to-hexb so)]
          [%broadcast bo]
          [%included bo]
      ==
    ++  block-info
      %-  ot
      :~  [%block ni]
          [%fee (mu ni)]
          [%blockhash (cu to-hexb so)]
          [%blockfilter (cu to-hexb so)]
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
    (hexb-to-cord txid.ract)
    ::
      %get-raw-tx
    %-  get-request
    %+  mk-url  '/getrawtx/'
    (hexb-to-cord txid.ract)
    ::
      %broadcast-tx
    %-  get-request
    %+  mk-url  '/broadcasttx/'
    (hexb-to-cord rawtx.ract)
    ::
      %get-block-count
    %-  get-request
    (mk-url '/getblockcount' '')
    ::
      %get-block-info
    %-  get-request
    (mk-url '/getblockinfo' '')
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
