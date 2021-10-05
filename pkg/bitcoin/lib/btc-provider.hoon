/-  bp=btc-provider, json-rpc
/+  bc=bitcoin, bcu=bitcoin-utils
~%  %btc-provider-lib  ..part  ~
|%
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
 |=  [=host-info:bp ract=action:rpc-types:bp]
  ^-  request:http
  %+  rpc-action-to-http
  api-url.host-info  ract
::
++  rpc
  ~/  %rpc
  =,  dejs:format
  |%
  ++  parse-result
    |=  res=response:json-rpc
    |^  ^-  result:rpc-types:bp
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
    ::
    ++  address-info
      %-  ot
      :~  [%address (cu from-cord:adr:bc so)]
          [%utxos (as utxo)]
          [%used bo]
          [%block ni]
      ==
    ::
    ++  utxo
      %-  ot
      :~  ['tx_pos' ni]
          ['tx_hash' (cu from-cord:hxb:bcu so)]
          [%height ni]
          [%value ni]
          [%recvd (cu from-epoch ni)]
      ==
    ::
    ++  tx-vals
      %-  ot
      :~  [%included bo]
          [%txid (cu from-cord:hxb:bcu so)]
          [%confs ni]
          [%recvd (cu from-epoch ni)]
          [%inputs (ar tx-val)]
          [%outputs (ar tx-val)]
      ==
    ::
    ++  tx-val
      %-  ot
      :~  [%txid (cu from-cord:hxb:bcu so)]
          [%pos ni]
          [%address (cu from-cord:adr:bc so)]
          [%value ni]
      ==
    ::
    ++  raw-tx
      %-  ot
      :~  [%txid (cu from-cord:hxb:bcu so)]
          [%rawtx (cu from-cord:hxb:bcu so)]
      ==
    ::
    ++  broadcast-tx
      %-  ot
      :~  [%txid (cu from-cord:hxb:bcu so)]
          [%broadcast bo]
          [%included bo]
      ==
    ::
    ++  block-info
      %-  ot
      :~  [%block ni]
          [%fee (mu ni)]
          [%blockhash (cu from-cord:hxb:bcu so)]
          [%blockfilter (cu from-cord:hxb:bcu so)]
      ==
    --
  --
::
++  rpc-action-to-http
  |=  [endpoint=@t ract=action:rpc-types:bp]
  |^  ^-  request:http
  ?-  -.ract
      %get-address-info
    %-  get-request
    %+  mk-url  '/addresses/info/'
    (to-cord:adr:bc address.ract)
    ::
      %get-tx-vals
    %-  get-request
    %+  mk-url  '/gettxvals/'
    (to-cord:hxb:bcu txid.ract)
    ::
      %get-raw-tx
    %-  get-request
    %+  mk-url  '/getrawtx/'
    (to-cord:hxb:bcu txid.ract)
    ::
      %broadcast-tx
    %-  get-request
    %+  mk-url  '/broadcasttx/'
    (to-cord:hxb:bcu rawtx.ract)
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
