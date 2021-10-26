::  Note: these are for BTC testnet
::
/-  spider, rpc=json-rpc
/+  strandio, bc=bitcoin, bcu=bitcoin-utils
=,  strand=strand:spider
=>
|%
++  url1  "http://localhost:50002"
++  addr  ^-(address:bc [%bech32 'bc1q39wus23jwe7m2j7xmrfr2svhrtejmsn262x3j2'])
++  btc-req
  ^-  request:http
  =,  enjs:format
  :*  method=%'POST'
      url=`@ta`(crip (weld url1 "/btc-rpc"))
      header-list=['Content-Type'^'application/json' ~]
      ^=  body
      %-  some
      %-  as-octt:mimes:html
      %-  en-json:html
      %-  pairs
      :~  jsonrpc+s+'2.0'
          id+s+'block-info'
          method+s+'getblockchaininfo'
      ==
  ==
++  electrs-req
  ^-  request:http
  =,  enjs:format
  :*  method=%'POST'
      url=`@ta`(crip (weld url1 "/electrs-rpc"))
      header-list=['Content-Type'^'application/json' ~]
      ^=  body
      %-  some
      %-  as-octt:mimes:html
      %-  en-json:html
      %-  pairs
      :~  jsonrpc+s+'2.0'
          id+s+'list-unspent'
          method+s+'blockchain.scripthash.listunspent'
          params+a+~[[%s '34aae877286aa09828803af27ce2315e72c4888efdf74d7d067c975b7c558789']]
      ==
  ==
::
::  convert address to Electrs ScriptHash that it uses to index
::   big-endian sha256 of the output script
::
++  electrs-script-hash
  |=  a=address:bc
  ^-  hexb:bc
  %-  flip:byt:bcu
  %-  sha256:bcu
  (to-script-pubkey:adr:bc a)
::
++  parse-json-rpc
  |=  =json
  ^-  (unit response:rpc)
  =/  res=(unit [@t ^json])
    %.  json
    =,  dejs-soft:format
    (ot id+so result+some ~)
  ?^  res  `[%result u.res]
  ~|  parse-one-response=json
  :+  ~  %error  %-  need
  %.  json
  =,  dejs-soft:format
  (ot id+so error+(ot code+no message+so ~) ~)
::
++  parse-response
  |=  =client-response:iris
  =/  m  (strand:strandio ,(unit response:rpc))
  ^-  form:m
  ?>  ?=(%finished -.client-response)
  ?~  full-file.client-response
  (pure:m ~)
  =/  body=@t  q.data.u.full-file.client-response
  =/  jon=(unit json)  (de-json:html body)
  ?~  jon  (pure:m ~)
  (pure:m (parse-json-rpc u.jon))
::
++  attempt-request
  |=  =request:http
  =/  m  (strand:strandio ,~)
  ^-  form:m
  (send-request:strandio request)
--
^-  thread:spider
|=  arg=vase
::  =+  !<([~ a=@ud] arg)
=/  m  (strand ,vase)
^-  form:m
;<  ~  bind:m  (attempt-request electrs-req)
;<  rep=client-response:iris  bind:m
  take-client-response:strandio
;<  rpc-resp=(unit response:rpc)  bind:m  (parse-response rep)
(pure:m !>(rpc-resp))
