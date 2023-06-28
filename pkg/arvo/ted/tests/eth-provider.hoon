/-  spider, ethdata=eth-provider, json-rpc
/+  strandio, ethereum, eth-provider
=,  strand=strand:spider
=,  dejs-soft:format
=,  strand-fail=strand-fail:libstrand:spider
=,  jael

^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  =bowl:spider  bind:m  get-bowl:strandio
?>  =(src.bowl our.bowl)

=/  address  0x829.ced8.5780.6e0f.d28d.f242.6d46.37bb.bad1.70f3
=/  logs-address  0xb59f.67a8.bff5.d8cd.03f6.ac17.265c.550e.d8f3.3907
=/  topics  
  :~
      0xddf2.52ad.1be2.c89b.69c2.b068.fc37.8daa.952b.a7f1.63c4.a116.28f5.5a4d.f523.b3ef
      :: 0x0000.0000.0000.0000.0000.0000.00b4.6c25.26e2.2748.2e2e.bb8f.4c69.e467.4d26.2e75
      :: 0x0000.0000.0000.0000.0000.0000.54a2.d42a.40f5.1259.dedd.1978.f6c1.18a0.f0ef.f078
  ==
=/  raw-tx  
0xf868.8086.0918.4e72.a000.8303.0000.94fa.3caa.bc8e.efec.2b5e.2895.e5af.bf79.379e.7268.a701.8082.0a95.a01b.454a.f567.4416.0fed.c195.0282.8de3.e3c6.a14e.b927.66ec.f494.bd22.ce11.9641.d3a0.44c2.d606.55b1.b052.bd00.d354.d1c2.b1c6.bc2c.52fa.b7ff.556f.c879.e5f7.0114.26de  

:: =/  request-rpc  [%request-rpc [~ 'u_tid'] [%eth-block-number ~]]
=/  request-rpc  [[~ 'u_tid'] [%eth-send-raw-transaction raw-tx]]
:: =/  request-batch-rpc-strict  ~[+.request-rpc]
=/  request-batch-rpc-loose  ~[+.request-rpc]
=/  read-contract  [[~ 'unitid'] address ['func' ~[[%address address]]]]
=/  batch-read-contract-strict  ~[+.read-contract]
=/  get-latest-block  %.n
=/  get-block-by-number  0
=/  get-tx-by-hash  0x123  :: bad hash
=/  get-logs-by-hash  [0x123 [address ~] ~]
=/  get-logs-by-range  [~[logs-address] topics 10 100]
=/  get-next-nonce  address
=/  get-balance  address
=/  proto-read-data  [[~ 'proto-read-req'] 0x123 ['func' ~[[%bool %.y]]]]

=/  request-batch-rpc-loose  
  :~
    :: :-  `'block by number'
    :: [%eth-get-block-by-number 0 |]
    :: :-  `'block by nber'
    :: [%eth-get-block-by-number 100.000.000.000.000 |]
    :: :-  `'nonce'
    :: [%eth-get-transaction-count address [%label %latest]]
    :: :-  `'nonfdjce'
    :: [%eth-get-transaction-count address [%label %latest]]
    :: :*  `'logs by hash'
    ::     %eth-get-logs-by-hash
    ::     0x123
    ::     ~[0x123]
    ::     topics
    :: ==
    :: :*  `'logs by hash2'
    ::     %eth-get-logs-by-hash
    ::     0x123
    ::     ~[0x123]
    ::     topics
    :: ==
    :-  `'transaction receipt'
    :: [%eth-get-transaction-receipt 0x123]
    [%eth-get-transaction-receipt 0xe11a.9017.5036.ba61.01e6.999d.1330.5fe0.75d9.f910.fa9f.8946.2a80.939f.58a6.728b]
  ==


:: Maybe TESTED?
:: ;<  res=(list event-log:rpc:ethereum)  bind:m  (get-logs-by-range:eth-provider get-logs-by-range)
:: ;<  res=(list event-log:rpc:ethereum)  bind:m  (get-logs-by-hash:eth-provider get-logs-by-hash)
:: ;<  res=id-response:ethdata  bind:m  (request-rpc:eth-provider request-rpc)

::  error handling done
::  TESTED
:: ;<  res=(list [@t res=@t])  bind:m  (batch-read-contract-strict:eth-provider ~[proto-read-data])
;<  res=block:rpc:ethereum  bind:m  (get-block-by-number:eth-provider get-block-by-number)
:: ;<  res=@ud  bind:m  (get-balance:eth-provider get-balance)
:: ;<  res=@ud  bind:m  (get-next-nonce:eth-provider get-next-nonce)
:: ;<  res=block:rpc:ethereum  bind:m  (get-latest-block:eth-provider get-latest-block)
:: ;<  res=(list id-response:ethdata)  bind:m  (request-batch-rpc-strict:eth-provider request-batch-rpc-loose)
:: ;<  res=ethout:ethdata  bind:m  (request-batch-rpc-loose:eth-provider request-batch-rpc-loose)
:: ;<  res=(unit transaction-result:rpc:ethereum)  bind:m  (get-tx-by-hash:eth-provider get-tx-by-hash)
:: ;<  res=@t  bind:m  (read-contract:eth-provider proto-read-data)


:: =/  res2  !>(res)
:: =/  res2  +<.res


~&  [%eth-provider-test-result res]
(pure:m !>(~))
