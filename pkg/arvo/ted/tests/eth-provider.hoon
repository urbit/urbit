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
=/  raw-tx  
0xf868.8086.0918.4e72.a000.8303.0000.94fa.3caa.bc8e.efec.2b5e.2895.e5af.bf79.379e.7268.a701.8082.0a95.a01b.454a.f567.4416.0fed.c195.0282.8de3.e3c6.a14e.b927.66ec.f494.bd22.ce11.9641.d3a0.44c2.d606.55b1.b052.bd00.d354.d1c2.b1c6.bc2c.52fa.b7ff.556f.c879.e5f7.0114.26de  

:: =/  request-rpc  [%request-rpc [~ 'u_tid'] [%eth-block-number ~]]
=/  request-rpc  [[~ 'u_tid'] [%eth-send-raw-transaction raw-tx]]
:: =/  request-batch-rpc-strict  ~[+.request-rpc]
=/  request-batch-rpc-loose  ~[+.request-rpc]
=/  read-contract  [[~ 'unitid'] address ['func' ~[[%address address]]]]
:: =/  batch-read-contract-strict  ~[+.read-contract]
=/  get-latest-block  %.n
=/  get-block-by-number  0
=/  get-tx-by-hash  0x123  :: bad hash
=/  get-logs-by-hash  [0x123 [address ~] ~]
=/  get-logs-by-range  [address ~ ~ 0 1]
=/  get-next-nonce  address
=/  get-balance  address

:: ;<  res=json  bind:m  (request-rpc:eth-provider request-rpc)
:: ;<  res=(list [id=@t =json])  bind:m  (request-batch-rpc-strict:eth-provider request-batch-rpc-strict)
:: ;<  res=(list response:json-rpc)  bind:m  (request-batch-rpc-loose:eth-provider request-batch-rpc-loose)
:: ;<  res=@t  bind:m  (read-contract:eth-provider read-contract)
:: ;<  res=(list [@t res=@t])  bind:m  (batch-read-contract-strict:eth-provider batch-read-contract-strict)
:: ;<  res=block  bind:m  (get-latest-block:eth-provider get-latest-block)
;<  res=block  bind:m  (get-block-by-number:eth-provider get-block-by-number)
:: ;<  res=transaction-result:rpc:ethereum  bind:m  (get-tx-by-hash:eth-provider get-tx-by-hash)
:: ;<  res=(list event-log:rpc:ethereum)  bind:m  (get-logs-by-hash:eth-provider get-logs-by-hash)
:: ;<  res=(list event-log:rpc:ethereum)  bind:m  (get-logs-by-range:eth-provider get-logs-by-range)
:: ;<  res=@ud  bind:m  (get-next-nonce:eth-provider get-next-nonce)
:: ;<  res=@ud  bind:m  (get-balance:eth-provider get-balance)


:: =/  res2  !>(res)
:: =/  res2  +<.res



(pure:m !>(res))
