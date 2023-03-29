::  aggregator/nonce: get next nonce
::
/-  rpc=json-rpc
/+  ethereum, eth-provider, strandio
::
|=  args=vase
=+  !<([endpoint=@t pk=@] args)
=/  m  (strand:strandio ,vase)
^-  form:m
::
=/  =address:ethereum
  (address-from-prv:key:ethereum pk)
;<  expected-nonce=@ud  bind:m
  (get-next-nonce:eth-provider address)
(pure:m !>(expected-nonce))
