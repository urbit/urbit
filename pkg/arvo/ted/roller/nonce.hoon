::  aggregator/nonce: get next nonce
::
/-  rpc=json-rpc, ethdata=eth-provider
/+  ethereum, strandio, eth-provider
::
|=  args=vase
=+  !<([endpoint=@t pk=@] args)
=/  m  (strand:strandio ,vase)
^-  form:m
::
=/  =address:ethereum
  (address-from-prv:key:ethereum pk)
;<  res=ethout:ethdata  bind:m
  (eth-provider [%get-next-nonce address])
?>  ?=(%get-next-nonce -.res)
=/  expected-nonce  +.res
(pure:m !>(expected-nonce))
