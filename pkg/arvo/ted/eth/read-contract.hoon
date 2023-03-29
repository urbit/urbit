::  eth/read-contract: query ethereum for contract data
::
::    produces hex string result, for use with +decode-results:rpc:ethereum
::
/+  ethereum, eth-provider, strandio
=,  ethereum-types
=,  jael
::
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
;<  res=@t  bind:m
  %-  read-contract:eth-provider
  !<(proto-read-request:rpc:ethereum args)
(pure:m !>(res))
