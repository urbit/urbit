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
=/  args2  !<([@t proto-read-request:rpc:ethereum] args)
;<  res=@t  bind:m
  %-  read-contract:eth-provider
  +.args2
(pure:m !>(res))
