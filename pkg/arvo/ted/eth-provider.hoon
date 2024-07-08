/-  spider, eth-provider, rpc=json-rpc
/+  strandio, ethio, ethereum, provider-lib=eth-provider
=,  strand=strand:spider
=,  dejs-soft:format
=,  strand-fail=strand-fail:libstrand:spider
=,  jael
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  reqs  !<((list [id=(unit @t) req=request:rpc:ethereum]) arg)
;<  =bowl:spider  bind:m  get-bowl:strandio
?>  =(src.bowl our.bowl)
;<    provider-mode=provider-mode:eth-provider
    bind:m
  (scry:strandio provider-mode:eth-provider /gx/eth-provider/get-provider-mode/noun)
?>  ?=(%provider -.provider-mode)
=/  provider  `provider:eth-provider`+.provider-mode
:: (list [id...] would be the type
;<  responses=ethout:eth-provider  bind:m  (request-batch-rpc-loose:provider-lib reqs)
:: =/  responses  ~[[%eth-transaction-hash 0x123]]
(pure:m !>([tid.bowl responses]))
