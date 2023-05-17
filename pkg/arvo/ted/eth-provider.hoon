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
;<  results=(list response:rpc)  bind:m  (request-batch-rpc-loose:provider-lib reqs)
(pure:m !>([tid.bowl results]))
