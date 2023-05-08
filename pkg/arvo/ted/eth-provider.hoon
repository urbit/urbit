/-  spider, eth-provider, json-rpc
/+  strandio, ethio, ethereum, provider-lib=eth-provider
=,  strand=strand:spider
=,  dejs-soft:format
=,  strand-fail=strand-fail:libstrand:spider
=,  jael
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  eth-input  !<(ethin:eth-provider arg)
;<  =bowl:spider  bind:m  get-bowl:strandio
?>  =(src.bowl our.bowl)
;<    provider-mode=provider-mode:eth-provider
    bind:m
  (scry:strandio provider-mode:eth-provider /gx/eth-provider/get-provider-mode/noun)
?>  ?=(%provider -.provider-mode)
=/  provider  `provider:eth-provider`+.provider-mode
;<  ethout=ethout:eth-provider  bind:m  (call-ethio:provider-lib eth-input %provider url.provider)
(pure:m !>([tid.bowl ethout]))
