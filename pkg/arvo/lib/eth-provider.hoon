/-  spider, eth-provider, json-rpc
/+  strandio, ethio, ethereum
=,  strand=strand:spider
=,  dejs-soft:format
=,  strand-fail=strand-fail:libstrand:spider
=,  jael
=<
|=  eth-input=ethin:eth-provider
=/  m  (strand ,ethout:eth-provider)
^-  form:m
;<  =bowl:spider  bind:m  get-bowl:strandio
?>  =(src.bowl our.bowl)
;<    state=state:eth-provider
    bind:m
  (scry:strandio state:eth-provider /gx/eth-provider/get-state/noun)
?-  active.state
  %local
(call-ethio eth-input active.state url.local.state)
  %provider
(call-ethio eth-input active.state url.provider.state)
  %client
;<  ~  bind:m  
  (watch:strandio /updates [~nut %eth-provider] [%updates tid.bowl ~])
;<  ~  bind:m  
  %+  poke:strandio 
    [provider.client.state %eth-provider] 
  [%provider-action !>([%provide tid.bowl eth-input])]
;<  =cage  bind:m  (take-fact:strandio /updates)
(pure:m !<(ethout:eth-provider q.cage))
==
|%
++  call-ethio
  |=  [arg=ethin:eth-provider active=active:eth-provider url=@ta]
  =/  m  (strand ,ethout:eth-provider)
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  ?-  -.arg
    %request-rpc
  ;<  out=json  bind:m  (request-rpc:ethio url +.arg)
  (pure:m [%request-rpc out])
    %request-batch-rpc-strict
  ;<  out=(list [id=@t =json])  bind:m  
    (request-batch-rpc-strict:ethio url +.arg)
  (pure:m [%request-batch-rpc-strict out])
    %request-batch-rpc-loose
  ;<  out=(list response:json-rpc)  bind:m  
    (request-batch-rpc-loose:ethio url +.arg)
  (pure:m [%request-batch-rpc-loose out])
    %read-contract
  ;<  out=@t  bind:m  (read-contract:ethio url +.arg)
  (pure:m [%read-contract out])
    %batch-read-contract-strict
  ;<  out=(list [@t res=@t])  bind:m  
    (batch-read-contract-strict:ethio url +.arg)
  (pure:m [%batch-read-contract-strict out])
    %get-latest-block
  ;<  out=block  bind:m  (get-latest-block:ethio url)
  (pure:m [%get-latest-block out])
    %get-block-by-number
  ;<  out=block  bind:m  (get-block-by-number:ethio url +.arg)
  (pure:m [%get-block-by-number out])
    %get-tx-by-hash
  ;<  out=transaction-result:rpc:ethereum  bind:m  
    (get-tx-by-hash:ethio url +.arg)
  (pure:m [%get-tx-by-hash out])
    %get-logs-by-hash
  ;<  out=(list event-log:rpc:ethereum)  bind:m  
    (get-logs-by-hash:ethio url +.arg)
  (pure:m [%get-logs-by-hash out])
    %get-logs-by-range
  ;<  out=(list event-log:rpc:ethereum)  bind:m  
    (get-logs-by-range:ethio url +.arg)
  (pure:m [%get-logs-by-range out])
    %get-next-nonce
  ;<  out=@ud  bind:m  (get-next-nonce:ethio url +.arg)
  (pure:m [%get-next-nonce out])
    %get-balance
  ;<  out=@ud  bind:m  (get-balance:ethio url +.arg)
  (pure:m [%get-balance out])
  ==
--
