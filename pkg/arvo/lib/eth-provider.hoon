/-  spider, eth-provider, json-rpc
/+  strandio, ethio, ethereum
=,  ethereum-types
=,  jael
=,  strand=strand:spider
|%
++  request-rpc
  |=  [id=(unit @t) req=request:rpc:ethereum]
  =/  m  (strand:strandio ,json)
  ;<  out=ethout:eth-provider  bind:m
  (select-provider-mode [%request-rpc [id req]])
  ?>  ?=(%request-rpc -.out)
  =+  `json`+.out
  (pure:m -)
++  request-batch-rpc-strict
  |=  reqs=(list [id=(unit @t) req=request:rpc:ethereum])
  =/  m  (strand:strandio ,(list [id=@t =json]))
  ;<  out=ethout:eth-provider  bind:m
  (select-provider-mode [%request-batch-rpc-strict reqs])
  ?>  ?=(%request-batch-rpc-strict -.out)
  =+  `(list [id=@t =json])`+.out
  (pure:m -)
++  request-batch-rpc-loose
  |=  reqs=(list [id=(unit @t) req=request:rpc:ethereum])
  =/  m  (strand:strandio ,(list response:json-rpc))
  ;<  out=ethout:eth-provider  bind:m
  (select-provider-mode [%request-batch-rpc-loose reqs])
  ?>  ?=(%request-batch-rpc-loose -.out)
  =+  `(list response:json-rpc)`+.out
  (pure:m -)
++  read-contract
  |=  req=proto-read-request:rpc:ethereum
  =/  m  (strand:strandio ,@t)
  ;<  out=ethout:eth-provider  bind:m
  (select-provider-mode [%read-contract req])
  ?>  ?=(%read-contract -.out)
  =+  `@t`+.out
  (pure:m -)
++  batch-read-contract-strict
  |=  reqs=(list proto-read-request:rpc:ethereum)
  =/  m  (strand:strandio ,(list [@t res=@t]))
  ;<  out=ethout:eth-provider  bind:m
  (select-provider-mode [%batch-read-contract-strict reqs])
  ?>  ?=(%batch-read-contract-strict -.out)
  =+  `(list [@t res=@t])`+.out
  (pure:m -)
++  get-latest-block
  |=  bool=?
  =/  m  (strand ,block)
  ;<  out=ethout:eth-provider  bind:m
  (select-provider-mode [%get-latest-block bool])
  ?>  ?=(%get-latest-block -.out)
  =+  `block`+.out
  (pure:m -)
  :: (select-provider-mode [%get-latest-block bool])
++  get-block-by-number
  =,  jael
  |=  =number:block
  =/  m  (strand:strandio ,block)
  ;<  out=ethout:eth-provider  bind:m
  (select-provider-mode [%get-block-by-number number])
  ?>  ?=(%get-block-by-number -.out)
  =+  `block`+.out
  (pure:m -)
++  get-tx-by-hash
  |=  tx-hash=@ux
  =/  m  (strand:strandio ,transaction-result:rpc:ethereum)
  ;<  out=ethout:eth-provider  bind:m
  (select-provider-mode [%get-tx-by-hash tx-hash])
  ?>  ?=(%get-tx-by-hash -.out)
  =+  `transaction-result:rpc:ethereum`+.out
  (pure:m -)
++  get-logs-by-hash
  |=  [=hash:block contracts=(list address) =topics:eth-provider]
  =/  m  (strand:strandio ,(list event-log:rpc:ethereum))
  ;<  out=ethout:eth-provider  bind:m
  (select-provider-mode [%get-logs-by-hash hash contracts topics])
  ?>  ?=(%get-logs-by-hash -.out)
  =+  `(list event-log:rpc:ethereum)`+.out
  (pure:m -)
++  get-logs-by-range
  |=  $:  
          contracts=(list address)
          =topics:eth-provider
          from=number:block
          to=number:block
      ==
  =/  m  (strand:strandio ,(list event-log:rpc:ethereum))
  ;<  out=ethout:eth-provider  bind:m
  (select-provider-mode [%get-logs-by-range contracts topics from to])
  ?>  ?=(%get-logs-by-range -.out)
  =+  `(list event-log:rpc:ethereum)`+.out
  (pure:m -)
++  get-next-nonce
  |=  =address
  =/  m  (strand:strandio ,@ud)
  ;<  out=ethout:eth-provider  bind:m
  (select-provider-mode [%get-next-nonce address])
  ?>  ?=(%get-next-nonce -.out)
  =+  `@ud`+.out
  (pure:m -)
++  get-balance
  |=  =address
  =/  m  (strand:strandio ,@ud)
  ;<  out=ethout:eth-provider  bind:m
  (select-provider-mode [%get-balance address])
  ?>  ?=(%get-balance -.out)
  =+  `@ud`+.out
  (pure:m -)
::
++  select-provider-mode
  =,  strand=strand:spider
  =,  dejs-soft:format
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
  ::  TODO subscribe to our thread id
  ;<  ~  bind:m  
    (watch:strandio [%updates tid.bowl ~] [provider.client.state %eth-provider] [%updates tid.bowl ~])
  ;<  ~  bind:m
    %+  poke:strandio
      [provider.client.state %eth-provider] 
    [%provider-action !>([%provide tid.bowl eth-input])]
  ;<  =cage  bind:m  (take-fact:strandio [%updates tid.bowl ~])
  (pure:m !<(ethout:eth-provider q.cage))
  ==
++  call-ethio
  |=  [arg=ethin:eth-provider active=active:eth-provider url=@ta]
  =,  strand=strand:spider
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
