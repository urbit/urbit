/-  spider, *aquarium
/+  ph-io, threadio
=,  thread=thread:spider
|%
++  vane-handler
  $_  ^|
  |_  bowl:mall
  ++  handle-unix-effect
    |~  [ship unix-effect]
    *(quip card:agent:mall _^|(..handle-unix-effect))
  ::
  ++  handle-arvo-response
    |~  [wire sign-arvo]
    *(quip card:agent:mall _^|(..handle-unix-effect))
  --
--
::
=;  core
  |=  handler=vane-handler
  ^-  imp:spider
  |=  [=bowl:mall vase]
  =/  m  (thread ,vase)
  ^-  form:m
  ;<  ~  bind:m  (watch-our:threadio /effects %aqua /effect)
  ;<  ~  bind:m
    %-  (main-loop:threadio ,_handler)
    :~  handle-unix-effect:core
        handle-arvo-response:core
        pure:(thread ,vane-handler)
    ==
  (pure:m *vase)
::
|%
++  handle-unix-effect
  |=  handler=vane-handler
  =/  m  (thread ,vane-handler)
  ^-  form:m
  ;<  [her=ship =unix-effect]  bind:m
    ((handle:threadio ,[ship unix-effect]) take-unix-effect:ph-io)
  ;<  =bowl:mall               bind:m  get-bowl:threadio
  =^  cards  handler
    (~(handle-unix-effect handler bowl) her unix-effect)
  ?~  cards
    (pure:m handler)
  ::  send in next event to avoid inverting subscription flow.  real
  ::  solution is probably for gall to drip subscription updates.
  ::
  ;<  ~                        bind:m  (sleep:threadio ~s0)
  ;<  ~                        bind:m  (send-raw-cards:threadio cards)
  (pure:m handler)
::
++  handle-arvo-response
  |=  handler=vane-handler
  =/  m  (thread ,vane-handler)
  ^-  form:m
  ;<  [=wire =sign-arvo]  bind:m
    ((handle:threadio ,[wire sign-arvo]) take-sign-arvo:threadio)
  ;<  =bowl:mall          bind:m  get-bowl:threadio
  =^  cards  handler
    (~(handle-arvo-response handler bowl) wire sign-arvo)
  ;<  ~                   bind:m  (send-raw-cards:threadio cards)
  (pure:m handler)
--
