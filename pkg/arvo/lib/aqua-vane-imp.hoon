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
  |=  =bowl:mall
  =/  m  (thread ,~)
  ^-  form:m
  ;<  ~  bind:m  (subscribe-our:threadio /effects %aqua /effect)
  %-  (main-loop:threadio ,_handler)
  :~  handle-unix-effect:core
      handle-arvo-response:core
      pure:(thread ,vane-handler)
  ==
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
