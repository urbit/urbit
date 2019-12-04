/-  spider, *aquarium
/+  ph-io, strandio
=,  strand=strand:spider
|%
++  vane-handler
  $_  ^|
  |_  bowl:spider
  ++  handle-unix-effect
    |~  [ship unix-effect]
    *(quip card:agent:gall _^|(..handle-unix-effect))
  ::
  ++  handle-arvo-response
    |~  [wire sign-arvo]
    *(quip card:agent:gall _^|(..handle-unix-effect))
  --
--
::
=;  core
  |=  [effect-filter=(list term) handler=vane-handler]
  ^-  thread:spider
  |=  vase
  =/  m  (strand ,vase)
  ^-  form:m
  =*  loop  $
  ?^  effect-filter
    =/  =path  /effect/[i.effect-filter]
    ;<  ~  bind:m  (watch-our:strandio path %aqua path)
    loop(effect-filter t.effect-filter)
  ;<  ~  bind:m
    %-  (main-loop:strandio ,_handler)
    :~  handle-unix-effect:core
        handle-arvo-response:core
        pure:(strand ,vane-handler)
    ==
  (pure:m *vase)
::
|%
++  handle-unix-effect
  |=  handler=vane-handler
  =/  m  (strand ,vane-handler)
  ^-  form:m
  ;<  [her=ship =unix-effect]  bind:m
    ((handle:strandio ,[ship unix-effect]) take-unix-effect:ph-io)
  ;<  =bowl:spider             bind:m  get-bowl:strandio
  =^  cards  handler
    (~(handle-unix-effect handler bowl) her unix-effect)
  ?~  cards
    (pure:m handler)
  ::  send in next event to avoid inverting subscription flow.  real
  ::  solution is probably for gall to drip subscription updates.
  ::
  ;<  ~                        bind:m  (sleep:strandio ~s0)
  ;<  ~                        bind:m  (send-raw-cards:strandio cards)
  (pure:m handler)
::
++  handle-arvo-response
  |=  handler=vane-handler
  =/  m  (strand ,vane-handler)
  ^-  form:m
  ;<  [=wire =sign-arvo]  bind:m
    ((handle:strandio ,[wire sign-arvo]) take-sign-arvo:strandio)
  ;<  =bowl:spider        bind:m  get-bowl:strandio
  =^  cards  handler
    (~(handle-arvo-response handler bowl) wire sign-arvo)
  ;<  ~                   bind:m  (send-raw-cards:strandio cards)
  (pure:m handler)
--
