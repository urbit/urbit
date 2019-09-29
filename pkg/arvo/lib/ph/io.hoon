/-  *aquarium
/+  libthread=thread, *threadio, util=ph-util
=,  thread=thread:libthread
|%
++  send-events
  |=  events=(list aqua-event)
  =/  m  (thread ,~)
  ^-  form:m
  (poke-our %aqua %aqua-events !>(events))
::
++  take-unix-effect
  =/  m  (thread ,[ship unix-effect])
  ^-  form:m
  ;<  =cage  bind:m  (take-subscription-update /effects)
  ?>  ?=(%aqua-effect p.cage)
  (pure:m !<([aqua-effect] q.cage))
::
++  start-simple
  =/  m  (thread ,~)
  ^-  form:m
  =/  vane-imps=(list term)
    ~[%aqua-ames %aqua-behn %aqua-dill %aqua-eyre]
  ;<  ~  bind:m  (start-imps vane-imps)
  ;<  ~  bind:m  start-watching
  (pure:m ~)
::
++  end-simple
  =/  m  (thread ,~)
  ^-  form:m
  =/  vane-imps=(list term)
    ~[%aqua-ames %aqua-behn %aqua-dill %aqua-eyre]
  ;<  ~  bind:m  (stop-imps vane-imps)
  ;<  ~  bind:m  stop-watching
  (pure:m ~)
::
++  start-imps
  |=  imps=(list term)
  =/  m  (thread ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  |-  ^-  form:m
  =*  loop  $
  ?~  imps
    (pure:m ~)
  ;<  now=@da  bind:m  get-time
  =/  imp-started
    .^(? %mx /(scot %p our)/spider/(scot %da now)/started/[i.imps]/noun)
  ?:  imp-started
    loop(imps t.imps)
  =/  poke-vase  !>([i.imps i.imps])
  ;<  ~  bind:m  (poke-our %spider %spider-start poke-vase)
  loop(imps t.imps)
::
++  stop-imps
  |=  imps=(list term)
  =/  m  (thread ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  |-  ^-  form:m
  =*  loop  $
  ?~  imps
    (pure:m ~)
  ;<  now=@da  bind:m  get-time
  =/  imp-started
    .^(? %mx /(scot %p our)/spider/(scot %da now)/started/[i.imps]/noun)
  ?.  imp-started
    loop(imps t.imps)
  =/  poke-vase  !>(i.imps)
  ;<  ~  bind:m  (poke-our %spider %spider-stop poke-vase)
  loop(imps t.imps)
::
++  start-watching
  =/  m  (thread ,~)
  ^-  form:m
  =*  loop  $
  (subscribe-our /effects %aqua /effect)
::
++  stop-watching
  =/  m  (thread ,~)
  ^-  form:m
  (unsubscribe-our /effects %aqua)
::
++  raw-ship
  |=  [=ship keys=(unit dawn-event:able:jael)]
  =/  m  (thread ,~)
  ^-  form:m
  ;<  ~  bind:m  (send-events (init:util ship keys))
  (check-ship-booted ship)
::
++  check-ship-booted
  |=  =ship
  =/  m  (thread ,~)
  ^-  form:m
  =*  loop  $
  ;<  [her=^ship =unix-effect]  bind:m  take-unix-effect
  =/  f  |=(=tape (is-dojo-output:util ship her unix-effect tape))
  ::  This is a pretty bad heuristic, but in general galaxies will
  ::  hit the first of these cases, and other ships will hit the
  ::  second.
  ::
  ?:  ?|  (f "clay: committed initial filesystem (all)")
          (f "is your neighbor")
      ==
    (pure:m ~)
  loop
::
++  dojo
  |=  [=ship =tape]
  =/  m  (thread ,~)
  ^-  form:m
  (send-events (dojo:util ship tape))
::
++  wait-for-output
  |=  [=ship =tape]
  =/  m  (thread ,~)
  ^-  form:m
  =*  loop  $
  ;<  [her=^ship =unix-effect]  bind:m  take-unix-effect
  ?:  (is-dojo-output:util ship her unix-effect tape)
    (pure:m ~)
  loop
--
