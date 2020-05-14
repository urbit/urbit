/-  spider
/+  *ph-io
=>
|%
++  wait-for-agent-start
  |=  [=ship agent=term]
  =/  m  (strand:spider ,~)
  ^-  form:m
  =*  loop  $
  ;<  [her=^ship =unix-effect]  bind:m  take-unix-effect
  ?:  (is-dojo-output:util ship her unix-effect "{(trip agent)} started")
    (pure:m ~)
  loop
::
++  start-agent
  |=  [=ship agent=term]
  =/  m  (strand:spider ,~)
  ^-  form:m
  =*  loop  $
  ;<  ~  bind:m  (dojo ship "|start {<agent>}")
  ;<  ~  bind:m  (wait-for-agent-start ship agent)
  (pure:m ~)
::
++  wait-for-goad
  |=  =ship
  =/  m  (strand:spider ,~)
  ^-  form:m
  =*  loop  $
  ;<  [her=^ship =unix-effect]  bind:m  take-unix-effect
  ?:  (is-dojo-output:util ship her unix-effect "p=%hood q=%bump")
    (pure:m ~)
  loop
::
++  start-group-agents
  |=  =ship
  =/  m  (strand:spider ,~)
  ^-  form:m
  ;<  ~  bind:m  (start-agent ship %group-store)
  ;<  ~  bind:m  (start-agent ship %group-listen-hook)
  ;<  ~  bind:m  (start-agent ship %group-proxy-hook)
  (pure:m ~)
--
=,  strand=strand:spider
^-  thread:spider
|=  args=vase
=/  m  (strand ,vase)
;<  az=tid:spider
  bind:m  start-azimuth
;<  ~  bind:m  (spawn az ~bud)
;<  ~  bind:m  (spawn az ~zod)
;<  ~  bind:m  (real-ship az ~bud)
;<  ~  bind:m  (wait-for-goad ~bud)
;<  ~  bind:m  (real-ship az ~zod)
;<  ~  bind:m  (wait-for-goad ~zod)
;<  ~  bind:m  (start-group-agents ~bud)
;<  ~  bind:m  (start-group-agents ~zod)
;<  ~  bind:m  (dojo ~bud ":group-store|create 'test-group'")
;<  ~  bind:m  (wait-for-output ~bud ">=")
;<  ~  bind:m  (dojo ~zod ":group-store|add ~bud 'test-group'")
;<  ~  bind:m  (wait-for-output ~zod ">=")
;<  ~  bind:m  (dojo ~zod ":group-listen-hook|add ~bud 'test-group'")
;<  ~  bind:m  (wait-for-output ~zod ">=")
(pure:m *vase)
