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
  ?:  (is-dojo-output:util ship her unix-effect "activated app base/{(trip agent)}")
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
  (pure:m ~)
--
=,  strand=strand:spider
^-  thread:spider
|=  args=vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-azimuth
;<  ~  bind:m  (spawn ~bud)
;<  ~  bind:m  (spawn ~marbud)
;<  ~  bind:m  (spawn ~zod)
;<  ~  bind:m  (spawn ~marzod)
;<  ~  bind:m  (init-ship ~bud |)
;<  ~  bind:m  (init-ship ~marbud |)
;<  ~  bind:m  (wait-for-goad ~marbud)
;<  ~  bind:m  (init-ship ~zod |)
;<  ~  bind:m  (init-ship ~marzod |)
;<  ~  bind:m  (wait-for-goad ~marzod)
;<  ~  bind:m  (start-group-agents ~marbud)
;<  ~  bind:m  (start-group-agents ~marzod)
;<  ~  bind:m  (dojo ~marbud ":group-store|create 'test-group'")
;<  ~  bind:m  (wait-for-output ~marbud ">=")
;<  ~  bind:m  (sleep ~s1)
;<  ~  bind:m  (breach-and-hear ~marzod ~marbud)
;<  ~  bind:m  (init-ship ~marzod |)
;<  ~  bind:m  (wait-for-goad ~marzod)
;<  ~  bind:m  (start-group-agents ~marzod)
;<  ~  bind:m  (sleep ~s3)
;<  ~  bind:m  end
(pure:m *vase)
