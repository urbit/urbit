/-  spider
/+  *ph-io, *ph-util
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  =bowl:spider  bind:m  get-bowl
;<  ~  bind:m  start-simple
;<  ~  bind:m  init-azimuth
;<  ~  bind:m  (spawn-aqua ~bud)
;<  ~  bind:m  (spawn-aqua ~dev)
;<  ~  bind:m  (init-ship ~bud)
;<  ~  bind:m  (init-ship ~dev)
;<  ~  bind:m  (send-hi ~bud ~dev)
;<  ~  bind:m  (breach-and-hear-aqua ~dev ~bud)
;<  ~  bind:m  (send-hi-not-responding ~bud ~dev)
;<  ~  bind:m  (init-ship ~dev)
;<  ~  bind:m  (wait-for-output ~bud "hi ~dev successful")
(pure:m *vase)
