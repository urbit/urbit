/-  spider
/+  *ph-io, *ph-util, strandio
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  =bowl:spider  bind:m  get-bowl
;<  ~  bind:m  start-azimuth
;<  ~  bind:m  (spawn ~bud)
;<  ~  bind:m  (spawn ~dev)
;<  ~  bind:m  (init-ship ~bud |)
;<  ~  bind:m  (init-ship ~dev |)
;<  ~  bind:m  (sleep:strandio ~s10)
;<  ~  bind:m  (send-hi ~bud ~dev)
;<  ~  bind:m  (breach-and-hear ~dev ~bud)
;<  ~  bind:m  (send-hi-not-responding ~bud ~dev)
;<  ~  bind:m  (init-ship ~dev |)
;<  ~  bind:m  (wait-for-output ~bud "hi ~dev successful")
(pure:m *vase)
