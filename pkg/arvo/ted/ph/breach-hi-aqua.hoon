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
;<  ~  bind:m  (send-hi ~bud ~dev)
;<  ~  bind:m  (breach-and-hear ~dev ~bud)
;<  ~  bind:m  (init-ship ~dev |)
;<  ~  bind:m  (send-hi ~bud ~dev)
(pure:m *vase)
