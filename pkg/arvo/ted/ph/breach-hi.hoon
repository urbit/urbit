/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  az=tid:spider  bind:m
  start-azimuth
;<  ~  bind:m  (spawn az ~bud)
;<  ~  bind:m  (spawn az ~dev)
;<  ~  bind:m  (real-ship az ~bud)
;<  ~  bind:m  (real-ship az ~dev)
;<  ~  bind:m  (send-hi ~bud ~dev)
;<  ~  bind:m  (breach-and-hear az ~dev ~bud)
;<  ~  bind:m  (send-hi-not-responding ~bud ~dev)
;<  ~  bind:m  (real-ship az ~dev)
;<  ~  bind:m  (wait-for-output ~bud "hi ~dev successful")
;<  ~  bind:m  end-azimuth
(pure:m *vase)
