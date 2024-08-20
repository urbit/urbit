/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-azimuth
;<  ~  bind:m  (spawn ~bud)
;<  ~  bind:m  (spawn ~marbud)
;<  ~  bind:m  (spawn ~dev)
;<  ~  bind:m  (spawn ~mardev)
;<  ~  bind:m  (init-ship ~bud |)
;<  ~  bind:m  (init-ship ~marbud |)
;<  ~  bind:m  (init-ship ~dev |)
;<  ~  bind:m  (init-ship ~mardev |)
;<  ~  bind:m  (send-hi ~mardev ~marbud)
;<  ~  bind:m  end
(pure:m *vase)
