/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-azimuth
;<  ~  bind:m  (spawn ~bud)
;<  ~  bind:m  (spawn ~marbud)
;<  ~  bind:m  (init-ship ~bud |)
;<  ~  bind:m  (init-ship ~marbud |)
;<  ~  bind:m  (send-hi ~bud ~marbud)
;<  ~  bind:m  end
(pure:m *vase)
