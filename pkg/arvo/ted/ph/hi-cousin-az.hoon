/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  t=drivers  bind:m  start-azimuth
;<  ~          bind:m  (spawn ~bud)
;<  ~          bind:m  (spawn ~marbud)
;<  ~          bind:m  (spawn ~dev)
;<  ~          bind:m  (spawn ~mardev)
;<  ~          bind:m  (init-ship ~bud |)
;<  ~          bind:m  (init-ship ~marbud |)
;<  ~          bind:m  (init-ship ~dev |)
;<  ~          bind:m  (init-ship ~mardev |)
;<  ~          bind:m  (send-hi ~mardev ~marbud)
;<  ~          bind:m  (end t)
(pure:m *vase)
