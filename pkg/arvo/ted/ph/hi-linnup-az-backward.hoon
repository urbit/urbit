/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  t=drivers  bind:m  start-azimuth
;<  ~          bind:m  (spawn ~bud)
;<  ~          bind:m  (spawn ~marbud)
;<  ~          bind:m  (spawn ~linnup-torsyx)
;<  ~          bind:m  (init-ship ~linnup-torsyx |)
;<  ~          bind:m  (init-ship ~marbud |)
;<  ~          bind:m  (init-ship ~bud |)
;<  ~          bind:m  (send-hi ~linnup-torsyx ~marbud)
;<  ~          bind:m  (end t)
(pure:m *vase)
