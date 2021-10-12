/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-azimuth
;<  ~  bind:m  (spawn ~bud)
;<  ~  bind:m  (spawn ~marbud)
;<  ~  bind:m  (spawn ~linnup-torsyx)
;<  ~  bind:m  (spawn ~dev)
;<  ~  bind:m  (init-ship ~bud |)
;<  ~  bind:m  (init-ship ~marbud |)
;<  ~  bind:m  (init-ship ~linnup-torsyx |)
;<  ~  bind:m  (init-ship ~linnup-torsyx-linnup-torsyx |)
;<  ~  bind:m  (send-hi ~bud ~linnup-torsyx-linnup-torsyx)
;<  ~  bind:m  (send-hi ~linnup-torsyx-linnup-torsyx ~marbud)
;<  ~  bind:m  (init-ship ~dev |)
;<  ~  bind:m  (send-hi ~linnup-torsyx-linnup-torsyx ~dev)
;<  ~  bind:m  (send-hi ~dev ~linnup-torsyx-linnup-torsyx)
;<  ~  bind:m  end
(pure:m *vase)
