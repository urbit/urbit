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
::NOTE  only shortmoons supported, see also /ted/aqua/ames +lane-to-ship
;<  ~  bind:m  (init-moon ~torsyx-linnup-torsyx |)
;<  ~  bind:m  (send-hi ~bud ~torsyx-linnup-torsyx)
;<  ~  bind:m  (send-hi ~torsyx-linnup-torsyx ~marbud)
;<  ~  bind:m  (init-ship ~dev |)
::TODO  these hi's never come through!
;<  ~  bind:m  (send-hi ~torsyx-linnup-torsyx ~dev)
;<  ~  bind:m  (send-hi ~dev ~torsyx-linnup-torsyx)
;<  ~  bind:m  end
(pure:m *vase)
