/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=/  cores=(list ?(%mesa %ames))  ~[%mesa %ames]
|-  ^-  form:m
?~  cores
  (pure:m *vase)
;<  t=drivers  bind:m  start-azimuth
;<  ~          bind:m  (switch-network-core i.cores)
;<  ~          bind:m  (spawn ~bud)
;<  ~          bind:m  (spawn ~marbud)
;<  ~          bind:m  (spawn ~linnup-torsyx)
;<  ~          bind:m  (spawn ~dev)
;<  ~          bind:m  (init-ship ~bud |)
;<  ~          bind:m  (init-ship ~marbud |)
;<  ~          bind:m  (init-ship ~linnup-torsyx |)
::NOTE  only shortmoons supported, see also /ted/aqua/ames +lane-to-ship
;<  ~          bind:m  (init-moon ~torsyx-linnup-torsyx |)
;<  ~          bind:m  (send-hi ~bud ~torsyx-linnup-torsyx)
;<  ~          bind:m  (send-hi ~torsyx-linnup-torsyx ~marbud)
;<  ~          bind:m  (init-ship ~dev |)
::  XX  these hi's never come through! (not true anymore)
;<  ~          bind:m  (send-hi ~torsyx-linnup-torsyx ~dev)
;<  ~          bind:m  (send-hi ~dev ~torsyx-linnup-torsyx)
;<  ~          bind:m  (end t)
$(cores t.cores)
