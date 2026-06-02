/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=/  comet=ship  ~londeg-tirlys-somlyd-poltus--pintyn-tarbyl-bicnux-marbud
::
;<  t=drivers  bind:m  start-azimuth
;<  ~          bind:m  (spawn ~bud)
;<  ~          bind:m  (init-ship ~bud |)
;<  ~          bind:m  (spawn ~marbud)
;<  ~          bind:m  (init-ship ~marbud |)
;<  ~          bind:m  (init-comet comet)
;<  ~  bind:m  (send-hi comet ~bud)
;<  ~          bind:m  (send-hi ~bud comet)
;<  ~          bind:m  (spawn ~linnup-torsyx)
::  slow down retries if comet attestations come out of order
::    (XX this should be controlled by the stateful driver)
::
;<  ~          bind:m  (aqua-setup %ames-retry ~m2)
;<  ~          bind:m  (init-ship ~linnup-torsyx |)
::
;<  ~  bind:m  (send-hi comet ~linnup-torsyx)
;<  ~  bind:m
  (dojo ~linnup-torsyx "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
::
;<  ~          bind:m  (send-hi ~linnup-torsyx comet)
::  recover previous value
::
;<  ~          bind:m  (aqua-setup %ames-retry ~s1)
::
;<  ~          bind:m  (end t)
(pure:m *vase)
