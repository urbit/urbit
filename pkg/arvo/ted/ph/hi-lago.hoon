/-  spider
/+  ph=ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=+  ~(. ph %lago)
=|  tids=drivers
;<  =_tids  bind:m  start-lago
;<  ~  bind:m  (aqua-setup %ames-retry ~s2)
;<  ~  bind:m  (switch-network-core %ames)
;<  ~  bind:m  (init-ship ~bud &)
;<  ~  bind:m  (init-ship ~dev &)
;<  ~  bind:m  (init-ship ~marbud &)
;<  ~  bind:m  (init-ship ~dister-dozzod-marbud &)
:: ;<  ~  bind:m  (dojo ~bud "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
:: ;<  ~  bind:m  (dojo ~dev "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
;<  ~  bind:m  (send-hi ~bud ~dev)
;<  ~  bind:m  (send-hi ~bud ~marbud)
;<  ~  bind:m  (send-hi ~dister-dozzod-marbud ~marbud)
;<  ~  bind:m  (send-hi ~marbud ~dister-dozzod-marbud)
;<  ~  bind:m  (send-hi ~dister-dozzod-marbud ~dev)
::  stop all driver threads and clean up ames packets in %lago
::
;<  ~  bind:m  (end tids)
;<  ~  bind:m  (send-events ~[reset-routing/~])
::
(pure:m *vase)
